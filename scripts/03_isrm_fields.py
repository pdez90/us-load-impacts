#!/usr/bin/env python3
# =============================================================================
# 03_isrm_fields.py  --  run the plant emissions through the InMAP
#                        Source-Receptor Matrix (ISRM v1.2.1)
# -----------------------------------------------------------------------------
# In : data-raw/isrm_v1.2.1.zip   (12.5 GB, Zenodo 10.5281/zenodo.3590127), read
#                                 IN PLACE: the matrix inside is ~165 GB, and
#                                 this script streams through it without
#                                 unzipping. An unzipped .ncf also works
#                                 (--isrm path/to/isrm_v1.2.1.ncf) and is faster.
#      build/plants.csv, build/county_points.csv   (from 02_build_plants.R)
# Out: build/isrm_cells.csv   one row per receptor cell: edges, population,
#                             baseline mortality, and for every subregion x mix
#                             the PM2.5 increment (ug/m3) per TWh/yr generated
#      build/isrm_source.csv  per county: population exposure and mortality
#                             weight per tonne/yr emitted there (on-site power)
#      build/isrm_meta.json   what was found in the file, and check totals
#
# Needs only Python 3.8+ and numpy. Streaming a zip takes two sequential passes
# (grid first, then the matrix rows); expect roughly 10-40 minutes each on a
# laptop. The first pass is cached in build/isrm_grid.npz.
#
# Method. The ISRM gives ug/m3 at every receptor per ug/s emitted in every
# source cell, for three release layers and five pathways. Used here:
# PrimaryPM25 <- primary PM2.5, pSO4 <- SO2, pNO3 <- NOx (NH3 and VOC from power
# plants are not modelled). Each plant's emissions go to the cell that contains
# it, split between the two release layers that bracket its effective height.
# The interpolation is written out below rather than taken from
# `inmap srpredict`, whose layer interpolation is inverted (inmap issue #121).
# =============================================================================
import argparse, csv, json, math, os, struct, sys, time, zipfile
import numpy as np

UGS_PER_TPY = 1e12 / (365.0 * 24 * 3600)          # ug/s per metric tonne/yr
PATHWAYS = {"pm25_t": "PrimaryPM25", "so2_t": "pSO4", "nox_t": "pNO3"}
GROUPS = ["WhiteNoLat", "Black", "Native", "Asian", "Latino"]
FALLBACK_MIDS = [29.0, 322.0, 917.5]              # 0-58, 253-391, 786-1049 m
NC_TYPES = {1: "i1", 2: "S1", 3: ">i2", 4: ">i4", 5: ">f4", 6: ">f8",
            7: "u1", 8: ">u2", 9: ">u4", 10: ">i8", 11: ">u8"}
T0 = time.time()


def log(msg):
    print("[%6.0fs] %s" % (time.time() - T0, msg), flush=True)


def die(msg):
    sys.exit("ERROR: " + msg)


# ---- NetCDF classic header (CDF-1, CDF-2 and CDF-5) ------------------------------------------
def parse_header(buf):
    if buf[:3] != b"CDF":
        die("not a NetCDF classic file (magic %r). ISRM v1.2.1 is NetCDF-3." % buf[:4])
    ver = buf[3]
    if ver not in (1, 2, 5):
        die("unsupported NetCDF classic version byte %d" % ver)
    pos = [4]

    def u32():
        v = struct.unpack_from(">I", buf, pos[0])[0]; pos[0] += 4; return v

    def u64():
        v = struct.unpack_from(">Q", buf, pos[0])[0]; pos[0] += 8; return v

    nn = u64 if ver == 5 else u32
    off = u64 if ver in (2, 5) else u32

    def name():
        n = nn(); s = buf[pos[0]:pos[0] + n]
        if len(s) < n: raise IndexError
        pos[0] += n + (-n) % 4
        return s.decode("utf-8", "replace")

    def atts():
        tag, n = u32(), nn(); out = {}
        for _ in range(n if tag else 0):
            nm = name(); t = u32(); ne = nn(); size = ne * np.dtype(NC_TYPES[t]).itemsize
            raw = buf[pos[0]:pos[0] + size]
            if len(raw) < size: raise IndexError
            pos[0] += size + (-size) % 4
            out[nm] = raw.decode("utf-8", "replace") if t == 2 else np.frombuffer(raw, NC_TYPES[t]).tolist()
        return out

    nn()                                              # numrecs
    tag, n = u32(), nn(); dims = []
    for _ in range(n if tag else 0):
        nm = name(); dims.append((nm, nn()))
    gatts = atts()
    tag, n = u32(), nn(); variables = {}
    for _ in range(n if tag else 0):
        nm = name(); nd = nn(); ids = [nn() for _ in range(nd)]
        va = atts(); t = u32(); nn(); begin = off()   # vsize is ignored: it overflows for >4 GB variables
        shape = tuple(dims[i][1] for i in ids)
        variables[nm] = {"dims": [dims[i][0] for i in ids], "shape": shape, "dtype": NC_TYPES[t],
                         "begin": begin, "atts": va}
    return {"version": ver, "dims": dict(dims), "vars": variables, "gatts": gatts}


# ---- sequential or random access to the matrix ----------------------------------------------
class Source:
    def __init__(self, path):
        self.path = path
        with open(path, "rb") as f:
            self.is_zip = f.read(4) == b"PK\x03\x04"
        if self.is_zip:
            self.zf = zipfile.ZipFile(path)
            ncf = [i for i in self.zf.infolist() if i.filename.lower().endswith((".ncf", ".nc"))]
            if not ncf:
                die("no .ncf inside %s: %s" % (path, [i.filename for i in self.zf.infolist()]))
            self.info = max(ncf, key=lambda i: i.file_size)
            self.size = self.info.file_size
            log("zip member %s, %.1f GB uncompressed (streamed, not extracted)" % (self.info.filename, self.size / 1e9))
        else:
            self.size = os.path.getsize(path)
            log("plain NetCDF file, %.1f GB (random access)" % (self.size / 1e9))
        self.f = None; self.pos = 0

    def open(self):
        if self.f: self.f.close()
        self.f = self.zf.open(self.info) if self.is_zip else open(self.path, "rb")
        self.pos = 0; self.next_log = 0.05

    def _read(self, n):
        parts = []; left = n
        while left:
            b = self.f.read(min(left, 1 << 26))
            if not b: die("unexpected end of ISRM data at byte %d" % self.pos)
            parts.append(b); left -= len(b); self.pos += len(b)
        return parts[0] if len(parts) == 1 else b"".join(parts)

    def read_at(self, offset, n):
        if not self.is_zip:
            self.f.seek(offset); self.pos = offset; return self._read(n)
        if offset < self.pos: die("internal: non-monotonic read in stream mode")
        while self.pos < offset:                       # decompress and discard
            b = self.f.read(min(offset - self.pos, 1 << 26))
            if not b: die("unexpected end of ISRM data while skipping")
            self.pos += len(b)
            if self.pos / self.size >= self.next_log:
                log("  ... %.0f%% of the matrix streamed" % (100 * self.pos / self.size)); self.next_log += 0.05
        return self._read(n)

    def header(self):
        n = 1 << 20
        while True:
            self.open()
            try:
                return parse_header(self._read(min(n, self.size)))
            except (IndexError, struct.error):
                if n >= self.size or n > (1 << 28): die("could not parse the NetCDF header")
                n *= 4


def read_var(src, v):
    count = int(np.prod(v["shape"])) if v["shape"] else 1
    raw = src.read_at(v["begin"], count * np.dtype(v["dtype"]).itemsize)
    return np.frombuffer(raw, v["dtype"]).astype(np.float64)


# ---- geometry -------------------------------------------------------------------------------
def lcc(lon, lat, lat1=33.0, lat2=45.0, lat0=40.0, lon0=-97.0, R=6370997.0):
    """Lambert conformal conic on a sphere: the InMAP grid's own projection."""
    p1, p2, p0 = map(math.radians, (lat1, lat2, lat0))
    n = math.log(math.cos(p1) / math.cos(p2)) / math.log(math.tan(math.pi / 4 + p2 / 2) / math.tan(math.pi / 4 + p1 / 2))
    F = math.cos(p1) * math.tan(math.pi / 4 + p1 / 2) ** n / n
    rho0 = R * F / math.tan(math.pi / 4 + p0 / 2) ** n
    rho = R * F / np.tan(np.pi / 4 + np.radians(lat) / 2) ** n
    th = n * np.radians(np.asarray(lon) - lon0)
    return rho * np.sin(th), rho0 - rho * np.cos(th)


def locate(x, y, W, S, E, N):
    """Index of the cell containing each point, -1 if none."""
    out = np.full(len(x), -1, dtype=np.int64)
    for a in range(0, len(x), 256):
        xs, ys = x[a:a + 256, None], y[a:a + 256, None]
        m = (W[None, :] <= xs) & (xs < E[None, :]) & (S[None, :] <= ys) & (ys < N[None, :])
        hit = m.any(1); out[a:a + 256][hit] = m.argmax(1)[hit]
    return out


def layer_fracs(h, mids):
    """Split a release at height h between the bracketing SR layers (linear in height)."""
    if h <= mids[0]: return {0: 1.0}
    if h >= mids[-1]: return {len(mids) - 1: 1.0}
    for i in range(len(mids) - 1):
        if mids[i] <= h <= mids[i + 1]:
            up = (h - mids[i]) / (mids[i + 1] - mids[i])      # nearer the upper layer -> more of it
            return {i: 1.0 - up, i + 1: up} if 0 < up < 1 else ({i: 1.0} if up == 0 else {i + 1: 1.0})


def find(names, patterns, what, required=True):
    low = {n.lower(): n for n in names}
    for p in patterns:
        if p.lower() in low: return low[p.lower()]
    for p in patterns:
        hits = [n for n in names if p.lower() in n.lower()]
        if hits: return hits[0]
    if required: die("no %s variable in the ISRM file. Per-cell variables present: %s" % (what, sorted(names)))
    return None


def read_csv(path):
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def fnum(v):
    try: return float(v)
    except (TypeError, ValueError): return float("nan")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--isrm", default="data-raw/isrm_v1.2.1.zip")
    ap.add_argument("--build", default="build")
    ap.add_argument("--plants", default=None, help="plant table (default <build>/plants.csv); 'subrgn' may hold any grouping, e.g. balancing authority")
    ap.add_argument("--tag", default="", help="suffix for the output file names, e.g. 'ba' -> isrm_cells_ba.csv")
    ap.add_argument("--sites", choices=["cells", "county", "none"], default="cells",
                    help="where to compute per-tonne source weights for on-site power: every grid cell (default; also writes the county file), county points only, or nowhere")
    ap.add_argument("--no-fields", action="store_true", help="skip the plant fields (only source weights)")
    ap.add_argument("--groups", default=None, help="build/bg_cell_groups.csv from 06_blockgroups.R: with --sites cells, also write "
                    "isrm_source_groups.csv, each site cell's per-tonne exposure and deaths for every population group")
    ap.add_argument("--test", action="store_true", help="skip the plausibility checks that only hold for the real ISRM")
    a = ap.parse_args()
    if not os.path.exists(a.isrm): die("%s not found (run scripts/01_download.sh)" % a.isrm)
    src = Source(a.isrm)
    H = src.header(); V = H["vars"]
    n = H["dims"].get("source")
    if not n or H["dims"].get("receptor") != n: die("unexpected dimensions %s" % H["dims"])
    nall = H["dims"].get("allcells", 0)
    sr_vars = sorted(k for k, v in V.items() if v["dims"] == ["layer", "source", "receptor"])
    cell_vars = sorted(k for k, v in V.items() if v["dims"] == ["allcells"])
    log("ISRM: CDF-%d, %d cells, %d layers; SR variables %s" % (H["version"], n, H["dims"]["layer"], sr_vars))
    for p in PATHWAYS.values():
        if p not in V: die("pathway %s missing; SR variables are %s" % (p, sr_vars))

    # ---- pass 1: grid, population, mortality (cached) -----------------------------------------
    cache = os.path.join(a.build, "isrm_grid.npz")
    stamp = "%s|%d|%d" % (os.path.basename(a.isrm), os.path.getsize(a.isrm), int(os.path.getmtime(a.isrm)))
    G = None
    if os.path.exists(cache):
        z = np.load(cache, allow_pickle=False)
        if str(z["stamp"]) == stamp and "lay" in z.files: G = {k: z[k] for k in z.files}; log("grid read from cache %s" % cache)
    if G is None:
        want = {"layers": "layers"}
        for e in "NSEW": want[e] = e
        want["pop"] = find(cell_vars, ["TotalPop"], "total population")
        want["mort"] = find(cell_vars, ["MortalityRate", "AllCause", "mort"], "baseline mortality")
        for g in GROUPS:
            nm = find(cell_vars, [g], g, required=False)
            if nm: want["g_" + g] = nm
        for k, pats in (("lh", ["LayerHeight"]), ("dz", ["Dz"]), ("lay", ["Layer"])):
            nm = find(cell_vars, pats, k, required=False)
            if nm and nm.lower() == pats[0].lower(): want[k] = nm
        log("pass 1: reading %s" % ", ".join("%s=%s" % kv for kv in want.items()))
        src.open(); G = {"stamp": np.array(stamp)}
        for k, nm in sorted(want.items(), key=lambda kv: V[kv[1]]["begin"]):
            G[k] = read_var(src, V[nm])
        G["names"] = np.array(json.dumps(want))
        os.makedirs(a.build, exist_ok=True); np.savez(cache, **G)
    want = json.loads(str(G["names"]))
    # The per-cell variables cover every model layer, and the upper layers have
    # fewer cells, so cells are picked by their Layer index. The ground layer
    # comes first and is the receptor grid.
    W, S, E, N = (G[e][:n] for e in "WSEN")
    pop, mort = G["pop"][:n], G["mort"][:n]
    layers = [int(x) for x in G["layers"]]
    if all(k in G for k in ("lh", "dz", "lay")):
        lay = np.rint(G["lay"]).astype(int); mids = []
        if not (lay[:n] == 0).all() or (lay == 0).sum() != n: die("the first %d cells are not exactly the ground layer" % n)
        for l in layers:
            ix = np.where(lay == l)[0]
            if len(ix) != n or not (np.allclose(G["W"][ix], W) and np.allclose(G["S"][ix], S)):
                die("model layer %d does not share the ground layer's %d cells" % (l, n))
            mids.append(float(np.mean(G["lh"][ix] + G["dz"][ix] / 2)))
    else:
        mids = FALLBACK_MIDS[:len(layers)]; log("WARNING: layer heights not in the file; using %s m" % mids)
    log("SR layers are model layers %s, mid-heights %s m" % (layers, [round(m, 1) for m in mids]))
    pw_mort = float((pop * mort).sum() / pop.sum())
    log("population %.1f million; population-weighted baseline mortality %.4g" % (pop.sum() / 1e6, pw_mort))
    mort_scale = 1e-5                                        # InMAP: deaths per 100,000 per year
    if 0.004 < pw_mort < 0.015: mort_scale = 1.0; log("mortality looks like a fraction per year, not per 100,000")
    elif not (400 < pw_mort < 1500) and not a.test:
        die("baseline mortality %.4g is neither per-100,000 nor a fraction: check variable '%s'" % (pw_mort, want["mort"]))
    if not a.test and not (2.4e8 < pop.sum() < 3.6e8): die("population %.3g is not a contiguous-US total" % pop.sum())
    if not a.test and mids != sorted(mids): die("layer heights not increasing: %s" % mids)
    mw = pop * mort * mort_scale                             # baseline deaths per cell per year
    GP = GD = None; gsets = []
    if a.groups:
        if a.sites != "cells": die("--groups needs --sites cells")
        gc = read_csv(a.groups)
        if len(gc) != n or any(int(r["cell"]) != i for i, r in enumerate(gc)): die("%s does not line up with the ISRM cells" % a.groups)
        gsets = [c[2:] for c in gc[0] if c.startswith("p_")]
        GP = np.array([[fnum(r["p_" + g]) for g in gsets] for r in gc]); GD = np.array([[fnum(r["d_" + g]) for g in gsets] for r in gc])
        log("population groups for site cells: %s" % ", ".join(gsets))

    # ---- which rows are needed ------------------------------------------------------------------
    tag = "_" + a.tag if a.tag else ""
    plants = [] if a.no_fields else read_csv(a.plants or os.path.join(a.build, "plants.csv"))
    regions = sorted({p["subrgn"] for p in plants})
    MIXES = [c[2:] for c in plants[0] if c.startswith("k_")] if plants else []      # avg, nb, and one per fuel group
    cols = ["%s_%s" % (r, m) for r in regions for m in MIXES]; col_ix = {c: i for i, c in enumerate(cols)}
    px, py = lcc(np.array([fnum(p["lon"]) for p in plants]), np.array([fnum(p["lat"]) for p in plants]))
    pcell = locate(px, py, W, S, E, N)
    rows = {}                                                # (var, layer index, source cell) -> {column: ug/s per TWh}
    tot = {k: 0.0 for k in PATHWAYS}; lost = {k: 0.0 for k in PATHWAYS}
    for p, c in zip(plants, pcell):
        for m in MIXES:
            k = fnum(p["k_" + m])
            if not k > 0: continue
            for ecol, var in PATHWAYS.items():
                e = fnum(p[ecol])
                if not e > 0: continue
                if m == "avg":
                    tot[ecol] += e
                    if c < 0: lost[ecol] += e
                if c < 0: continue
                for li, fr in layer_fracs(fnum(p["eff_height_m"]), mids).items():
                    d = rows.setdefault((var, li, int(c)), {})
                    ci = col_ix["%s_%s" % (p["subrgn"], m)]
                    d[ci] = d.get(ci, 0.0) + e * k * 1e6 * UGS_PER_TPY * fr
    for k in PATHWAYS:
        log("%s: %.4f%% of emissions fall outside the ISRM grid" % (k, 100 * lost[k] / max(tot[k], 1e-30)))
        if lost[k] / max(tot[k], 1e-30) > 0.01: die("more than 1% of emissions are outside the grid: projection problem?")
    if a.sites == "county":
        counties = read_csv(os.path.join(a.build, "county_points.csv"))
        cx, cy = lcc(np.array([fnum(c["lon"]) for c in counties]), np.array([fnum(c["lat"]) for c in counties]))
        ccell = locate(cx, cy, W, S, E, N)
        log("%d of %d county points fall in a grid cell" % ((ccell >= 0).sum(), len(counties)))
    elif a.sites == "cells":                                 # every grid cell is a possible site
        counties = [{"fips": str(i)} for i in range(n)]; ccell = np.arange(n)
    else:
        counties = []; ccell = np.zeros(0, dtype=np.int64)
    site_layers = list(range(min(2, len(mids))))             # on-site power: ground and first elevated layer
    site = {}
    for i, c in enumerate(ccell):
        if c < 0: continue
        for var in PATHWAYS.values():
            for li in site_layers: site.setdefault((var, li, int(c)), []).append(i)
    keys = sorted(set(rows) | set(site), key=lambda k: V[k[0]]["begin"] + (k[1] * n + k[2]) * n * np.dtype(V[k[0]]["dtype"]).itemsize)
    log("pass 2: %d matrix rows to read (%d plant rows, %d county rows)" % (len(keys), len(rows), len(site)))

    # ---- pass 2: the matrix ---------------------------------------------------------------------
    F = np.zeros((len(cols), n))
    pkeys = list(PATHWAYS)                                   # pm25_t, so2_t, nox_t
    Es = np.zeros((len(counties), len(site_layers), len(pkeys))); Ks = np.zeros_like(Es)
    if GP is not None:
        EG = np.zeros((len(counties), len(site_layers), len(pkeys), len(gsets)), dtype=np.float32); KG = np.zeros_like(EG)
    src.open(); done = 0
    for key in keys:
        var, li, s = key; v = V[var]; isz = np.dtype(v["dtype"]).itemsize
        row = np.frombuffer(src.read_at(v["begin"] + (li * n + s) * n * isz, n * isz), v["dtype"]).astype(np.float64)
        for ci, coef in rows.get(key, {}).items(): F[ci] += coef * row
        if key in site:
            pi = pkeys.index([k for k, vv in PATHWAYS.items() if vv == var][0])
            e, k = float(row @ pop) * UGS_PER_TPY, float(row @ mw) * UGS_PER_TPY
            for i in site[key]: Es[i, li, pi] = e; Ks[i, li, pi] = k
            if GP is not None:
                eg, kg = (row @ GP) * UGS_PER_TPY, (row @ GD) * UGS_PER_TPY
                for i in site[key]: EG[i, li, pi] = eg; KG[i, li, pi] = kg
        done += 1
        if not src.is_zip and done % 20000 == 0: log("  ... %d of %d rows" % (done, len(keys)))
    if not np.isfinite(F).all() or (F < -1e-12).any(): die("non-finite or negative concentrations")

    keep = [j for j, c in enumerate(cols) if F[j].any() or c.endswith(("_avg", "_nb"))]   # drop mixes with no emissions
    log("%d of %d fields are non-zero and kept" % (len(keep), len(cols)))
    F = F[keep]; cols = [cols[j] for j in keep]

    # ---- write ----------------------------------------------------------------------------------
    gnames = [g for g in GROUPS if "g_" + g in G]
    if not a.no_fields:
      with open(os.path.join(a.build, "isrm_cells%s.csv" % tag), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["cell", "W", "S", "E", "N", "pop", "mort_per_person"] + ["pop_" + g for g in gnames] + ["f_" + c for c in cols])
        for i in range(n):
            w.writerow([i, "%.3f" % W[i], "%.3f" % S[i], "%.3f" % E[i], "%.3f" % N[i], "%.6g" % pop[i], "%.6g" % (mort[i] * mort_scale)]
                       + ["%.6g" % G["g_" + g][i] for g in gnames] + ["%.6g" % F[j, i] for j in range(len(cols))])
    short = {"pm25_t": "pm25", "so2_t": "so2", "nox_t": "nox"}
    def write_sources(name, ids, cellix, rowix):
        with open(os.path.join(a.build, name), "w", newline="") as f:
            w = csv.writer(f)
            w.writerow(["fips", "cell"] + ["%s_%s_l%d" % (q, short[p], li) for li in site_layers for p in pkeys for q in ("e", "k")])
            for sid, c, i in zip(ids, cellix, rowix):
                w.writerow([sid, int(c)] + (["%.6g" % x for li in site_layers for pi in range(len(pkeys)) for x in (Es[i, li, pi], Ks[i, li, pi])] if i >= 0
                                             else ["0"] * (2 * len(site_layers) * len(pkeys))))
    if a.sites == "county":
        write_sources("isrm_source%s.csv" % tag, [c["fips"] for c in counties], ccell, range(len(counties)))
    elif a.sites == "cells":                                 # all cells, plus the county file read off the cell holding each county point
        write_sources("isrm_source_cells%s.csv" % tag, range(n), range(n), range(n))
        cp = read_csv(os.path.join(a.build, "county_points.csv"))
        kx, ky = lcc(np.array([fnum(c["lon"]) for c in cp]), np.array([fnum(c["lat"]) for c in cp])); kc = locate(kx, ky, W, S, E, N)
        log("%d of %d county points fall in a grid cell" % ((kc >= 0).sum(), len(cp)))
        write_sources("isrm_source%s.csv" % tag, [c["fips"] for c in cp], kc, kc)
    if GP is not None:                                       # one row per site cell: e_<group>_<pollutant>_l<layer>, k_... (per tonne/yr)
        with open(os.path.join(a.build, "isrm_source_groups%s.csv" % tag), "w", newline="") as f:
            w = csv.writer(f)
            hdr = [(q, g, pi, li) for q in ("e", "k") for g in range(len(gsets)) for li in range(len(site_layers)) for pi in range(len(pkeys))]
            w.writerow(["cell"] + ["%s_%s_%s_l%d" % (q, gsets[g], short[pkeys[pi]], site_layers[li]) for q, g, pi, li in hdr])
            for i in range(n):
                w.writerow([i] + ["%.4g" % (EG if q == "e" else KG)[i, li, pi, g] for q, g, pi, li in hdr])
        log("wrote isrm_source_groups%s.csv: %d site cells x %d groups" % (tag, n, len(gsets)))
    if a.no_fields:                                          # source weights only: keep the metadata from the full run
        log("--no-fields: %s left as it was" % ("isrm_meta%s.json" % tag)); return
    meta = {"isrm_file": os.path.basename(a.isrm), "cdf_version": H["version"], "n_cells": int(n), "sr_layers": layers,
            "layer_mid_m": mids, "sr_variables": sr_vars, "cell_variables": cell_vars, "used": want,
            "pathways": PATHWAYS, "groups": gnames, "population": float(pop.sum()),
            "pw_baseline_mortality_per_100k": pw_mort * mort_scale * 1e5,
            "emissions_outside_grid_share": {k: lost[k] / max(tot[k], 1e-30) for k in PATHWAYS},
            "site_layers": site_layers, "units": "ug/m3 per TWh/yr generated; e_/k_ per tonne/yr emitted",
            "per_twh": {c: {"pw_ugm3": float(F[j] @ pop / pop.sum()), "k": float(F[j] @ mw)} for j, c in enumerate(cols)},
            "elapsed_s": round(time.time() - T0)}
    with open(os.path.join(a.build, "isrm_meta%s.json" % tag), "w") as f: json.dump(meta, f, indent=1)
    log("wrote outputs with suffix '%s' in %s: %d cells x %d fields, %d source sites" % (tag, a.build, n, len(cols), len(counties)))
    if meta["per_twh"]:
        worst = max(meta["per_twh"].items(), key=lambda kv: kv[1]["k"])
        log("largest mortality weight: %s, %.3g deaths per TWh at Krewski (beta 0.00583)" % (worst[0], worst[1]["k"] * math.log(1.06) / 10))


if __name__ == "__main__":
    main()
