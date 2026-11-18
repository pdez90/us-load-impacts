# Independent recomputation of script 03's outputs with scipy and whole arrays.
import sys, csv, json, numpy as np
from scipy.io import netcdf_file
sys.path.insert(0, "scripts"); m = __import__("03_isrm_fields")
raw, build = sys.argv[1], sys.argv[2]
try: f = netcdf_file(raw + "/isrm_v1.2.1.ncf", "r", mmap=False); get = lambda k: np.array(f.variables[k][:], dtype="f8")
except Exception:
    import netCDF4; f = netCDF4.Dataset(raw + "/isrm_v1.2.1.ncf"); get = lambda k: np.array(f.variables[k][:], dtype="f8")
n = get("PrimaryPM25").shape[1]
W, S, E, N, pop, mort = (get(k)[:n] for k in ("W", "S", "E", "N", "TotalPop", "MortalityRate"))
mids = [29.0, (253 + 391) / 2, (786 + 1049) / 2]
plants = list(csv.DictReader(open(build + "/plants.csv")))
cells = list(csv.DictReader(open(build + "/isrm_cells.csv")))
SR = {"pm25_t": get("PrimaryPM25"), "so2_t": get("pSO4"), "nox_t": get("pNO3")}
worst = 0
for col in [c for c in cells[0] if c.startswith("f_")]:
    reg, mix = col[2:].rsplit("_", 1); want = np.zeros(n)
    for p in plants:
        if p["subrgn"] != reg or not float(p["k_" + mix]) > 0: continue
        x, y = m.lcc(np.array([float(p["lon"])]), np.array([float(p["lat"])]))
        s = int(np.where((W <= x) & (x < E) & (S <= y) & (y < N))[0][0]); h = float(p["eff_height_m"])
        lo = max(i for i in range(3) if mids[i] <= h) if h >= mids[0] else 0
        fr = {0: 1.0} if h <= mids[0] else {lo: (mids[lo + 1] - h) / (mids[lo + 1] - mids[lo]), lo + 1: (h - mids[lo]) / (mids[lo + 1] - mids[lo])}
        for e, A in SR.items():
            for l, w in fr.items():
                want += float(p[e]) * float(p["k_" + mix]) * 1e6 * 1e12 / 31536000 * w * A[l, s]
    got = np.array([float(c[col]) for c in cells]); worst = max(worst, np.abs(got - want).max() / want.max())
print("fields: max relative error %.2e" % worst); assert worst < 1e-5
src = list(csv.DictReader(open(build + "/isrm_source.csv"))); pts = list(csv.DictReader(open(build + "/county_points.csv")))
r = src[5]; x, y = m.lcc(np.array([float(pts[5]["lon"])]), np.array([float(pts[5]["lat"])]))
s = int(np.where((W <= x) & (x < E) & (S <= y) & (y < N))[0][0]); assert s == int(r["cell"])
want = (SR["nox_t"][1, s] * pop * mort / 1e5).sum() * 1e12 / 31536000
print("on-site k_nox_l1: got %.6g want %.6g" % (float(r["k_nox_l1"]), want)); assert abs(float(r["k_nox_l1"]) / want - 1) < 1e-5
want = (SR["pm25_t"][0, s] * pop).sum() * 1e12 / 31536000; assert abs(float(r["e_pm25_l0"]) / want - 1) < 1e-5
meta = json.load(open(build + "/isrm_meta.json")); assert meta["sr_layers"] == [0, 3, 6]; assert np.allclose(meta["layer_mid_m"], mids)
print("check_isrm OK")
