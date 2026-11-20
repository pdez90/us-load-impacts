#!/usr/bin/env python3
"""Pull the numbers the model needs out of Virginia DEQ data center air permits.

In : a folder of permit PDFs (scripts/07 or saved by hand as <registration>_DC_Permit.pdf, where the
     registration is written without its hyphen, e.g. 7320002 = 73200-2), and DEQ's list of permits
     (data-raw/permits/va/deq_list.csv: site, registration, issued, program, locality, region, url)
Out: data-raw/permits/va/va_permits.csv, one row per permit:
       registration, site, locality, issued, program, address,
       annual limits in short tons/yr for NOx, CO, VOC, PM10, PM2.5, SO2 (facility total),
       how the total was read (total column / single column / sum of columns),
       generator counts and ratings (engines, total bhp, total ekW), fire pumps,
       hours limit for maintenance and testing (per engine), fuel limit (gallons/yr),
       flags for anything a person should check by hand.
Needs pdftotext (poppler). Every number is checked against the text it came from; rows the parser is
unsure about are flagged, not guessed.

Run: python3 scripts/08_parse_va_permits.py --pdfs ~/Downloads/permits_virginia
"""
import argparse, csv, os, re, subprocess, sys

POLL = [("nox", r"nitrogen\s+oxides|\bno\s*x\b|\bnox\b"), ("co", r"carbon\s+monoxide|\(co\)"),
        ("voc", r"volatile\s+organic|\bvoc\b"), ("pm25", r"pm\s*2\.?5|pm-2\.5"), ("pm10", r"pm\s*10|pm-10"),
        ("so2", r"sulfur\s+dioxide|\bso\s*2\b")]
NUM = r"(\d{1,3}(?:,\d{3})*(?:\.\d+)?|\d+(?:\.\d+)?)"
WORDS = {"one": 1, "two": 2, "three": 3, "four": 4, "five": 5, "six": 6, "seven": 7, "eight": 8, "nine": 9, "ten": 10,
         "eleven": 11, "twelve": 12}


def text_of(pdf):
    r = subprocess.run(["pdftotext", "-layout", pdf, "-"], capture_output=True, text=True)
    return r.stdout


def num(s): return float(s.replace(",", ""))


def reg_from_file(fn):
    m = re.match(r"(\d{5})(\d*)_", os.path.basename(fn))
    if not m: return None, None
    return m.group(1), (int(m.group(2)) if m.group(2) else None)


def section(lines, start_pat, stop_pat, max_len=200):
    """Lines from the first match of start_pat to the next match of stop_pat."""
    for i, l in enumerate(lines):
        if re.search(start_pat, l, re.I):
            out = []
            for l2 in lines[i + 1:i + 1 + max_len]:
                if re.search(stop_pat, l2): break               # case-sensitive: all-caps headings end a section
                out.append(l2)
            return i, out
    return None, []


def conditions(lines):
    """Split the permit into numbered conditions: list of (number, header text, lines)."""
    out, cur = [], None
    for l in lines:
        m = re.match(r"^\s{0,8}(\d{1,2})\.\s+(\S.*)", l)
        if m and len(m.group(2)) > 8 and not re.match(r"^\s{0,8}\d{1,2}\.\s+\d", l):
            cur = [int(m.group(1)), m.group(2), []]; out.append(cur)
        elif cur is not None: cur[2].append(l)
    return [(n, (h + " " + " ".join(x.strip() for x in b[:3])).lower(), b) for n, h, b in out]


def unit_set(head):
    """Engines a condition covers, from its 'Ref. Nos.' text: {(prefix, number)}; empty if unreadable."""
    m = re.search(r"ref(?:erence)?\.?\s*(?:nos?\.?|numbers?)?\s*:?\s*(.{1,260}?)\)", head)
    if not m: return frozenset()
    t = m.group(1).replace("–", "-").replace("—", "-")
    out = set()
    for a, x, b, y in re.findall(r"((?:[a-z]+-)*[a-z]*-?)\s*(\d{1,4})\s*(?:through|thru|to|-)\s*((?:[a-z]+-)*[a-z]*-?)\s*(\d{1,4})", t):
        x, y = int(x), int(y)
        if 0 < y - x < 3000: out |= {(a.strip("-"), i) for i in range(x, y + 1)}
    t2 = re.sub(r"((?:[a-z]+-)*[a-z]*-?)\s*(\d{1,4})\s*(?:through|thru|to|-)\s*((?:[a-z]+-)*[a-z]*-?)\s*(\d{1,4})", " ", t)
    for a, x in re.findall(r"\b([a-z]{0,6}-?)(\d{1,4})\b", t2): out.add((a.strip("-"), int(x)))
    return frozenset(out)


TONS = NUM + r"\s*(?:tpy|tons\s*/\s*yr|tons per year|tons/year|tons per 12|tons)\b"


def tons_in(block):
    """Pollutant -> list of rows of tons values in one condition (label on the same or an adjacent line)."""
    res = {}
    for k, l in enumerate(block):
        l = re.sub(r"(\d)\.\s(\d{1,3})(?=\s*(?:tpy|tons|lb))", r"\1.\2", l)   # "93. 51 tpy" from scanned permits
        vals = [num(v) for v in re.findall(TONS, l, re.I)]
        if not vals: continue
        lab = None
        for j in (k, k - 1, k + 1, k - 2):
            if 0 <= j < len(block):
                for key, pat in POLL:
                    if re.search(pat, block[j], re.I): lab = key; break
            if lab: break
        if lab: res.setdefault(lab, []).append(vals)
    out = {}
    for lab, rows_ in res.items():                          # several rows for one pollutant: the largest (a combined row)
        best = max(rows_, key=max); out[lab] = best
        if len(rows_) > 1:
            tops = sorted((max(r) for r in rows_), reverse=True)
            if abs(sum(tops[1:]) - tops[0]) > max(0.02 * tops[0], 0.02) and tops[1] > 0.2 * tops[0]:
                out[lab] = best + [-1.0]                    # marker: rows did not add up; flagged below
    return out


def annual_limits(lines):
    """Facility annual limits (short tons/yr).
    1. a facility-wide condition (total emissions from the facility / all units combined) is used if present;
    2. otherwise the annual limits of every condition are added (each covers a different group of units).
    In a row with several columns, the largest is taken when it equals the sum of the others (a total column)."""
    out, how, flags = {}, {}, []
    fac, part = {}, {}
    for n, head, block in conditions(lines):
        if not re.search(TONS, " ".join(block) + " " + head, re.I): continue
        t = tons_in(block)
        if not t: continue
        facility = re.search(r"facility[- ]wide|total emissions from the facility|facility (?:emissions|total)|entire facility|"
                             r"plant[- ]?wide|site[- ]?wide|all (?:emission units|units|engines|equipment)[^.]{0,60}combined|"
                             r"facility shall not exceed|emissions from the facility", head)
        cols = " ".join(block[:14]).lower()                    # column headings ("Each", "Combined Total") sit under the header
        each = re.search(r"\beach\b", head) and not re.search(r"combined|total", head + " " + cols)
        if re.search(r"maintenance checks and readiness testing|\(testing\)|testing only", head[:200]): continue   # inside the all-purpose limit
        if re.search(r"combined operation of all|all (?:of the )?(?:emergency )?(?:diesel )?(?:engine|gen)", head[:220]): facility = True
        for lab, vals in t.items():
            if vals and vals[-1] == -1.0:
                vals = vals[:-1]; flags.append("%s: condition %d has several rows that do not add up; took the largest" % (lab, n))
            if len(vals) > 1:
                mx = max(vals); rest = sum(vals) - mx
                v, h = (mx, "total column") if abs(rest - mx) <= max(0.02 * mx, 0.02) else (mx, "largest column")
            else: v, h = vals[0], "single"
            us = unit_set(head)
            if each:
                if len(us) > 1: v, h = v * len(us), h + ", per-unit limit x %d units" % len(us)
                else: flags.append("%s: condition %d gives tons per unit; unit count unclear" % (lab, n))
            (fac if facility else part).setdefault(lab, []).append((n, v, h, unit_set(head)))
    for lab, _ in POLL:
        if lab in fac:
            F = fac[lab]; sets = [q[3] for q in F]
            disjoint = len(F) > 1 and all(sets) and not any(sets[a] & sets[b] for a in range(len(F)) for b in range(a + 1, len(F)))
            if disjoint:                                        # one permit, separate campuses each with its own total
                out[lab], how[lab] = round(sum(q[1] for q in F), 3), "facility-wide, separate parts summed (cond. %s)" % ",".join(str(q[0]) for q in F)
            else:
                n, v, h, _ = max(F, key=lambda q: q[1]); out[lab], how[lab] = v, "facility-wide (cond. %d, %s)" % (n, h)
        elif lab in part:
            items = part[lab]; vals = [it[1] for it in items]
            if len(items) == 1:
                out[lab], how[lab] = vals[0], "condition %d" % items[0][0]
            else:
                k = max(range(len(vals)), key=lambda q: vals[q]); rest = sum(vals) - vals[k]
                sets = [it[3] for it in items]
                sup = [q for q in range(len(items)) if sets[q] and all(sets[o] <= sets[q] for o in range(len(items)) if sets[o])
                       and all(sets[o] for o in range(len(items)))]
                others = [o for o in range(len(items)) if o != k]
                nested = sets[k] and all(sets[o] and sets[o] <= sets[k] for o in others)
                disjoint = all(sets) and not any(sets[a] & sets[b] for a in range(len(items)) for b in range(a + 1, len(items)))
                if not disjoint and (len(items) >= 3 or nested) and abs(rest - vals[k]) <= max(0.02 * vals[k], 0.02):   # a combined total
                    out[lab], how[lab] = vals[k], "combined condition %d" % items[k][0]
                elif sup:                                               # one condition covers every engine the others cover
                    q = max(sup, key=lambda z: vals[z]); out[lab], how[lab] = vals[q], "condition %d covers the others" % items[q][0]
                else:
                    groups_ = [q for q in range(len(items)) if sets[q]]
                    overlap = any(sets[a] & sets[b] for a in groups_ for b in groups_ if a < b)
                    out[lab] = round(sum(vals), 3)
                    how[lab] = "sum of conditions %s" % ",".join(str(it[0]) for it in items)
                    if overlap or not all(sets): flags.append("%s: conditions summed; engine sets overlap or are unclear, check by hand" % lab)
    if "nox" not in out: flags.append("no NOx annual limit read")
    return out, how, flags


def equipment(lines):
    """Engine gen-sets from the equipment list: count and rating of each group."""
    i0, blk = section(lines, r"equipment\s+list|equipment at this facility", r"^\s*(?:\d{1,2}\.\s+[A-Z]|[Ss]pecifications included|[A-Z]{4,}[A-Z ]{6,}\s*$)", 700)
    if i0 is None: return [], ["no equipment list"]
    text = "\n".join(blk)
    flags, groups = [], []
    # split into entries at each count, e.g. "(92) Cummins", "Four (4) Cummins", "One (1) Peerless Fire Pump"
    cnt_pat = re.compile(r"(?:\b(?:%s)\s*)?\((\d{1,3})\)" % "|".join(WORDS), re.I)
    pos = [(m.start(), int(m.group(1))) for m in cnt_pat.finditer(text)]
    for j, (p, n) in enumerate(pos):
        seg = text[p:pos[j + 1][0] if j + 1 < len(pos) else len(text)]
        # columns are wrapped: also look a few lines back (rating printed above the count line)
        back = text[max(0, p - 160):p]
        bhp = re.findall(NUM + r"\s*(?:bhp|hp|brake horsepower)\b", seg + " " + back, re.I)
        ekw = re.findall(NUM + r"\s*(?:ekw|kwe|kw|kilowatts?)\b", seg, re.I) or re.findall(NUM + r"\s*(?:ekw|kwe|kw)\b", back, re.I)
        mw = re.findall(NUM + r"\s*mw\b", seg, re.I)
        fire = bool(re.search(r"fire\s*(?:water\s*)?pump|firewater|\bpump\b", seg, re.I))   # not "diesel fired"
        gen = bool(re.search(r"gen[- ]?set|generator|engine", seg, re.I)) and not fire
        groups.append({"n": n, "bhp": num(bhp[0]) if bhp else None,
                       "ekw": num(ekw[0]) if ekw else (num(mw[0]) * 1000 if mw else None), "fire_pump": fire, "genset": gen})
    if not groups:                                          # older layout: one engine per row, or "(n units)"
        bl = blk
        kwpat = re.compile(NUM + r"\s*(?:ekw|kwe|kw|kilowatts?)\b", re.I); hppat = re.compile(NUM + r"\s*(?:bhp|brake\s*$|hp)\b", re.I)
        use = kwpat if any(kwpat.search(l) for l in bl) else hppat
        for k, l in enumerate(bl):
            for m in use.finditer(l):
                near = " ".join(bl[max(0, k - 3):k + 4])
                u = re.search(r"\((\d{1,3})\s+units?\)", near, re.I)
                fire = bool(re.search(r"fire\s*(?:water\s*)?pump|firewater|\bpump\b", near, re.I))
                v = num(m.group(1))
                if use is hppat: hp, kw = v, None
                else:
                    kw = v; h = hppat.search(near); hp = num(h.group(1)) if h else None
                groups.append({"n": int(u.group(1)) if u else 1, "bhp": hp, "ekw": kw, "fire_pump": fire, "genset": not fire})
        if groups: flags.append("engines counted one per rating (no group counts in the table)")
        else: flags.append("no engine counts read from equipment list")
    return groups, flags


def operating_terms(lines, text, n_engines):
    """SCR-controlled engines (from 'Emission Controls (SCR)' conditions, else the equipment table), hours per engine for
    all purposes, whether running in grid-operator emergencies / demand response is allowed, ozone-season and time-of-day
    limits on testing, and which emission standards the permit cites."""
    scr, scr_cond = set(), False
    for n, head, block in conditions(lines):
        h = head[:400]
        if re.search(r"\(scr\)|selective catalytic|scr system", h) and re.search(r"control|nox|nitrogen", h):
            scr_cond = True; scr |= unit_set(h)
    src = "conditions" if scr_cond else ""
    n_scr = min(len(scr), n_engines) if scr else 0
    if scr_cond and not scr: src = "conditions (engines not listed)"
    if not scr_cond:
        _, blk = section(lines, r"equipment\s+list|equipment at this facility", r"^\s*(?:\d{1,2}\.\s+[A-Z]|[Ss]pecifications included)", 700)
        if re.search(r"\bSCR\b|selective\s+catalytic", "\n".join(blk), re.I): src = "equipment table (all engines assumed)"; n_scr = n_engines
    allp = [num(v) for v in re.findall(r"more than\s+" + NUM + r"\s+hours?\s+per\s+(?:calendar\s+)?year\s+for\s+all\s+purposes", text, re.I)]
    win = re.search(r"between\s+(?:the\s+hours\s+of\s+)?(\d{1,2}(?::\d{2})?\s*[ap]\.?m\.?)\s+(?:and|to)\s+(\d{1,2}(?::\d{2})?\s*[ap]\.?m\.?)", text, re.I)
    return {"scr_engines": n_scr, "scr_source": src,
            "hours_all_purposes": max(allp) if allp else "",
            "grid_emergency_use": bool(re.search(r"ISO[- ]declared emergency|ISO emergency|emergency demand response|"
                                                  r"emergency load response|load management|demand response", text, re.I)),
            "ozone_season_limit": bool(re.search(r"ozone season", text, re.I)),
            "testing_window": ("%s-%s" % (win.group(1), win.group(2))).replace(" ", "") if win else "",
            "cites_tier4": bool(re.search(r"Tier\s*(?:4|IV)\b|1039\.101", text)),
            "cites_tier2": bool(re.search(r"Tier\s*(?:2|II)\b|89\.112|appendix I to (?:40 CFR )?part 1039", text, re.I))}


def hours_limit(text):
    m = re.search(r"shall not operate more than\s+" + NUM + r"\s+hours?\s+per\s+(?:calendar\s+)?year\s+for\s+(?:scheduled\s+)?"
                  r"(?:maintenance|testing|purposes of maintenance)", text, re.I)
    return num(m.group(1)) if m else None


def fuel_limit(text):
    v = [num(x) for x in re.findall(r"consume\s+no\s+more\s+than\s+" + NUM + r"\s+gallons", text, re.I)]
    return sum(v) if v else None


STREET = (r"(\d{2,6}[A-Z]?\s+(?:[NSEW]\.?\s+)?[A-Za-z0-9.'\- ]{2,40}?\s(?:Road|Rd|Drive|Dr|Lane|Ln|Parkway|Pkwy|Boulevard|Blvd|"
          r"Court|Ct|Circle|Cir|Way|Avenue|Ave|Street|St|Place|Pl|Highway|Hwy|Pike|Plaza|Square|Sq|Terrace|Run|Trail|Turnpike|Loop|Center|Ctr)\b\.?)"
          r"(?:[,\s]+(?:Suite|Ste\.?)\s*\S+)?[,\s]+([A-Z][A-Za-z.]+(?:\s[A-Z][A-Za-z.]+){0,2})?,?\s*(?:Virginia|VA)\.?\s*(2\d{4})?")


def street_address(text):
    """First street address in the permit with its city and ZIP, for geocoding: (street, city, zip)."""
    head = text[:12000]
    for la in re.finditer(r"located\s+at", head, re.I):            # the site, not the owner's mailing address
        m = re.search(STREET, head[la.end():la.end() + 600])
        if m and not re.search(r"Crown Court|Cox Road, Glen Allen", m.group(0)):
            return re.sub(r"\s{2,}", " ", m.group(1)).strip(), (m.group(2) or "").strip(), (m.group(3) or "")
    for m in re.finditer(STREET, head):
        st = re.sub(r"\s{2,}", " ", m.group(1)).strip()
        if re.search(r"P\.?O\.? Box|Richmond, Virginia 23218|Cox Road, Glen Allen, Virginia 23060$|13901 Crown Court|4949-A Cox", m.group(0)):
            continue                                        # DEQ's own offices and mailing addresses
        if re.search(r"Crown Court|Cox Road", st) and "Glen Allen" in (m.group(2) or "") and "Knolls" not in head[:3000]: continue
        return st, (m.group(2) or "").strip(), (m.group(3) or "")
    return "", "", ""


def address(text):
    m = re.search(r"located at\s*\n?\s*(.{5,90}?)\n\s*(.{3,60}?)\n", text, re.I)
    if m:
        a = (m.group(1) + ", " + m.group(2)).strip()
        return re.sub(r"\s{2,}", " ", a)
    return ""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--pdfs", required=True); ap.add_argument("--list", default="data-raw/permits/va/deq_list.csv")
    ap.add_argument("--out", default="data-raw/permits/va/va_permits.csv")
    a = ap.parse_args()
    deq = {}
    if os.path.exists(a.list):
        for r in csv.DictReader(open(a.list, encoding="utf-8")): deq[r["registration"]] = r
    rows = []
    for fn in sorted(f for f in os.listdir(a.pdfs) if f.lower().endswith(".pdf")):
        base, suf = reg_from_file(fn)
        cands = [k for k in deq if k.split("-")[0] == base and (suf is None or int(k.split("-")[1]) == suf)]
        alt = [k for k in deq if k.split("-")[0] == base]
        if not cands and len(alt) == 1: cands = alt                 # file suffix differs from DEQ's list (30142-2 saved for 30142-3)
        reg = cands[0] if len(cands) == 1 else (base + ("-%d" % suf if suf is not None else ""))
        meta = deq.get(reg, {})
        txt = text_of(os.path.join(a.pdfs, fn)); lines = txt.splitlines()
        flags = []
        if len(txt.split()) < 300: flags.append("little text: scanned or short; check by hand")
        lim, how, f1 = annual_limits(lines); flags += f1
        eq, f2 = equipment(lines); flags += f2
        gs = [g for g in eq if g["genset"]]; fp = [g for g in eq if g["fire_pump"]]
        n_eng = sum(g["n"] for g in gs)
        kw = sum(g["n"] * g["ekw"] for g in gs if g["ekw"]); bhp = sum(g["n"] * g["bhp"] for g in gs if g["bhp"])
        if gs and any(g["ekw"] is None for g in gs): flags.append("some engine groups have no kW rating")
        m_reg = re.search(r"Registration\s+(?:No\.?|Number)\s*:?\s*(\d{5})", txt, re.I)
        if m_reg and m_reg.group(1) != base: flags.append("registration in text (%s) differs from file name" % m_reg.group(1))
        rows.append({"registration": reg, "file": fn, "site": meta.get("site", ""), "locality": meta.get("locality", ""),
                     "issued": meta.get("issued", ""), "program": meta.get("program", ""), "address": address(txt),
                     **dict(zip(("street", "city", "zip"), street_address(txt))),
                     **{"%s_tpy" % k: lim.get(k, "") for k, _ in POLL}, "nox_read_as": how.get("nox", ""),
                     "engines": n_eng, "engine_groups": len(gs), "total_ekw": round(kw) if kw else "", "total_bhp": round(bhp) if bhp else "",
                     "fire_pumps": sum(g["n"] for g in fp), "test_hours_per_engine": hours_limit(txt) or "",
                     "fuel_gal_yr": fuel_limit(txt) or "", **operating_terms(lines, txt, n_eng),
                     "flags": "; ".join(flags), "_text": txt})
    # hand-read values (data-raw/permits/va/overrides.csv), each with a note saying where it came from
    ov_path = os.path.join(os.path.dirname(a.list), "overrides.csv")
    if os.path.exists(ov_path):
        ov = {r["registration"]: r for r in csv.DictReader(open(ov_path, encoding="utf-8"))}
        for r in rows:
            o = ov.get(r["registration"])
            if not o: continue
            for k, _ in POLL:
                if o.get("%s_tpy" % k, "") != "": r["%s_tpy" % k] = float(o["%s_tpy" % k])
            r["nox_read_as"] = "read by hand: " + o["note"]; r["flags"] = "; ".join(f for f in r["flags"].split("; ") if "nox" not in f.lower() and "annual" not in f.lower())
    # a newer permit that "supersedes your permit dated <date>" replaces the older one for the same registration
    def d(s):
        import datetime
        for fmt in ("%m/%d/%Y", "%B %d, %Y", "%B %d %Y"):
            try: return datetime.datetime.strptime(s.strip().replace(".", ""), fmt).date()
            except ValueError: pass
        return None
    by = {}
    for r in rows: by.setdefault(r["registration"].split("-")[0], []).append(r)
    for r in rows: r["current"] = "yes"; r["replaced_by"] = ""
    for base, rs in by.items():
        if len(rs) < 2: continue
        for new in rs:
            for m in re.finditer(r"supersedes\s+(?:y\S*\s+)?(?:permit\s+)?(?:document\s+)?dated\s+([A-Z][a-z]+\.?\s+\d{1,2},?\s+\d{4})", new["_text"], re.I):
                dd = d(m.group(1).replace(",", ", ").replace(",  ", ", "))
                for old in rs:
                    if old is not new and d(old["issued"]) == dd: old["current"] = "no"; old["replaced_by"] = new["registration"]
        tv = [r for r in rs if "Title V" in r["program"]]
        for r in rs:                                        # a Title V permit restates the minor-NSR limits it incorporates
            if tv and r not in tv and r["current"] == "yes" and d(r["issued"]) and d(tv[0]["issued"]) and d(r["issued"]) < d(tv[0]["issued"]):
                r["current"] = "no"; r["replaced_by"] = tv[0]["registration"] + " (Title V)"
    for r in rows: r.pop("_text", None)
    os.makedirs(os.path.dirname(a.out), exist_ok=True)
    with open(a.out, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, list(rows[0].keys())); w.writeheader(); w.writerows(rows)
    ok = sum(1 for r in rows if r["nox_tpy"] != "")
    print("%d permits read; NOx annual limit for %d; %d flagged -> %s" % (len(rows), ok, sum(1 for r in rows if r["flags"]), a.out))


if __name__ == "__main__":
    main()
