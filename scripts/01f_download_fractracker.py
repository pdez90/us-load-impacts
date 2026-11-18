#!/usr/bin/env python3
"""Download FracTracker Alliance's Open U.S. Data Centers Tracker.

Source  : https://www.fractracker.org/data-centers/  (map: experience.arcgis.com/experience/5a4d072ad01449bba5698a80103fb909)
Service : the tracker's public "Data Center Summary" layer (data_centers_v4_agol_all), read page by page
Terms   : FracTracker says the dataset "can be downloaded and shared freely for non-commercial use
          with credit to FracTracker". Credit: FracTracker Alliance, Open U.S. Data Centers Tracker.
Output  : data-raw/facilities/fractracker_data_centers.csv  (+ fractracker_SOURCE.txt)

Run from the repo root:  python3 scripts/01f_download_fractracker.py
"""
import csv, datetime, json, os, sys, urllib.parse, urllib.request

URL = ("https://services.arcgis.com/jDGuO8tYggdCCnUJ/arcgis/rest/services/"
       "data_centers_v4_agol_all/FeatureServer/0/query")
ROOT = os.environ.get("USLOAD_ROOT", ".")
OUT = os.path.join(os.environ.get("USLOAD_RAW", os.path.join(ROOT, "data-raw")), "facilities")


def get(params):
    q = urllib.parse.urlencode(dict(params, f="json"))
    with urllib.request.urlopen(URL + "?" + q, timeout=60) as r:
        j = json.load(r)
    if "error" in j:
        sys.exit("ArcGIS error: %s" % j["error"])
    return j


def main():
    os.makedirs(OUT, exist_ok=True)
    n = get({"where": "1=1", "returnCountOnly": "true"})["count"]
    rows, off = [], 0
    while True:
        j = get({"where": "1=1", "outFields": "*", "orderByFields": "OBJECTID", "returnGeometry": "false",
                 "resultOffset": off, "resultRecordCount": 1000})
        rows += [f["attributes"] for f in j.get("features", [])]
        off += 1000
        if not j.get("exceededTransferLimit") and off >= n:
            break
    if len(rows) != n:
        sys.exit("expected %d records, got %d" % (n, len(rows)))
    for r in rows:                                   # dates arrive as epoch milliseconds
        for k in ("date_created", "date_updated"):
            if r.get(k):
                r[k] = datetime.datetime.utcfromtimestamp(r[k] / 1000).date().isoformat()
    keys = list(rows[0].keys())
    f = os.path.join(OUT, "fractracker_data_centers.csv")
    with open(f, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, keys)
        w.writeheader()
        for r in rows:
            w.writerow({k: ("" if r.get(k) is None else str(r[k]).replace("\r", " ").replace("\n", " ")) for k in keys})
    with open(os.path.join(OUT, "fractracker_SOURCE.txt"), "w") as fh:
        fh.write("FracTracker Alliance, Open U.S. Data Centers Tracker\n"
                 "https://www.fractracker.org/data-centers/\n%s\n"
                 "Downloaded %s UTC, %d records.\n"
                 "Terms: shared freely for non-commercial use with credit to FracTracker.\n"
                 % (URL, datetime.datetime.utcnow().strftime("%Y-%m-%d %H:%M"), len(rows)))
    st = {}
    for r in rows:
        st[r.get("status")] = st.get(r.get("status"), 0) + 1
    print("wrote %s (%d records)" % (f, len(rows)))
    for k, v in sorted(st.items(), key=lambda t: -t[1]):
        print("  %-40s %d" % (k, v))


if __name__ == "__main__":
    main()
