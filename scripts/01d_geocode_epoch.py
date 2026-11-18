#!/usr/bin/env python3
"""Geocode the US rows of Epoch AI's data_centers.csv with the US Census Bureau
one-line address geocoder (public, no key). Standard library only.

usage: python3 scripts/01d_geocode_epoch.py <data_centers.csv> <out.csv>
Writes name, address, lat, lon, match_type (exact / non_exact / none), matched_address.
Rows with no address, or that the geocoder cannot match, are kept with empty
coordinates and reported, so they can be placed by hand.
"""
import csv, json, sys, time, urllib.parse, urllib.request

src, out = sys.argv[1], sys.argv[2]
URL = "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?"
rows = [r for r in csv.DictReader(open(src, newline="", encoding="utf-8")) if r.get("Country") == "United States"]
res, miss = [], []
for r in rows:
    a = (r.get("Address") or "").strip()
    lat = lon = ""; mt = "none"; ma = ""
    if a:
        q = URL + urllib.parse.urlencode({"address": a, "benchmark": "Public_AR_Current", "format": "json"})
        for attempt in range(3):
            try:
                j = json.load(urllib.request.urlopen(q, timeout=30)); break
            except Exception as e:
                j = None; time.sleep(2)
        m = (j or {}).get("result", {}).get("addressMatches", [])
        if m:
            lon, lat = m[0]["coordinates"]["x"], m[0]["coordinates"]["y"]
            ma = m[0].get("matchedAddress", ""); mt = "exact" if len(m) == 1 else "non_exact"
        time.sleep(0.3)
    if mt == "none": miss.append(r["Name"] + (" (no address)" if not a else " | " + a))
    res.append({"name": r["Name"], "address": a, "lat": lat, "lon": lon, "match_type": mt, "matched_address": ma})
with open(out, "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=list(res[0])); w.writeheader(); w.writerows(res)
print("Epoch US sites: %d; geocoded %d; not placed %d" % (len(res), len(res) - len(miss), len(miss)))
for m in miss: print("  not placed:", m)
