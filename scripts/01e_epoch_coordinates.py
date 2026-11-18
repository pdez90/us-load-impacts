#!/usr/bin/env python3
"""Site coordinates for Epoch AI's data centers, read from Epoch's public map page
(https://epoch.ai/data/ai-data-centers/map; Epoch data are CC BY 4.0).

Epoch's CSV gives street addresses only, and many rural sites do not geocode.
The map page embeds each site's id (= the Name column of data_centers.csv) and its
map point (lngLat). Standard library only.

usage: python3 scripts/01e_epoch_coordinates.py data-raw/facilities/epoch_map_coordinates.csv
"""
import csv, html, json, re, sys, urllib.request

out = sys.argv[1]
req = urllib.request.Request("https://epoch.ai/data/ai-data-centers/map", headers={"User-Agent": "us-load-impacts research script"})
page = urllib.request.urlopen(req, timeout=60).read().decode("utf-8")
m = re.search(r'<astro-island[^>]*component-url="[^"]*SearchFilterMap[^"]*"[^>]*props="([^"]*)"', page)
if not m: sys.exit("map component not found: the Epoch page layout has changed")
props = json.loads(html.unescape(m.group(1)))

def dec(v):                      # Astro serialises values as [type, value] pairs
    if isinstance(v, list) and len(v) == 2 and isinstance(v[0], int):
        t, x = v
        if t == 0: return {k: dec(w) for k, w in x.items()} if isinstance(x, dict) else x
        if t == 1: return [dec(w) for w in x]
        return x
    return v
d = {k: dec(v) for k, v in props.items()}
val = lambda x: x.get("value") if isinstance(x, dict) and "value" in x else x
with open(out, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f); w.writerow(["name", "lon", "lat", "city", "state", "country"])
    for x in d["dataCenters"]:
        fa = x.get("facts", {})
        w.writerow([x["id"], x["lngLat"][0], x["lngLat"][1], str(val(fa.get("city")) or "").strip(), str(val(fa.get("state")) or "").strip(), val(fa.get("country"))])
print("wrote %s (%d sites)" % (out, len(d["dataCenters"])))
