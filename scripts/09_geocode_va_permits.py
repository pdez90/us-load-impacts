#!/usr/bin/env python3
"""Geocode the site addresses read from Virginia data center air permits (scripts/08) with the US Census
Bureau's batch geocoder (public, free, no key).

In : data-raw/permits/va/va_permits.csv  (street, city, zip)
Out: data-raw/permits/va/va_permits_geocoded.csv  (registration, matched address, lon, lat, state and county FIPS,
     match type). analysis/05_va_permits.R keeps a point only if it falls in the county DEQ lists for the permit;
     otherwise the site is placed at its county's interior point.
Run from the repo root:  python3 scripts/09_geocode_va_permits.py
"""
import csv, io, os, sys, urllib.request, uuid

URL = "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
IN = "data-raw/permits/va/va_permits.csv"; OUT = "data-raw/permits/va/va_permits_geocoded.csv"
UA = "us-load-impacts/1.0 (research script; https://github.com/pdez90/us-load-impacts)"


def main():
    rows = [r for r in csv.DictReader(open(IN, encoding="utf-8")) if r["street"]]
    buf = io.StringIO(); w = csv.writer(buf)
    for r in rows: w.writerow([r["registration"], r["street"], r["city"], "VA", r["zip"]])
    b = uuid.uuid4().hex
    body = ("--%s\r\nContent-Disposition: form-data; name=\"addressFile\"; filename=\"a.csv\"\r\nContent-Type: text/csv\r\n\r\n" % b
            + buf.getvalue() + "\r\n")
    for k, v in (("benchmark", "Public_AR_Current"), ("vintage", "Current_Current")):
        body += "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n" % (b, k, v)
    body += "--%s--\r\n" % b
    req = urllib.request.Request(URL, data=body.encode(), headers={"Content-Type": "multipart/form-data; boundary=" + b, "User-Agent": UA})
    try:
        out = urllib.request.urlopen(req, timeout=300).read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        sys.exit("Census geocoder refused the request (HTTP %d)" % e.code)
    res = []
    for f in csv.reader(io.StringIO(out)):
        if len(f) < 3: continue
        rec = {"registration": f[0], "input": f[1], "match": f[2], "match_type": f[3] if len(f) > 3 else "",
               "matched_address": f[4] if len(f) > 4 else "", "lon": "", "lat": "", "state_fips": "", "county_fips": ""}
        if f[2] == "Match" and len(f) > 5 and "," in f[5]:
            rec["lon"], rec["lat"] = f[5].split(",")
            if len(f) > 9: rec["state_fips"], rec["county_fips"] = f[8], f[9]
        res.append(rec)
    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, list(res[0].keys())); wr.writeheader(); wr.writerows(sorted(res, key=lambda r: r["registration"]))
    print("%d addresses sent, %d matched -> %s" % (len(rows), sum(r["match"] == "Match" for r in res), OUT))


if __name__ == "__main__":
    main()
