#!/usr/bin/env bash
# =============================================================================
# 01b_download_census.sh  --  census block groups and ACS 2019-2023 tables, for
#   current population and socioeconomic analysis at the finest census level
#   that carries income, poverty and education (run in your Terminal).
#
#   bash scripts/01b_download_census.sh
#
# A failed file is reported and the script carries on, so paste the output if
# anything says FAILED.
# =============================================================================
set -uo pipefail
cd "$(dirname "$0")/.."
mkdir -p data-raw/acs
get() {  # get <url> <file>
  if [ -s "data-raw/$2" ]; then echo "have   $2"; return; fi
  echo "fetch  $2"
  if curl -L --fail --retry 3 -sS -o "data-raw/$2.part" "$1"; then mv "data-raw/$2.part" "data-raw/$2"; else rm -f "data-raw/$2.part"; echo "FAILED $2  <-  $1"; fi
}
# Block-group boundaries, 2023 cartographic file (about 240,000 polygons)
get "https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_bg_500k.zip" cb_2023_us_bg_500k.zip
[ -s data-raw/cb_2023_us_bg_500k.zip ] && [ ! -d data-raw/cb_2023_us_bg_500k ] && unzip -q -o data-raw/cb_2023_us_bg_500k.zip -d data-raw/cb_2023_us_bg_500k

# ACS 5-year 2019-2023, table-based summary files (one national file per table, all geographies)
#   B03002 race and Hispanic origin   B19013 median household income
#   C17002 income-to-poverty ratio    B15003 education, adults 25+
BASE="https://www2.census.gov/programs-surveys/acs/summary_file/2023/table-based-SF/data/5YRData"
for t in b03002 b19013 c17002 b15003; do get "$BASE/acsdt5y2023-$t.dat" "acs/acsdt5y2023-$t.dat"; done
echo; ls -lh data-raw/acs data-raw/cb_2023_us_bg_500k.zip 2>/dev/null
for f in data-raw/acs/*.dat; do [ -s "$f" ] && { echo "== $f"; head -c 400 "$f" | head -2; echo; }; done
