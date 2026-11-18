#!/usr/bin/env bash
# =============================================================================
# 01c_download_facilities.sh  --  openly licensed US data center inventories
#   (run in your Terminal; everything lands in data-raw/facilities/)
#
#   Epoch AI, AI data centers            CC BY 4.0   epoch.ai/data/ai-data-centers
#   Kyars, Data Center Atlas (latest)    CC BY 4.0   kiankyars.github.io/datacenter-atlas
#   PNNL, IM3 Open Source Data Center    ODbL 1.0    doi:10.57931/2550666 (copy shipped
#         Atlas (OpenStreetMap-derived)               with PNNL's public atlas app)
#
# Epoch gives street addresses, not coordinates: they are geocoded with the US
# Census Bureau's public geocoder (scripts/01d_geocode_epoch.py).
#
# Optional, added by hand: the Atlas of Data Center Politics (CC BY 4.0) asks
# people not to download its data files by script and to ask for a versioned
# export. If you obtain a copy, save it as data-raw/facilities/adp_data_centers.geojson.
# =============================================================================
set -uo pipefail
cd "$(dirname "$0")/.."
D=data-raw/facilities; mkdir -p "$D"
get() {  # get <url> <file>
  echo "fetch  $2"
  if curl -L --fail --retry 3 -sS -o "$D/$2.part" "$1"; then mv "$D/$2.part" "$D/$2"; else rm -f "$D/$2.part"; echo "FAILED $2  <-  $1"; fi
}
get "https://epoch.ai/data/data_centers/data_centers.zip" epoch_data_centers.zip
[ -s "$D/epoch_data_centers.zip" ] && unzip -q -o "$D/epoch_data_centers.zip" -d "$D/epoch"
K="https://kiankyars.github.io/datacenter-atlas/downloads/latest"
get "$K/sites.csv" kyars_sites.csv
get "$K/projects.csv" kyars_projects.csv
get "https://raw.githubusercontent.com/kiankyars/datacenter-atlas/main/DATA_LICENSE.md" kyars_DATA_LICENSE.md
get "https://raw.githubusercontent.com/IMMM-SFA/datacenter-atlas/main/data_center_database/im3_us_data_center_locations.gpkg" pnnl_im3_data_center_locations.gpkg
date -u +"%Y-%m-%dT%H:%MZ" > "$D/DOWNLOADED_AT.txt"

echo; echo "geocoding Epoch addresses (US Census geocoder, about a minute) ..."
python3 scripts/01d_geocode_epoch.py "$D/epoch/data_centers.csv" "$D/epoch_geocoded.csv"
echo; ls -lh "$D" | sed 1d
