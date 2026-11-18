#!/usr/bin/env bash
# =============================================================================
# 01_download.sh  --  fetch the raw inputs into data-raw/  (run in your Terminal)
# -----------------------------------------------------------------------------
# Nothing here is committed to git (data-raw/ is ignored). Downloads resume if
# interrupted. The ISRM zip is 12.5 GB; everything else is under 100 MB.
#
#   bash scripts/01_download.sh            # everything
#   SKIP_ISRM=1 bash scripts/01_download.sh  # all but the 12.5 GB ISRM zip
# =============================================================================
set -euo pipefail
cd "$(dirname "$0")/.."
mkdir -p data-raw
get() {  # get <url> <file>
  if [ -s "data-raw/$2" ] && [ ! -e "data-raw/$2.part" ]; then echo "have  $2"; return; fi
  echo "fetch $2"
  curl -L --fail --retry 5 -C - -A "Mozilla/5.0" -o "data-raw/$2.part" "$1"
  mv "data-raw/$2.part" "data-raw/$2"
}

# EPA eGRID2023 (rev 2, June 2025): plants, emissions, generation, subregions
get "https://www.epa.gov/system/files/documents/2025-06/egrid2023_data_rev2.xlsx"            egrid2023_data.xlsx
# EPA eGRID draft PM2.5 (unit level, 2018-2021). Optional: script 02 falls back to emission factors.
get "https://www.epa.gov/system/files/documents/2024-06/egrid-draft-pm-emissions.xlsx"       egrid_pm25_2018_2021.xlsx || echo "WARN: PM2.5 file not fetched"
# EPA eGRID2023 subregion polygons
get "https://www.epa.gov/system/files/other-files/2025-01/egrid2023_subregions.zip"          egrid2023_subregions.zip
# EIA thermoelectric cooling water, 2023 summary. Optional: script 02 falls back to fuel factors.
get "https://www.eia.gov/electricity/data/water/archive/xls/Cooling_Boiler_Generator_Data_Summary_2023.xlsx" eia_cooling_summary_2023.xlsx || echo "WARN: EIA cooling file not fetched"
# Census 2023 cartographic county boundaries (1:500k)
get "https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip"              cb_2023_us_county_500k.zip

for z in egrid2023_subregions cb_2023_us_county_500k; do
  [ -d "data-raw/$z" ] || unzip -q -o "data-raw/$z.zip" -d "data-raw/$z"
done

# InMAP Source-Receptor Matrix v1.2.1 (Goodkind et al. 2019), Zenodo 10.5281/zenodo.3590127
# Zenodo refuses some scripted requests, so this uses curl's own user agent and
# tries two address forms. If both fail, download isrm_v1.2.1.zip in a browser
# from https://zenodo.org/records/3590127 and move it into data-raw/.
if [ "${SKIP_ISRM:-0}" != "1" ] && [ ! -s data-raw/isrm_v1.2.1.zip ]; then
  ok=0
  for u in "https://zenodo.org/records/3590127/files/isrm_v1.2.1.zip?download=1" \
           "https://zenodo.org/api/records/3590127/files/isrm_v1.2.1.zip/content"; do
    echo "fetch isrm_v1.2.1.zip (12.5 GB) from $u"
    if curl -L --fail --retry 5 -C - -o data-raw/isrm_v1.2.1.zip.part "$u"; then
      mv data-raw/isrm_v1.2.1.zip.part data-raw/isrm_v1.2.1.zip; ok=1; break
    fi
  done
  [ "$ok" = 1 ] || echo "ISRM NOT DOWNLOADED: get isrm_v1.2.1.zip in a browser from https://zenodo.org/records/3590127 and move it into data-raw/"
fi
echo; ls -lh data-raw | sed 1d
echo "Done. Do NOT unzip the ISRM zip unless you have ~170 GB free: script 03 reads it in place."
