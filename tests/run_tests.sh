#!/usr/bin/env bash
# End-to-end test on synthetic inputs (no downloads). Run from the repo root:
#   bash tests/run_tests.sh
# Needs R (shiny, bslib, leaflet, sf, DT, data.table, readxl, writexl, jsonlite, testthat)
# and Python 3 with numpy and scipy.
set -euo pipefail
T=$(mktemp -d); mkdir -p "$T/raw" "$T/build" "$T/data"
export USLOAD_TEST=1 USLOAD_RAW="$T/raw" USLOAD_BUILD="$T/build" USLOAD_DATA="$T/data"
Rscript tests/make_synthetic.R
python3 tests/make_synthetic_isrm.py "$T/raw" cdf2
Rscript scripts/02_build_plants.R
python3 scripts/03_isrm_fields.py --isrm "$T/raw/isrm_v1.2.1.zip" --build "$T/build" --test
python3 tests/check_isrm.py "$T/raw" "$T/build"
python3 scripts/03_isrm_fields.py --isrm "$T/raw/isrm_v1.2.1.ncf" --build "$T/build" --test   # unzipped path
python3 scripts/03_isrm_fields.py --isrm "$T/raw/isrm_v1.2.1.ncf" --build "$T/build" --test --sites county --no-fields --tag chk   # county-only mode must agree
cmp "$T/build/isrm_source.csv" "$T/build/isrm_source_chk.csv"
python3 tests/check_isrm.py "$T/raw" "$T/build"
Rscript scripts/04_bake_app_data.R
Rscript scripts/06_blockgroups.R
python3 scripts/03_isrm_fields.py --isrm "$T/raw/isrm_v1.2.1.ncf" --build "$T/build" --test --sites cells --no-fields --groups "$T/build/bg_cell_groups.csv"
printf 'cell\n3\n17\n200\n' > "$T/build/sub.csv"       # --site-cells: the same numbers as the full run, for the named cells only
python3 scripts/03_isrm_fields.py --isrm "$T/raw/isrm_v1.2.1.ncf" --build "$T/build" --test --sites cells --no-fields --groups "$T/build/bg_cell_groups.csv" --site-cells "$T/build/sub.csv" --tag sub
python3 -c "
import csv,sys
full={r['cell']:r for r in csv.DictReader(open(sys.argv[1]+'/isrm_source_groups.csv'))}
sub=list(csv.DictReader(open(sys.argv[1]+'/isrm_source_groups_sub.csv')))
assert [r['cell'] for r in sub]==['3','17','200'], sub
assert all(r==full[r['cell']] for r in sub), 'subset rows differ from the full run'
print('site-cells subset OK')" "$T/build"
Rscript scripts/04_bake_app_data.R          # again: attaches build/ses.rds and writes data/onsite_groups.rds
Rscript -e 'a <- readRDS(file.path(Sys.getenv("USLOAD_DATA"), "app_data.rds")); stopifnot(!is.null(a$ses)); stopifnot(file.exists(file.path(Sys.getenv("USLOAD_DATA"), "onsite_groups.rds")))'
Rscript tests/test_calc.R
echo "ALL TESTS PASSED"
