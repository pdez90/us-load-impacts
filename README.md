# US large-load impact explorer

A Shiny app: enter the MW of a new electricity load (a data center, an industrial plant, a charging hub) and the US county it sits in, and get

- annual electricity and **CO2**,
- the downwind **PM2.5** increment by county and the deaths attributable to it, through the InMAP Source-Receptor Matrix (ISRM),
- **water** consumed on site and at the power plants,

for four supply options: the regional grid's average mix, its non-baseload mix (a marginal proxy), on-site gas generation, and hourly-matched carbon-free power.

## Status

Step 02 has been run on the real eGRID2023, eGRID PM2.5 and EIA files (18 Nov 2026): 12,329 plants in 22 subregions, CO2 rates identical to eGRID's subregion table, plant PM2.5 rates covering 98.6% of heat input, EIA water data covering 90% of thermal generation. **Steps 03 and 04 have only been run on a synthetic matrix**, because the real ISRM has not been read yet; the first real run may need a fix to how the population or mortality variable is found. Until `data/app_data.rds` is built the app shows a "No data yet" page.

## Build the data (once, on your own machine)

Needs R (`data.table`, `sf`, `readxl`, `jsonlite`), Python 3 with `numpy`, about 15 GB of disk, and `curl`.

```bash
bash scripts/01_download.sh                # ~13 GB into data-raw/ (git-ignored)
Rscript scripts/02_build_plants.R          # eGRID plants, mixes, county points  -> build/
python3 scripts/03_isrm_fields.py          # streams the ISRM zip in place       -> build/
                                           #   (add --isrm data-raw/isrm_v1.2.1.ncf if you unzipped it: seconds, not hours)
Rscript scripts/04_bake_app_data.R         # counties + everything the app needs -> data/app_data.rds
Rscript scripts/05_write_manifest.R        # manifest.json for Posit Connect Cloud
```

Step 03 does not unzip the matrix (about 165 GB uncompressed). It decompresses the zip as a stream, twice: once for the grid, population and mortality (cached in `build/isrm_grid.npz`), once for the matrix rows it needs. Timing on the real file is not yet known; plan for tens of minutes per pass. If you have the disk, unzip it and pass `--isrm data-raw/isrm_v1.2.1.ncf` for random access.

Read `data/build_report.txt` after step 04. It records how well the computed CO2 rates match eGRID's own subregion table, how much of the PM2.5 and water data came from plant records rather than fallback factors, and the implied national death total for all generation, which should be compared with published estimates before the app is shared.

The app file also carries the model's grid cells (for the zoomed-in map) and the plant list behind each mix, so rerun step 04 whenever `scripts/04_bake_app_data.R` changes.

## Reproducing and extending the Harvard hyperscale paper

`analysis/` reproduces the CO2 of Guidi, Dominici et al. (403 US hyperscale data centers, May 2024 to April 2025) from their public balancing-authority totals and adds PM2.5 deaths and power-plant water:

```bash
Rscript analysis/01_ba_mixes.R
python3 scripts/03_isrm_fields.py --isrm data-raw/isrm_v1.2.1.ncf --plants build/plants_ba.csv --tag ba --sites none
Rscript analysis/02_harvard_extension.R    # -> analysis/output/
```

Their facility list is not public, so this works from `data-raw/harvard/` (their aggregates, with the commit they came from in `SOURCE.txt`).

## Why the inventory matters

`analysis/03_compare_datasets.R` runs every public US data center inventory through the same chain (capacity, electricity at Harvard's load coefficient 0.58, grid region, CO2, PM2.5 deaths and power-plant water) and measures how far the inventories overlap:

```bash
bash scripts/01c_download_facilities.sh                              # Epoch AI, Kyars Data Center Atlas, PNNL IM3
python3 scripts/01e_epoch_coordinates.py data-raw/facilities/epoch_map_coordinates.csv
Rscript analysis/03_compare_datasets.R                               # -> analysis/output/datasets_*
```

With the method held fixed, the choice of inventory changes operating-fleet PM2.5 deaths about six-fold (27 to 175 a year), more than the load coefficient range (x1.46) or switching from the average to the marginal mix (about x2). It also changes which states receive the deaths and, to a lesser degree, who is exposed: `datasets_inventory_matters.png`, `datasets_receptor_states.csv` and `datasets_groups.csv`.

The Atlas of Data Center Politics is used too if a copy is saved as `data-raw/facilities/adp_data_centers.geojson` (its site asks for exports to be requested rather than scripted). Atlas campuses marked operating but still listing construction or planned phases are reported as "partly built", because their capacity is the full campus build-out; crypto-mining sites are broken out in `datasets_summary.txt`.

FracTracker Alliance's Open U.S. Data Centers Tracker is added with `python3 scripts/01f_download_fractracker.py`, which reads the tracker's public map layer into `data-raw/facilities/fractracker_data_centers.csv`. FracTracker shares it freely for non-commercial use with credit: *FracTracker Alliance, Open U.S. Data Centers Tracker, https://www.fractracker.org/data-centers/*. Its MW field is often a range; operating sites count the low end as running load and the rest as pipeline, and "Expanding" sites are reported as partly built.

## Who is exposed, by block group

`scripts/06_blockgroups.R` (after step 04) matches each of about 238,000 contiguous-US census block groups to the model grid cells it overlaps and counts its people from the ACS 2019-2023 (race and ethnicity, median household income, poverty ratio, education). It adds group exposure and death fields to `data/app_data.rds`; the app's "Who is exposed" tab and the inventory comparison use them. Needs the files from `scripts/01b_download_census.sh`.

```bash
Rscript scripts/06_blockgroups.R    # about 2 minutes
```

For on-site power and backup generators, "Who is exposed" needs one more pass of the matrix: for every grid cell, the exposure of each group per tonne released there. It reads `build/bg_cell_groups.csv` from step 06 and writes `data/onsite_groups.rds` (about 15-20 MB):

```bash
python3 scripts/03_isrm_fields.py --isrm data-raw/isrm_v1.2.1.ncf --sites cells --no-fields --groups build/bg_cell_groups.csv
Rscript scripts/04_bake_app_data.R  # rebuilds app_data (keeping the block-group groups) and writes onsite_groups.rds
Rscript scripts/05_write_manifest.R
```

Backup diesel generators are an option in the app's sidebar, added to any supply option: count, kW, hours run per year, load while running, EPA Tier 2 or Tier 4 final emission rates, released at the site.

## Virginia data center air permits

Virginia DEQ lists its issued air permits for data centers (194 as of September 2026), mostly for backup diesel generators. DEQ's site refuses scripted downloads, so the PDFs are saved by hand from the list into one folder (`<registration without hyphen>_DC_Permit.pdf`, as DEQ names them). Then:

```bash
python3 scripts/08_parse_va_permits.py --pdfs ~/Downloads/permits_virginia   # -> data-raw/permits/va/va_permits.csv
python3 scripts/09_geocode_va_permits.py                                       # site addresses -> coordinates (Census geocoder)
Rscript analysis/05_va_permits.R                                               # -> analysis/output/va_permits_*
```

Step 08 reads each permit's annual emission limits (tons/yr), engine counts and ratings, testing-hour limits and site address. When a permit sets limits for several groups of engines it uses a facility-wide or combined limit if there is one, the limit covering every engine if limits overlap, and otherwise the sum. Values that could not be read reliably (scanned pages, tables drawn as images) were read by hand and are in `data-raw/permits/va/overrides.csv`, each with a note; DEQ's list is in `deq_list.csv` (both tracked in git). Permits replaced by a later one ("supersedes your permit dated ...") or restated in a Title V permit are dropped. Step 05 models two cases for each site: every site at its permitted annual limits, and routine testing only (engines x rated kW x the permit's testing hours x 75% load at Tier 2 rates).

## Run and deploy

```r
shiny::runApp()
```

Posit Connect Cloud: push the repo to GitHub (including `data/app_data.rds` and `manifest.json`), then in Connect Cloud choose Publish, Shiny, pick the repository and branch, and set `app.R` as the primary file. Republish after each push.

## Layout

| Path | What |
|---|---|
| `app.R`, `methods.html` | the app and its methods tab |
| `R/calc.R` | all arithmetic, free of Shiny |
| `scripts/00_config.R` | paths, constants, assumed release heights, fallback factors |
| `scripts/01`-`06` | the pipeline above |
| `tests/` | synthetic inputs, an independent recomputation of the ISRM step, app tests |
| `data/` | `app_data.rds` and the build report (committed) |
| `data-raw/`, `build/` | downloads and intermediates (git-ignored) |

## Method in brief

1. **Energy.** MW x PUE (if the figure is IT load) x load factor x 8,760 h; grid options divide by (1 - T&D loss).
2. **Plants.** EPA eGRID2023 plant file, contiguous US. Average mix: all plants in the eGRID subregion by net generation. Non-baseload mix: combustion plants weighted by eGRID's non-baseload factor. The script checks both CO2 rates against eGRID's published subregion rates.
3. **Emissions.** CO2, NOx, SO2 from eGRID. Primary PM2.5 from EPA's draft unit-level PM2.5 file (latest year's rate per MMBtu applied to 2023 heat input) where readable, otherwise fuel emission factors in `00_config.R`.
4. **Air quality.** ISRM v1.2.1 (Goodkind et al. 2019; Zenodo 10.5281/zenodo.3590127): primary PM2.5, sulfate from SO2, nitrate from NOx. Each plant goes to the grid cell containing it and is split between the two release layers that bracket an assumed effective height by fuel (coal 400 m, oil 200 m, gas 150 m, others 100 m; layer mid-heights about 29, 322 and 917 m). The interpolation is implemented here because the one in `inmap srpredict` is inverted (inmap issue #121).
5. **Health.** Deaths = beta x increment x population x baseline mortality, with the population and mortality fields carried in the ISRM file; Krewski et al. 2009 (RR 1.06 per 10 ug/m3) or Lepeule et al. 2012 (RR 1.14).
6. **Counties.** Receptor cells are split among counties by overlapping area; population is conserved.
7. **Water.** On site: L per kWh entered by the user. At plants: EIA thermoelectric cooling data (2023) by plant where the file passes a national-total check, otherwise consumption factors by fuel. No hydropower evaporation.

## Known limits

- ISRM meteorology and chemistry are from 2005 and its population from about 2010.
- Release heights are assumed by fuel, not taken from stack data. Next step: EIA-860 stack parameters with plume rise.
- The mixes are annual and regional: no hourly marginal emissions, no inter-regional trade, no new capacity built for the load.
- Counties are mapped to the subregion polygon that contains them; overlapping utility territories are not represented (the app lets the user override the region).
- On-site generation gives totals only, no downwind map. It uses the grid cell containing the site (latitude and longitude) or the county's interior point.
- No ozone, NH3 or VOC pathways; no backup generators, construction or upstream fuel-cycle emissions.
- Alaska, Hawaii and Puerto Rico are outside the ISRM domain.
- Default presets (PUE, on-site water, on-site gas emission rates, fallback PM2.5 and water factors) are starting values to be replaced with facility or permit data.

## Tests

```bash
bash tests/run_tests.sh
```

Builds a small NetCDF-3 file laid out like the ISRM, zips it, runs steps 02-04, recomputes the concentration fields independently from the full arrays, checks the app arithmetic against cell-level sums, and drives the Shiny server.
