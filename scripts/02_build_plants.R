# =============================================================================
# 02_build_plants.R  --  plant table, regional mixes and county points
# -----------------------------------------------------------------------------
# In : data-raw/egrid2023_data.xlsx            (required)
#      data-raw/egrid_pm25_2018_2021.xlsx      (optional; else emission factors)
#      data-raw/eia_cooling_summary_2023.xlsx  (optional; else fuel water factors)
#      data-raw/cb_2023_us_county_500k/        (required)
# Out: build/plants.csv         one row per contiguous-US plant with emissions,
#                               water, release height and its share of each mix
#      build/region_mix.csv     per subregion x mix: kg/MWh, L/MWh, fuel shares
#      build/county_points.csv  county centroids (source locations for on-site power)
#      build/qa_02.txt          checks against eGRID's own subregion rates
#
# Two mixes per eGRID subregion:
#   avg  all generation in the subregion (attributional view)
#   nb   eGRID non-baseload: combustion plants weighted by NBFACTOR, EPA's
#        proxy for the plants that respond to a change in load
# =============================================================================
source(file.path(if (nzchar(Sys.getenv("USLOAD_ROOT"))) Sys.getenv("USLOAD_ROOT") else ".", "scripts/00_config.R"))
suppressPackageStartupMessages(library(readxl))
qa <- character(); note <- function(...) { s <- sprintf(...); qa <<- c(qa, s); msg("%s", s) }
num <- function(x) suppressWarnings(as.numeric(x))

# ---- 1. eGRID plant sheet -------------------------------------------------------------------
f_eg <- file.path(RAW, "egrid2023_data.xlsx")
assert(file.exists(f_eg), "missing %s (run scripts/01_download.sh)", f_eg)
read_egrid <- function(prefix, key) {
  sh <- grep(paste0("^", prefix), excel_sheets(f_eg), value = TRUE, ignore.case = TRUE)
  assert(length(sh) >= 1, "no sheet starting '%s' in %s; sheets: %s", prefix, basename(f_eg), paste(excel_sheets(f_eg), collapse = ", "))
  top <- suppressMessages(read_excel(f_eg, sheet = sh[1], n_max = 6, col_names = FALSE, .name_repair = "minimal"))
  hdr <- which(apply(top, 1, function(r) key %in% toupper(trimws(as.character(r)))))[1]
  assert(!is.na(hdr), "sheet %s has no header row containing %s", sh[1], key)
  d <- as.data.table(suppressMessages(read_excel(f_eg, sheet = sh[1], skip = hdr - 1, guess_max = 50000)))
  setnames(d, toupper(trimws(names(d)))); d
}
P <- read_egrid("PLNT", "ORISPL")
need <- c("ORISPL", "PNAME", "PSTATABB", "SUBRGN", "LAT", "LON", "PLFUELCT", "NBFACTOR",
          "PLHTIAN", "PLNGENAN", "PLNOXAN", "PLSO2AN", "PLCO2AN")
assert(all(need %in% names(P)), "eGRID plant sheet lacks: %s", paste(setdiff(need, names(P)), collapse = ", "))
for (v in c("LAT", "LON", "NBFACTOR", "PLHTIAN", "PLNGENAN", "PLNOXAN", "PLSO2AN", "PLCO2AN")) set(P, j = v, value = num(P[[v]]))
P <- P[!is.na(SUBRGN) & !(SUBRGN %in% NON_CONUS) & nzchar(SUBRGN)]
P[, PLFUELCT := toupper(trimws(PLFUELCT))]
for (v in c("NBFACTOR", "PLHTIAN", "PLNGENAN", "PLNOXAN", "PLSO2AN", "PLCO2AN")) set(P, which(is.na(P[[v]])), v, 0)
note("eGRID: %d contiguous-US plants in %d subregions; net generation %.0f TWh; CO2 %.0f Mt",
     nrow(P), uniqueN(P$SUBRGN), sum(P$PLNGENAN) / 1e6, sum(P$PLCO2AN) * SHORT_TON / 1e6)
assert(uniqueN(P$SUBRGN) >= 20 || Sys.getenv("USLOAD_TEST") == "1", "only %d subregions found", uniqueN(P$SUBRGN))

# ---- 2. primary PM2.5 -----------------------------------------------------------------------
# Plant rate (lb/MMBtu) from EPA's unit-level draft PM2.5 file where it can be
# read, applied to 2023 heat input; otherwise the fuel emission factor.
P[, pm_rate := PM25_EF_LB_MMBTU[PLFUELCT]][is.na(pm_rate), pm_rate := 0]
P[, pm_source := fifelse(PLHTIAN > 0, "emission factor", "none")]
f_pm <- file.path(RAW, "egrid_pm25_2018_2021.xlsx")
pm_plant <- tryCatch({
  assert(file.exists(f_pm), "file not present")
  # The workbook has one "<year> PM Plant-level Data" sheet per year: a row of
  # long names, then a row of eGRID codes (ORISPL, PLHTIAN, PLPM25AN). Plant
  # values carry the same CHP adjustment as the eGRID plant file.
  shs  <- excel_sheets(f_pm); cand <- grep("plant-level", shs, ignore.case = TRUE, value = TRUE)
  assert(length(cand) >= 1, "no plant-level sheet; sheets: %s", paste(shs, collapse = " | "))
  yrs <- as.integer(sub(".*?(20[0-9]{2}).*", "\\1", cand)); sh <- cand[which.max(yrs)]; yr <- max(yrs)
  top <- suppressMessages(read_excel(f_pm, sheet = sh, n_max = 6, col_names = FALSE, .name_repair = "minimal"))
  hdr <- which(apply(top, 1, function(r) all(c("ORISPL", "PLHTIAN", "PLPM25AN") %in% toupper(trimws(as.character(r))))))[1]
  assert(!is.na(hdr), "sheet '%s' has no code row with ORISPL, PLHTIAN, PLPM25AN", sh)
  d <- as.data.table(suppressMessages(read_excel(f_pm, sheet = sh, skip = hdr - 1, guess_max = 50000)))
  setnames(d, toupper(trimws(names(d))))
  d <- d[, .(ORISPL = num(ORISPL), pm_tons = num(PLPM25AN), hi = num(PLHTIAN))]
  d <- d[!is.na(ORISPL), .(pm_tons = sum(pm_tons, na.rm = TRUE), hi = sum(hi, na.rm = TRUE)), by = ORISPL]
  msg("PM2.5 file: sheet '%s', %d plants, %.0f kt PM2.5", sh, nrow(d), sum(d$pm_tons) * SHORT_TON / 1e3)
  d <- d[hi > 0 & pm_tons >= 0][, rate := pm_tons * 2000 / hi][rate <= 1]    # > 1 lb/MMBtu is not credible
  attr(d, "year") <- yr; d
}, error = function(e) { note("PM2.5 file not used (%s): emission factors for every plant", conditionMessage(e)); NULL })
if (!is.null(pm_plant)) {
  P[, ORISPL := num(ORISPL)]
  P[pm_plant, on = "ORISPL", `:=`(pm_rate = i.rate, pm_source = sprintf("eGRID PM2.5 %s rate", attr(pm_plant, "year")))]
  note("PM2.5: plant rates from the eGRID %s file cover %.1f%% of combustion heat input; emission factors the rest",
       attr(pm_plant, "year"), 100 * P[pm_source != "emission factor" & pm_source != "none", sum(PLHTIAN)] / sum(P$PLHTIAN))
}
P[, pm25_t := pm_rate * PLHTIAN / 2000 * SHORT_TON]
P[, `:=`(nox_t = PLNOXAN * SHORT_TON, so2_t = PLSO2AN * SHORT_TON, co2_t = PLCO2AN * SHORT_TON)]
note("Emissions (kt): SO2 %.0f, NOx %.0f, primary PM2.5 %.0f", sum(P$so2_t) / 1e3, sum(P$nox_t) / 1e3, sum(P$pm25_t) / 1e3)

# ---- 3. cooling water -----------------------------------------------------------------------
P[, `:=`(wcons_l = pmax(PLNGENAN, 0) * fifelse(is.na(WATER_CONS_L_MWH[PLFUELCT]), 0, WATER_CONS_L_MWH[PLFUELCT]),
         wwith_l = NA_real_, water_source = "fuel factor")]
f_w <- file.path(RAW, "eia_cooling_summary_2023.xlsx")
w_plant <- tryCatch({
  assert(file.exists(f_w), "file not present")
  sh  <- excel_sheets(f_w)[1]
  top <- suppressMessages(read_excel(f_w, sheet = sh, n_max = 12, col_names = FALSE, .name_repair = "minimal"))
  hdr <- which(apply(top, 1, function(r) any(grepl("^plant (code|id)$", trimws(as.character(r)), ignore.case = TRUE))))[1]
  assert(!is.na(hdr), "no header row with 'Plant Code' in sheet '%s'", sh)
  d <- as.data.table(suppressMessages(read_excel(f_w, sheet = sh, skip = hdr - 1, guess_max = 100000)))
  nm <- names(d)
  c_id <- grep("^plant (code|id)$", trimws(nm), ignore.case = TRUE)[1]
  c_w  <- grep("withdrawal volume", nm, ignore.case = TRUE)[1]
  c_c  <- grep("consumption volume", nm, ignore.case = TRUE)[1]
  assert(!is.na(c_w) && !is.na(c_c), "volume columns not recognised: %s", paste(nm, collapse = " | "))
  msg("EIA cooling: sheet '%s'; withdrawal='%s' consumption='%s'", sh, nm[c_w], nm[c_c])
  # Volumes are reported per cooling system and repeated on every generator /
  # boiler row attached to it: keep one row per plant-month-cooling system.
  c_m  <- grep("^month$", trimws(nm), ignore.case = TRUE)[1]
  c_cs <- grep("cooling (system )?id", nm, ignore.case = TRUE)[1]
  keys <- nm[na.omit(c(c_id, c_m, c_cs))]
  d <- unique(d, by = keys)
  d <- d[, .(ORISPL = num(get(nm[c_id])), with_mg = num(get(nm[c_w])), cons_mg = num(get(nm[c_c])))]
  d <- d[!is.na(ORISPL), .(with_mg = sum(with_mg, na.rm = TRUE), cons_mg = sum(cons_mg, na.rm = TRUE)), by = ORISPL]
  tw <- sum(d$with_mg) / 1e6; tc <- sum(d$cons_mg) / 1e6     # trillion gallons
  msg("EIA cooling totals: withdrawal %.1f, consumption %.2f trillion gal", tw, tc)
  # EIA reports about 45-55 trillion gal withdrawn a year. Outside a wide band
  # means the rows were double-counted or misread: do not use them.
  assert(Sys.getenv("USLOAD_TEST") == "1" || (tw > 20 && tw < 80 && tc > 0.5 && tc < 3),
         "national totals implausible (withdrawal %.1f, consumption %.2f trillion gal)", tw, tc)
  d
}, error = function(e) { note("EIA cooling file not used (%s): fuel water factors for every plant", conditionMessage(e)); NULL })
if (!is.null(w_plant)) {
  P[, ORISPL := num(ORISPL)]
  P[w_plant, on = "ORISPL", `:=`(wcons_l = i.cons_mg * 1e6 * L_PER_GAL, wwith_l = i.with_mg * 1e6 * L_PER_GAL, water_source = "EIA 2023")]
  note("Water: EIA plant data cover %.1f%% of thermal generation; fuel factors the rest",
       100 * P[water_source == "EIA 2023", sum(pmax(PLNGENAN, 0))] / P[PLFUELCT %in% c("COAL", "GAS", "OIL", "NUCLEAR", "BIOMASS", "OFSL", "OTHF"), sum(pmax(PLNGENAN, 0))])
}

# ---- 4. mixes -------------------------------------------------------------------------------
P[, eff_height_m := EFF_HEIGHT[PLFUELCT]][is.na(eff_height_m), eff_height_m := 100]
P[, w_avg := 1][, w_nb := fifelse(PLNGENAN > 0, pmax(NBFACTOR, 0), 0)]
P[, k_avg := w_avg / sum(PLNGENAN * w_avg), by = SUBRGN]      # share of each MWh generated, 1/MWh
P[, k_nb  := w_nb  / sum(PLNGENAN * w_nb),  by = SUBRGN]
assert(all(is.finite(P$k_avg)), "a subregion has zero total generation")
if (any(!is.finite(P$k_nb))) { note("subregions without non-baseload plants: %s", paste(unique(P[!is.finite(k_nb), SUBRGN]), collapse = ", ")); P[!is.finite(k_nb), k_nb := 0] }

# Single-fuel mixes: k_f<group> is each plant's share of one MWh from that fuel group.
for (g in names(FUEL_GROUPS)) {
  inb <- P$PLFUELCT %in% FUEL_GROUPS[[g]] & P$PLNGENAN > 0
  P[, tmp := fifelse(inb, 1, 0)][, tmp := { d <- sum(PLNGENAN * tmp); if (d > 0) tmp / d else 0 }, by = SUBRGN]
  setnames(P, "tmp", paste0("k_f", g))
}
ALL_MIXES <- c(MIXES, paste0("f", names(FUEL_GROUPS)))

fuel_cols <- c(coal = "PLGENACL", oil = "PLGENAOL", gas = "PLGENAGS", nuclear = "PLGENANC", hydro = "PLGENAHY",
               biomass = "PLGENABM", wind = "PLGENAWI", solar = "PLGENASO", geothermal = "PLGENAGT")
fuel_cols <- fuel_cols[fuel_cols %in% names(P)]
for (v in fuel_cols) set(P, j = v, value = { x <- num(P[[v]]); x[is.na(x)] <- 0; x })
mix_tab <- rbindlist(lapply(ALL_MIXES, function(m) {
  k <- P[[paste0("k_", m)]]
  out <- P[, .(subrgn = SUBRGN, k = k, co2_t, nox_t, so2_t, pm25_t, wcons_l, wwith_l, PLNGENAN)]
  r <- out[, .(mix = m,
               co2_kg_mwh  = sum(co2_t  * k) * 1e3, nox_kg_mwh = sum(nox_t * k) * 1e3,
               so2_kg_mwh  = sum(so2_t  * k) * 1e3, pm25_kg_mwh = sum(pm25_t * k) * 1e3,
               wcons_l_mwh = sum(wcons_l * k),
               wwith_l_mwh = if (all(is.na(wwith_l))) NA_real_ else sum(wwith_l * k, na.rm = TRUE),
               gen_twh     = sum(PLNGENAN * (k > 0)) / 1e6, n_plants = sum(k > 0)), by = subrgn]
  if (length(fuel_cols)) {
    sh <- cbind(P[, .(subrgn = SUBRGN)], P[, unname(fuel_cols), with = FALSE] * k)[, lapply(.SD, sum), by = subrgn]
    setnames(sh, unname(fuel_cols), paste0("share_", names(fuel_cols)))
    r <- merge(r, sh, by = "subrgn")
  }
  r
}), fill = TRUE)
mix_tab <- mix_tab[n_plants > 0]
# Share of the subregion's generation that each fuel group supplies (the default for a custom mix).
tot_gen <- mix_tab[mix == "avg", .(subrgn, tot = gen_twh)]
mix_tab <- merge(mix_tab, tot_gen, by = "subrgn"); mix_tab[, cat_share := fifelse(grepl("^f", mix), gen_twh / tot, NA_real_)][, tot := NULL]

# Grid gross loss by interconnection (eGRID sheet GGL).
mix_tab[, interconnect := INTERCONNECT(subrgn)][, grid_loss := NA_real_]
ggl <- tryCatch(read_egrid("GGL", "GGRSLOSS"), error = function(e) NULL)
if (!is.null(ggl)) {
  ggl <- ggl[, .(interconnect = trimws(as.character(REGION)), gl = num(GGRSLOSS))]
  mix_tab[ggl, on = "interconnect", grid_loss := i.gl]
  note("eGRID grid gross loss: %s", paste(sprintf("%s %.1f%%", ggl$interconnect, 100 * ggl$gl), collapse = ", "))
}
if (anyNA(mix_tab$grid_loss)) { note("grid gross loss not found for some regions: 4.2%% used"); mix_tab[is.na(grid_loss), grid_loss := 0.042] }

if ("SRNAME" %in% names(P)) mix_tab <- merge(mix_tab, unique(P[, .(subrgn = SUBRGN, srname = SRNAME)]), by = "subrgn", all.x = TRUE)

# QA against eGRID's published subregion rates (lb/MWh).
srl <- tryCatch(read_egrid("SRL", "SUBRGN"), error = function(e) NULL)
if (!is.null(srl) && all(c("SRCO2RTA", "SRNBCO2") %in% names(srl))) {
  cmp <- merge(dcast(mix_tab, subrgn ~ mix, value.var = "co2_kg_mwh"),
               srl[, .(subrgn = SUBRGN, eg_avg = num(SRCO2RTA) * 0.45359237, eg_nb = num(SRNBCO2) * 0.45359237)], by = "subrgn")
  cmp[, `:=`(d_avg = 100 * (avg / eg_avg - 1), d_nb = 100 * (nb / eg_nb - 1))]
  note("CO2 rate vs eGRID subregion table: average mix max |diff| %.2f%%; non-baseload max |diff| %.2f%%",
       max(abs(cmp$d_avg), na.rm = TRUE), max(abs(cmp$d_nb), na.rm = TRUE))
  fwrite(cmp, file.path(BUILD, "qa_02_co2_vs_egrid.csv"))
  if (max(abs(cmp$d_avg), na.rm = TRUE) > 2) note("WARNING: average-mix CO2 differs from eGRID by more than 2%% somewhere; see build/qa_02_co2_vs_egrid.csv")
}

# ---- 5. write -------------------------------------------------------------------------------
out <- P[!is.na(LAT) & !is.na(LON),
         .(orispl = ORISPL, name = PNAME, state = PSTATABB, subrgn = SUBRGN, lat = LAT, lon = LON, fuelcat = PLFUELCT,
           eff_height_m, gen_mwh = PLNGENAN, nbfactor = NBFACTOR, co2_t, nox_t, so2_t, pm25_t, pm_source,
           wcons_l, wwith_l, water_source)]
out <- cbind(out, P[!is.na(LAT) & !is.na(LON), paste0("k_", ALL_MIXES), with = FALSE])
lost <- P[is.na(LAT) | is.na(LON), sum(so2_t + nox_t + pm25_t)] / P[, sum(so2_t + nox_t + pm25_t)]
note("plants without coordinates carry %.3f%% of SO2+NOx+PM2.5 (dropped from the air-quality fields)", 100 * lost)
fwrite(out, file.path(BUILD, "plants.csv"))
fwrite(mix_tab[order(subrgn, mix)], file.path(BUILD, "region_mix.csv"))

# ---- 6. county points -----------------------------------------------------------------------
shp <- list.files(file.path(RAW, "cb_2023_us_county_500k"), "\\.shp$", full.names = TRUE)[1]
assert(!is.na(shp), "county shapefile not found under data-raw/cb_2023_us_county_500k")
cty <- st_read(shp, quiet = TRUE)
cty <- cty[!(cty$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")), ]
cen <- st_transform(suppressWarnings(st_point_on_surface(st_transform(st_geometry(cty), LCC))), 4326)
xy  <- st_coordinates(cen)
fwrite(data.table(fips = cty$GEOID, lon = xy[, 1], lat = xy[, 2]), file.path(BUILD, "county_points.csv"))
note("%d contiguous-US counties", nrow(cty))
writeLines(qa, file.path(BUILD, "qa_02.txt"))
msg("wrote build/plants.csv (%d plants), build/region_mix.csv, build/county_points.csv", nrow(out))
