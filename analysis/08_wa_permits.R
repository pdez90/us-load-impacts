# =============================================================================
# 08_wa_permits.R  --  Washington data centers: generator emissions, deaths and noise from Ecology's approval orders
# -----------------------------------------------------------------------------
# Run: Rscript scripts/11_download_wa_permits.R   (once; the orders, for reference)
#      Rscript analysis/08_wa_permits.R
# In : data-raw/permits/wa/wa_permits.csv   read by hand from the 11 approval orders Ecology posts (each value has
#                                           its condition or table in source_pages)
#      data/app_data.rds, data/onsite_groups.rds, build/bg_demog.csv, data-raw/cb_2023_us_bg_500k/
# Out: analysis/output/wa_permits_sites.csv, wa_permits_summary.txt
#      data-raw/permits/wa/wa_geocoded.csv  (Census geocoder; cached, so later runs need no network)
#
# Washington's orders differ from Virginia's: each is written for its site, caps hours for all purposes at
# 25-200 h per engine a year (Virginia's are mostly 500 h), and often says how many engines may run at once in
# each kind of test. Three cases, as for Virginia, with the grid-support case replaced by the order's own hours:
#   routine testing     : the order's testing hours per engine (or its all-purpose cap, if lower than 32 h, else 32 h),
#                         at 75% load and EPA Tier 2 rates
#   all permitted hours : every engine for its all-purpose hour cap at 75% load; engines tested to Tier 4 limits
#                         (SCR) at Tier 4 rates
#   permitted maximum   : the order's facility-wide NOx limit, or its potential to emit where it sets none
# Every case is capped at the permitted maximum. PM2.5 from evaporative cooling towers (where an order counts it)
# is added to every case, since the towers run all year. Noise uses the shared screening functions in R/calc.R,
# with the order's own "engines at once" for testing where it gives one.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(sf) })
sf_use_s2(FALSE)
source("R/calc.R")
OUT <- "analysis/output"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
lines <- character(); say <- function(...) { s <- sprintf(...); lines <<- c(lines, s); cat(s, "\n") }
SHORT_TON <- 0.90718474; B_K <- CRF$krewski$beta; H_GEN <- 10; LOAD <- 0.75; H_TEST_DEFAULT <- 32

app <- readRDS("data/app_data.rds"); app$og <- if (file.exists("data/onsite_groups.rds")) readRDS("data/onsite_groups.rds")
P <- fread("data-raw/permits/wa/wa_permits.csv", colClasses = list(character = c("county_fips", "order_no")))

# ---- 1. where each site is ------------------------------------------------------------------------------
GEO <- "data-raw/permits/wa/wa_geocoded.csv"
CITY <- data.table(city = c("Quincy", "Moses Lake", "Cle Elum", "East Wenatchee", "Wenatchee"),   # city centres, fallback only
                   clon = c(-119.8525, -119.2781, -120.9395, -120.2876, -120.3103), clat = c(47.2343, 47.1301, 47.1954, 47.4157, 47.4235))
geocode <- function(street, city) {
  if (!nzchar(street) || !requireNamespace("jsonlite", quietly = TRUE)) return(c(NA, NA))
  u <- sprintf("https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?address=%s&benchmark=Public_AR_Current&format=json",
               utils::URLencode(paste0(street, ", ", city, ", WA"), reserved = TRUE))
  r <- tryCatch(suppressWarnings(jsonlite::fromJSON(u)), error = function(e) NULL)
  m <- r$result$addressMatches
  if (is.null(m) || !NROW(m)) return(c(NA, NA)); c(m$coordinates$x[1], m$coordinates$y[1])
}
spell_out <- function(street) {                       # second try: the geocoder sometimes misses abbreviations
  s <- gsub("\\bPkwy\\b", "Parkway", street); s <- gsub("\\bSt\\b", "Street", s); s <- gsub("\\bRd\\b", "Road", s)
  gsub("\\bSR ([0-9]+)", "State Route \\1", s)
}
geocode2 <- function(street, city) {
  g <- geocode(street, city); if (is.finite(as.numeric(g[1])) || spell_out(street) == street) return(g)
  Sys.sleep(1); geocode(spell_out(street), city)
}
G <- if (file.exists(GEO)) fread(GEO, colClasses = list(character = "order_no")) else data.table(order_no = character(), lon = numeric(), lat = numeric())
todo <- P[!order_no %in% G$order_no & nzchar(street)]
if (nrow(todo)) {
  new <- rbindlist(lapply(seq_len(nrow(todo)), function(k) { Sys.sleep(1); g <- geocode2(todo$street[k], todo$city[k])
    data.table(order_no = todo$order_no[k], lon = as.numeric(g[1]), lat = as.numeric(g[2])) }))
  if (any(is.finite(new$lon))) { G <- rbind(G, new[is.finite(lon)]); fwrite(G, GEO) }
}
P[G, on = "order_no", `:=`(lon = i.lon, lat = i.lat)]
if (!"lon" %in% names(P)) P[, `:=`(lon = NA_real_, lat = NA_real_)]
P[, placed := ifelse(is.finite(lon), "geocoded address", "city centre")]
P[CITY, on = "city", `:=`(lon = fifelse(is.finite(lon), lon, i.clon), lat = fifelse(is.finite(lat), lat, i.clat))]
P[, i := match(county_fips, app$counties$fips)]; stopifnot(!anyNA(P$i))
P[, cell := mapply(function(x, y) locate_cell(app, x, y), lon, lat)]

# ---- 2. three cases ------------------------------------------------------------------------------------------
P[, th := fifelse(is.finite(test_hours_per_engine), test_hours_per_engine, pmin(fcoalesce(as.numeric(hours_all_purposes), Inf), H_TEST_DEFAULT))]
P[, test_hours_assumed := !is.finite(test_hours_per_engine)]
P[, f_t4 := pmin(fcoalesce(as.numeric(tier4_engines), 0) / engines, 1)]
P[, `:=`(nox_max = nox_tpy * SHORT_TON, pm_gen_max = pm25_gen_tpy * SHORT_TON,
         pm_towers = pmax(fcoalesce(as.numeric(pm25_total_tpy), as.numeric(pm25_gen_tpy)) - pm25_gen_tpy, 0) * SHORT_TON, so2_max = fcoalesce(as.numeric(so2_tpy), 0) * SHORT_TON)]
P[, test_mwh := total_ekw * th * LOAD / 1e3]
P[, all_mwh := total_ekw * hours_all_purposes * LOAD / 1e3]
P[, `:=`(nox_test = pmin(6.4 * test_mwh / 1e3, nox_max), pm_test = pmin(0.20 * test_mwh / 1e3, pm_gen_max))]
P[, `:=`(nox_all = pmin(((1 - f_t4) * 6.4 + f_t4 * 0.67) * all_mwh / 1e3, nox_max),
         pm_all = pmin(((1 - f_t4) * 0.20 + f_t4 * 0.03) * all_mwh / 1e3, pm_gen_max))]
run <- function(nox, pm, so2, k) {
  t <- c(pm25 = fcoalesce(pm, 0) + P$pm_towers[k], so2 = so2, nox = nox)
  local_source(app, t, H_GEN, P$cell[k], P$i[k], B_K)$deaths
}
P[, deaths_test := vapply(seq_len(.N), function(k) run(nox_test[k], pm_test[k], 0, k), 0)]
P[, deaths_all := vapply(seq_len(.N), function(k) run(nox_all[k], pm_all[k], 0, k), 0)]
P[, deaths_max := vapply(seq_len(.N), function(k) run(nox_max[k], pm_gen_max[k], so2_max[k], k), 0)]
P[, deaths_towers := vapply(seq_len(.N), function(k) run(0, 0, 0, k), 0)]

# ---- 3. noise (screening; same method as the app and analysis/06) ------------------------------------------
P[, kw_unit := total_ekw / engines]; P[, heat_mw := total_ekw / 1e3 / 1.25]
P[, at_once := fcoalesce(as.numeric(test_at_once), 1)]
for (cs in names(NOISE_GEN)) {
  lw1 <- noise_lw_gen(P$kw_unit, NOISE_GEN[[cs]]$lp7)
  P[, (paste0("d55_test_", cs)) := round(noise_dist(lw1 + 10 * log10(at_once), 55, NOISE_H[["gen"]], NOISE_ALPHA[["gen"]]))]
  P[, (paste0("d55_outage_", cs)) := round(noise_dist(lw1 + 10 * log10(engines), 55, NOISE_H[["gen"]], NOISE_ALPHA[["gen"]]))]
  P[, (paste0("d55_cool_", cs)) := round(noise_dist(NOISE_COOL[[cs]]$lw_mw + 10 * log10(heat_mw), 55, NOISE_H[["cool"]], NOISE_ALPHA[["cool"]]))]
  P[, (paste0("d45_cool_", cs)) := round(noise_dist(NOISE_COOL[[cs]]$lw_mw + 10 * log10(heat_mw), 45, NOISE_H[["cool"]], NOISE_ALPHA[["cool"]]))]
}
# People within the central cooling and outage levels, all sites together, on a 50 m grid (as analysis/06).
CRS_WA <- 2855                                                  # Washington State Plane North, metres
pts <- st_coordinates(st_transform(st_as_sf(P, coords = c("lon", "lat"), crs = 4326), CRS_WA)); P[, `:=`(x = pts[, 1], y = pts[, 2])]
CELL <- 50; REACH <- 5000
gp <- unique(rbindlist(lapply(seq_len(nrow(P)), function(k) {
  g <- CJ(ix = seq(floor((P$x[k] - REACH) / CELL), ceiling((P$x[k] + REACH) / CELL)), iy = seq(floor((P$y[k] - REACH) / CELL), ceiling((P$y[k] + REACH) / CELL)))
  g[((ix * CELL - P$x[k])^2 + (iy * CELL - P$y[k])^2) <= REACH^2] })))
gp[, `:=`(x = ix * CELL, y = iy * CELL)]
shp <- Sys.getenv("BG_SHP", "data-raw/cb_2023_us_bg_500k/cb_2023_us_bg_500k.shp"); lyr <- sub("\\.shp$", "", basename(shp))
bg <- st_transform(st_read(shp, query = sprintf("SELECT GEOID FROM \"%s\" WHERE STATEFP = '53'", lyr), quiet = TRUE), CRS_WA)
bg$area <- as.numeric(st_area(bg)); D <- fread("build/bg_demog.csv", colClasses = list(character = "GEOID"), select = c("GEOID", "pop"))
bg$pop <- D$pop[match(bg$GEOID, D$GEOID)]
j <- vapply(st_intersects(st_as_sf(gp, coords = c("x", "y"), crs = CRS_WA), bg), function(h) if (length(h)) h[1] else NA_integer_, 0L)
gp[, pop := fcoalesce(bg$pop[j] / bg$area[j] * CELL^2, 0)]
level_sum <- function(lw, hs, a, combine = "sum") {
  acc <- rep(if (combine == "sum") 0 else -Inf, nrow(gp))
  for (k in seq_len(nrow(P))) { d <- sqrt((gp$x - P$x[k])^2 + (gp$y - P$y[k])^2); lp <- noise_lp(lw[k], d, hs, a)
    acc <- if (combine == "sum") acc + 10^(lp / 10) else pmax(acc, lp) }
  if (combine == "sum") 10 * log10(pmax(acc, 1e-10)) else acc
}
ppl <- list()
for (cs in names(NOISE_GEN)) {
  lw1 <- noise_lw_gen(P$kw_unit, NOISE_GEN[[cs]]$lp7)
  L <- list(test = level_sum(lw1 + 10 * log10(P$at_once), NOISE_H[["gen"]], NOISE_ALPHA[["gen"]], "max"),
            outage = level_sum(lw1 + 10 * log10(P$engines), NOISE_H[["gen"]], NOISE_ALPHA[["gen"]]),
            cool = level_sum(NOISE_COOL[[cs]]$lw_mw + 10 * log10(P$heat_mw), NOISE_H[["cool"]], NOISE_ALPHA[["cool"]]))
  for (src in names(L)) for (lv in c(55, 45)) ppl[[length(ppl) + 1]] <- data.table(source = src, case = cs, level_dba = lv, people = sum(gp$pop[L[[src]] >= lv]))
}
ppl <- rbindlist(ppl)

# ---- 4. outputs ------------------------------------------------------------------------------------------------
keep <- c("order_no", "facility", "street", "city", "county_fips", "issued", "placed", "lon", "lat", "engines", "total_ekw", "models",
          "hours_all_purposes", "test_hours_per_engine", "th", "test_hours_assumed", "test_at_once", "fuel_gal_yr", "nox_tpy", "nox_basis",
          "pm25_gen_tpy", "pm25_total_tpy", "tier4_engines", "cooling_towers", "nox_test", "nox_all", "nox_max", "deaths_test", "deaths_all",
          "deaths_max", "deaths_towers", grep("^d[0-9]+_", names(P), value = TRUE))
fwrite(P[order(-deaths_max), ..keep], file.path(OUT, "wa_permits_sites.csv"))
fwrite(ppl, file.path(OUT, "wa_noise_people.csv"))

say("Washington data center generators, from Ecology approval orders (analysis/08_wa_permits.R)")
say("Orders read: %d (Ecology lists %d data centers; %d have no posted document; Olympia is permitted by ORCAA)", nrow(P), 22L, 9L)
say("Engines: %s, %.0f MW; sites placed at a geocoded address: %d of %d", fmt(sum(P$engines)), sum(P$total_ekw) / 1e3, sum(P$placed == "geocoded address"), nrow(P))
say("All-purpose hour caps per engine: median %.0f h (range %.0f-%.0f); testing hours stated in %d orders (median %.0f h)",
    median(P$hours_all_purposes), min(P$hours_all_purposes), max(P$hours_all_purposes), sum(!P$test_hours_assumed), median(P$test_hours_per_engine, na.rm = TRUE))
say("Permitted NOx: %s short tons/yr in all (median order %.1f t; Virginia's median permit is 95 t)", fmt(sum(P$nox_tpy), 1), median(P$nox_tpy))
say("")
say("THREE CASES (NOx in metric tonnes; deaths/yr from PM2.5, including %.1f t/yr of cooling-tower PM2.5 in every case)", sum(P$pm_towers))
say("  routine testing      NOx %6.0f t   deaths %.2f", sum(P$nox_test), sum(P$deaths_test))
say("  all permitted hours  NOx %6.0f t   deaths %.2f", sum(P$nox_all), sum(P$deaths_all))
say("  permitted maximum    NOx %6.0f t   deaths %.2f", sum(P$nox_max), sum(P$deaths_max))
say("  of which cooling towers alone: %.2f deaths/yr", sum(P$deaths_towers))
say("")
say("NOISE (central case; distance to 55 dB(A), median / largest site)")
say("  routine test (%s engines at once, as the orders allow)  %s / %s m", paste(range(P$at_once), collapse = "-"), median(P$d55_test_central), max(P$d55_test_central))
say("  outage, every engine                                  %s / %s m", median(P$d55_outage_central), max(P$d55_outage_central))
say("  cooling                                               %s / %s m (45 dB(A): %s / %s m)", median(P$d55_cool_central), max(P$d55_cool_central),
    median(P$d45_cool_central), max(P$d45_cool_central))
for (src in c("test", "outage", "cool")) for (lv in c(55, 45)) {
  v <- ppl[source == src & level_dba == lv]
  say("  people >= %d dB(A), %-6s quiet %7s  central %7s  loud %7s", lv, src, fmt(round(v[case == "quiet", people], -1)),
      fmt(round(v[case == "central", people], -1)), fmt(round(v[case == "loud", people], -1)))
}
say("")
say("Not covered: the 9 East Wenatchee and Wenatchee orders Ecology lists without a document, Microsoft's Malaga site, and data")
say("centers permitted by local clean air agencies. NTT's order gives no site address (placed at Quincy's centre).")
writeLines(lines, file.path(OUT, "wa_permits_summary.txt"))
cat("wrote", file.path(OUT, c("wa_permits_sites.csv", "wa_noise_people.csv", "wa_permits_summary.txt")), sep = "\n")
