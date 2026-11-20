# =============================================================================
# 05_va_permits.R  --  Virginia data center backup generators, from their air permits
# -----------------------------------------------------------------------------
# In : data-raw/permits/va/va_permits.csv           (scripts/08: limits, engines, hours, addresses)
#      data-raw/permits/va/va_permits_geocoded.csv  (scripts/09, optional: site coordinates)
#      data/app_data.rds, data/onsite_groups.rds
# Out: analysis/output/va_permits_sites.csv, va_permits_by_county.csv, va_permits_groups.csv,
#      va_permits_summary.txt, va_permits_map.png
#
# Three cases for each current permit (routine testing; testing + 50 h of grid support; permitted maximum):
#   permitted maximum : the annual tons/yr limits in the permit (NOx, PM2.5 or else PM10, SO2 if set)
#   routine testing   : engines x rated kW x the permit's maintenance/testing hours per engine (32 h, the median,
#                       where none is stated) x 75% load, at EPA Tier 2 rates (6.4 g/kWh NOx, 0.2 g/kWh PM), capped
#                       at the permitted maximum; needs the engine count and ratings
#   grid support      : routine testing plus 50 h per engine in grid emergencies, SCR engines at Tier 4 rates
# Emissions are released near the ground (10 m) in the site's model grid cell and run through the
# same source-receptor matrix as the app (R/calc.R local_source). Short tons are converted to tonnes.
# Permits that a later one replaces are dropped (column 'current' from scripts/08).
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(sf) })
sf_use_s2(FALSE)
source("R/calc.R")
OUT <- "analysis/output"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
lines <- character(); say <- function(...) { s <- sprintf(...); lines <<- c(lines, s); cat(s, "\n") }
SHORT_TON <- 0.90718474; B_K <- CRF$krewski$beta; H_GEN <- 10

app <- readRDS("data/app_data.rds"); app$og <- if (file.exists("data/onsite_groups.rds")) readRDS("data/onsite_groups.rds")
P <- fread("data-raw/permits/va/va_permits.csv", colClasses = list(character = c("registration", "zip")))
n_all <- nrow(P); P <- P[current == "yes"]

# ---- where each site is -------------------------------------------------------------------------------
cty <- as.data.table(st_drop_geometry(app$counties))[state == "VA"]
cty[, city := as.integer(fips) >= 51500]
loc_fips <- function(l) {                                   # "Loudoun Co." -> 51107, "Manassas City" -> 51683
  nm <- sub("\\s+(Co\\.|County|City)$", "", l); is_city <- grepl("City$", l)
  hit <- cty[name == nm & city == is_city]; if (nrow(hit) == 1) hit$fips else NA_character_
}
P[, fips := vapply(locality, loc_fips, "")]
stopifnot(!anyNA(P$fips))
G <- if (file.exists("data-raw/permits/va/va_permits_geocoded.csv")) fread("data-raw/permits/va/va_permits_geocoded.csv",
       colClasses = list(character = c("registration", "state_fips", "county_fips"))) else NULL
P[, `:=`(lon = NA_real_, lat = NA_real_, placed = "county point")]
if (!is.null(G)) {
  P[G[match == "Match"], on = "registration", `:=`(glon = as.numeric(i.lon), glat = as.numeric(i.lat), gf = paste0(i.state_fips, i.county_fips))]
  # Owners' offices that some permits give before the site (Amazon's Herndon addresses) are never used.
  # A point in a neighbouring county is kept if it is within 10 km of the county DEQ lists: DEQ's locality
  # can be the mailing city (Manassas) or the wrong side of a line (Capital One's West Creek campus is in Goochland).
  office <- grepl("13820 SUNRISE VALLEY|13200 WOODLAND PARK|410 TERRY AVE", toupper(G$matched_address[match(P$registration, G$registration)]))
  near <- rep(FALSE, nrow(P)); has <- which(!is.na(P$glon))
  if (length(has)) {
    pts <- st_transform(st_as_sf(P[has], coords = c("glon", "glat"), crs = 4326), LCC_PROJ)
    pol <- st_transform(app$counties[match(P$fips[has], app$counties$fips), ], LCC_PROJ)
    near[has] <- as.numeric(st_distance(pts, pol, by_element = TRUE)) <= 10000
  }
  same <- !is.na(P$glon) & P$gf == P$fips
  ok <- !is.na(P$glon) & !office & (same | near)
  P[ok, `:=`(lon = glon, lat = glat, placed = ifelse(same[ok], "geocoded address", "geocoded address (neighbouring county)"))]
  rej <- which(!ok & !is.na(P$glon))
  P[rej, placed := ifelse(office[rej], "county point (address is the owner's office)", "county point (address geocoded far from the permit's county)")]
}
P[, i := match(fips, app$counties$fips)]
P[, cell := ifelse(is.na(lon), app$onsite$cell[i] + 1, mapply(function(x, y) locate_cell(app, x, y), lon, lat))]
P[is.na(lon), `:=`(lon = app$counties$lon[i], lat = app$counties$lat[i])]

# ---- emissions ------------------------------------------------------------------------------------------
num <- function(x) suppressWarnings(as.numeric(x))
P[, pm25 := fcoalesce(num(pm25_tpy), num(pm10_tpy))]
P[, `:=`(nox_max = num(nox_tpy) * SHORT_TON, pm25_max = pm25 * SHORT_TON, so2_max = fcoalesce(num(so2_tpy), 0) * SHORT_TON)]
# Rates (g/kWh): uncontrolled Tier 2 NOx 6.4 (NMHC+NOx limit), PM 0.20; engines with SCR (and particle filters) at the
# Tier 4 generator-set limits, NOx 0.67, PM 0.03, but only in longer runs: in short test runs the catalyst is not at
# temperature, so testing is counted uncontrolled.
H_GRID <- 50; LOAD <- 0.75; H_TEST_DEFAULT <- 32                  # 50 h: federal allowance for non-emergency use of emergency engines
P[, has_eq := num(engines) > 0 & num(total_ekw) > 0]
P[, th := fcoalesce(num(test_hours_per_engine), H_TEST_DEFAULT)]
P[, test_hours_assumed := is.na(num(test_hours_per_engine))]
P[, f_scr := pmin(fcoalesce(num(scr_engines), 0) / pmax(num(engines), 1), 1)]
P[, test_mwh := ifelse(has_eq, num(total_ekw) * th * LOAD / 1e3, NA)]          # stated testing hours, or 32 (flagged)
P[, `:=`(nox_test = pmin(6.4 * test_mwh / 1e3, nox_max), pm25_test = pmin(0.2 * test_mwh / 1e3, fcoalesce(pm25_max, Inf)))]
# grid support: testing (stated hours, or 32) plus 50 h per engine in grid emergencies, capped at the permitted limits
P[, grid_mwh := ifelse(has_eq, num(total_ekw) * H_GRID * LOAD / 1e3, NA)]
P[, all_test_mwh := ifelse(has_eq, num(total_ekw) * th * LOAD / 1e3, NA)]
P[, nox_grid := pmin((6.4 * all_test_mwh + ((1 - f_scr) * 6.4 + f_scr * 0.67) * grid_mwh) / 1e3, nox_max)]
P[, pm25_grid := pmin((0.20 * all_test_mwh + ((1 - f_scr) * 0.20 + f_scr * 0.03) * grid_mwh) / 1e3, fcoalesce(pm25_max, Inf))]

run <- function(nox, pm, so2, cell, i) {
  if (!is.finite(nox) || is.na(cell)) return(list(deaths = NA_real_, ses = NULL))
  t <- c(pm25 = fcoalesce(pm, 0), so2 = fcoalesce(so2, 0), nox = nox)
  local_source(app, t, H_GEN, cell, i, B_K)
}
res_max <- lapply(seq_len(nrow(P)), function(k) run(P$nox_max[k], P$pm25_max[k], P$so2_max[k], P$cell[k], P$i[k]))
res_tst <- lapply(seq_len(nrow(P)), function(k) run(P$nox_test[k], P$pm25_test[k], 0, P$cell[k], P$i[k]))
res_grd <- lapply(seq_len(nrow(P)), function(k) run(P$nox_grid[k], P$pm25_grid[k], 0, P$cell[k], P$i[k]))
P[, deaths_max := vapply(res_max, `[[`, 0, "deaths")]; P[, deaths_test := vapply(res_tst, `[[`, 0, "deaths")]
P[, deaths_grid := vapply(res_grd, `[[`, 0, "deaths")]

keep <- c("registration", "site", "locality", "fips", "issued", "program", "street", "placed", "lon", "lat", "engines", "total_ekw",
          "test_hours_per_engine", "scr_engines", "scr_source", "hours_all_purposes", "grid_emergency_use", "ozone_season_limit", "testing_window",
          "fuel_gal_yr", "co_tpy", "voc_tpy", "nox_tpy", "pm25_tpy", "pm10_tpy", "nox_read_as", "deaths_max", "nox_test", "deaths_test",
          "nox_grid", "deaths_grid", "test_hours_assumed", "flags")
fwrite(P[order(-deaths_max), ..keep], file.path(OUT, "va_permits_sites.csv"))

# ---- summaries ----------------------------------------------------------------------------------------
nova <- c("51107", "51153", "51059", "51683", "51685", "51600", "51610", "51013", "51510")   # Loudoun, Prince William, Fairfax, Manassas, Manassas Park, Fairfax city, Falls Church, Arlington, Alexandria
say("VIRGINIA DATA CENTER AIR PERMITS (DEQ list of 194; %d read, %d current after replaced permits are dropped)", n_all, nrow(P))
say("  not available: 74331-1 (Westfax 4-5A, Fairfax Co.); DEQ's link for it opens the Amazon IAD-45 permit (74333-1)")
say("  engines: %s in %d permits with an equipment list read; rated %.1f GW", format(sum(num(P$engines), na.rm = TRUE), big.mark = ","),
    sum(num(P$engines) > 0, na.rm = TRUE), sum(num(P$total_ekw), na.rm = TRUE) / 1e6)
say("  located: %d by geocoded address (%d of them just over the listed county's line), %d at the county point (%d owner's office addresses)",
    sum(grepl("^geocoded", P$placed)), sum(grepl("neighbouring", P$placed)), sum(grepl("^county", P$placed)), sum(grepl("office", P$placed)))
say("PERMITTED MAXIMUM (every site at its annual limits; short tons/yr)")
say("  NOx %s  PM2.5 %s   ->  %.1f PM2.5 deaths/yr (Krewski)", format(round(sum(num(P$nox_tpy), na.rm = TRUE)), big.mark = ","),
    format(round(sum(P$pm25, na.rm = TRUE)), big.mark = ","), sum(P$deaths_max, na.rm = TRUE))
say("  Northern Virginia (Loudoun, Prince William, Fairfax and cities): NOx %s tons/yr, %.1f deaths/yr",
    format(round(sum(num(P[fips %in% nova]$nox_tpy), na.rm = TRUE)), big.mark = ","), sum(P[fips %in% nova]$deaths_max, na.rm = TRUE))
say("OPERATING TERMS")
say("  engines with SCR: %s of %s (%.0f%%); permits allowing use in grid-operator emergencies: %d; 500-hour all-purpose cap: %d",
    format(sum(num(P$scr_engines), na.rm = TRUE), big.mark = ","), format(sum(num(P$engines), na.rm = TRUE), big.mark = ","),
    100 * sum(num(P$scr_engines), na.rm = TRUE) / sum(num(P$engines), na.rm = TRUE), sum(P$grid_emergency_use %in% c(TRUE, "True")),
    sum(num(P$hours_all_purposes) == 500, na.rm = TRUE))
say("  testing restricted in ozone season: %d permits; testing window set: %d (mostly 7 am-5 pm); annual fuel caps: %d permits, %.1f million gal",
    sum(P$ozone_season_limit %in% c(TRUE, "True")), sum(nzchar(P$testing_window)), sum(!is.na(num(P$fuel_gal_yr))), sum(num(P$fuel_gal_yr), na.rm = TRUE) / 1e6)
gg <- P[is.finite(deaths_grid)]
say("THREE CASES, permits with an equipment list (%d; testing hours assumed 32 for %d)", nrow(gg), sum(gg$test_hours_assumed))
say("  routine testing            NOx %6s t/yr  %5.1f deaths/yr", format(round(sum(gg$nox_test)), big.mark = ","), sum(gg$deaths_test))
say("  testing + 50 h grid support NOx %6s t/yr  %5.1f deaths/yr (SCR credited in the grid hours)", format(round(sum(gg$nox_grid)), big.mark = ","), sum(gg$deaths_grid))
say("  permitted maximum          NOx %6s t/yr  %5.1f deaths/yr", format(round(sum(gg$nox_max, na.rm = TRUE)), big.mark = ","), sum(gg$deaths_max, na.rm = TRUE))
gx <- copy(gg); gx[, f_scr := 0]
say("  grid support if no engine had SCR: NOx %s t/yr", format(round(sum(pmin((6.4 * gx$all_test_mwh + 6.4 * gx$grid_mwh) / 1e3, gx$nox_max))), big.mark = ","))
byc <- P[, .(permits = .N, engines = sum(num(engines), na.rm = TRUE), gw = sum(num(total_ekw), na.rm = TRUE) / 1e6,
             nox_tpy = sum(num(nox_tpy), na.rm = TRUE), deaths_max = sum(deaths_max, na.rm = TRUE)), by = locality][order(-nox_tpy)]
fwrite(byc, file.path(OUT, "va_permits_by_county.csv"))
say("BY LOCALITY (permitted maximum)")
for (k in seq_len(min(8, nrow(byc)))) with(byc[k], say("  %-20s %3d permits %6s engines %5.1f GW  NOx %6s tons/yr  %.1f deaths/yr",
                                                         locality, permits, format(engines, big.mark = ","), gw, format(round(nox_tpy), big.mark = ","), deaths_max))
# who is exposed: all sites at their limits
grp <- Reduce(add_ses, Filter(Negate(is.null), lapply(res_max, `[[`, "ses")))
if (!is.null(grp)) {
  fwrite(grp, file.path(OUT, "va_permits_groups.csv"))
  say("WHO IS EXPOSED (all sites at their limits; exposure relative to everyone in the contiguous US)")
  for (g in c("WhiteNoLat", "Black", "Asian", "Latino", "income_q1", "income_q5", "below_poverty"))
    with(grp[grp$group == g, ], say("  %-28s x%.2f", label, ratio))
}
# map
va <- st_transform(app$counties[app$counties$state == "VA", ], 4326)
png(file.path(OUT, "va_permits_map.png"), width = 2000, height = 1150, res = 200)
par(mar = c(0.5, 0.5, 2.5, 0.5))
plot(st_geometry(va), col = "grey96", border = "grey75", main = "Virginia data center generator permits: permitted NOx (circle area ~ tons/yr)")
o <- P[is.finite(num(nox_tpy))][order(-num(nox_tpy))]
points(o$lon, o$lat, pch = 21, cex = 0.4 + 2.2 * sqrt(num(o$nox_tpy) / max(num(o$nox_tpy))),
       bg = adjustcolor(ifelse(grepl("^geocoded", o$placed), "#c0392b", "#7f8c8d"), 0.5), col = "grey30", lwd = 0.4)
legend("bottomleft", c("site address", "county point (address not located)"), pt.bg = adjustcolor(c("#c0392b", "#7f8c8d"), 0.6),
       pch = 21, pt.cex = 1.3, bty = "n", cex = 0.8)
dev.off()
writeLines(lines, file.path(OUT, "va_permits_summary.txt"))
cat("wrote", OUT, "\n")
