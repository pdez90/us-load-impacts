# CoreSite DE3 (4900 N Race St, Denver): backup generators under CDPHE construction permit 24DE0836 (issued 2025-07-17),
# run through the app chain. Permit terms: 14 Kohler KD3500 (5,040 hp, Tier 2); facility NOx < 24.9 tpy; fuel <= 181,679 gal/yr;
# 25 non-emergency h per engine; at most two engines at once outside emergencies, 7 am-7 pm. Manufacturer factors from Note 4.
# Run from the repo root: Rscript analysis/04_coresite_de3.R
source("R/calc.R")
app <- readRDS("data/app_data.rds"); app$og <- readRDS("data/onsite_groups.rds")
lon <- -104.9633; lat <- 39.7858
cell <- locate_cell(app, lon, lat); i <- match("08031", app$counties$fips)
cat(sprintf("site cell %d, %.0f km; county %s\n", cell, app$cells$km[cell], app$counties$name[i]))
gal_t <- 3.785 * 0.85 / 1000                                # tonnes of fuel per gallon
sc <- list(
  "Permitted maximum (fuel cap 181,679 gal at full load)" = list(gal = 181679, nox_lb_gal = 0.2741, hp_hr_gal = 5040 / 241.8),
  "Testing only: 25 h per engine at 75% load"            = list(gal = 14 * 25 * 198.8, nox_lb_gal = 0.1532, hp_hr_gal = 3780 / 198.8),
  "Testing as CoreSite reports elsewhere: 10 h at 75%"   = list(gal = 14 * 10 * 198.8, nox_lb_gal = 0.1532, hp_hr_gal = 3780 / 198.8))
B <- CRF$krewski$beta; out <- list()
for (nm in names(sc)) { s <- sc[[nm]]
  t <- c(pm25 = 0.2 * s$gal * s$hp_hr_gal / 1e6, so2 = s$gal * gal_t * 15e-6 * 2, nox = s$nox_lb_gal * s$gal * 0.4536 / 1e3)
  for (h in c(15, 60)) { ls <- local_source(app, t, h, cell, i, B); d <- ls$ses
    out[[length(out) + 1]] <- data.frame(scenario = nm, height_m = h, fuel_gal = s$gal, nox_t = t[["nox"]], pm25_t = t[["pm25"]], so2_t = t[["so2"]],
      co2_t = s$gal * 10.21 / 1e3, deaths = ls$deaths,
      black = d$ratio[d$group == "Black"], latino = d$ratio[d$group == "Latino"], white = d$ratio[d$group == "WhiteNoLat"],
      inc_q1 = d$ratio[d$group == "income_q1"], inc_q5 = d$ratio[d$group == "income_q5"], below_pov = d$ratio[d$group == "below_poverty"]) } }
o <- do.call(rbind, out); print(format(o, digits = 3), row.names = FALSE)
# share of deaths by pollutant, permitted max at 15 m
s <- sc[[1]]; t <- c(pm25 = 0.2 * s$gal * s$hp_hr_gal / 1e6, so2 = s$gal * gal_t * 30e-6, nox = 0.2741 * s$gal * 0.4536 / 1e3)
for (p in names(t)) { tt <- t * 0; tt[p] <- t[p]; cat(sprintf("  %s: %.4f deaths/yr\n", p, local_source(app, tt, 15, cell, i, B)$deaths)) }
# compare: the whole Denver-area grid supply for the same facility? DE3 IT load not public; for scale, 1 MW of avg grid supply
r <- impact(app, "08031", 8.76e-3 * 0.7, scenario = "avg", site_cell = cell)$totals
cat(sprintf("For scale: 1 MW of average-mix grid power at 70%% load in Denver: %.4f deaths/yr, %.0f t NOx\n", r$deaths, r$nox_t))
