# =============================================================================
# calc.R  --  the arithmetic behind the app, kept free of Shiny so it can be
#             tested on its own (tests/test_calc.R)
# =============================================================================
# Everything in data/app_data.rds is stored per TWh/yr generated (grid mixes) or
# per tonne/yr emitted (on-site power), so a result is a rescaling.

#' GEMM (Burnett et al. 2018, PNAS 115:9592) for non-communicable disease and lower respiratory infection deaths at
#' ages 25+, with the Chinese cohort: log RR(z) = theta log(1 + z/alpha) / (1 + exp(-(z - mu)/nu)), z = max(0, C - 2.4).
#' The tool's increments are small, so GEMM is used through its slope at a baseline concentration c0 (default 8 ug/m3,
#' roughly the US population-weighted annual mean), and scaled by the share of all deaths that are NCD + LRI at ages
#' 25+ (about 0.87 in the US: injuries are about 9% of deaths and deaths under 25 about 2%), because the tool's
#' baseline mortality is all-cause at all ages. Chen et al. (2026, ERL 21 214046) use the full GEMM with age-specific
#' rates; the slope is steeper at lower baseline concentrations, so this single value is an approximation.
gemm_beta <- function(c0 = 8, theta = 0.1430, alpha = 1.6, mu = 15.5, nu = 36.8, cf = 2.4, ncd_lri_share = 0.87) {
  z <- max(c0 - cf, 0); w <- 1 / (1 + exp(-(z - mu) / nu))
  ncd_lri_share * theta * (w / (alpha + z) + log(1 + z / alpha) * w * (1 - w) / nu)
}
CRF <- list(
  krewski = list(label = "Krewski et al. 2009 (ACS): RR 1.06 per 10 ug/m3", beta = log(1.06) / 10),
  orellano = list(label = "Orellano et al. 2024 (WHO meta-analysis): RR 1.095 per 10 ug/m3", beta = log(1.095) / 10),
  lepeule = list(label = "Lepeule et al. 2012 (Six Cities): RR 1.14 per 10 ug/m3", beta = log(1.14) / 10),
  gemm    = list(label = "GEMM, Burnett et al. 2018 (as Chen et al. 2026): slope at 8 ug/m3, about RR 1.09 per 10 ug/m3", beta = gemm_beta()))

#' The ISRM's chemistry is that of 2005. As SO2 and NOx have fallen since, a tonne of NOx forms less particulate and
#' a tonne of SO2 more. Factors for 2012 relative to 2005 from Holt et al. 2015 (Environ. Sci. Technol. 49:4834), as
#' applied to the ISRM by Thakrar et al. 2024 and Bekbulat et al. 2025 (who extend the trend to 2019). Primary PM2.5 is unchanged.
CHEM_2012 <- c(pm25 = 1, so2 = 1.23, nox = 0.71)

PRESETS <- list(
  dc_evap  = list(label = "Data center, evaporative cooling", basis = "it", pue = 1.20, lf = 0.70, wue = 1.8),
  dc_dry   = list(label = "Data center, air or closed-loop cooling", basis = "it", pue = 1.35, lf = 0.70, wue = 0.2),
  industry = list(label = "Industrial load", basis = "facility", pue = 1, lf = 0.85, wue = 0),
  ev       = list(label = "EV charging hub", basis = "facility", pue = 1, lf = 0.15, wue = 0),
  custom   = list(label = "Custom", basis = "facility", pue = 1, lf = 1, wue = 0))

# kg per MWh generated and litres per MWh; illustrative, to be replaced by permit values.
ONSITE <- list(
  cc_scr = list(label = "Combined cycle, SCR, wet cooling", co2 = 370, nox = 0.03, so2 = 0.002, pm25 = 0.02, water = 750),
  sc_scr = list(label = "Simple-cycle turbines, SCR",       co2 = 530, nox = 0.04, so2 = 0.003, pm25 = 0.03, water = 0),
  sc_dln = list(label = "Simple-cycle turbines, no SCR",    co2 = 530, nox = 0.15, so2 = 0.003, pm25 = 0.03, water = 0))

# Backup diesel generators, kg per MWh of generator output (= g/kWh).
# NOx and PM2.5: EPA limits for engines of 560 kW and over. Tier 2 (40 CFR 89.112): NMHC+NOx 6.4, PM 0.20; the
# whole NMHC+NOx limit is counted as NOx, an upper bound. Tier 4 final generator sets (40 CFR 1039.101): NOx 0.67,
# PM 0.03. Engines used only for emergencies are exempt from Tier 4, so most backup units are Tier 2.
# SO2: ultra-low-sulfur diesel (15 ppm) with AP-42 section 3.4's factor, 8.09e-3 x S(%) lb/hp-hr -> 0.007.
# CO2: AP-42 3.4 average fuel use, 7,000 Btu/hp-hr (9.39 MMBtu/MWh), x 73.96 kg CO2/MMBtu for distillate -> about 700.
GENSETS <- list(
  tier2 = list(label = "EPA Tier 2 (typical for emergency backup)", nox = 6.4, pm25 = 0.20, so2 = 0.007, co2 = 700),
  tier4 = list(label = "EPA Tier 4 final (generator sets)",          nox = 0.67, pm25 = 0.03, so2 = 0.007, co2 = 700))

SCENARIOS <- c("Grid: the plants that respond to new load" = "nb", "Grid: average of all plants" = "avg",
               "Grid: a mix you set yourself" = "custom",
               "On-site gas generation" = "onsite", "Carbon-free power matched every hour" = "clean")
GRID_SCENARIOS <- c("nb", "avg", "custom")
FUEL_LABELS <- c(coal = "Coal", gas = "Gas", oil = "Oil", biomass = "Biomass", otherfossil = "Other fossil", nuclear = "Nuclear",
                 hydro = "Hydro", wind = "Wind", solar = "Solar", geothermal = "Geothermal")

# Plain-language notes on each supply option, shown in the app.
OPTION_INFO <- list(
  nb = list(
    what = "The load is served by the regional grid, and the extra electricity comes from the plants that ramp up and down with demand: mostly gas and coal. Nuclear, wind, solar and hydro are left out because they run whenever they can, whatever the load. This is EPA eGRID's 'non-baseload' mix.",
    use = "Use it to ask what a NEW load adds. EPA suggests non-baseload rates for rough estimates of the emissions change when electricity use changes.",
    caveat = "It is an annual regional approximation, not a dispatch model. It ignores which hours the load runs, power traded between regions, and any new plants built because of the load."),
  avg = list(
    what = "The load is served by the regional grid and takes a proportional share of every plant in the region, clean and dirty alike, according to how much each generated in the data year.",
    use = "Use it for footprints and inventories (for example location-based Scope 2). EPA recommends these total-output rates for that purpose.",
    caveat = "It understates what a new load adds when the region has a lot of nuclear, hydro, wind or solar, because that output would have been produced anyway."),
  custom = list(
    what = "The load is served by the regional grid, with fuel shares you choose. The sliders start at the region's current mix (the same as 'average of all plants'), so you can ask what happens if the grid gets cleaner or dirtier: more wind and solar, a coal retirement, more gas.",
    use = "Use it for what-if questions about a different or future grid. Each fuel is represented by the region's existing plants of that fuel, in their current proportions.",
    caveat = "Moving a slider changes how much each fuel supplies, not which plants within that fuel or where they are. New plants would not sit where today's do, and new gas plants are cleaner than the existing fleet. Mapped by county only."),
  onsite = list(
    what = "The facility generates its own power on site with gas turbines or engines, as some large data centers now do. Emissions are released in the host county, at the rates you enter.",
    use = "Use it for behind-the-meter gas plants, with the emission rates from the air permit.",
    caveat = "No downwind map; totals and who is exposed only. The default rates are illustrative. Backup diesel generators are separate: add them below, with any supply option."),
  clean = list(
    what = "Every hour of the load is matched by new carbon-free generation (wind, solar, storage, nuclear), so there are no operational emissions and no power-plant water use.",
    use = "Use it as the best case, or to see what on-site water use alone amounts to.",
    caveat = "Annual renewable certificates or power purchase agreements that are not matched hour by hour do not achieve this. Use the carbon-free share slider on the grid options for partial matching."))

# The InMAP grid's projection (Lambert conformal conic on a sphere).
LCC_PROJ <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370997 +b=6370997 +units=m +no_defs"

#' Index of the model grid cell that contains a longitude/latitude, or NA outside the grid.
locate_cell <- function(app, lon, lat) {
  if (is.null(app$cells$W)) return(NA_integer_)
  xy <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326), LCC_PROJ))
  i <- which(app$cells$W <= xy[1] & xy[1] < app$cells$E & app$cells$S <= xy[2] & xy[2] < app$cells$N)
  if (length(i)) i[1] else NA_integer_
}

#' Annual electricity at the meter.
#' @param basis "it": mw is IT/process load and is multiplied by PUE; "facility": mw is the whole site.
load_energy <- function(mw, basis = "it", pue = 1, load_factor = 1) {
  stopifnot(mw >= 0, pue >= 1, load_factor >= 0, load_factor <= 1)
  fac <- if (basis == "it") mw * pue else mw
  list(facility_mw = fac, twh = fac * load_factor * 8760 / 1e6, it_twh = fac * load_factor * 8760 / 1e6 / pue)
}

#' Split a release at height h (m) between the SR layers that bracket it.
layer_weights <- function(h, mids) {
  w <- numeric(length(mids))
  if (h <= mids[1]) { w[1] <- 1; return(w) }
  if (h >= mids[length(mids)]) { w[length(mids)] <- 1; return(w) }
  i <- max(which(mids <= h)); up <- (h - mids[i]) / (mids[i + 1] - mids[i])
  w[i] <- 1 - up; w[i + 1] <- up; w
}

#' The region's current fuel shares (by plant fuel category), the starting point for a custom mix.
default_shares <- function(app, subrgn) {
  mx <- as.data.frame(app$mix); r <- mx[mx$subrgn == subrgn & grepl("^f", mx$mix), ]
  s <- setNames(r$cat_share, sub("^f", "", r$mix)); s <- s[intersect(names(FUEL_LABELS), names(s))]
  s / sum(s)
}

#' Emission and water rates per MWh, and the weights on the stored fields, for a grid scenario.
grid_mix <- function(app, subrgn, scenario, shares = NULL) {
  mx <- as.data.frame(app$mix); rate_cols <- c("co2_kg_mwh", "nox_kg_mwh", "so2_kg_mwh", "pm25_kg_mwh", "wcons_l_mwh", "wwith_l_mwh")
  if (scenario %in% c("avg", "nb")) {
    m <- mx[mx$subrgn == subrgn & mx$mix == scenario, ]; stopifnot(nrow(m) == 1)
    return(list(rates = as.list(m[rate_cols]), w = setNames(1, paste(subrgn, scenario, sep = "_"))))
  }
  d <- default_shares(app, subrgn)
  if (is.null(shares)) shares <- d
  s <- setNames(numeric(length(d)), names(d)); ok <- intersect(names(shares), names(d)); s[ok] <- pmax(unlist(shares[ok]), 0)
  if (sum(s) <= 0) s <- d
  s <- s / sum(s)
  r <- mx[mx$subrgn == subrgn & mx$mix %in% paste0("f", names(s)), ]; r <- r[match(paste0("f", names(s)), r$mix), ]
  rates <- lapply(setNames(rate_cols, rate_cols), function(cn) if (all(is.na(r[[cn]]))) NA_real_ else sum(s * ifelse(is.na(r[[cn]]), 0, r[[cn]])))
  w <- setNames(s, paste(subrgn, r$mix, sep = "_")); w <- w[names(w) %in% colnames(app$K) & w > 0]   # fuels without emissions have no field
  list(rates = rates, w = w, shares = s)
}

#' Impacts of one load under one electricity scenario.
#' @param site_cell for on-site generation: index of the grid cell holding the site (see locate_cell)
#' @param shares for scenario "custom": named fuel shares (see default_shares); NULL uses the region's current mix.
#' @return list with totals (one-row data.frame), county (data.frame or NULL), groups (named vector or NULL), w (field weights)
#' @param gensets backup diesel generators: list(n, kw, hours, load (0-1), rates = one of GENSETS, height_m), or NULL.
#'   Their emissions are released in the site's cell and added to every scenario; reported separately as genset_*.
impact <- function(app, fips, twh, ..., beta = CRF$krewski$beta, site_cell = NULL, gensets = NULL) {
  r <- impact_supply(app, fips, twh, ..., beta = beta, site_cell = site_cell)
  z <- r$totals; z$genset_mwh <- 0; z$genset_co2_t <- 0; z$genset_nox_t <- 0; z$genset_pm25_t <- 0; z$genset_so2_t <- 0; z$genset_deaths <- 0
  if (!is.null(gensets) && gensets$n > 0 && gensets$kw > 0 && gensets$hours > 0) {
    mwh <- gensets$n * gensets$kw * gensets$hours * gensets$load / 1e3
    q <- gensets$rates; t <- c(pm25 = q$pm25, so2 = q$so2, nox = q$nox) * mwh / 1e3      # kg/MWh x MWh -> tonnes
    ls <- local_source(app, t, gensets$height_m, site_cell, match(fips, app$counties$fips), beta)
    z$genset_mwh <- mwh; z$genset_co2_t <- q$co2 * mwh / 1e3
    z$genset_nox_t <- t[["nox"]]; z$genset_pm25_t <- t[["pm25"]]; z$genset_so2_t <- t[["so2"]]; z$genset_deaths <- ls$deaths
    z$co2_t <- z$co2_t + z$genset_co2_t; z$nox_t <- z$nox_t + t[["nox"]]; z$so2_t <- z$so2_t + t[["so2"]]; z$pm25_t <- z$pm25_t + t[["pm25"]]
    z$deaths <- z$deaths + ls$deaths; z$pw_ugm3 <- z$pw_ugm3 + ls$e / sum(app$counties$pop)
    r$ses <- add_ses(r$ses, ls$ses)
  }
  r$totals <- z; r
}

#' The electricity supply alone (no backup generators); see impact().
impact_supply <- function(app, fips, twh, it_twh = twh, scenario = "nb", subrgn = NULL, loss = 0.05, clean_share = 0,
                   beta = CRF$krewski$beta, wue = 0, onsite = ONSITE$sc_scr, onsite_height = 60, shares = NULL, site_cell = NULL) {
  stopifnot(fips %in% app$counties$fips, loss >= 0, loss < 1, clean_share >= 0, clean_share <= 1)
  i <- match(fips, app$counties$fips)
  if (is.null(subrgn) || !nzchar(subrgn)) subrgn <- app$counties$subrgn[i]
  water_site <- wue * it_twh * 1e9 / 1e6                      # L/kWh x kWh -> megalitres
  z <- data.frame(scenario = scenario, subrgn = subrgn, col = NA_character_, twh = twh, gen_twh = 0, co2_t = 0, nox_t = 0, so2_t = 0, pm25_t = 0,
                  deaths = 0, pw_ugm3 = 0, water_site_ML = water_site, water_gen_cons_ML = 0, water_gen_with_ML = NA_real_,
                  deaths_host_county = NA_real_, deaths_host_state = NA_real_)
  if (scenario == "clean" || twh == 0) return(list(totals = z, county = NULL, groups = NULL, w = NULL))
  pop_total <- sum(app$counties$pop)
  if (scenario == "onsite") {
    mwh <- twh * 1e6
    t <- c(pm25 = onsite$pm25, so2 = onsite$so2, nox = onsite$nox) * mwh / 1e3      # tonnes/yr
    ls <- local_source(app, t, onsite_height, site_cell, i, beta)
    z$gen_twh <- twh; z$co2_t <- onsite$co2 * mwh / 1e3
    z$nox_t <- t[["nox"]]; z$so2_t <- t[["so2"]]; z$pm25_t <- t[["pm25"]]
    z$deaths <- ls$deaths; z$pw_ugm3 <- ls$e / pop_total
    z$water_gen_cons_ML <- onsite$water * mwh / 1e6
    return(list(totals = z, county = NULL, groups = NULL, ses = ls$ses, w = NULL))
  }
  g <- grid_mix(app, subrgn, scenario, shares); m <- g$rates; w <- g$w
  gen <- twh / (1 - loss) * (1 - clean_share); mwh <- gen * 1e6
  z$gen_twh <- gen; z$col <- if (scenario == "custom") "custom" else names(w)
  z$co2_t <- m$co2_kg_mwh * mwh / 1e3; z$nox_t <- m$nox_kg_mwh * mwh / 1e3
  z$so2_t <- m$so2_kg_mwh * mwh / 1e3; z$pm25_t <- m$pm25_kg_mwh * mwh / 1e3
  z$water_gen_cons_ML <- m$wcons_l_mwh * mwh / 1e6; z$water_gen_with_ML <- m$wwith_l_mwh * mwh / 1e6
  comb <- function(M) if (length(w)) as.vector(M[, names(w), drop = FALSE] %*% w) else numeric(nrow(M))
  K <- comb(app$K); E <- comb(app$E); A <- comb(app$A)
  d <- beta * K * gen
  conc <- ifelse(app$counties$pop > 0, E / pmax(app$counties$pop, 1e-9), A) * gen
  z$deaths <- sum(d); z$pw_ugm3 <- sum(E) * gen / pop_total
  z$deaths_host_county <- d[i]; z$deaths_host_state <- sum(d[app$counties$state == app$counties$state[i]])
  county <- data.frame(fips = app$counties$fips, name = app$counties$name, state = app$counties$state,
                       pop = app$counties$pop, dpm_ugm3 = conc, deaths = d)
  groups <- if (is.null(app$groups)) NULL else if (length(w)) setNames(as.vector(app$groups[, names(w), drop = FALSE] %*% w) * gen, rownames(app$groups)) else setNames(numeric(nrow(app$groups)), rownames(app$groups))
  list(totals = z, county = county, groups = groups, ses = ses_values(app, w, gen, beta), w = w, shares = g$shares)
}

#' Exposure and deaths by race/ethnicity, income, poverty and education, from block groups
#' (scripts/06_blockgroups.R). NULL when app_data was built without that step.
#' @return data.frame: group, label, pop, dpm_ugm3 (population-weighted increment), ratio (to everyone),
#'   deaths, deaths_per_million
ses_values <- function(app, w, gen, beta) {
  s <- app$ses; if (is.null(s) || !length(w) || !all(names(w) %in% colnames(s$exposure))) return(NULL)
  ex <- as.vector(s$exposure[, names(w), drop = FALSE] %*% w) * gen
  de <- beta * as.vector(s$mort_weighted[, names(w), drop = FALSE] %*% w) * gen
  data.frame(group = rownames(s$exposure), label = unname(s$labels[rownames(s$exposure)]), pop = unname(s$pop),
             dpm_ugm3 = ex, ratio = if (ex[1] > 0) ex / ex[1] else NA_real_, deaths = de, deaths_per_million = de / unname(s$pop) * 1e6)
}

#' A source released at the site (on-site power plant or backup generators).
#' @param t tonnes per year, named pm25, so2, nox
#' @param site_cell grid-cell row of the site (locate_cell), or NULL to use the cell at the county's interior point (row i)
#' @return list(deaths, e = sum of people x ug/m3, ses = data.frame as ses_values() or NULL)
local_source <- function(app, t, height, site_cell, i, beta) {
  sl <- app$meta$site_layers + 1                            # releases above the highest stored layer use that layer
  w <- layer_weights(min(height, app$meta$layer_mid_m[max(sl)]), app$meta$layer_mid_m)[sl]
  # the site's own grid cell when known, otherwise the cell at the county's interior point
  own <- !is.null(site_cell) && !is.na(site_cell) && !is.null(app$onsite_cells)
  o <- if (own) app$onsite_cells[site_cell, ] else as.data.frame(app$onsite)[i, ]
  k <- e <- 0
  for (p in names(t)) for (l in seq_along(w)) {
    k <- k + t[[p]] * w[l] * o[[sprintf("k_%s_l%d", p, app$meta$site_layers[l])]]
    e <- e + t[[p]] * w[l] * o[[sprintf("e_%s_l%d", p, app$meta$site_layers[l])]]
  }
  ses <- NULL; og <- app$og
  if (!is.null(og) && !is.null(app$ses)) {
    cell <- if (own) site_cell else app$onsite$cell[i] + 1
    if (is.finite(cell) && cell >= 1 && cell <= nrow(og$q)) {
      v <- 10^(og$q[cell, ] / og$scale); v[is.na(v)] <- 0                 # this cell's per-tonne values (see 04_bake_app_data.R)
      wt <- w[og$key$l] * unname(t[og$key$p]); wt[is.na(wt)] <- 0
      agg <- function(q) { j <- og$key$q == q; tapply(v[j] * wt[j], factor(og$key$g[j], levels = og$groups), sum) }
      eg <- as.vector(agg("e")); kg <- as.vector(agg("k"))
      g <- rownames(app$ses$exposure); j <- match(g, og$groups)
      if (!anyNA(j)) {
        pop <- unname(app$ses$pop); ex <- eg[j] / pop; de <- beta * kg[j]
        ses <- data.frame(group = g, label = unname(app$ses$labels[g]), pop = pop, dpm_ugm3 = ex,
                          ratio = if (ex[1] > 0) ex / ex[1] else NA_real_, deaths = de, deaths_per_million = de / pop * 1e6)
      }
    }
  }
  list(deaths = beta * k, e = e, ses = ses)
}

#' Add two group tables (same groups), recomputing the ratios.
add_ses <- function(a, b) {
  if (is.null(b)) return(a); if (is.null(a)) return(b)
  a$dpm_ugm3 <- a$dpm_ugm3 + b$dpm_ugm3; a$deaths <- a$deaths + b$deaths
  a$ratio <- if (a$dpm_ugm3[1] > 0) a$dpm_ugm3 / a$dpm_ugm3[1] else NA_real_; a$deaths_per_million <- a$deaths / a$pop * 1e6; a
}

#' Which block-group rows go together on the exposure chart.
SES_SETS <- list("Race and ethnicity" = c("WhiteNoLat", "Black", "Native", "Asian", "Latino"),
                 "Household income (block-group median)" = paste0("income_q", 1:5),
                 "Poverty" = c("below_poverty", "near_poverty", "above_2x_poverty"),
                 "Education (adults 25+)" = c("no_hs_diploma", "bachelors_plus"))

#' Increment (ug/m3) and deaths in the model's own grid cells `idx`, for a grid result `r` from impact().
#' NULL when the cell-level fields are not stored for that mix (custom mixes).
cell_values <- function(app, r, beta, idx = seq_len(nrow(app$cells$F))) {
  if (is.null(r$w) || !length(r$w) || !all(names(r$w) %in% colnames(app$cells$F))) return(NULL)
  f <- as.vector(app$cells$F[idx, names(r$w), drop = FALSE] %*% r$w) * r$totals$gen_twh
  data.frame(cell = idx, lon = app$cells$clng[idx], lat = app$cells$clat[idx], size_km = app$cells$km[idx],
             pop = app$cells$pop[idx], dpm_ugm3 = f, deaths = beta * app$cells$d0[idx] * f)
}

#' Every supply option side by side.
compare_scenarios <- function(app, ...) {
  do.call(rbind, lapply(unname(SCENARIOS), function(s) impact(app, scenario = s, ...)$totals))
}

# ---- noise: a screening estimate (also used by analysis/06_va_noise.R; assumptions in analysis/06_va_noise_assumptions.md) ----
# Generators: makers' A-weighted level at 7 m, full load, for a 2,500 kW set (Caterpillar 3516C enclosures, Level 2 and
# Level 1; MTU 20V4000 DS2500 open set, exhaust excluded), scaled by rating. Cooling: A-weighted sound power per MW of heat
# rejected by air-cooled chillers (Trane RTAF published data). Spreading: ISO 9613-2 simplified A-weighted method
# (downwind, flat ground, no barriers).
NOISE_GEN <- list(
  quiet   = list(label = "Level 2 sound-attenuated enclosure", lp7 = 74),
  central = list(label = "Level 1 sound-attenuated enclosure", lp7 = 79),
  loud    = list(label = "No enclosure (open set)", lp7 = 97.5))
NOISE_COOL <- list(
  quiet   = list(label = "Extra-low-noise chillers", lw_mw = 92),
  central = list(label = "Low-noise chillers", lw_mw = 95),
  loud    = list(label = "Standard chillers", lw_mw = 98))
NOISE_REF_KW <- 2500
NOISE_H <- c(gen = 3, cool = 15, rec = 1.5)              # source and receiver heights, m
NOISE_ALPHA <- c(gen = 1.9e-3, cool = 1.0e-3)            # air absorption, dB/m (500 Hz and 250 Hz, 10 C, 70% RH)
noise_lw_gen <- function(kw, lp7) lp7 + 20 * log10(7) + 8 + 10 * log10(kw / NOISE_REF_KW)   # sound power from the 7 m level
noise_lp <- function(lw, d, hs, alpha, hr = NOISE_H[["rec"]]) {                            # A-weighted level at d metres
  d <- pmax(d, 1)
  adiv <- 20 * log10(d) + 11
  agr <- pmax(0, 4.8 - (2 * (hs + hr) / 2 / d) * (17 + 300 / d))
  dom <- 10 * log10(1 + (d^2 + (hs - hr)^2) / (d^2 + (hs + hr)^2))
  lw + dom - adiv - alpha * d - agr
}
noise_dist <- function(lw, level, hs, alpha) {                                              # metres to fall to `level`
  one <- function(w, h, a) {
    if (!is.finite(w)) return(NA_real_)
    f <- function(d) noise_lp(w, d, h, a) - level
    if (f(1) < 0) return(0)
    if (f(20000) > 0) return(20000)
    uniroot(f, c(1, 20000), tol = 0.5)$root
  }
  n <- max(length(lw), length(hs), length(alpha))
  unname(mapply(one, rep_len(lw, n), rep_len(hs, n), rep_len(alpha, n)))
}
#' The three sources for one site: a routine test (`at_once` engines), an outage (all engines), and cooling.
noise_sources <- function(n, kw, at_once = 1, enclosure = "central", heat_mw = 0, chillers = "central") {
  lw1 <- noise_lw_gen(max(kw, 1), NOISE_GEN[[enclosure]]$lp7)
  data.frame(source = c("Generator test", "Outage: every generator", "Cooling equipment"),
             lw = c(lw1 + 10 * log10(max(min(at_once, n), 1)), lw1 + 10 * log10(max(n, 1)),
                    if (heat_mw > 0) NOISE_COOL[[chillers]]$lw_mw + 10 * log10(heat_mw) else NA),
             hs = NOISE_H[c("gen", "gen", "cool")], alpha = NOISE_ALPHA[c("gen", "gen", "cool")],
             active = c(n > 0 && kw > 0, n > 0 && kw > 0, heat_mw > 0), row.names = NULL)
}

fmt <- function(x, d = 0) ifelse(is.na(x), "-", formatC(x, format = "f", digits = d, big.mark = ","))
sig <- function(x, d = 2) ifelse(is.na(x), "-", formatC(signif(x, d), format = "fg", big.mark = ","))
