# =============================================================================
# calc.R  --  the arithmetic behind the app, kept free of Shiny so it can be
#             tested on its own (tests/test_calc.R)
# =============================================================================
# Everything in data/app_data.rds is stored per TWh/yr generated (grid mixes) or
# per tonne/yr emitted (on-site power), so a result is a rescaling.

CRF <- list(
  krewski = list(label = "Krewski et al. 2009 (ACS): RR 1.06 per 10 ug/m3", beta = log(1.06) / 10),
  lepeule = list(label = "Lepeule et al. 2012 (Six Cities): RR 1.14 per 10 ug/m3", beta = log(1.14) / 10))

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
    caveat = "Totals only, no downwind map. The default rates are illustrative. Backup diesel generators are not included."),
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
impact <- function(app, fips, twh, it_twh = twh, scenario = "nb", subrgn = NULL, loss = 0.05, clean_share = 0,
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
    sl <- app$meta$site_layers + 1                            # releases above the highest stored layer use that layer
    w <- layer_weights(min(onsite_height, app$meta$layer_mid_m[max(sl)]), app$meta$layer_mid_m)[sl]
    # the site's own grid cell when known, otherwise the cell at the county's interior point
    o <- if (!is.null(site_cell) && !is.na(site_cell) && !is.null(app$onsite_cells)) app$onsite_cells[site_cell, ] else as.data.frame(app$onsite)[i, ]
    k <- e <- 0
    for (p in names(t)) for (l in seq_along(w)) {
      k <- k + t[[p]] * w[l] * o[[sprintf("k_%s_l%d", p, app$meta$site_layers[l])]]
      e <- e + t[[p]] * w[l] * o[[sprintf("e_%s_l%d", p, app$meta$site_layers[l])]]
    }
    z$gen_twh <- twh; z$co2_t <- onsite$co2 * mwh / 1e3
    z$nox_t <- t[["nox"]]; z$so2_t <- t[["so2"]]; z$pm25_t <- t[["pm25"]]
    z$deaths <- beta * k; z$pw_ugm3 <- e / pop_total
    z$water_gen_cons_ML <- onsite$water * mwh / 1e6
    return(list(totals = z, county = NULL, groups = NULL, w = NULL))
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
  list(totals = z, county = county, groups = groups, w = w, shares = g$shares)
}

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

fmt <- function(x, d = 0) ifelse(is.na(x), "-", formatC(x, format = "f", digits = d, big.mark = ","))
sig <- function(x, d = 2) ifelse(is.na(x), "-", formatC(signif(x, d), format = "fg", big.mark = ","))
