# Checks the app arithmetic against the cell-level build outputs, then drives the Shiny server.
suppressPackageStartupMessages({ library(testthat); library(data.table); library(sf); library(shiny) })
source("R/calc.R"); options(datatable.integer64 = "double")
BUILD <- Sys.getenv("USLOAD_BUILD"); app <- readRDS(file.path(Sys.getenv("USLOAD_DATA"), "app_data.rds"))
cells <- fread(file.path(BUILD, "isrm_cells.csv")); plants <- fread(file.path(BUILD, "plants.csv"))
fips <- app$counties$fips[10]; reg <- app$counties$subrgn[10]

test_that("energy", {
  e <- load_energy(100, "it", 1.2, 0.75)
  expect_equal(e$facility_mw, 120); expect_equal(e$twh, 120 * 0.75 * 8760 / 1e6); expect_equal(e$it_twh, 100 * 0.75 * 8760 / 1e6)
  expect_equal(load_energy(100, "facility", 1.2, 1)$twh, 0.876)
})
test_that("layer weights interpolate towards the nearer layer", {
  m <- c(29, 322, 917.5)
  expect_equal(layer_weights(10, m), c(1, 0, 0)); expect_equal(layer_weights(2000, m), c(0, 0, 1))
  expect_equal(layer_weights(322, m), c(0, 1, 0)); w <- layer_weights(60, m)
  expect_equal(sum(w), 1); expect_gt(w[1], 0.85); expect_equal(w[2], (60 - 29) / (322 - 29))
})
test_that("grid scenario equals a direct cell-level calculation", {
  twh <- 1.5; loss <- 0.05; gen <- twh / (1 - loss); b <- CRF$krewski$beta
  for (m in c("avg", "nb")) {
    r <- impact(app, fips, twh, scenario = m, loss = loss, beta = b)
    f <- cells[[paste0("f_", reg, "_", m)]]
    expect_equal(r$totals$deaths, b * gen * sum(cells$pop * cells$mort_per_person * f), tolerance = 1e-5)
    expect_equal(r$totals$pw_ugm3, gen * sum(cells$pop * f) / sum(cells$pop), tolerance = 1e-5)
    expect_equal(sum(r$county$deaths), r$totals$deaths)
    p <- plants[subrgn == reg]; k <- p[[paste0("k_", m)]]
    expect_equal(r$totals$co2_t, sum(p$co2_t * k) * gen * 1e6, tolerance = 1e-6)
    expect_equal(r$totals$so2_t, sum(p$so2_t * k) * gen * 1e6, tolerance = 1e-6)
    expect_equal(r$totals$water_gen_cons_ML, sum(p$wcons_l * k) * gen * 1e6 / 1e6, tolerance = 1e-6)
    expect_equal(unname(r$groups["Black"]), gen * sum(cells$pop_Black * f) / sum(cells$pop_Black), tolerance = 1e-5)
  }
  expect_equal(sum(plants[subrgn == reg, k_avg * gen_mwh]), 1)                       # shares of one MWh
  expect_equal(impact(app, fips, twh, scenario = "nb", clean_share = 0.4)$totals$deaths,
               0.6 * impact(app, fips, twh, scenario = "nb")$totals$deaths)
  expect_equal(impact(app, fips, 2 * twh, scenario = "avg")$totals$deaths, 2 * impact(app, fips, twh, scenario = "avg")$totals$deaths)
})
test_that("grid cells add up to the totals", {
  rr <- impact(app, fips, 1.5, scenario = "nb"); r <- rr$totals; d <- cell_values(app, rr, CRF$krewski$beta)
  expect_null(cell_values(app, impact(app, fips, 1.5, scenario = "custom"), 1))
  expect_equal(nrow(d), nrow(cells)); expect_equal(sum(d$deaths), r$deaths, tolerance = 1e-3)
  expect_equal(sum(d$dpm_ugm3 * d$pop) / sum(d$pop), r$pw_ugm3, tolerance = 1e-3)
  expect_equal(dim(app$cells$lng), c(nrow(cells), 4L)); expect_true(all(c("share_avg", "share_nb", "fuelcat") %in% names(app$plants)))
  expect_equal(sum(app$plants$share_avg[app$plants$subrgn == reg]), 1, tolerance = 1e-3)
})
test_that("a custom mix starts at the average and responds to the shares", {
  a <- impact(app, fips, 1, scenario = "avg")$totals; cst <- impact(app, fips, 1, scenario = "custom")$totals
  for (v in c("co2_t", "so2_t", "nox_t", "pm25_t", "deaths", "pw_ugm3", "water_gen_cons_ML")) expect_equal(cst[[v]], a[[v]], tolerance = 1e-6)
  d <- default_shares(app, reg); expect_equal(sum(d), 1); mx <- as.data.frame(app$mix)
  one <- names(d)[1]; sh <- setNames(as.numeric(names(d) == one), names(d))
  r1 <- impact(app, fips, 1, scenario = "custom", shares = sh, loss = 0)$totals
  expect_equal(r1$co2_t, mx$co2_kg_mwh[mx$subrgn == reg & mx$mix == paste0("f", one)] * 1e6 / 1e3)
  f <- paste0("f_", reg, "_f", one)
  if (f %in% names(cells)) expect_equal(r1$deaths, CRF$krewski$beta * sum(cells$pop * cells$mort_per_person * cells[[f]]), tolerance = 1e-5)
  clean <- setNames(as.numeric(names(d) %in% c("wind", "solar")), names(d))
  if (sum(clean) > 0) expect_equal(impact(app, fips, 1, scenario = "custom", shares = clean)$totals$deaths, 0)
  expect_true(all(is.finite(mx$grid_loss)) && all(mx$grid_loss > 0.02 & mx$grid_loss < 0.08))
})
test_that("on-site and clean scenarios", {
  o <- ONSITE$sc_scr; twh <- 0.5; b <- CRF$lepeule$beta; s <- app$onsite[match(fips, app$onsite$fips), ]
  r <- impact(app, fips, twh, scenario = "onsite", onsite = o, onsite_height = 0, beta = b)$totals
  expect_equal(r$co2_t, 530 * 0.5e6 / 1e3)
  expect_equal(r$deaths, b * 0.5e6 / 1e3 * (o$pm25 * s$k_pm25_l0 + o$so2 * s$k_so2_l0 + o$nox * s$k_nox_l0))
  hi <- impact(app, fips, twh, scenario = "onsite", onsite = o, onsite_height = 5000, beta = b)$totals
  expect_equal(hi$deaths, b * 0.5e6 / 1e3 * (o$pm25 * s$k_pm25_l1 + o$so2 * s$k_so2_l1 + o$nox * s$k_nox_l1))
  cl <- s$cell + 1                                                                    # the cell holding the county point
  expect_equal(locate_cell(app, app$cells$clng[cl], app$cells$clat[cl]), cl); expect_equal(locate_cell(app, app$cells$clng[7], app$cells$clat[7]), 7)
  expect_equal(impact(app, fips, twh, scenario = "onsite", onsite = o, onsite_height = 0, beta = b, site_cell = cl)$totals$deaths, r$deaths, tolerance = 1e-3)
  other <- if (cl > 1) cl - 1 else cl + 1
  expect_false(isTRUE(all.equal(impact(app, fips, twh, scenario = "onsite", onsite = o, site_cell = other)$totals$deaths,
                                impact(app, fips, twh, scenario = "onsite", onsite = o, site_cell = cl)$totals$deaths)))
  expect_true(is.na(locate_cell(app, 10, 50)))
  z <- impact(app, fips, twh, it_twh = 0.4, scenario = "clean", wue = 1.8)$totals
  expect_equal(c(z$co2_t, z$deaths), c(0, 0)); expect_equal(z$water_site_ML, 1.8 * 0.4e9 / 1e6)
  expect_equal(nrow(compare_scenarios(app, fips = fips, twh = twh)), 5)
})
test_that("the Shiny server runs", {
  Sys.setenv(USLOAD_APP_DATA = file.path(Sys.getenv("USLOAD_DATA"), "app_data.rds"))
  env <- new.env(); sys.source("app.R", envir = env, chdir = FALSE)
  testServer(env$server, {
    session$setInputs(loc_mode = "county", state = app$counties$state_name[10], show_plants = TRUE, fips = fips, mw = 100, preset = "dc_evap", basis = "it", lf = 0.75, pue = 1.2, wue = 1.8, scenario = "nb",
                      subrgn = "", loss = 5, clean = 0, onsite_preset = "sc_scr", os_co2 = 530, os_nox = 0.04, os_so2 = 0.003,
                      os_pm = 0.03, os_water = 0, os_h = 60, crf = "krewski", vsl = 13)
    expect_match(output$vb_twh, "0.788 TWh"); expect_match(output$vb_deaths, "per year"); expect_match(output$vb_water, "ML/yr")
    expect_true(is.list(output$where_tab) || is.character(output$where_tab)); expect_silent(output$cmp_tab); expect_silent(output$mix_tab)
    expect_silent(output$top_tab); expect_silent(output$grp_plot); expect_silent(output$plant_tab); expect_silent(output$mix_plot)
    expect_match(output$scenario_help$html, "kg CO2 per MWh"); expect_match(output$region_note$html, "Found automatically")
    expect_match(output$options_help$html, "Keep in mind")
    session$setInputs(map_zoom = 9, map_bounds = list(north = 60, south = 10, east = -60, west = -130)); session$elapse(500)
    expect_match(output$map_mode, "grid cells")
    session$setInputs(scenario = "onsite"); expect_match(output$vb_co2, "kt/yr"); expect_error(output$where_tab)
    session$setInputs(scenario = "clean"); expect_match(output$vb_co2, "^0 kt")
    session$setInputs(loc_mode = "latlon", lat = app$counties$lat[20] + 0.3, lon = app$counties$lon[20] + 0.3, scenario = "onsite")
    expect_match(output$latlon_note$html, app$counties$name[20]); expect_match(output$vb_co2, "kt/yr")
    session$setInputs(lat = 10, lon = 10); expect_error(output$vb_co2, "outside the contiguous")
    session$setInputs(loc_mode = "county")
    session$setInputs(scenario = "custom"); expect_match(output$vb_co2, "kt/yr"); expect_match(output$custom_ui$html, "cs_"); expect_match(output$loss_help$html, "eGRID2023")
    expect_match(output$map_mode, "county only")
    session$setInputs(scenario = "avg", subrgn = sort(unique(app$mix$subrgn))[1], mw = NA); expect_match(output$vb_twh, "^0 TWh")
  })
})
