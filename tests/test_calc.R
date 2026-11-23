# Checks the app arithmetic against the cell-level build outputs, then drives the Shiny server.
suppressPackageStartupMessages({ library(testthat); library(data.table); library(sf); library(shiny) })
source("R/calc.R"); options(datatable.integer64 = "double")
BUILD <- Sys.getenv("USLOAD_BUILD"); app <- readRDS(file.path(Sys.getenv("USLOAD_DATA"), "app_data.rds"))
og_file <- file.path(Sys.getenv("USLOAD_DATA"), "onsite_groups.rds"); if (file.exists(og_file)) app$og <- readRDS(og_file)
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
test_that("block-group groups are consistent", {
  expect_false(is.null(app$ses)); fips <- app$counties$fips[10]; twh <- 1
  r <- impact(app, fips, twh, scenario = "avg"); d <- r$ses
  expect_equal(d$ratio[d$group == "Total"], 1)
  expect_equal(sum(d$pop[d$group %in% paste0("income_q", 1:5)]), sum(app$ses$pop["Total"]) - 0, tolerance = 0.3)   # minus block groups without an income
  expect_equal(d$deaths[d$group == "Total"], sum(d$deaths[d$group %in% c("below_poverty", "near_poverty", "above_2x_poverty")]), tolerance = 0.5)
  r2 <- impact(app, fips, 2 * twh, scenario = "avg"); expect_equal(r2$ses$dpm_ugm3, 2 * d$dpm_ugm3)   # linear in load
  rc <- impact(app, fips, twh, scenario = "custom"); expect_equal(rc$ses$dpm_ugm3, d$dpm_ugm3, tolerance = 1e-6)  # default custom = average
})
test_that("backup generators add to every option and scale with hours", {
  fips <- app$counties$fips[10]; twh <- 1
  gs <- list(n = 10, kw = 2500, hours = 50, load = 0.75, rates = GENSETS$tier2, height_m = 10)
  for (sc in c("nb", "avg", "custom", "onsite", "clean")) {
    a <- impact(app, fips, twh, scenario = sc)$totals; b <- impact(app, fips, twh, scenario = sc, gensets = gs)$totals
    expect_equal(b$genset_mwh, 10 * 2500 * 50 * 0.75 / 1e3)
    expect_equal(b$deaths - a$deaths, b$genset_deaths, tolerance = 1e-9)
    expect_equal(b$co2_t - a$co2_t, 0.7 * b$genset_mwh, tolerance = 1e-9)
    expect_equal(b$nox_t - a$nox_t, 6.4 * b$genset_mwh / 1e3, tolerance = 1e-9)
    expect_gt(b$genset_deaths, 0)
  }
  g2 <- impact(app, fips, twh, scenario = "clean", gensets = modifyList(gs, list(hours = 100)))$totals
  g1 <- impact(app, fips, twh, scenario = "clean", gensets = gs)$totals
  expect_equal(g2$genset_deaths, 2 * g1$genset_deaths)
  t4 <- impact(app, fips, twh, scenario = "clean", gensets = modifyList(gs, list(rates = GENSETS$tier4)))$totals
  expect_lt(t4$genset_deaths, g1$genset_deaths)
  expect_equal(impact(app, fips, twh, scenario = "clean", gensets = modifyList(gs, list(hours = 0)))$totals$genset_deaths, 0)
  # generators released at the ground count the same as an on-site plant with the same tonnes at the same height
  o <- list(co2 = 0, nox = 6.4, so2 = 0.007, pm25 = 0.2, water = 0)
  os <- impact(app, fips, g1$genset_mwh / 1e6, scenario = "onsite", onsite = o, onsite_height = 10)$totals
  expect_equal(os$deaths, g1$genset_deaths, tolerance = 1e-9)
})

test_that("who is exposed works for sources at the site", {
  skip_if(is.null(app$og)); fips <- app$counties$fips[10]
  r <- impact(app, fips, 1, scenario = "onsite"); d <- r$ses
  expect_false(is.null(d)); expect_equal(d$ratio[d$group == "Total"], 1)
  expect_equal(impact(app, fips, 2, scenario = "onsite")$ses$dpm_ugm3, 2 * d$dpm_ugm3, tolerance = 1e-9)
  expect_equal(d$deaths[d$group == "Total"], sum(d$deaths[d$group %in% c("below_poverty", "near_poverty", "above_2x_poverty")]), tolerance = 0.5)
  # grid + generators: group increments add
  gs <- list(n = 10, kw = 2500, hours = 50, load = 0.75, rates = GENSETS$tier2, height_m = 10)
  a <- impact(app, fips, 1, scenario = "avg")$ses; b <- impact(app, fips, 1, scenario = "avg", gensets = gs)$ses
  c0 <- impact(app, fips, 1, scenario = "clean", gensets = gs)$ses
  expect_equal(b$dpm_ugm3, a$dpm_ugm3 + c0$dpm_ugm3, tolerance = 1e-9)
  # the site's own cell is used when known
  cl <- locate_cell(app, app$counties$lon[10], app$counties$lat[10])
  expect_false(is.null(impact(app, fips, 1, scenario = "onsite", site_cell = cl)$ses))
})

test_that("the GEMM option is a slope between Krewski and Lepeule", {
  expect_equal(CRF$gemm$beta, gemm_beta())
  expect_gt(CRF$gemm$beta, CRF$krewski$beta); expect_lt(CRF$gemm$beta, CRF$lepeule$beta)
  expect_gt(gemm_beta(6), gemm_beta(10))                    # steeper in cleaner air
  h <- 1e-4; z <- 8 - 2.4; lr <- function(z) 0.1430 * log(1 + z / 1.6) / (1 + exp(-(z - 15.5) / 36.8))
  expect_equal(gemm_beta(ncd_lri_share = 1), (lr(z + h) - lr(z - h)) / (2 * h), tolerance = 1e-6)
})

test_that("noise screening reproduces the makers' level at 7 m and falls with distance", {
  lw <- noise_lw_gen(2500, NOISE_GEN$central$lp7)
  expect_equal(noise_lp(lw, 7, NOISE_H[["gen"]], NOISE_ALPHA[["gen"]]), 79, tolerance = 1.5)
  d <- noise_dist(c(lw, lw + 10), 55, NOISE_H[["gen"]], NOISE_ALPHA[["gen"]]); expect_true(d[2] > 2.5 * d[1])
  expect_equal(noise_lp(lw, d[1], NOISE_H[["gen"]], NOISE_ALPHA[["gen"]]), 55, tolerance = 0.1)
  s <- noise_sources(10, 2500, 1, "central", 100, "central")
  expect_equal(s$lw[2] - s$lw[1], 10); expect_equal(s$lw[3], 95 + 20)
  expect_false(noise_sources(0, 2500, heat_mw = 0)$active[3])
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
    expect_silent(output$top_tab); expect_match(output$grp_ui$html, "grp_set"); expect_silent(output$ses_plot); expect_silent(output$ses_tab);
    session$setInputs(grp_set = "Poverty"); expect_match(output$ses_tab, "poverty line");
    session$setInputs(gs_on = TRUE, gs_tier = "tier2", gs_n = 10, gs_kw = 2500, gs_hours = 50, gs_load = 75, gs_h = 10)
    expect_match(output$vb_deaths_sub, "backup generators"); expect_match(output$vb_co2_sub, "generators:")
    session$setInputs(gs_on = FALSE); expect_silent(output$plant_tab); expect_silent(output$mix_plot)
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
    session$setInputs(scenario = "custom"); expect_match(output$vb_co2, "kt/yr"); expect_silent(output$ses_plot); expect_match(output$custom_ui$html, "cs_"); expect_match(output$loss_help$html, "eGRID2023")
    expect_match(output$map_mode, "county only")
    session$setInputs(scenario = "avg", subrgn = sort(unique(app$mix$subrgn))[1], mw = NA); expect_match(output$vb_twh, "^0 TWh")
  })
})

test_that("Orellano slope and the 2012 chemistry factors are as documented", {
  expect_equal(CRF$orellano$beta, log(1.095) / 10)
  expect_gt(CRF$orellano$beta, CRF$krewski$beta); expect_lt(CRF$orellano$beta, CRF$lepeule$beta)
  expect_equal(unname(CHEM_2012[c("pm25", "so2", "nox")]), c(1, 1.23, 0.71))
})
