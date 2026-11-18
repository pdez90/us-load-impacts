# =============================================================================
# analysis/02_harvard_extension.R  --  reproduce the CO2 of Guidi, Dominici et
#   al.'s 403 US hyperscale data centers by balancing authority, then add what
#   their paper does not have: PM2.5 exposure and deaths, and power-plant water.
# -----------------------------------------------------------------------------
# In : data-raw/harvard/data_BA_level.csv   their public central-scenario totals
#                                           (u = 0.58; github.com/gianguidi/hyperscale-emissions)
#      build/ba_mix.csv                     (analysis/01_ba_mixes.R)
#      build/isrm_cells_ba.csv              (scripts/03_isrm_fields.py --plants build/plants_ba.csv --tag ba --sites none)
#      data-raw/cb_2023_us_county_500k/
# Out: analysis/output/harvard_*.csv, harvard_summary.txt, harvard_fleet_pm25.png
#
# Their convention is kept: electricity at the facility is attributed to plants
# one-for-one, with no gross-up for transmission losses.
# =============================================================================
source(file.path(if (nzchar(Sys.getenv("USLOAD_ROOT"))) Sys.getenv("USLOAD_ROOT") else ".", "scripts/00_config.R"))
OUT <- file.path(ROOT, "analysis", "output"); dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
B_K <- log(1.06) / 10; B_L <- log(1.14) / 10; U <- c(low = 0.48, central = 0.58, continuity = 0.663, ai_high = 0.70); GAL <- 3.785411784
lines <- character(); say <- function(...) { s <- sprintf(...); lines <<- c(lines, s); cat(s, "\n") }

hv <- fread(file.path(RAW, "harvard", "data_BA_level.csv")); setnames(hv, "region_B_1", "ba")
mx <- fread(file.path(BUILD, "ba_mix.csv")); cells <- fread(file.path(BUILD, "isrm_cells_ba.csv"))
d <- merge(hv[, .(ba, n_facilities, capacity_mw = total_capacity_mw, twh = electricity_twh, co2_mt_harvard = co2_mt)], mx, by = "ba")
assert(nrow(d) == nrow(hv), "a BA is missing from the mixes")
mwh <- d$twh * 1e6
d[, co2_mt := co2_kg_mwh * mwh / 1e9][, co2_diff_pct := fifelse(co2_mt_harvard > 0, 100 * (co2_mt / co2_mt_harvard - 1), co2_mt - co2_mt_harvard)]
d[, `:=`(nox_t = nox_kg_mwh * mwh / 1e3, so2_t = so2_kg_mwh * mwh / 1e3, pm25_t = pm25_kg_mwh * mwh / 1e3,
         water_cons_ML = wcons_l_mwh * mwh / 1e6, water_with_ML = wwith_l_mwh * mwh / 1e6)]
base <- cells$pop * cells$mort_per_person
F <- as.matrix(cells[, paste0("f_", d$ba, "_avg"), with = FALSE])          # ug/m3 per TWh, cells x BAs
d[, k_per_twh := as.vector(crossprod(F, base))]
d[, `:=`(deaths_krewski = B_K * k_per_twh * twh, deaths_lepeule = B_L * k_per_twh * twh, deaths_per_twh_krewski = B_K * k_per_twh)]
d[, pw_ng_m3 := as.vector(crossprod(F, cells$pop)) / sum(cells$pop) * twh * 1e3]
setorder(d, -twh)
fwrite(d[, .(ba, baname, n_facilities, capacity_mw, twh, co2_mt_harvard, co2_mt, co2_diff_pct, co2_kg_mwh, nox_t, so2_t, pm25_t,
             deaths_krewski, deaths_lepeule, deaths_per_twh_krewski, pw_ng_m3, water_cons_ML, water_with_ML)], file.path(OUT, "harvard_ba_results.csv"))

say("REPRODUCTION (central scenario, u = 0.58, %d balancing authorities, %d facilities)", nrow(d), sum(d$n_facilities))
say("  Electricity: %.2f TWh.  CO2: theirs %.2f Mt, ours %.2f Mt (difference %.3f%%); largest BA difference %.3f%%.",
    sum(d$twh), sum(d$co2_mt_harvard), sum(d$co2_mt), 100 * (sum(d$co2_mt) / sum(d$co2_mt_harvard) - 1), max(abs(d[co2_mt_harvard > 0, co2_diff_pct])))
say("EXTENSION (same electricity and attribution)")
say("  Plant emissions attributed: SO2 %.0f t, NOx %.0f t, primary PM2.5 %.0f t per year.", sum(d$so2_t), sum(d$nox_t), sum(d$pm25_t))
say("  PM2.5 deaths per year: %.0f (Krewski RR 1.06) to %.0f (Lepeule RR 1.14); %.2f per TWh at Krewski.", sum(d$deaths_krewski), sum(d$deaths_lepeule), sum(d$deaths_krewski) / sum(d$twh))
say("  Power-plant water: %.0f GL consumed (%.1f billion gal), %.0f GL withdrawn (%.0f billion gal) per year.",
    sum(d$water_cons_ML) / 1e3, sum(d$water_cons_ML) * 1e6 / GAL / 1e9, sum(d$water_with_ML) / 1e3, sum(d$water_with_ML) * 1e6 / GAL / 1e9)
sc <- data.table(scenario = names(U), u = U, twh = sum(d$twh) * U / 0.58, co2_mt = sum(d$co2_mt) * U / 0.58,
                 deaths_krewski = sum(d$deaths_krewski) * U / 0.58, deaths_lepeule = sum(d$deaths_lepeule) * U / 0.58, water_cons_GL = sum(d$water_cons_ML) / 1e3 * U / 0.58)
fwrite(sc, file.path(OUT, "harvard_scenarios.csv"))
say("  Across their load scenarios (u 0.48-0.70): %.0f-%.0f deaths (Krewski), %.0f-%.0f GL consumed.", min(sc$deaths_krewski), max(sc$deaths_krewski), min(sc$water_cons_GL), max(sc$water_cons_GL))
say("  With a 4.2%% transmission-loss gross-up (not in their method) every figure above is %.1f%% higher.", 100 * (1 / (1 - 0.042) - 1))

# ---- the fleet's PM2.5 field, and who is exposed -----------------------------------------------
F <- F[, paste0("f_", d$ba, "_avg")]                                       # d was re-sorted: realign the columns
fleet <- as.vector(F %*% d$twh)                                            # ug/m3 in each cell
assert(abs(sum(B_K * base * fleet) / sum(d$deaths_krewski) - 1) < 1e-9, "fleet field does not add up to the BA totals")
dk <- B_K * base * fleet
grp <- grep("^pop_", names(cells), value = TRUE)
ex <- c(Total = sum(cells$pop * fleet) / sum(cells$pop), sapply(grp, function(g) sum(cells[[g]] * fleet) / sum(cells[[g]])))
exd <- data.table(group = sub("^pop_", "", names(ex)), pw_ng_m3 = ex * 1e3, relative = ex / ex[["Total"]]); fwrite(exd, file.path(OUT, "harvard_exposure_by_group.csv"))
say("  Population-weighted mean increment %.2f ng/m3; highest cell %.1f ng/m3.", ex[["Total"]] * 1e3, max(fleet) * 1e3)
say("  Exposure relative to the average: %s (ISRM census populations).", paste(sprintf("%s %.2f", exd$group[-1], exd$relative[-1]), collapse = ", "))

shp <- list.files(file.path(RAW, "cb_2023_us_county_500k"), "\\.shp$", full.names = TRUE)[1]
cty <- st_read(shp, quiet = TRUE); cty <- cty[!(cty$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")), ]
cty <- st_make_valid(st_transform(cty, LCC)); st_agr(cty) <- "constant"
cellpoly <- function(w, s, e, n) st_polygon(list(matrix(c(w, s, e, s, e, n, w, n, w, s), ncol = 2, byrow = TRUE)))
cg <- st_sf(cell = cells$cell, geometry = st_sfc(Map(cellpoly, cells$W, cells$S, cells$E, cells$N), crs = LCC)); st_agr(cg) <- "constant"
ix <- st_intersection(cg, cty[, c("GEOID", "NAME", "STUSPS")])
ov <- data.table(cell = ix$cell, fips = ix$GEOID, county = ix$NAME, state = ix$STUSPS, a = as.numeric(st_area(ix)))[a > 0]
ov[, w := a / sum(a), by = cell]; ov[, `:=`(deaths = dk[cell + 1] * w, pop = cells$pop[cell + 1] * w, e = cells$pop[cell + 1] * w * fleet[cell + 1])]
st <- ov[, .(deaths_krewski = sum(deaths), pop = sum(pop), pw_ng_m3 = sum(e) / sum(pop) * 1e3), by = state][order(-deaths_krewski)]
hs <- fread(file.path(RAW, "harvard", "data_state_level.csv"))[, .(state = STUSPS, hdc_facilities = n_facilities, hdc_twh = electricity_twh)]
st <- merge(st, hs, by = "state", all.x = TRUE)[order(-deaths_krewski)]; st[is.na(hdc_twh), `:=`(hdc_twh = 0, hdc_facilities = 0L)]
fwrite(st, file.path(OUT, "harvard_deaths_by_state.csv"))
co <- ov[, .(deaths_krewski = sum(deaths), pop = sum(pop), pw_ng_m3 = sum(e) / sum(pop) * 1e3), by = .(fips, county, state)][order(-deaths_krewski)]
fwrite(co, file.path(OUT, "harvard_deaths_by_county.csv"))
say("  Deaths assigned to a county: %.1f of %.1f (the rest are in cells with no county overlap).", sum(co$deaths_krewski), sum(dk))
say("  Most deaths by state: %s.", paste(sprintf("%s %.0f", st$state[1:8], st$deaths_krewski[1:8]), collapse = ", "))
say("  Deaths in states with no hyperscale facility in their data: %.0f (%.0f%%).", st[hdc_twh == 0, sum(deaths_krewski)], 100 * st[hdc_twh == 0, sum(deaths_krewski)] / sum(st$deaths_krewski))
say("  Most deaths by BA: %s.", paste(sprintf("%s %.0f", d[order(-deaths_krewski)]$ba[1:6], d[order(-deaths_krewski)]$deaths_krewski[1:6]), collapse = ", "))

# ---- map ----------------------------------------------------------------------------------------
png(file.path(OUT, "harvard_fleet_pm25.png"), width = 2200, height = 1400, res = 220)
v <- fleet * 1e3; top <- max(v); brk <- 10^seq(log10(top) - 3, log10(top), length.out = 10); pal <- hcl.colors(9, "YlOrRd", rev = TRUE)
par(mar = c(0.5, 0.5, 2.5, 0.5))
plot(NA, xlim = range(cells$W, cells$E), ylim = range(cells$S, cells$N), asp = 1, axes = FALSE, xlab = "", ylab = "",
     main = "PM2.5 from electricity attributed to 403 US hyperscale data centers (ng/m3, annual mean)", cex.main = 0.95)
rect(cells$W, cells$S, cells$E, cells$N, col = pal[findInterval(pmax(v, brk[1]), brk, all.inside = TRUE)], border = NA)
plot(st_geometry(st_union(cty)), add = TRUE, border = "grey30", lwd = 0.6)
legend("bottomleft", legend = signif(brk[c(1, 4, 7, 10)], 2), fill = pal[c(1, 4, 7, 9)], border = NA, bty = "n", cex = 0.8, title = "ng/m3")
dev.off()
writeLines(lines, file.path(OUT, "harvard_summary.txt"))

# ---- a small file for the app's replication page ----------------------------------------------------
src <- file.path(RAW, "harvard", "SOURCE.txt")
saveRDS(list(
  ba = as.data.frame(d[, .(ba, baname, n_facilities, capacity_mw, twh, co2_mt_harvard, co2_mt, co2_diff_pct, co2_kg_mwh, so2_t, nox_t, pm25_t,
                           deaths_krewski, deaths_lepeule, water_cons_ML, water_with_ML)]),
  state = as.data.frame(st), county = as.data.frame(co[, .(fips, deaths_krewski, pw_ng_m3)]), groups = as.data.frame(exd),
  u = U, u_central = 0.58, lepeule_ratio = B_L / B_K, max_cell_ng_m3 = max(fleet) * 1e3,
  source = if (file.exists(src)) readLines(src)[1] else NA, built = format(Sys.time(), "%Y-%m-%d %H:%M")),
  file.path(DATA, "harvard.rds"))
msg("wrote %s", OUT)
