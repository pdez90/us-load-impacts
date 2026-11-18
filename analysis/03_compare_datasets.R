# =============================================================================
# analysis/03_compare_datasets.R  --  run every openly licensed US data center
#   inventory through the same impact chain and compare the results
# -----------------------------------------------------------------------------
# In : data-raw/facilities/  (scripts/01c_download_facilities.sh, 01e_epoch_coordinates.py)
#        epoch/data_centers.csv, epoch/data_center_timelines.csv, epoch_map_coordinates.csv
#        kyars_projects.csv
#        pnnl_im3_data_center_locations.gpkg
#        adp_data_centers.geojson           (optional: Atlas of Data Center Politics)
#      data-raw/harvard/data_BA_level.csv   (reference total)
#      data-raw/egrid2023_subregions/, data/app_data.rds
# Out: analysis/output/datasets_sites.csv       every site, harmonised, with its impacts
#      analysis/output/datasets_summary.csv     one row per dataset x status x mix
#      analysis/output/datasets_overlap.csv     share of each dataset's sites found in another
#      analysis/output/datasets_summary.txt, datasets_compare.png, datasets_map.png
#
# The same rules for every dataset:
#   capacity   facility MW (IT x 1.2 where only IT power is given; PNNL floor area
#              x 1,600 W/m2, the mean facility power density Harvard reports)
#   energy     MW x 8,760 h x u, u = 0.58 (Harvard central; 0.48-0.70 range)
#   supply     the eGRID subregion containing the site; average and non-baseload mixes
#   impacts    CO2, PM2.5 deaths (Krewski) and power-plant water per MWh from the
#              tool's per-subregion results; no transmission-loss gross-up (as Harvard)
# Status: "operating" (built and running) and "pipeline" (under construction,
# permitted, planned or announced, at full build-out). Cancelled sites are dropped.
# =============================================================================
source(file.path(if (nzchar(Sys.getenv("USLOAD_ROOT"))) Sys.getenv("USLOAD_ROOT") else ".", "scripts/00_config.R"))
suppressPackageStartupMessages(library(jsonlite))
FAC <- file.path(RAW, "facilities"); OUT <- file.path(ROOT, "analysis", "output"); dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
U <- c(low = 0.48, central = 0.58, high = 0.70); PUE_IT <- 1.2; W_PER_M2 <- 1600; SQFT_M2 <- 0.09290304; B_K <- log(1.06) / 10
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
lines <- character(); say <- function(...) { s <- sprintf(...); lines <<- c(lines, s); cat(s, "\n") }
num <- function(x) suppressWarnings(as.numeric(x))

# ---- 1. Epoch AI: large AI sites, power timeline -----------------------------------------------
read_epoch <- function() {
  dc <- fread(file.path(FAC, "epoch", "data_centers.csv"))[Country == "United States"]
  co <- fread(file.path(FAC, "epoch_map_coordinates.csv"))
  tl <- fread(file.path(FAC, "epoch", "data_center_timelines.csv"))[, Date := as.IDate(Date)]
  setnames(tl, c("Data center", "Power (MW)", "IT power (MW)"), c("name", "P", "IT"), skip_absent = TRUE)
  today <- as.IDate(Sys.Date())
  cur <- tl[Date <= today][order(Date)][, .(P_now = P[.N], IT_now = IT[.N]), by = name]
  bld <- tl[, .(P_max = max(P, na.rm = TRUE)), by = name]
  d <- merge(merge(dc[, .(name = Name)], cur, by = "name", all.x = TRUE), bld, by = "name", all.x = TRUE)
  d <- merge(d, co[, .(name, lon, lat)], by = "name", all.x = TRUE)
  for (v in c("P_now", "IT_now", "P_max")) set(d, which(is.na(d[[v]]) | !is.finite(d[[v]])), v, 0)
  d[is.na(P_now) | P_now == 0 & IT_now > 0, P_now := IT_now * PUE_IT]
  rbind(d[P_now > 0, .(dataset = "Epoch AI", site = name, lon, lat, status = "operating", mw = P_now, mw_basis = "reported facility power (current)")],
        d[P_max > P_now, .(dataset = "Epoch AI", site = name, lon, lat, status = "pipeline", mw = P_max - P_now, mw_basis = "reported facility power (planned build-out)")])
}

# ---- 2. Kyars Data Center Atlas: projects with status, capacity where reported -----------------
read_kyars <- function() {
  p <- fread(file.path(FAC, "kyars_projects.csv"))[country_iso_a2 == "US"]
  un <- function(s) gsub("\"\"", "\"", s, fixed = TRUE)            # the JSON columns arrive with doubled quotes
  # Each project can list several power_capacity figures, each with a basis (operational,
  # contracted, permitted, under construction, planned) and a scope (campus or phase).
  # Operating MW = the operational figures; build-out MW = the largest figure of any basis.
  pick <- function(val, camp) if (!length(val)) NA_real_ else if (any(camp)) max(val[camp]) else sum(val)
  cap <- rbindlist(lapply(seq_len(nrow(p)), function(i) {
    op <- bo <- NA_real_
    s <- un(p$power_observations_json[i])                               # typed observations (critical IT MW)
    if (nchar(s) > 2) { o <- tryCatch(fromJSON(s, simplifyVector = FALSE), error = function(e) list())
      v <- unlist(lapply(o, function(z) if (identical(z$unit, "MW")) num(z$base) * if (grepl("it", z$metric %||% "")) PUE_IT else 1))
      if (length(v)) bo <- max(v, na.rm = TRUE) }
    s <- un(p$project_details_json[i])                                  # power_capacity in the source record
    if (nchar(s) > 2) { d <- tryCatch(fromJSON(s, simplifyVector = FALSE), error = function(e) list())
      m <- unlist(lapply(d, function(x) x$record$metrics), recursive = FALSE)
      m <- Filter(function(z) identical(z$metric, "power_capacity") && (z$unit %||% "") %in% c("MW", "GW"), m)
      if (length(m)) {
        val <- vapply(m, function(z) num(z$value) * if (identical(z$unit, "GW")) 1000 else 1, 0)
        camp <- vapply(m, function(z) identical(z$applies_to, "campus"), TRUE)
        isop <- vapply(m, function(z) identical(z$basis, "operational"), TRUE)
        op <- pick(val[isop], camp[isop]); bo <- max(bo, pick(val, camp), op, na.rm = TRUE)
      } }
    data.table(op_mw = op, build_mw = bo) }))
  st <- p$last_observed_physical_status
  cls <- fifelse(st %in% c("operational", "partially_operational", "completed", "expansion"), "operating",
         fifelse(st %in% c("cancelled", "withdrawn", "retired", "decommissioned"), "dropped", "pipeline"))
  d <- data.table(dataset = "Kyars Data Center Atlas", site_id = p$site_id, site = p$name, lon = p$longitude, lat = p$latitude, cls = cls, cap)[cls != "dropped"]
  rows <- rbind(
    d[cls == "operating", .(dataset, site_id, site, lon, lat, status = "operating", mw = op_mw,
                            mw_basis = fifelse(is.na(op_mw), "not reported", "reported operational capacity"))],
    d[cls == "operating" & build_mw > fcoalesce(op_mw, 0), .(dataset, site_id, site, lon, lat, status = "pipeline", mw = build_mw - fcoalesce(op_mw, 0),
                            mw_basis = fifelse(is.na(op_mw), "reported build-out (operating share unknown)", "reported build-out beyond operational"))],
    d[cls == "pipeline", .(dataset, site_id, site, lon, lat, status = "pipeline", mw = build_mw,
                           mw_basis = fifelse(is.na(build_mw), "not reported", "reported planned capacity"))])
  # several projects (phases) can share one physical site: keep the largest figure per site and status
  rows[, .(site = site[1], lon = lon[1], lat = lat[1], mw = if (all(is.na(mw))) NA_real_ else max(mw, na.rm = TRUE),
           mw_basis = mw_basis[which.max(replace(mw, is.na(mw), -1))]), by = .(dataset, site_id, status)][, site_id := NULL][]
}

# ---- 3. PNNL IM3 atlas (OpenStreetMap): buildings with floor area --------------------------------
read_pnnl <- function() {
  b <- st_read(file.path(FAC, "pnnl_im3_data_center_locations.gpkg"), "building", quiet = TRUE)
  b <- st_drop_geometry(b)
  data.table(dataset = "PNNL IM3 (OpenStreetMap)", site = fifelse(is.na(b$name), b$id, b$name), lon = b$lon, lat = b$lat, status = "operating",
             mw = num(b$sqft) * SQFT_M2 * W_PER_M2 / 1e6, mw_basis = "floor area x 1,600 W/m2")
}

# ---- 4. Atlas of Data Center Politics (optional, if a copy has been saved) ----------------------
read_adp <- function() {
  f <- file.path(FAC, "adp_data_centers.geojson"); if (!file.exists(f)) return(NULL)
  g <- st_read(f, quiet = TRUE); g <- g[g$country_a3 == "USA", ]; xy <- st_coordinates(st_centroid(st_geometry(g)))
  st <- tolower(g$status)
  data.table(dataset = "Atlas of Data Center Politics", site = g$name, lon = xy[, 1], lat = xy[, 2],
             status = fifelse(st == "operating", "operating", fifelse(st == "planned", "pipeline", "dropped")),
             mw = num(g$capacity_mw), mw_basis = fifelse(is.na(num(g$capacity_mw)), "size class only", "reported capacity"))[status != "dropped"]
}

S <- rbindlist(Filter(Negate(is.null), list(read_epoch(), read_kyars(), read_pnnl(), read_adp())), fill = TRUE)
S <- S[is.finite(lon) & is.finite(lat)]

# ---- grid region of each site ---------------------------------------------------------------------
sshp <- list.files(file.path(RAW, "egrid2023_subregions"), "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
sr <- st_make_valid(st_read(sshp, quiet = TRUE)); names(sr)[1] <- "subrgn"
pt <- st_as_sf(S, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
suppressMessages(sf_use_s2(FALSE))
j <- as.integer(st_intersects(pt, sr)); far <- is.na(j); if (any(far)) j[far] <- st_nearest_feature(pt[far, ], sr)
S[, subrgn := sr$subrgn[j]]
S <- S[!(subrgn %in% NON_CONUS) & lon > -125 & lon < -66 & lat > 24 & lat < 50]                                     # Alaska and Hawaii are outside the air-quality model

# ---- impacts per MWh by subregion and mix, from the tool's own results ------------------------------
app <- readRDS(file.path(DATA, "app_data.rds")); mx <- as.data.table(app$mix)[mix %in% c("avg", "nb")]
mx[, deaths_per_twh := B_K * colSums(app$K)[paste(subrgn, mix, sep = "_")]]
rate <- dcast(mx, subrgn ~ mix, value.var = c("co2_kg_mwh", "deaths_per_twh", "wcons_l_mwh"))
S <- merge(S, rate, by = "subrgn", all.x = TRUE)
S[, twh := fifelse(is.na(mw), NA_real_, mw * 8760 * U[["central"]] / 1e6)]
for (m in c("avg", "nb")) {
  S[, (paste0("co2_kt_", m)) := twh * 1e6 * get(paste0("co2_kg_mwh_", m)) / 1e6]
  S[, (paste0("deaths_", m)) := twh * get(paste0("deaths_per_twh_", m))]
  S[, (paste0("water_ML_", m)) := twh * 1e6 * get(paste0("wcons_l_mwh_", m)) / 1e6]
}
fwrite(S[order(dataset, status, -mw)], file.path(OUT, "datasets_sites.csv"))

# ---- summary --------------------------------------------------------------------------------------
sm <- S[, .(sites = .N, sites_with_mw = sum(!is.na(mw)), states = uniqueN(subrgn), gw = sum(mw, na.rm = TRUE) / 1e3, twh = sum(twh, na.rm = TRUE),
            co2_mt_avg = sum(co2_kt_avg, na.rm = TRUE) / 1e3, co2_mt_nb = sum(co2_kt_nb, na.rm = TRUE) / 1e3,
            deaths_avg = sum(deaths_avg, na.rm = TRUE), deaths_nb = sum(deaths_nb, na.rm = TRUE),
            water_GL_avg = sum(water_ML_avg, na.rm = TRUE) / 1e3), by = .(dataset, status)][order(dataset, status)]
setnames(sm, "states", "grid_regions")
hv <- fread(file.path(RAW, "harvard", "data_BA_level.csv"))
hvr <- fread(file.path(ROOT, "analysis", "output", "harvard_ba_results.csv"))
sm <- rbind(sm, data.table(dataset = "Harvard (Guidi et al.), reference", status = "operating", sites = sum(hv$n_facilities), sites_with_mw = sum(hv$n_facilities),
                           grid_regions = nrow(hv), gw = sum(hv$total_capacity_mw) / 1e3, twh = sum(hv$electricity_twh), co2_mt_avg = sum(hvr$co2_mt), co2_mt_nb = NA,
                           deaths_avg = sum(hvr$deaths_krewski), deaths_nb = NA, water_GL_avg = sum(hvr$water_cons_ML) / 1e3), fill = TRUE)
for (k in names(U)) sm[, (paste0("twh_u", U[[k]])) := twh * U[[k]] / U[["central"]]]
fwrite(sm, file.path(OUT, "datasets_summary.csv"))

say("US DATA CENTER INVENTORIES THROUGH ONE IMPACT CHAIN (u = %.2f; no transmission-loss gross-up)", U[["central"]])
for (i in seq_len(nrow(sm))) with(sm[i], say("  %-36s %-9s %5d sites (%4d with MW)  %6.1f GW  %6.1f TWh  CO2 %5.1f Mt (avg) / %5s (marginal)  deaths %5.0f / %5s  water %5.0f GL",
                                           dataset, status, sites, sites_with_mw, gw, twh, co2_mt_avg, ifelse(is.na(co2_mt_nb), "-", sprintf("%.1f", co2_mt_nb)),
                                           deaths_avg, ifelse(is.na(deaths_nb), "-", sprintf("%.0f", deaths_nb)), water_GL_avg))

# ---- overlap: how many of one dataset's sites appear in another (within 5 km) ---------------------
# one point per physical site (operating and pipeline rows of a site are merged; MW = operating + pipeline)
P <- S[, .(mw = if (all(is.na(mw))) NA_real_ else sum(mw, na.rm = TRUE)), by = .(dataset, site, lon, lat)]
ds <- unique(S$dataset); xy <- st_transform(st_as_sf(S, coords = c("lon", "lat"), crs = 4326), LCC)
xyP <- st_transform(st_as_sf(P, coords = c("lon", "lat"), crs = 4326), LCC)
ov <- rbindlist(lapply(ds, function(a) rbindlist(lapply(setdiff(ds, a), function(b) {
  ia <- which(P$dataset == a); ib <- which(P$dataset == b)
  nn <- st_nearest_feature(xyP[ia, ], xyP[ib, ]); dkm <- as.numeric(st_distance(xyP[ia, ], xyP[ib[nn], ], by_element = TRUE)) / 1e3
  hit <- dkm <= 5; w <- P$mw[ia]
  data.table(dataset = a, found_in = b, sites = length(ia), share_sites = mean(hit),
             share_mw = if (all(is.na(w))) NA_real_ else sum(w[hit], na.rm = TRUE) / sum(w, na.rm = TRUE))
}))))
fwrite(ov, file.path(OUT, "datasets_overlap.csv"))
say("OVERLAP (a site counts as found if the other dataset has one within 5 km)")
for (i in seq_len(nrow(ov))) with(ov[i], say("  %-28s in %-28s %5.1f%% of sites, %5s of MW", dataset, found_in, 100 * share_sites, ifelse(is.na(share_mw), "-", sprintf("%.1f%%", 100 * share_mw))))

# ---- figures --------------------------------------------------------------------------------------
pal <- c("Epoch AI" = "#e67e22", "Kyars Data Center Atlas" = "#2980b9", "PNNL IM3 (OpenStreetMap)" = "#27ae60",
         "Atlas of Data Center Politics" = "#8e44ad", "Harvard (Guidi et al.), reference" = "#2c3e50")
op <- sm[status == "operating"]; op[, lab := sub(" \\(.*", "", dataset)]
png(file.path(OUT, "datasets_compare.png"), width = 2400, height = 1000, res = 200)
par(mfrow = c(1, 4), mar = c(9, 4.5, 3, 1))
for (v in list(c("twh", "Electricity (TWh/yr)"), c("co2_mt_avg", "CO2, average mix (Mt/yr)"), c("deaths_avg", "PM2.5 deaths, average mix (/yr)"), c("water_GL_avg", "Power-plant water (GL/yr)"))) {
  y <- op[[v[1]]]; b <- barplot(y, names.arg = op$lab, las = 2, col = pal[op$dataset], border = NA, main = v[2], cex.names = 0.85, cex.main = 0.95, ylim = c(0, max(y, na.rm = TRUE) * 1.15))
  text(b, y, formatC(y, format = "fg", digits = 3, big.mark = ","), pos = 3, cex = 0.8, xpd = NA)
}
mtext("Operating sites only, u = 0.58. Kyars: only sites with a reported operational MW. PNNL: MW estimated from floor area.", side = 1, outer = TRUE, line = -1, cex = 0.7)
dev.off()
png(file.path(OUT, "datasets_map.png"), width = 2200, height = 1400, res = 220)
par(mar = c(0.5, 0.5, 2, 0.5)); us <- st_transform(st_union(sr[!(sr$subrgn %in% NON_CONUS), ]), LCC)
bb <- st_bbox(st_transform(st_as_sfc(st_bbox(c(xmin = -125, ymin = 24, xmax = -66.5, ymax = 49.5), crs = 4326)), LCC))
plot(st_geometry(us), col = "grey95", border = "grey60", main = "Operating sites in each open inventory (circle area ~ MW)",
     xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")])
for (d in rev(ds)) { k <- which(S$dataset == d & S$status == "operating"); if (!length(k)) next
  p <- st_coordinates(xy[k, ]); r <- ifelse(is.na(S$mw[k]), 0.4, 0.35 + 2.2 * sqrt(S$mw[k] / max(S$mw, na.rm = TRUE)))
  points(p, pch = 21, cex = r, bg = adjustcolor(pal[d], 0.45), col = pal[d], lwd = 0.5) }
legend("bottomleft", legend = ds, pt.bg = adjustcolor(pal[ds], 0.6), col = pal[ds], pch = 21, pt.cex = 1.4, bty = "n", cex = 0.8)
dev.off()
writeLines(lines, file.path(OUT, "datasets_summary.txt"))
msg("wrote %s", OUT)
