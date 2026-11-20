# =============================================================================
# 04_bake_app_data.R  --  aggregate the ISRM fields to counties and write the
#                         single file the app reads (data/app_data.rds)
# -----------------------------------------------------------------------------
# In : build/isrm_cells.csv, build/isrm_source.csv, build/isrm_meta.json  (03)
#      build/region_mix.csv, build/plants.csv                             (02)
#      data-raw/cb_2023_us_county_500k/, data-raw/egrid2023_subregions/
# Out: data/app_data.rds, data/build_report.txt
#
# Receptor cells (1-48 km) are split among counties by overlapping area, with
# each cell's shares renormalised so that no population is lost at coasts and
# borders. Everything is stored per TWh/yr generated, so the app only scales.
# =============================================================================
source(file.path(if (nzchar(Sys.getenv("USLOAD_ROOT"))) Sys.getenv("USLOAD_ROOT") else ".", "scripts/00_config.R"))
suppressPackageStartupMessages(library(jsonlite))
TEST <- Sys.getenv("USLOAD_TEST") == "1"
rep_lines <- character(); note <- function(...) { s <- sprintf(...); rep_lines <<- c(rep_lines, s); msg("%s", s) }

cells <- fread(file.path(BUILD, "isrm_cells.csv"))
meta  <- fromJSON(file.path(BUILD, "isrm_meta.json"))
mix   <- fread(file.path(BUILD, "region_mix.csv"))
fcols <- grep("^f_", names(cells), value = TRUE); cols <- sub("^f_", "", fcols)
gcols <- grep("^pop_", names(cells), value = TRUE)
note("%d cells, %d fields, population %.1f million", nrow(cells), length(fcols), sum(cells$pop) / 1e6)

# ---- counties -------------------------------------------------------------------------------
shp <- list.files(file.path(RAW, "cb_2023_us_county_500k"), "\\.shp$", full.names = TRUE)[1]
cty <- st_read(shp, quiet = TRUE)
cty <- cty[!(cty$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")), ]
cty <- st_make_valid(st_transform(cty, LCC)); cty <- cty[order(cty$GEOID), ]
st_agr(cty) <- "constant"

# ---- cells -> counties by area ---------------------------------------------------------------
rect <- function(w, s, e, n) st_polygon(list(matrix(c(w, s, e, s, e, n, w, n, w, s), ncol = 2, byrow = TRUE)))
cg <- st_sf(cell = cells$cell, geometry = st_sfc(Map(rect, cells$W, cells$S, cells$E, cells$N), crs = LCC))
st_agr(cg) <- "constant"
msg("intersecting cells with counties (a few minutes) ...")
ix <- st_intersection(cg, cty[, "GEOID"])
ov <- data.table(cell = ix$cell, fips = ix$GEOID, a = as.numeric(st_area(ix)))[a > 0]
miss <- setdiff(cells[pop > 0, cell], ov$cell)
if (length(miss)) {                                        # populated cells touching no county: nearest
  pts <- st_centroid(st_geometry(cg[match(miss, cg$cell), ]))
  ov  <- rbind(ov, data.table(cell = miss, fips = cty$GEOID[st_nearest_feature(pts, cty)], a = 1))
  note("%d populated cells touch no county polygon (%.3f%% of population): assigned to the nearest county",
       length(miss), 100 * cells[cell %in% miss, sum(pop)] / sum(cells$pop))
}
ov[, w := a / sum(a), by = cell]
ov <- merge(ov, cells[, c("cell", "pop", "mort_per_person", fcols), with = FALSE], by = "cell")
ov[, `:=`(p = pop * w, d0 = pop * w * mort_per_person)]
agg <- ov[, c(list(pop = sum(p), base_deaths = sum(d0)),
              setNames(lapply(fcols, function(f) sum(p * get(f))), paste0("E_", cols)),
              setNames(lapply(fcols, function(f) sum(d0 * get(f))), paste0("K_", cols))), by = fips]
agg <- agg[data.table(fips = cty$GEOID), on = "fips"]
for (v in names(agg)[-1]) set(agg, which(is.na(agg[[v]])), v, 0)
E <- as.matrix(agg[, paste0("E_", cols), with = FALSE]); K <- as.matrix(agg[, paste0("K_", cols), with = FALSE])
dimnames(E) <- dimnames(K) <- list(agg$fips, cols)
assert(abs(sum(agg$pop) / sum(cells$pop) - 1) < 1e-6, "population not conserved in the county aggregation")
# Area-mean concentration per county, for counties too small or empty to carry population.
am <- ov[, lapply(.SD, function(f) sum(f * a) / sum(a)), by = fips, .SDcols = fcols][data.table(fips = cty$GEOID), on = "fips"]
A <- as.matrix(am[, fcols, with = FALSE]); dimnames(A) <- list(am$fips, cols)
note("%d of %d counties carry ISRM population; %d have no overlapping cell", sum(agg$pop > 0), nrow(agg), sum(!is.finite(A[, 1])))

# Checks against the cell-level totals from script 03.
k_cell <- vapply(cols, function(cn) meta$per_twh[[cn]]$k, 0)
dev <- max(abs(colSums(K) / k_cell - 1)[k_cell > 0])
note("county mortality weights vs cell-level totals: max relative difference %.2e", dev)
assert(dev < 1e-3, "county aggregation lost mortality weight")

# ---- exposure by group (national, population-weighted, per TWh) --------------------------------
grp <- NULL
if (length(gcols)) {
  grp <- rbind(sapply(fcols, function(f) sum(cells$pop * cells[[f]]) / sum(cells$pop)),
               t(sapply(gcols, function(g) sapply(fcols, function(f) sum(cells[[g]] * cells[[f]]) / sum(cells[[g]])))))
  dimnames(grp) <- list(c("Total", sub("^pop_", "", gcols)), cols)
  attr(grp, "pop") <- c(Total = sum(cells$pop), sapply(gcols, function(g) sum(cells[[g]])))
}

# ---- eGRID subregion of each county -----------------------------------------------------------
regs <- sort(unique(mix$subrgn))
sshp <- list.files(file.path(RAW, "egrid2023_subregions"), "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
assert(!is.na(sshp), "eGRID subregion shapefile not found under data-raw/egrid2023_subregions")
sr <- st_make_valid(st_transform(st_read(sshp, quiet = TRUE), LCC))
hit <- sapply(names(st_drop_geometry(sr)), function(v) mean(toupper(trimws(as.character(sr[[v]]))) %in% regs))
assert(max(hit) > 0.5, "no column of the subregion shapefile holds subregion acronyms; columns: %s", paste(names(sr), collapse = ", "))
sr$subrgn <- toupper(trimws(as.character(sr[[names(hit)[which.max(hit)]]])))
sr <- sr[sr$subrgn %in% regs, "subrgn"]; st_agr(sr) <- "constant"
pos <- suppressWarnings(st_point_on_surface(st_geometry(cty)))
j <- as.integer(st_intersects(pos, sr)); far <- is.na(j)
j[far] <- st_nearest_feature(pos[far], sr)
cty$subrgn <- sr$subrgn[j]
note("county -> subregion: %d by containment, %d by nearest polygon; %d subregions used of %d",
     sum(!far), sum(far), uniqueN(cty$subrgn), length(regs))
# Territory overlaps (the EPA 'multiple subregions' file) are not represented:
# the app lets the user override the subregion.

# ---- on-site source weights -------------------------------------------------------------------
onsite <- fread(file.path(BUILD, "isrm_source.csv"), colClasses = list(character = "fips"))
onsite <- onsite[data.table(fips = cty$GEOID), on = "fips"]

# Source weights for every grid cell, so a site given by latitude and longitude uses its own cell.
f_sc <- file.path(BUILD, "isrm_source_cells.csv"); onsite_cells <- NULL
if (file.exists(f_sc)) {
  sc <- fread(f_sc); assert(nrow(sc) == nrow(cells) && all(sc$cell == cells$cell), "isrm_source_cells.csv does not line up with the cells")
  onsite_cells <- as.data.frame(lapply(sc[, !c("fips", "cell")], signif, 4))
  note("per-cell source weights for on-site power: %d cells", nrow(onsite_cells))
}

# ---- geometry for the map ---------------------------------------------------------------------
simp <- function(x, tol) st_transform(st_simplify(x, dTolerance = tol, preserveTopology = TRUE), 4326)
xy <- st_coordinates(st_transform(pos, 4326))
county_sf <- simp(cty[, c("GEOID", "NAME", "STUSPS", "subrgn")], if (TEST) 1 else 400)
names(county_sf)[1:3] <- c("fips", "name", "state")
county_sf$lon <- xy[, 1]; county_sf$lat <- xy[, 2]
county_sf$state_name <- if ("STATE_NAME" %in% names(cty)) cty$STATE_NAME else cty$STUSPS
county_sf$pop <- agg$pop; county_sf$base_deaths <- agg$base_deaths
region_sf <- simp(do.call(rbind, lapply(split(sr, sr$subrgn), function(d) st_sf(subrgn = d$subrgn[1], geometry = st_union(st_geometry(d))))),
                  if (TEST) 1 else 1500)

# ---- model grid cells, for the zoomed-in map -----------------------------------------------------
# Corner coordinates in lon/lat (the cells are rectangles in the model projection,
# so slightly tilted on a web map), population, baseline deaths, and the fields.
corner <- function(x, y) st_coordinates(st_transform(st_sfc(lapply(seq_along(x), function(i) st_point(c(x[i], y[i]))), crs = LCC), 4326))
ll <- cbind(corner(cells$W, cells$S), corner(cells$E, cells$S), corner(cells$E, cells$N), corner(cells$W, cells$N))
std <- grepl("_(avg|nb)$", cols)                            # cell-level fields only for the two standard mixes
Fm <- signif(as.matrix(cells[, fcols[std], with = FALSE]), 4); colnames(Fm) <- cols[std]
cell_list <- list(lng = round(ll[, c(1, 3, 5, 7)], 4), lat = round(ll[, c(2, 4, 6, 8)], 4),
                  clng = round(rowMeans(ll[, c(1, 3, 5, 7)]), 4), clat = round(rowMeans(ll[, c(2, 4, 6, 8)]), 4),
                  W = cells$W, S = cells$S, E = cells$E, N = cells$N,
                  km = round((cells$E - cells$W) / 1000), pop = signif(cells$pop, 5),
                  d0 = signif(cells$pop * cells$mort_per_person, 5), F = Fm)
note("cell sizes (km): %s", paste(sprintf("%s: %d", names(table(cell_list$km)), as.integer(table(cell_list$km))), collapse = ", "))

# ---- plants behind each mix -----------------------------------------------------------------------
pl <- fread(file.path(BUILD, "plants.csv"))
pl <- pl[k_avg > 0 | k_nb > 0, .(name, state, subrgn, lat = round(lat, 4), lon = round(lon, 4), fuelcat,
                                gen_twh = signif(gen_mwh / 1e6, 4), share_avg = signif(k_avg * gen_mwh, 4), share_nb = signif(k_nb * gen_mwh, 4),
                                co2_kg_mwh = signif(co2_t / gen_mwh * 1e3, 4), so2_t = signif(so2_t, 4), nox_t = signif(nox_t, 4), pm25_t = signif(pm25_t, 4))]
pl <- as.data.frame(pl[gen_twh > 0])

# ---- headline sanity number --------------------------------------------------------------------
avg <- mix[mix == "avg"]; b_k <- log(1.06) / 10
all_deaths <- sum(colSums(K)[paste0(avg$subrgn, "_avg")] * avg$gen_twh) * b_k
note("All contiguous-US generation in the data year: %.0f TWh, %.0f PM2.5 deaths/yr at Krewski (RR 1.06 per 10 ug/m3)",
     sum(avg$gen_twh), all_deaths)

# Group exposure for a source in any cell (03 --groups): data/onsite_groups.rds, a separate file read by the app
f_sg <- file.path(BUILD, "isrm_source_groups.csv")
if (file.exists(f_sg) && !is.null(onsite_cells)) {
  sg <- fread(f_sg); assert(nrow(sg) == nrow(cells) && all(sg$cell == cells$cell), "isrm_source_groups.csv does not line up with the cells")
  gn <- unique(sub("^e_(.*)_(pm25|so2|nox)_l[0-9]+$", "\\1", grep("^e_", names(sg), value = TRUE)))
  sl <- meta$site_layers; pl3 <- c("pm25", "so2", "nox")
  # 10 million numbers: stored as log10 x 2000 in integers (0.06% precision; NA = zero) to keep the file small
  key <- expand.grid(l = seq_along(sl), p = pl3, g = gn, q = c("e", "k"), stringsAsFactors = FALSE)
  Q <- sapply(seq_len(nrow(key)), function(j) { x <- sg[[sprintf("%s_%s_%s_l%d", key$q[j], key$g[j], key$p[j], sl[key$l[j]])]]
    out <- rep(NA_integer_, length(x)); ok <- is.finite(x) & x > 0; out[ok] <- as.integer(round(log10(x[ok]) * 2000)); out })
  og <- list(q = Q, key = key, groups = gn, scale = 2000)
  saveRDS(og, file.path(DATA, "onsite_groups.rds"), compress = "xz")
  note("group exposure for on-site sources: %d cells x %d groups -> data/onsite_groups.rds (%.1f MB)",
       nrow(sg), length(gn), file.size(file.path(DATA, "onsite_groups.rds")) / 2^20)
}

app <- list(counties = county_sf, regions = region_sf, E = E, K = K, A = A, groups = grp, mix = as.data.frame(mix), onsite = as.data.frame(onsite), onsite_cells = onsite_cells, cells = cell_list, plants = pl,
            meta = list(built = format(Sys.time(), "%Y-%m-%d %H:%M"), synthetic = TEST, egrid = "eGRID2023 rev2",
                        isrm = meta$isrm_file, layer_mid_m = meta$layer_mid_m, site_layers = meta$site_layers,
                        isrm_population = meta$population, mortality_per_100k = meta$pw_baseline_mortality_per_100k,
                        groups = meta$groups, eff_height = as.list(EFF_HEIGHT), all_gen_twh = sum(avg$gen_twh),
                        all_deaths_krewski = all_deaths, qa = c(readLines(file.path(BUILD, "qa_02.txt")), rep_lines)))
f_ses <- file.path(BUILD, "ses.rds")                      # block-group groups from 06, if it has been run
if (file.exists(f_ses)) { app$ses <- readRDS(f_ses); note("block-group exposure groups attached from build/ses.rds") }
saveRDS(app, file.path(DATA, "app_data.rds"), compress = "xz")
writeLines(app$meta$qa, file.path(DATA, "build_report.txt"))
msg("wrote data/app_data.rds (%.1f MB)", file.size(file.path(DATA, "app_data.rds")) / 1e6)
