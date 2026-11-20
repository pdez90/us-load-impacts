# =============================================================================
# 06_blockgroups.R  --  who is exposed, at block-group resolution
# -----------------------------------------------------------------------------
# In : build/isrm_cells.csv                          (03: ISRM receptor cells, all fields)
#      data-raw/cb_2023_us_bg_500k/                  (01b: 2023 block-group boundaries)
#      data-raw/acs/acsdt5y2023-{b03002,b19013,c17002,b15003}.dat  (01b: ACS 5-year 2023)
#      data/app_data.rds                             (04)
# Out: build/bg_cells.csv     block group x ISRM cell overlap weights
#      build/bg_cell_groups.csv  people of each group in each ISRM cell (p_) and x baseline mortality (d_), for 03 --groups
#      build/ses.rds          the group matrices, picked up by 04 when it rebuilds app_data
#      build/bg_demog.csv     block-group population, race/ethnicity, income, poverty, education
#      data/app_data.rds      adds $ses: exposure and mortality-weighted fields for each group
#      build/bg_report.txt
#
# Each block group takes the area-weighted mean of the ISRM cells it overlaps
# (concentration is per cubic metre, so it is averaged, not split). Most block
# groups lie inside one cell; large rural ones span several. Groups are counted
# in people from the ACS, so the exposure of, say, people below the poverty line
# is sum(people below poverty x dPM2.5) / sum(people below poverty) over all
# contiguous-US block groups. Income quintiles rank block groups by median
# household income, with cut points set so each holds a fifth of the people.
# Everything is linear in the emission fields, so the app can apply any mix.
# =============================================================================
source(file.path(if (nzchar(Sys.getenv("USLOAD_ROOT"))) Sys.getenv("USLOAD_ROOT") else ".", "scripts/00_config.R"))
rep <- character(); note <- function(...) { s <- sprintf(...); rep <<- c(rep, s); msg("%s", s) }
NON_CONUS_ST <- c("02", "15", "60", "66", "69", "72", "78")

# ---- 1. ISRM cells ----------------------------------------------------------------------------------
cells <- fread(file.path(BUILD, "isrm_cells.csv"))
fcols <- grep("^f_", names(cells), value = TRUE)
Fm <- as.matrix(cells[, ..fcols]); colnames(Fm) <- sub("^f_", "", fcols)
rect <- function(w, s, e, n) st_polygon(list(matrix(c(w, s, e, s, e, n, w, n, w, s), ncol = 2, byrow = TRUE)))
cg <- st_sf(cell = seq_len(nrow(cells)), geometry = st_sfc(Map(rect, cells$W, cells$S, cells$E, cells$N), crs = LCC))

# ---- 2. block groups -> cells ------------------------------------------------------------------------
shp <- list.files(file.path(RAW, "cb_2023_us_bg_500k"), "\\.shp$", full.names = TRUE)[1]
assert(!is.na(shp), "block-group boundaries missing: run scripts/01b_download_census.sh")
bg <- st_read(shp, quiet = TRUE)[, c("GEOID", "STATEFP")]
bg <- bg[!(bg$STATEFP %in% NON_CONUS_ST), ]
bg <- st_transform(st_make_valid(bg), LCC); bg$area <- as.numeric(st_area(bg))
note("%d contiguous-US block groups", nrow(bg))
suppressMessages(sf_use_s2(FALSE))
msg("matching block groups to cells ...")
hits <- st_intersects(bg, cg)
n_hit <- lengths(hits)
one <- which(n_hit == 1); many <- which(n_hit > 1); none <- which(n_hit == 0)
pairs <- data.table(bg = one, cell = unlist(hits[one]), w = 1)
if (length(many)) {
  msg("splitting %d block groups that span several cells ...", length(many))
  chunks <- split(many, ceiling(seq_along(many) / 2000))
  pm <- rbindlist(lapply(chunks, function(ix) {
    sub <- bg[ix, ]; sub$bg <- ix
    cc <- unique(unlist(hits[ix]))
    x <- suppressWarnings(st_intersection(sub[, c("bg", "area")], cg[cc, ]))
    data.table(bg = x$bg, cell = x$cell, a = as.numeric(st_area(x)))
  }))
  pm <- pm[a > 0][, .(cell, w = a / sum(a)), by = bg]
  pairs <- rbind(pairs, pm)
}
if (length(none)) {                                       # outside the grid (rare, coastal slivers): nearest cell
  near <- st_nearest_feature(st_centroid(st_geometry(bg[none, ])), cg)
  pairs <- rbind(pairs, data.table(bg = none, cell = near, w = 1))
}
note("block groups inside one cell: %.1f%%; spanning several: %.1f%%; matched to the nearest cell: %d",
     100 * length(one) / nrow(bg), 100 * length(many) / nrow(bg), length(none))
assert(all(abs(pairs[, sum(w), by = bg]$V1 - 1) < 1e-6), "overlap weights do not sum to one")
bg_ids <- bg$GEOID; pairs[, GEOID := bg_ids[pairs$bg]]
fwrite(pairs[, .(GEOID, cell = cell - 1L, w = signif(w, 6))], file.path(BUILD, "bg_cells.csv"))   # cell ids 0-based, as in 03

# ---- 3. ACS demographics -----------------------------------------------------------------------------
acs <- function(tab, keep) {
  f <- file.path(RAW, "acs", sprintf("acsdt5y2023-%s.dat", tab)); assert(file.exists(f), "missing %s", f)
  d <- fread(cmd = sprintf("grep -E '^(GEO_ID|1500000US)' '%s'", f), sep = "|", colClasses = list(character = "GEO_ID"))
  d[, GEOID := sub("^1500000US", "", GEO_ID)]
  d[, c("GEOID", keep), with = FALSE]
}
e <- function(t, i) sprintf("%s_E%03d", t, i)
r <- acs("b03002", e("B03002", c(1, 3:6, 12)))
setnames(r, e("B03002", c(1, 3:6, 12)), c("pop", "WhiteNoLat", "Black", "Native", "Asian", "Latino"))
inc <- acs("b19013", e("B19013", 1)); setnames(inc, 2, "med_income")
inc[med_income < 0, med_income := NA]                      # ACS jam values (-666666666 etc.) mean "not available"
pov <- acs("c17002", e("C17002", 1:8))
pov[, `:=`(pov_universe = C17002_E001, below_poverty = C17002_E002 + C17002_E003,
           near_poverty = C17002_E004 + C17002_E005 + C17002_E006 + C17002_E007, above_2x_poverty = C17002_E008)]
edu <- acs("b15003", e("B15003", 1:25))
edu[, `:=`(adults25 = B15003_E001, no_hs_diploma = rowSums(.SD[, e("B15003", 2:16), with = FALSE]),
           bachelors_plus = rowSums(.SD[, e("B15003", 22:25), with = FALSE]))]
D <- Reduce(function(a, b) merge(a, b, by = "GEOID", all.x = TRUE),
            list(r, inc, pov[, .(GEOID, pov_universe, below_poverty, near_poverty, above_2x_poverty)],
                 edu[, .(GEOID, adults25, no_hs_diploma, bachelors_plus)]))
D <- D[GEOID %in% bg$GEOID]
note("ACS 2019-2023: %.1f million people in %d contiguous-US block groups; median income missing for %.1f%% of people",
     sum(D$pop) / 1e6, nrow(D), 100 * D[is.na(med_income), sum(pop)] / sum(D$pop))

# income quintiles, person-weighted, over block groups with a reported median
q <- D[!is.na(med_income) & pop > 0][order(med_income)][, cp := cumsum(pop) / sum(pop)]
cut_at <- sapply(1:4 / 5, function(p) q[cp >= p, med_income][1])
D[, income_q := fifelse(is.na(med_income), NA_integer_, findInterval(med_income, cut_at, left.open = TRUE) + 1L)]
note("income quintile cut points (block-group median household income): %s", paste(sprintf("$%s", formatC(cut_at, format = "d", big.mark = ",")), collapse = ", "))
for (k in 1:5) D[, (sprintf("income_q%d", k)) := fifelse(income_q %in% k, pop, 0)]
fwrite(D, file.path(BUILD, "bg_demog.csv"))

# ---- 4. group x field matrices -----------------------------------------------------------------------
groups <- c(Total = "pop", WhiteNoLat = "WhiteNoLat", Black = "Black", Native = "Native", Asian = "Asian", Latino = "Latino",
            income_q1 = "income_q1", income_q2 = "income_q2", income_q3 = "income_q3", income_q4 = "income_q4", income_q5 = "income_q5",
            below_poverty = "below_poverty", near_poverty = "near_poverty", above_2x_poverty = "above_2x_poverty",
            no_hs_diploma = "no_hs_diploma", bachelors_plus = "bachelors_plus")
labels <- c(Total = "Everyone", WhiteNoLat = "White (non-Latino)", Black = "Black (non-Latino)", Native = "Native American (non-Latino)",
            Asian = "Asian (non-Latino)", Latino = "Latino", income_q1 = "Lowest-income fifth", income_q2 = "Second fifth",
            income_q3 = "Middle fifth", income_q4 = "Fourth fifth", income_q5 = "Highest-income fifth",
            below_poverty = "Below the poverty line", near_poverty = "1-2x the poverty line", above_2x_poverty = "Over 2x the poverty line",
            no_hs_diploma = "Adults without a high-school diploma", bachelors_plus = "Adults with a bachelor's degree or more")
P <- merge(pairs[, .(GEOID, cell, w)], D[, c("GEOID", groups), with = FALSE], by = "GEOID")
P[, mort := cells$mort_per_person[cell]]
for (g in unique(groups)) set(P, j = g, value = fcoalesce(as.numeric(P[[g]]), 0))
# G[g, cell] = people of group g living in that cell's share of each block group
Gp <- P[, lapply(.SD, function(x) sum(x * w)), by = cell, .SDcols = unique(groups)]
Gd <- P[, lapply(.SD, function(x) sum(x * w * mort)), by = cell, .SDcols = unique(groups)]
toM <- function(G) { M <- matrix(0, length(groups), nrow(cells), dimnames = list(names(groups), NULL)); M[, G$cell] <- t(as.matrix(G[, groups, with = FALSE])); M }
Gp <- toM(Gp); Gd <- toM(Gd)
pop_g <- rowSums(Gp)
# per-cell group populations, for scripts/03_isrm_fields.py --groups (exposure by group from a source in any cell)
cg_out <- data.table(cell = seq_len(nrow(cells)) - 1L)
for (g in names(groups)) { set(cg_out, j = paste0("p_", g), value = signif(Gp[g, ], 6)); set(cg_out, j = paste0("d_", g), value = signif(Gd[g, ], 6)) }
fwrite(cg_out, file.path(BUILD, "bg_cell_groups.csv"))
X <- (Gp %*% Fm) / pop_g                               # population-weighted mean dPM2.5 per unit field
M <- Gd %*% Fm                                         # sum of people x baseline mortality x dPM2.5 (x beta = deaths)
assert(abs(pop_g[["Total"]] / sum(D$pop) - 1) < 1e-6, "population not conserved in the block-group step")
ses <- list(exposure = X, mort_weighted = M, pop = pop_g, labels = labels[names(groups)], income_cuts = cut_at,
            source = "ACS 2019-2023 5-year estimates; 2023 cartographic block groups; ISRM cells area-weighted")

saveRDS(ses, file.path(BUILD, "ses.rds"))                 # 04 attaches this when it rebuilds app_data
app <- readRDS(file.path(DATA, "app_data.rds"))
assert(all(colnames(Fm) %in% colnames(app$K)) || all(colnames(app$K) %in% colnames(Fm)), "fields do not match app_data")
app$ses <- ses; app$meta$ses_built <- format(Sys.time(), "%Y-%m-%d %H:%M")
saveRDS(app, file.path(DATA, "app_data.rds"), compress = "xz")

# ---- 5. QA: all-generation exposure by group ----------------------------------------------------------
mx <- as.data.table(app$mix)[mix == "avg"]
w <- setNames(mx$gen_twh, paste0(mx$subrgn, "_avg")); w <- w[names(w) %in% colnames(X)]
ex <- as.vector(X[, names(w)] %*% w)
B_K <- log(1.06) / 10
note("All US power generation (avg fields), population-weighted dPM2.5 by group (ug/m3) and ratio to everyone:")
for (i in seq_along(ex)) note("  %-42s %8.1f M people  %.3f  x%.2f", labels[[names(groups)[i]]], pop_g[i] / 1e6, ex[i], ex[i] / ex[1])
note("Deaths, block-group populations (ACS 2023) x ISRM baseline mortality: %.0f (ISRM 2010-era populations: %.0f)",
     B_K * sum(M["Total", names(w)] * w), app$meta$all_deaths_krewski)
writeLines(rep, file.path(BUILD, "bg_report.txt"))
msg("done: %s", file.path(DATA, "app_data.rds"))
