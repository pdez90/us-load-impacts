# =============================================================================
# 06_va_noise.R  --  screening estimate of noise from Virginia data centers:
#                    backup generator testing and outages (from the air permits)
#                    and a rough, generic estimate of cooling-equipment noise
# -----------------------------------------------------------------------------
# Run: Rscript analysis/06_va_noise.R ~/Downloads/permits_virginia
#      (the folder of permit PDFs; their text is cached in build/permit_text/, so later runs need no
#      argument. Reading the PDFs needs pdftotext (poppler), as scripts/08 does.)
# In : analysis/output/va_permits_sites.csv       (analysis/05: current permits, engines, kW, site points)
#      data-raw/permits/va/va_permits.csv         (scripts/08: PDF file name of each permit)
#      data-raw/cb_2023_us_bg_500k/               (block groups; VA, MD and DC are read)
#      build/bg_demog.csv                         (scripts/06: block-group population, race, income fifth)
# Out: analysis/output/va_noise_sites.csv         one row per current permit with an equipment list
#      analysis/output/va_noise_people.csv        people inside each contour, by case and group
#      analysis/output/va_noise_summary.txt
#      analysis/output/va_noise_map.png
#      data/va_permits.rds                        what the app's "Virginia data centers" tab shows: every current permit
#                                                 (air results from analysis/05 plus these noise results)
#
# This is a screening calculation, not a noise study. What the permits give: engine make, model, count,
# rating, testing hours and testing window, and the site. What they do not give, and is assumed here
# (all set in section 1, with sources): how loud each engine is (makers' published levels at 7 m for a
# reference 2.5 MW set, scaled by rating), what enclosure is fitted (three levels), how many engines run
# at once (one for a routine test, all in an outage), and everything about cooling (generic sound power
# per MW of heat rejected; heat rejected taken from generator capacity). Sound is spread with the
# simplified outdoor method of ISO 9613-2 (A-weighted, downwind, flat ground, no barriers). See
# analysis/06_va_noise_assumptions.md for the full discussion.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(sf) })
sf_use_s2(FALSE)
OUT <- "analysis/output"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
lines <- character(); say <- function(...) { s <- sprintf(...); lines <<- c(lines, s); cat(s, "\n") }

# ---- 1. assumptions ---------------------------------------------------------------------------------
# The sound levels, heights and propagation are shared with the app, in R/calc.R (NOISE_GEN, NOISE_COOL, NOISE_H,
# NOISE_ALPHA, noise_lw_gen, noise_lp, noise_dist), so both give the same numbers.
source("R/calc.R")
# Generator sound, A-weighted sound pressure at 7 m, full load, for a reference 2,500 kW set.
GEN_LEVELS <- data.table(
  case   = names(NOISE_GEN),
  label  = vapply(NOISE_GEN, `[[`, "", "label"),
  lp7    = vapply(NOISE_GEN, `[[`, 0, "lp7"),
  source = c("Caterpillar, 3516C 60 Hz sound attenuated enclosure: Level 2, 74 dB(A) at 7 m",
             "Caterpillar, 3516C 60 Hz sound attenuated enclosure: Level 1, 79 dB(A) at 7 m",
             "MTU 20V4000 DS2500 spec sheet: open power unit 97.5 dB(A) at 7 m, full load, infinite exhaust"))
# Sound power from a level at 7 m over hard ground (hemispherical spreading): Lw = Lp + 20 log10(7) + 8.
# Smaller and larger engines are scaled by rating (sound power roughly in proportion to engine power).
lw_gen <- noise_lw_gen
H_GEN <- NOISE_H[["gen"]]   # 3 m, sound centre of an enclosed set (exhaust outlet and radiator top are 3-5 m)

# Cooling: A-weighted sound power per MW of heat rejected by air-cooled chillers, from published unit data.
# Trane Sintesis RTAF air-cooled chillers, 326-732 kW: standard 95-97 dB(A), low noise 92-94 dB(A), extra-low
# noise about 89 dB(A) (average); at 732 kW the standard unit is 97 dB(A), i.e. 98.4 dB(A) per MW.
COOL_LEVELS <- data.table(
  case   = names(NOISE_COOL),
  label  = vapply(NOISE_COOL, `[[`, "", "label"),
  lw_mw  = vapply(NOISE_COOL, `[[`, 0, "lw_mw"),
  source = "Trane RTAF (Sintesis) published sound power, scaled to 1 MW of cooling; check: 72 chillers at about 96 dB(A) each = 115 dB(A) (Havtech)")
GEN_PER_HEAT <- 1.25  # MW of generators per MW of heat rejected: N+1 backup on the whole facility load
                      # (2N backup would be 2.0, i.e. 2 dB less cooling noise); assumes the campus is built out
H_COOL <- NOISE_H[["cool"]]   # 15 m, rooftop or raised chiller yard

# Propagation: ISO 9613-2 section 7, A-weighted, with the simplified ground term (eq. 10) and its directivity
# correction (eq. 11). Air absorption at 10 C, 70% humidity (ISO 9613-2 table 2): 1.9 dB/km at 500 Hz for
# generators; 1.0 dB/km at 250 Hz for cooling, whose energy sits at 63-250 Hz.
H_REC <- NOISE_H[["rec"]]; ALPHA <- NOISE_ALPHA
lp_at <- noise_lp
dist_to <- noise_dist
LEVELS <- c(55, 45)   # 55 dB(A): Loudoun's residential property-line limit (Fairfax: 55 at night, 60 by day);
                      # 45 dB(A): about 10 dB above night background measured in a Prince William
                      # neighbourhood (32-35 dB), i.e. clearly audible

# ---- 2. permits: sites, engines, models ---------------------------------------------------------------
S <- fread(file.path(OUT, "va_permits_sites.csv"), colClasses = list(character = c("registration", "fips")))
P <- fread("data-raw/permits/va/va_permits.csv", colClasses = list(character = c("registration", "zip")))
S[P, on = "registration", file := i.file]

args <- commandArgs(trailingOnly = TRUE); pdf_dir <- if (length(args)) path.expand(args[1]) else NA
TXT <- "build/permit_text"; dir.create(TXT, showWarnings = FALSE, recursive = TRUE)
permit_text <- function(file) {
  txt <- file.path(TXT, sub("\\.pdf$", ".txt", file, ignore.case = TRUE))
  if (!file.exists(txt)) {
    if (is.na(pdf_dir)) stop("no cached text for ", file, "; run with the folder of permit PDFs as the argument")
    pdf <- file.path(pdf_dir, file); if (!file.exists(pdf)) return("")
    status <- system2("pdftotext", c("-q", shQuote(pdf), shQuote(txt)))
    if (status != 0 || !file.exists(txt)) stop("pdftotext failed on ", pdf)
  }
  gsub("\\s+", " ", paste(readLines(txt, warn = FALSE), collapse = " "))
}
# Make and model as the permit names them (most-mentioned model wins), grouped into engine families.
MODEL_RX <- paste0("(Caterpillar|CAT|Cummins|MTU|Kohler|Mitsubishi|Generac)\\W{0,4}(Model\\s+|model\\s+)?",
                   "([0-9]{2}V[0-9]{4}[A-Z0-9-]*|C[0-9]{2,3}[-0-9A-Z]*|3[45][0-9]{2}[A-Z]?[-A-Z]*|DQ[A-Z]{2,4}|",
                   "QSK[0-9]+[-A-Z0-9]*|[0-9]{4}DQ[A-Z]+|KD[0-9]{3,4}[-0-9]*|S16R[-A-Z0-9]*)")
family <- function(make, model) {
  make <- ifelse(make == "CAT", "Caterpillar", make)
  fam <- fcase(grepl("^35(16|12|08)", model), paste0(make, " ", substr(model, 1, 4)),
               grepl("^C175", model), paste0(make, " C175"),
               grepl("^C(13|15|18|27|32)", model), paste0(make, " C13-C32 (small)"),
               grepl("^QSK|DQ", model), paste0(make, " QSK/DQ series"),
               grepl("^C[0-9]{4}", model), paste0(make, " ", sub("^(C[0-9]{4}).*", "\\1", model)),
               grepl("^[0-9]{2}V[0-9]{4}", model), paste0(make, " ", substr(model, 1, 7)),
               default = paste(make, model))
  fam
}
mods <- rbindlist(lapply(seq_len(nrow(S)), function(i) {
  t <- if (is.na(S$file[i])) "" else permit_text(S$file[i])
  m <- regmatches(t, gregexpr(MODEL_RX, t, perl = TRUE))[[1]]
  if (!length(m)) return(data.table(registration = S$registration[i], make = NA_character_, model_family = NA_character_, models = NA_character_))
  mk <- sub("^(\\S+?)\\W.*$", "\\1", m); md <- sub("^.*[ )®]", "", m)
  fam <- family(mk, md); tab <- sort(table(fam), decreasing = TRUE)
  data.table(registration = S$registration[i], make = ifelse(mk[1] == "CAT", "Caterpillar", mk[1]),
             model_family = names(tab)[1], models = paste(unique(md), collapse = "; "))
}))
S <- merge(S, mods, by = "registration", all.x = TRUE)
S[, make := sub("^(\\S+).*", "\\1", model_family)]

E <- S[!is.na(total_ekw) & engines > 0]                     # sites with an equipment list and ratings
say("Virginia data center noise screening (analysis/06_va_noise.R)")
say("Current permits: %d; with engine counts and ratings: %d (%d engines, %.1f GW)", nrow(S), nrow(E), sum(E$engines), sum(E$total_ekw) / 1e6)
say("Make and model named in the permit text: %d of %d", sum(!is.na(E$model_family)), nrow(E))

# ---- 3. per-site source levels and contour distances ---------------------------------------------------
E[, kw_unit := total_ekw / engines]                         # mean rating; permits list ratings by group
E[, heat_mw := total_ekw / 1e3 / GEN_PER_HEAT]
for (k in seq_len(nrow(GEN_LEVELS))) {
  cs <- GEN_LEVELS$case[k]; lp7 <- GEN_LEVELS$lp7[k]
  set(E, j = paste0("lw_test_", cs), value = lw_gen(E$kw_unit, lp7))                     # one engine
  set(E, j = paste0("lw_outage_", cs), value = lw_gen(E$kw_unit, lp7) + 10 * log10(E$engines))   # all engines
}
for (k in seq_len(nrow(COOL_LEVELS))) {
  cs <- COOL_LEVELS$case[k]
  set(E, j = paste0("lw_cool_", cs), value = COOL_LEVELS$lw_mw[k] + 10 * log10(E$heat_mw))
}
SRC <- list(test = c(H_GEN, ALPHA[["gen"]]), outage = c(H_GEN, ALPHA[["gen"]]), cool = c(H_COOL, ALPHA[["cool"]]))
for (src in names(SRC)) for (cs in GEN_LEVELS$case) for (lv in LEVELS) {
  set(E, j = sprintf("d%d_%s_%s", lv, src, cs),
      value = round(dist_to(E[[sprintf("lw_%s_%s", src, cs)]], lv, SRC[[src]][1], SRC[[src]][2])))
}
# Testing time: engine-hours per year, and if engines are tested one at a time, the share of the weekday
# daytime hours inside the permit's window (assumed 7am-5pm, 10 h, 260 days, where none is parsed).
E[, test_engine_hours := engines * test_hours_per_engine]
E[, test_share_of_daytime := test_engine_hours / (10 * 260)]
E[, test_min_at_once := pmax(1, ceiling(test_engine_hours / (10 * 260)))]   # engines that must run together to fit

# ---- 4. people inside the contours (geocoded sites only, all sites added together) ----------------------
CRS_VA <- 3968                                              # Virginia Lambert, metres
G <- E[grepl("geocoded", placed)]
say("Sites placed at a geocoded address (used for people counts): %d of %d; %d at county points are left out of them",
    nrow(G), nrow(E), nrow(E) - nrow(G))
pts <- st_transform(st_as_sf(G, coords = c("lon", "lat"), crs = 4326), CRS_VA)
xy <- st_coordinates(pts); G[, `:=`(x = xy[, 1], y = xy[, 2])]

CELL <- 50; REACH <- 5000                                   # 50 m receptor grid out to 5 km from any site
grid_pts <- unique(rbindlist(lapply(seq_len(nrow(G)), function(i) {
  gx <- seq(floor((G$x[i] - REACH) / CELL), ceiling((G$x[i] + REACH) / CELL))
  gy <- seq(floor((G$y[i] - REACH) / CELL), ceiling((G$y[i] + REACH) / CELL))
  g <- CJ(ix = gx, iy = gy); g[((ix * CELL - G$x[i])^2 + (iy * CELL - G$y[i])^2) <= REACH^2]
})))
grid_pts[, `:=`(x = ix * CELL, y = iy * CELL)]
say("Receptor grid: %s points at %d m", format(nrow(grid_pts), big.mark = ","), CELL)

shp <- Sys.getenv("BG_SHP", "data-raw/cb_2023_us_bg_500k/cb_2023_us_bg_500k.shp")
lyr <- sub("\\.shp$", "", basename(shp))
bg <- st_read(shp, query = sprintf("SELECT GEOID, STATEFP FROM \"%s\" WHERE STATEFP IN ('51', '24', '11')", lyr), quiet = TRUE)
bg <- st_transform(bg, CRS_VA); bg$area_m2 <- as.numeric(st_area(bg))
D <- fread("build/bg_demog.csv", colClasses = list(character = "GEOID"))
GROUPS <- c("pop", "WhiteNoLat", "Black", "Asian", "Latino", paste0("income_q", 1:5))
bg <- merge(bg, D[, c("GEOID", GROUPS), with = FALSE], by = "GEOID", all.x = TRUE)
gp <- st_as_sf(grid_pts, coords = c("x", "y"), crs = CRS_VA, remove = FALSE)
hit <- st_intersects(gp, bg); j <- vapply(hit, function(h) if (length(h)) h[1] else NA_integer_, 0L)
B <- as.data.table(st_drop_geometry(bg))
for (g in GROUPS) set(grid_pts, j = g, value = fcoalesce(B[[g]][j] / B$area_m2[j] * CELL^2, 0))   # people per cell

# Level at every receptor. Cooling and outages: every site at once, added as energy. Routine test: one
# engine at one site at a time, so each receptor gets the loudest single site.
energy_sum <- function(lw, hs, alpha, combine = c("sum", "max")) {
  combine <- match.arg(combine); acc <- rep(if (combine == "sum") 0 else -Inf, nrow(grid_pts))
  for (i in seq_len(nrow(G))) {
    near <- which(abs(grid_pts$x - G$x[i]) <= REACH & abs(grid_pts$y - G$y[i]) <= REACH)
    d <- sqrt((grid_pts$x[near] - G$x[i])^2 + (grid_pts$y[near] - G$y[i])^2)
    lp <- lp_at(lw[i], d, hs, alpha)
    acc[near] <- if (combine == "sum") acc[near] + 10^(lp / 10) else pmax(acc[near], lp)
  }
  if (combine == "sum") 10 * log10(pmax(acc, 1e-10)) else acc
}
people <- list()
for (src in names(SRC)) for (cs in GEN_LEVELS$case) {
  lev <- energy_sum(G[[sprintf("lw_%s_%s", src, cs)]], SRC[[src]][1], SRC[[src]][2], if (src == "test") "max" else "sum")
  if (src == "cool" && cs == "central") grid_pts[, lev_cool := lev]
  if (src == "outage" && cs == "central") grid_pts[, lev_outage := lev]
  for (lv in LEVELS) {
    inside <- lev >= lv
    people[[length(people) + 1]] <- data.table(source = src, case = cs, level_dba = lv, group = GROUPS,
                                               people = vapply(GROUPS, function(g) sum(grid_pts[[g]][inside]), 0))
  }
}
people <- rbindlist(people)
# Group shares inside each contour, against Virginia as a whole.
va <- D[substr(GEOID, 1, 2) == "51", lapply(.SD, sum, na.rm = TRUE), .SDcols = GROUPS]
people[, va_share := unlist(va)[group] / va$pop]
people[, share := people / people[group == "pop"], by = .(source, case, level_dba)]
people[, ratio_to_va := ifelse(group == "pop", NA_real_, share / va_share)]
fwrite(people, file.path(OUT, "va_noise_people.csv"))

# ---- 5. outputs ---------------------------------------------------------------------------------------
keep <- c("registration", "site", "locality", "placed", "lon", "lat", "engines", "total_ekw", "kw_unit", "make", "model_family", "models",
          "test_hours_per_engine", "test_hours_assumed", "testing_window", "test_engine_hours", "test_share_of_daytime", "test_min_at_once", "heat_mw",
          grep("^(lw|d[0-9]+)_", names(E), value = TRUE))
fwrite(E[, ..keep], file.path(OUT, "va_noise_sites.csv"))

q <- function(x) paste(round(quantile(x, c(.5, .9, 1), na.rm = TRUE)), collapse = " / ")
say("")
say("ENGINES: mean rating per engine, median %.0f kW. Most common families: %s", median(E$kw_unit),
    paste(head(E[!is.na(model_family), .N, by = model_family][order(-N)][, sprintf("%s (%d permits)", model_family, N)], 6), collapse = ", "))
say("")
say("DISTANCE TO 55 / 45 dB(A), per site, median / 90th percentile / largest (m); A-weighted, downwind, no barriers")
for (src in names(SRC)) {
  lab <- c(test = "Routine test, one engine at full load", outage = "Outage or grid emergency, all engines",
           cool = "Cooling, design-day full load")[[src]]
  say("  %s", lab)
  for (cs in GEN_LEVELS$case) {
    nm <- if (src == "cool") COOL_LEVELS[case == cs, label] else GEN_LEVELS[case == cs, label]
    say("    %-8s %-36s 55 dB(A): %-18s 45 dB(A): %s", cs, nm, q(E[[sprintf("d55_%s_%s", src, cs)]]), q(E[[sprintf("d45_%s_%s", src, cs)]]))
  }
}
say("")
say("TESTING TIME: %s engine-hours/yr in all (median %.0f per site). Tested one engine at a time, the median site would",
    format(round(sum(E$test_engine_hours, na.rm = TRUE)), big.mark = ","), median(E$test_engine_hours, na.rm = TRUE))
say("  be testing during %.0f%% of weekday daytime hours (7am-5pm); %d sites more than half of them", 100 * median(E$test_share_of_daytime, na.rm = TRUE),
    sum(E$test_share_of_daytime > 0.5, na.rm = TRUE))
say("  %d sites cannot fit their permitted testing into those hours one engine at a time (up to %d at once); the routine-test",
    sum(E$test_min_at_once > 1, na.rm = TRUE), max(E$test_min_at_once, na.rm = TRUE))
say("  case is one engine, so it is the quietest testing can be; two engines together add 3 dB, four add 6 dB")
say("")
say("PEOPLE INSIDE THE CONTOURS (%d geocoded sites, block-group population spread evenly over each block group)", nrow(G))
for (src in names(SRC)) for (lv in LEVELS) {
  v <- people[source == src & level_dba == lv & group == "pop"]
  say("  %-7s >= %d dB(A): quiet %8s   central %8s   loud %8s", src, lv,
      format(round(v[case == "quiet", people], -1), big.mark = ","), format(round(v[case == "central", people], -1), big.mark = ","),
      format(round(v[case == "loud", people], -1), big.mark = ","))
}
say("")
say("WHO LIVES INSIDE (central case), share of people vs Virginia (ratio)")
for (src in names(SRC)) {
  v <- people[source == src & case == "central" & level_dba == 45 & group != "pop"]
  say("  %-7s >= 45 dB(A): %s", src, paste(sprintf("%s %.2f", v$group, v$ratio_to_va), collapse = ", "))
}
say("")
say("Contours closer than about 150 m are inside most campuses; a point source overstates levels that close to a large site.")
say("The %d sites at county points are in va_noise_sites.csv (distances) but not in the people counts.", nrow(E) - nrow(G))
say("Assumptions and their effect: analysis/06_va_noise_assumptions.md")
# For the app: every current permit with its air results (analysis/05) and these noise results.
noise_cols <- setdiff(keep, names(S))
V <- merge(S, E[, c("registration", noise_cols), with = FALSE], by = "registration", all.x = TRUE)
grp_air <- if (file.exists(file.path(OUT, "va_permits_groups.csv"))) fread(file.path(OUT, "va_permits_groups.csv")) else NULL
saveRDS(list(sites = as.data.frame(V), people = as.data.frame(people), air_groups = as.data.frame(grp_air),
             levels = LEVELS, n_list = 194L, missing = "74331-1 (Westfax 4-5A, Fairfax Co., issued 2026-03-04): DEQ's link opens the Amazon IAD-45 permit (74333-1)",
             built = format(Sys.Date())), "data/va_permits.rds", compress = "xz")
say("App data: data/va_permits.rds (%d permits)", nrow(V))
writeLines(lines, file.path(OUT, "va_noise_summary.txt"))

# Map of Northern Virginia: central cooling level and central outage level.
bb <- st_bbox(st_transform(st_as_sfc(st_bbox(c(xmin = -77.72, ymin = 38.70, xmax = -77.36, ymax = 39.08), crs = 4326)), CRS_VA))
nova <- grid_pts[x > bb[["xmin"]] & x < bb[["xmax"]] & y > bb[["ymin"]] & y < bb[["ymax"]]]   # Loudoun and Prince William
cty <- readRDS("data/app_data.rds")$counties; cty$STATEFP <- substr(cty$fips, 1, 2)
png(file.path(OUT, "va_noise_map.png"), width = 2400, height = 1300, res = 200)
par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
brk <- c(-Inf, 45, 50, 55, 60, 65, Inf); pal <- c(NA, "#fde0c5", "#facba6", "#f59e72", "#e05c4b", "#a8202f")
for (v in c("lev_cool", "lev_outage")) {
  ttl <- c(lev_cool = "Cooling, low-noise chillers (central)", lev_outage = "Outage, all generators, Level 1 enclosures (central)")[[v]]
  plot(nova$x, nova$y, type = "n", asp = 1, axes = FALSE, xlab = "", ylab = "", main = ttl, cex.main = 0.95)
  cc <- cut(nova[[v]], brk); keep_c <- !is.na(pal[as.integer(cc)])
  points(nova$x[keep_c], nova$y[keep_c], pch = 15, cex = 0.25, col = pal[as.integer(cc)][keep_c])
  plot(st_geometry(st_transform(cty[cty$STATEFP %in% c("51", "24", "11"), ], CRS_VA)), add = TRUE, border = "grey55", lwd = 0.6)
  points(G$x, G$y, pch = 3, cex = 0.4, col = "grey20")
  mtext("Northern Virginia sites (+). Downwind, no barriers; sites at county points not shown", side = 1, cex = 0.6, line = -0.2)
  legend("bottomleft", fill = pal[-1], legend = c("45-50", "50-55", "55-60", "60-65", "65+"), title = "dB(A)", bty = "n", cex = 0.75)
}
dev.off()
cat("wrote", file.path(OUT, c("va_noise_sites.csv", "va_noise_people.csv", "va_noise_summary.txt", "va_noise_map.png")), sep = "\n")
