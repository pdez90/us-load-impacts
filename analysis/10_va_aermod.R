# =============================================================================
# 10_va_aermod.R  --  near-field concentrations around Virginia data center generators (AERMOD)
# -----------------------------------------------------------------------------
# The rest of this repository uses InMAP's source-receptor matrix, whose grid cells are 1 km or more across: right for
# the PM2.5 that forms downwind, too coarse for the plume next to a site. This script sets up EPA's near-field model.
#
# Usage (from the repository root):
#   Rscript analysis/10_va_aermod.R write    # write the input files            -> build/aermod/runs/<run>/aermod.inp
#   Rscript analysis/10_va_aermod.R run      # run AERMOD in each run folder    (needs the program and the weather files)
#   Rscript analysis/10_va_aermod.R read     # read the results                 -> analysis/output/va_aermod_results.csv
#   Rscript analysis/10_va_aermod.R          # all three
#
# Needs: the AERMOD program (scripts/13_build_aermod.R builds it from EPA's source; or set AERMOD_EXE), and
#        AERMOD-ready weather files for Washington Dulles, one .SFC and one .PFL, in data-raw/aermod/met/
#        (or set AERMOD_SFC and AERMOD_PFL). Virginia DEQ does not post these; see README.md.
# In   : analysis/output/va_permits_sites.csv (analysis/05)
# Out  : analysis/output/va_aermod_results.csv, va_aermod_runs.csv, va_aermod_summary.txt
#
# WHAT IS MODELLED
#   engine_h5, engine_h10, engine_h15 : one 2,500 kW engine at full load, stack 5, 10 and 15 m: 1-hour NO2
#   <permit>_outage : every engine at the site at full load (an outage or grid emergency): 1-hour NO2
#   <permit>_annual : routine testing spread over the permit's testing hours: annual diesel particulate matter
#   for three located Northern Virginia sites: the largest by generator capacity, the median and the lower quartile.
#
# ASSUMPTIONS (the permits give none of these; all are stated in the output)
#   Stack, per 2,500 kW engine (Caterpillar 3516C class, maker's data sheet values, rounded): exhaust 763 K (490 C),
#     9.0 m3/s, from a 0.5 m stack (46 m/s). Flow scales with the engine's rating; velocity is held.
#   Stack height 10 m at the sites (the same release height as the regional runs); 5 and 15 m for the single engine.
#   Emission rates: EPA Tier 2, NOx 6.4 g/kWh and PM 0.20 g/kWh; engines with SCR at 0.67 g/kWh NOx in an outage
#     (a long run, catalyst hot), uncontrolled in testing. Testing at 75% load, outage at 100%.
#   NO2 is formed from NOx by EPA's ARM2 method (between 50% and 90% of NOx, by concentration). No background added.
#   Layout: engines on a square grid 10 m apart centred on the site point. Real engine yards are rows along the buildings.
#   Flat terrain, rural dispersion, NO BUILDING DOWNWASH: the buildings beside the engines are not known, and their
#     wake pulls plumes down. Near the site (within a few building heights, some hundreds of metres) concentrations are
#     therefore understated, most of all for the short stack.
#   Receptors: rings every 10 degrees at 50 m to 5 km, at ground level (flagpole height 0); every 20 degrees for the
#     outage run of a site with more than 150 engines, to keep the run to about an hour.
#   An outage is a few hours in a year at a time nobody chooses. The 1-hour results are what the weather could give:
#     the highest hour, the 8th-highest daily maximum (the form of the 1-hour standard, 188 ug/m3) and the all-hours mean.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
OUT <- "analysis/output"; RUNS <- "build/aermod/runs"; MET <- "data-raw/aermod/met"
step <- commandArgs(trailingOnly = TRUE); if (!length(step)) step <- c("write", "run", "read")

KW_REF <- 2500; T_STACK <- 763; FLOW_REF <- 9.0; D_REF <- 0.5        # per 2,500 kW engine
EF <- c(nox = 6.4, nox_scr = 0.67, pm = 0.20)                          # g/kWh
LOAD_TEST <- 0.75; H_TEST_DEFAULT <- 32; H_SITE <- 10; SPACING <- 10
BIG_SITE <- 150                                                       # outage runs for sites with more engines use rings every 20 degrees
DIST <- c(50, 75, 100, 150, 200, 300, 400, 500, 750, 1000, 1500, 2000, 3000, 5000)
NAAQS_NO2_1H <- 188; RFC_DPM <- 5                                     # ug/m3: 1-hour NO2 standard (100 ppb); EPA reference concentration for diesel exhaust
NOVA <- c("51107", "51153", "51059", "51683", "51685", "51600", "51610", "51013", "51510")

num <- function(x) suppressWarnings(as.numeric(x))
stack <- function(kw) { d <- D_REF * sqrt(kw / KW_REF); list(d = d, v = FLOW_REF * (kw / KW_REF) / (pi * d^2 / 4)) }

# ---- which sites ------------------------------------------------------------------------------------------
pick_sites <- function() {
  S <- fread(file.path(OUT, "va_permits_sites.csv"), colClasses = list(character = c("registration", "fips")))
  S <- S[!grepl("^county", placed) & num(engines) > 0 & num(total_ekw) > 0 & fips %in% NOVA][order(-num(total_ekw), registration)]
  if (nrow(S) < 3) stop("fewer than three located Northern Virginia sites with equipment; run scripts/12 and analysis/05 first")
  k <- unique(c(1L, ceiling(nrow(S) / 2), ceiling(nrow(S) * 3 / 4)))
  S[k][, pick := c("largest", "median", "lower quartile")[seq_along(k)]]
}
test_hours <- function(w) {                                           # "7 am-5 pm" style window -> hours ending 8..17; default the same
  h <- rep(0, 24); h[8:17] <- 1; h
}

# ---- AERMOD input ----------------------------------------------------------------------------------------
met_files <- function() {
  sfc <- Sys.getenv("AERMOD_SFC", ""); pfl <- Sys.getenv("AERMOD_PFL", "")
  if (!nzchar(sfc)) { f <- list.files(MET, pattern = "\\.sfc$", ignore.case = TRUE, full.names = TRUE); if (length(f) == 1) sfc <- f }
  if (!nzchar(pfl)) { f <- list.files(MET, pattern = "\\.pfl$", ignore.case = TRUE, full.names = TRUE); if (length(f) == 1) pfl <- f }
  if (!nzchar(sfc) || !nzchar(pfl) || !file.exists(sfc) || !file.exists(pfl)) return(NULL)
  hd <- readLines(sfc, n = 2)
  id <- function(tag) { m <- regmatches(hd[1], regexpr(paste0(tag, ":\\s*[0-9]+"), hd[1])); if (length(m)) sub(".*:\\s*", "", m) else "99999" }
  yr <- as.integer(strsplit(trimws(hd[2]), "\\s+")[[1]][1]); yr <- if (yr < 100) ifelse(yr > 50, 1900 + yr, 2000 + yr) else yr
  list(sfc = normalizePath(sfc), pfl = normalizePath(pfl), sf_id = id("SF_ID"), ua_id = id("UA_ID"), year = yr, header = trimws(gsub("\\s+", " ", hd[1])))
}
write_inp <- function(run, title, src, pollutant, avertime, out_lines, met, emisfact = NULL, ndir = 36) {
  dir.create(file.path(RUNS, run), recursive = TRUE, showWarnings = FALSE)
  m <- if (is.null(met)) list(sfc = "MET.SFC", pfl = "MET.PFL", sf_id = "93738", ua_id = "93734", year = 2020) else met
  L <- c("CO STARTING",
         sprintf("   TITLEONE  %s", substr(title, 1, 68)),
         "   TITLETWO  us-load-impacts analysis/10_va_aermod.R; assumed stacks, no downwash, flat",
         sprintf("   MODELOPT  CONC FLAT%s", if (pollutant == "NO2") " ARM2" else ""),
         sprintf("   AVERTIME  %s", avertime),
         sprintf("   POLLUTID  %s", pollutant),
         "   RUNORNOT  RUN",
         "   ERRORFIL  errors.out",
         "CO FINISHED", "",
         "SO STARTING",
         sprintf("   LOCATION  %-6s POINT %9.1f %9.1f 0.0", src$id, src$x, src$y),
         sprintf("   SRCPARAM  %-6s %10.5f %6.1f %6.1f %6.2f %5.3f", src$id, src$q, src$h, T_STACK, src$v, src$d),
         if (!is.null(emisfact)) sprintf("   EMISFACT  %-6s HROFDY %s", src$id, paste(emisfact, collapse = " ")),
         "   SRCGROUP  ALL",
         "SO FINISHED", "",
         "RE STARTING",
         "   GRIDPOLR  RING STA",
         "   GRIDPOLR  RING ORIG 0.0 0.0",
         sprintf("   GRIDPOLR  RING DIST %s", paste(DIST[1:7], collapse = " ")),
         sprintf("   GRIDPOLR  RING DIST %s", paste(DIST[8:14], collapse = " ")),
         sprintf("   GRIDPOLR  RING GDIR %d %d %d", ndir, 360 / ndir, 360 / ndir),
         "   GRIDPOLR  RING END",
         "RE FINISHED", "",
         "ME STARTING",
         sprintf("   SURFFILE  \"%s\"", m$sfc),
         sprintf("   PROFFILE  \"%s\"", m$pfl),
         sprintf("   SURFDATA  %s %d", m$sf_id, m$year),
         sprintf("   UAIRDATA  %s %d", m$ua_id, m$year),
         "   PROFBASE  95.0 METERS",
         "ME FINISHED", "",
         "OU STARTING", out_lines, "OU FINISHED")
  writeLines(L, file.path(RUNS, run, "aermod.inp"))
}
OUT_NO2 <- c("   RECTABLE  1 1ST 8TH", "   PLOTFILE  1 ALL 1ST h1h.plt", "   PLOTFILE  1 ALL 8TH h8h.plt", "   PLOTFILE  PERIOD ALL period.plt")
OUT_ANN <- c("   PLOTFILE  PERIOD ALL period.plt")

layout_grid <- function(n) {                                          # n stacks on a square grid, SPACING apart, centred on 0,0
  side <- ceiling(sqrt(n)); g <- expand.grid(ix = seq_len(side), iy = seq_len(side))[seq_len(n), ]
  data.frame(x = (g$ix - (side + 1) / 2) * SPACING, y = (g$iy - (max(g$iy) + 1) / 2) * SPACING)
}

do_write <- function() {
  met <- met_files()
  if (is.null(met)) cat("No weather files found in", MET, "- writing inputs with placeholder names; rerun 'write' once the files are there.\n")
  runs <- list()
  for (h in c(5, 10, 15)) {
    st <- stack(KW_REF); run <- sprintf("engine_h%d", h)
    src <- data.frame(id = "E001", x = 0, y = 0, q = EF[["nox"]] * KW_REF / 3600, h = h, v = st$v, d = st$d)
    write_inp(run, sprintf("One %d kW diesel generator at full load, stack %d m: 1-hour NO2", KW_REF, h), src, "NO2", "1 PERIOD", OUT_NO2, met)
    runs[[run]] <- data.table(run = run, registration = NA_character_, site = sprintf("One %s kW engine", format(KW_REF, big.mark = ",")), pick = "one engine",
                              case = "one engine at full load", engines = 1L, kw_unit = KW_REF, scr_engines = 0L, stack_m = h, pollutant = "NO2")
  }
  S <- pick_sites()
  for (k in seq_len(nrow(S))) {
    s <- S[k]; n <- as.integer(num(s$engines)); kw <- num(s$total_ekw) / n; st <- stack(kw); xy <- layout_grid(n)
    nscr <- min(as.integer(fcoalesce(num(s$scr_engines), 0)), n); th <- fcoalesce(num(s$test_hours_per_engine), H_TEST_DEFAULT)
    id <- sprintf("E%03d", seq_len(n)); tag <- gsub("[^0-9A-Za-z]", "_", s$registration)
    # outage: every engine at full load; SCR engines at the Tier 4 rate
    q_out <- ifelse(seq_len(n) <= nscr, EF[["nox_scr"]], EF[["nox"]]) * kw / 3600
    write_inp(paste0(tag, "_outage"), sprintf("%s (%s): all %d engines at full load, 1-hour NO2", s$registration, s$pick, n),
              data.frame(id = id, x = xy$x, y = xy$y, q = q_out, h = H_SITE, v = st$v, d = st$d), "NO2", "1 PERIOD", OUT_NO2, met,
              ndir = if (n > BIG_SITE) 18 else 36)             # run time grows with engines x receptors x hours
    # routine testing: the year's testing emissions released evenly over the testing window (hours ending 8-17, every day)
    hf <- test_hours(s$testing_window); q_ann <- EF[["pm"]] * kw * th * LOAD_TEST / (sum(hf) * 365 * 3600)
    write_inp(paste0(tag, "_annual"), sprintf("%s (%s): routine testing, annual diesel PM", s$registration, s$pick),
              data.frame(id = id, x = xy$x, y = xy$y, q = q_ann, h = H_SITE, v = st$v, d = st$d), "OTHER", "PERIOD", OUT_ANN, met, emisfact = hf)
    base <- data.table(registration = s$registration, site = s$site, pick = s$pick, engines = n, kw_unit = round(kw), scr_engines = nscr, stack_m = H_SITE)
    runs[[paste0(tag, "_outage")]] <- cbind(run = paste0(tag, "_outage"), base, case = "outage: every engine at full load", pollutant = "NO2")
    runs[[paste0(tag, "_annual")]] <- cbind(run = paste0(tag, "_annual"), base, case = sprintf("routine testing, %g h per engine a year", th), pollutant = "diesel PM")
  }
  R <- rbindlist(runs, use.names = TRUE, fill = TRUE)
  R[, weather := if (is.null(met)) NA_character_ else met$header]
  dir.create("build/aermod", recursive = TRUE, showWarnings = FALSE); fwrite(R, "build/aermod/runs.csv")
  cat(sprintf("wrote %d input files under %s\n", nrow(R), RUNS)); print(R[, .(run, site = substr(site, 1, 34), engines, kw_unit, scr_engines, stack_m)])
}

# ---- run -------------------------------------------------------------------------------------------------
do_run <- function() {
  exe <- Sys.getenv("AERMOD_EXE", "build/aermod/bin/aermod")
  if (!file.exists(exe)) stop("AERMOD program not found at ", exe, ". Build it with: Rscript scripts/13_build_aermod.R")
  if (is.null(met_files())) stop("weather files not found: put one .SFC and one .PFL for Washington Dulles in ", MET, " and run 'write' again")
  exe <- normalizePath(exe); R <- fread("build/aermod/runs.csv")
  for (r in R$run) {
    d <- file.path(RUNS, r); cat(sprintf("%-22s ", r)); t0 <- Sys.time()
    old <- setwd(d); status <- tryCatch(system2(exe, stdout = "screen.txt", stderr = "screen.txt"), error = function(e) 1L); setwd(old)
    o <- if (file.exists(file.path(d, "aermod.out"))) readLines(file.path(d, "aermod.out"), warn = FALSE) else character()
    ok <- any(grepl("AERMOD Finishes Successfully", o, ignore.case = TRUE))
    cat(if (ok) "ok" else "FAILED", sprintf("(%.0f s)\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    if (!ok) {                                                        # show AERMOD's own error lines and stop: nothing is retried
      e <- grep("^\\s*[A-Z]{2}\\s+E[0-9]{3}", o, value = TRUE); if (file.exists(file.path(d, "errors.out"))) e <- c(e, readLines(file.path(d, "errors.out"), warn = FALSE))
      cat(head(unique(e), 30), sep = "\n"); stop("AERMOD did not finish run ", r, "; see ", file.path(d, "aermod.out"))
    }
  }
}

# ---- read ------------------------------------------------------------------------------------------------
read_plt <- function(f) {
  x <- readLines(f, warn = FALSE); x <- x[!grepl("^\\s*\\*", x) & nzchar(trimws(x))]
  m <- do.call(rbind, lapply(strsplit(trimws(x), "\\s+"), function(v) as.numeric(v[1:3])))
  data.table(x = m[, 1], y = m[, 2], conc = m[, 3])
}
do_read <- function() {
  R <- fread("build/aermod/runs.csv"); res <- list()
  for (i in seq_len(nrow(R))) for (st in c("h1h", "h8h", "period")) {
    f <- file.path(RUNS, R$run[i], paste0(st, ".plt")); if (!file.exists(f)) next
    p <- read_plt(f); p[, distance_m := round(sqrt(x^2 + y^2))]
    res[[length(res) + 1]] <- cbind(run = R$run[i], stat = st, p[, .(max = max(conc), mean = mean(conc)), by = distance_m])
  }
  if (!length(res)) stop("no AERMOD results found under ", RUNS, "; run the 'run' step first")
  res <- rbindlist(res)
  ver <- vapply(R$run, function(r) { o <- readLines(file.path(RUNS, r, "aermod.out"), n = 200, warn = FALSE)
    v <- regmatches(o, regexpr("VERSION\\s+[0-9]+[A-Za-z]*", o)); if (length(v)) sub("VERSION\\s+", "", v[1]) else NA_character_ }, "")
  R[, aermod_version := ver]
  fwrite(res, file.path(OUT, "va_aermod_results.csv")); fwrite(R, file.path(OUT, "va_aermod_runs.csv"))
  lines <- character(); say <- function(...) { s <- sprintf(...); lines <<- c(lines, s); cat(s, "\n") }
  far <- function(d, v, lim) { k <- which(v >= lim); if (length(k)) max(d[k]) else 0 }
  say("NEAR-FIELD CONCENTRATIONS (AERMOD %s; weather: %s)", R$aermod_version[1], R$weather[1])
  say("Assumed stacks, flat terrain, no building downwash, no background. ug/m3; highest receptor on each ring.")
  for (i in seq_len(nrow(R))) {
    r <- R[i]; say(""); say("%s  [%s; %d x %s kW; stack %g m]", r$site, r$case, r$engines, format(r$kw_unit, big.mark = ","), r$stack_m)
    for (st in unique(res[run == r$run, stat])) {
      v <- res[run == r$run & stat == st][order(distance_m)]
      lab <- c(h1h = "highest hour", h8h = "8th-highest daily max", period = if (r$pollutant == "NO2") "mean of all hours" else "annual mean")[[st]]
      say("  %-22s %s", lab, paste(sprintf("%gm %s", v$distance_m[c(1, 3, 5, 8, 10, 12, 14)], signif(v$max[c(1, 3, 5, 8, 10, 12, 14)], 3)), collapse = " | "))
      if (r$pollutant == "NO2" && st != "period") { f <- far(v$distance_m, v$max, NAAQS_NO2_1H)
        say("  %-22s %s", "", if (f == 0) sprintf("below %d ug/m3 (the 1-hour NO2 standard) on every ring", NAAQS_NO2_1H) else if (f >= max(DIST))
          sprintf("at or above %d ug/m3 (the 1-hour NO2 standard) to the edge of the grid (5 km) and beyond", NAAQS_NO2_1H) else
          sprintf("at or above %d ug/m3 (the 1-hour NO2 standard) out to %s m", NAAQS_NO2_1H, format(f, big.mark = ","))) }
    }
  }
  say(""); say("Diesel PM: EPA's reference concentration for long-term exposure is %g ug/m3; California's cancer unit risk is 300 in a million per ug/m3 over a lifetime.", RFC_DPM)
  writeLines(lines, file.path(OUT, "va_aermod_summary.txt"))
  cat("wrote", file.path(OUT, c("va_aermod_results.csv", "va_aermod_runs.csv", "va_aermod_summary.txt")), sep = "\n")
}

if ("write" %in% step) do_write()
if ("run" %in% step) do_run()
if ("read" %in% step) do_read()
