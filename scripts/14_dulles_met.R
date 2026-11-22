# =============================================================================
# 14_dulles_met.R  --  AERMOD-ready weather files for Washington Dulles, from NOAA observations
# -----------------------------------------------------------------------------
# Virginia DEQ does not post processed weather files, so this builds them with EPA's AERMET from public NOAA data.
#
# Usage: Rscript scripts/14_dulles_met.R                # 2021-2024 (NOAA's 2025 file for Dulles has no wind for 35% of hours)
#        Rscript scripts/14_dulles_met.R 2023 2023      # first and last year
# Needs: build/aermet/bin/aermet  (Rscript scripts/13_build_aermod.R aermet), network access to NOAA (ncei.noaa.gov)
# Out  : data-raw/aermod/met/dulles_<y1>_<y2>.sfc and .pfl  (read by analysis/10_va_aermod.R), and a short summary
#        build/aermet/dulles/  : downloads, the control file (aermet.inp), AERMET's report and messages
# Downloads: one file per year of hourly surface observations and one file of balloon soundings, each requested once,
#        identified by a User-Agent, 2 s apart; on a refusal the script stops and does not retry. Files already there are kept.
#
# DATA
#   Surface : NOAA Integrated Surface Database, Washington Dulles International (USAF 724030, WBAN 93738)
#   Upper air: NOAA IGRA soundings, Sterling, Virginia (USM00072403), 5 km from the airport
#
# CHOICES AND WHAT THEY LEAVE OUT (differences from a set DEQ would prepare)
#   * No 1-minute wind data (EPA's AERMINUTE step). Hourly observations report light winds as calm more often, and
#     AERMOD skips calm hours, so the lightest-wind hours are under-represented. The summary printed at the end gives
#     the share of calm and missing hours.
#   * Surface characteristics are typed in, not derived from land-cover data (EPA's AERSURFACE step): one sector,
#     by season, EPA's table values for grassland as at an airfield (AERMET user's guide, Tables 3-1 to 3-5), with
#     winter taken as snow-free. See SITE_CHAR below.
#   * Low-wind adjustment of friction velocity (ADJ_U*) is on, as EPA recommends for airport data.
#   * Sounding nearest sunrise is used (UASELECT SUNRISE).
# =============================================================================
a <- commandArgs(trailingOnly = TRUE)
Y1 <- if (length(a) >= 1) as.integer(a[1]) else 2021L; Y2 <- if (length(a) >= 2) as.integer(a[2]) else 2024L
stopifnot(Y1 <= Y2, Y1 >= 2000)
WORK <- "build/aermet/dulles"; MET <- "data-raw/aermod/met"; EXE <- Sys.getenv("AERMET_EXE", "build/aermet/bin/aermet")
for (d in c(WORK, MET)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
UA <- "us-load-impacts research script (github.com/pdez90/us-load-impacts)"

SF <- list(usaf = "724030", wban = "93738", lat = 38.935, lon = 77.447, elev = 95, anem = 10)   # Dulles ASOS
UP <- list(igra = "USM00072403", lat = 38.977, lon = 77.486, elev = 87)                           # Sterling
# The station id AERMET wants for an IGRA file is not stated in its guide. Default: the id's last eight characters.
UA_ID <- Sys.getenv("AERMET_UA_ID", "00072403")
TADJ <- 5                                                           # both files are in UTC; Eastern Standard Time = UTC - 5
# season 1 winter (Dec-Feb, no snow), 2 spring, 3 summer, 4 autumn: albedo, Bowen ratio (average moisture), roughness (m)
SITE_CHAR <- data.frame(season = 1:4, albedo = c(0.20, 0.18, 0.18, 0.20), bowen = c(1.0, 0.4, 0.8, 1.0), z0 = c(0.01, 0.05, 0.10, 0.01))

get <- function(url, dest) {
  if (file.exists(dest) && file.size(dest) > 0) return(invisible(TRUE))
  cat("downloading", url, "\n")
  ok <- tryCatch(download.file(url, dest, mode = "wb", quiet = TRUE, headers = c("User-Agent" = UA)) == 0,
                 error = function(e) { message(conditionMessage(e)); FALSE }, warning = function(w) { message(conditionMessage(w)); FALSE })
  if (!ok) { unlink(dest); stop("NOAA refused or the download failed; not retrying. Save it by hand as ", dest, " and rerun.\n  ", url) }
  Sys.sleep(2)
}

# ---- 1. downloads -----------------------------------------------------------------------------------------
isd <- file.path(WORK, sprintf("dulles_isd_%d_%d.txt", Y1, Y2))
if (!file.exists(isd)) {
  con <- file(isd, "w")
  for (y in Y1:Y2) {
    gz <- file.path(WORK, sprintf("%s-%s-%d.gz", SF$usaf, SF$wban, y))
    get(sprintf("https://www.ncei.noaa.gov/pub/data/noaa/%d/%s-%s-%d.gz", y, SF$usaf, SF$wban, y), gz)
    g <- gzfile(gz, "r"); x <- readLines(g, warn = FALSE); close(g); writeLines(x, con)
    cat(sprintf("  %d: %s surface records\n", y, format(length(x), big.mark = ",")))
  }
  close(con)
}
zip <- file.path(WORK, paste0(UP$igra, "-data.txt.zip")); igra <- file.path(WORK, paste0(UP$igra, "-data.txt"))
if (!file.exists(igra)) {
  get(sprintf("https://www.ncei.noaa.gov/data/integrated-global-radiosonde-archive/access/data-por/%s-data.txt.zip", UP$igra), zip)
  unzip(zip, exdir = WORK); if (!file.exists(igra)) stop("expected ", igra, " inside ", zip)
}

# ---- 2. control file (Stage 1 and 2 in one run) -------------------------------------------------------------
xd <- sprintf("   XDATES    %d/01/01 TO %d/12/31", Y1, Y2)
out <- sprintf("dulles_%d_%d", Y1, Y2)
inp <- c("JOB",
         "   REPORT    aermet.rpt",
         "   MESSAGES  aermet.msg", "",
         "UPPERAIR",
         sprintf("   DATA      %s IGRA", basename(igra)),
         "   EXTRACT   ua_extract.txt",
         "   QAOUT     ua_qa.txt",
         xd,
         sprintf("   LOCATION  %s %.3fN %.3fW %d %g", UA_ID, UP$lat, UP$lon, TADJ, UP$elev),
         "   MODIFY", "",
         "SURFACE",
         sprintf("   DATA      %s ISHD", basename(isd)),
         "   EXTRACT   sf_extract.txt",
         "   QAOUT     sf_qa.txt",
         xd,
         sprintf("   LOCATION  %s %.3fN %.3fW %d %g", SF$wban, SF$lat, SF$lon, TADJ, SF$elev), "",
         "METPREP",
         xd,
         "   METHOD    REFLEVEL SUBNWS",
         "   METHOD    WIND_DIR RANDOM",
         "   METHOD    STABLEBL ADJ_U*",
         "   METHOD    UASELECT SUNRISE",
         sprintf("   NWS_HGT   WIND %g", SF$anem),
         "   FREQ_SECT SEASONAL 1",
         "   SECTOR    1 0 360",
         sprintf("   SITE_CHAR %d 1 %.2f %.2f %.3f", SITE_CHAR$season, SITE_CHAR$albedo, SITE_CHAR$bowen, SITE_CHAR$z0),
         sprintf("   OUTPUT    %s.sfc", out),
         sprintf("   PROFILE   %s.pfl", out))
writeLines(inp, file.path(WORK, "aermet.inp"))
cat("wrote", file.path(WORK, "aermet.inp"), "\n")

# ---- 3. run -------------------------------------------------------------------------------------------------
if (!file.exists(EXE)) stop("AERMET not found at ", EXE, ". Build it with: Rscript scripts/13_build_aermod.R aermet")
exe <- normalizePath(EXE); unlink(file.path(WORK, paste0(out, c(".sfc", ".pfl"))))
old <- setwd(WORK); system2(exe, "aermet.inp", stdout = "screen.txt", stderr = "screen.txt"); setwd(old)
sfc <- file.path(WORK, paste0(out, ".sfc")); pfl <- file.path(WORK, paste0(out, ".pfl"))
if (!file.exists(sfc) || file.size(sfc) < 1000 || !file.exists(pfl)) {                 # show AERMET's own messages and stop
  for (f in c("screen.txt", "aermet.msg")) if (file.exists(file.path(WORK, f))) {
    m <- readLines(file.path(WORK, f), warn = FALSE); e <- grep("\\bE[0-9]{2}\\b|ERROR|error|abort", m, value = TRUE)
    cat("---", f, "---\n"); cat(head(if (length(e)) unique(e) else tail(m, 25), 30), sep = "\n")
  }
  stop("AERMET did not write the weather files; see ", file.path(WORK, "aermet.rpt"), " and aermet.msg")
}

# ---- 4. check and copy --------------------------------------------------------------------------------------
x <- readLines(sfc, warn = FALSE); hd <- x[1]; v <- strsplit(trimws(x[-1]), "\\s+")
ws <- as.numeric(vapply(v, `[`, "", 16)); L <- as.numeric(vapply(v, `[`, "", 12))          # columns: 12 Monin-Obukhov length, 16 wind speed
n <- length(ws); expect <- as.numeric(difftime(as.Date(sprintf("%d-01-01", Y2 + 1)), as.Date(sprintf("%d-01-01", Y1)), units = "hours"))
calm <- sum(ws == 0, na.rm = TRUE); miss <- sum(ws >= 900 | (L <= -99990 & ws > 0), na.rm = TRUE)
s <- c(sprintf("Washington Dulles weather for AERMOD, %d-%d (scripts/14_dulles_met.R)", Y1, Y2),
       sprintf("  header: %s", trimws(gsub("\\s+", " ", hd))),
       sprintf("  hours in file: %s of %s expected", format(n, big.mark = ","), format(expect, big.mark = ",")),
       sprintf("  calm (wind speed 0): %.1f%%;  missing wind or stability: %.1f%%  -> AERMOD skips both", 100 * calm / n, 100 * miss / n),
       sprintf("  mean wind speed in the other hours: %.2f m/s", mean(ws[ws > 0 & ws < 900], na.rm = TRUE)),
       "  EPA's completeness guide is 90% of hours per quarter; no 1-minute winds were used, so calms are more frequent than in a regulatory set.",
       "  Surface characteristics typed in (airfield grass, by season), not from AERSURFACE.")
cat(s, sep = "\n")
old_met <- list.files(MET, pattern = "\\.(sfc|pfl)$", ignore.case = TRUE, full.names = TRUE)
if (length(old_met)) { dir.create(file.path(MET, "old"), showWarnings = FALSE); file.rename(old_met, file.path(MET, "old", basename(old_met))) }
invisible(file.copy(c(sfc, pfl), MET, overwrite = TRUE)); writeLines(s, file.path(MET, paste0(out, "_summary.txt")))
cat("wrote", file.path(MET, basename(c(sfc, pfl))), sep = "\n")
cat("next: Rscript analysis/10_va_aermod.R\n")
