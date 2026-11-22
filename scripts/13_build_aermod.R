# =============================================================================
# 13_build_aermod.R  --  download EPA's AERMOD source and compile it with gfortran
# -----------------------------------------------------------------------------
# Usage: Rscript scripts/13_build_aermod.R            # AERMOD, the dispersion model   -> build/aermod/bin/aermod
#        Rscript scripts/13_build_aermod.R aermet     # AERMET, its weather processor  -> build/aermet/bin/aermet
# Needs: gfortran (macOS: `brew install gcc`), unzip, network access to EPA's file server.
# Out  : build/<program>/bin/<program>   (build/ is not committed)
# One request to EPA, identified by a User-Agent; on a refusal the script stops and does not retry.
# If the download is refused, save aermod_source.zip by hand from EPA's AERMOD page
# (https://www.epa.gov/scram/air-quality-dispersion-modeling-preferred-and-recommended-models; AERMET is under
# Meteorological Processors on the same site) into build/aermod/ or build/aermet/ and rerun.
# =============================================================================
PROG <- commandArgs(trailingOnly = TRUE); PROG <- if (length(PROG)) tolower(PROG[1]) else "aermod"
stopifnot(PROG %in% c("aermod", "aermet"))
URL <- c(aermod = "https://gaftp.epa.gov/Air/aqmg/SCRAM/models/preferred/aermod/aermod_source.zip",
         aermet = "https://gaftp.epa.gov/Air/aqmg/SCRAM/models/met/aermet/aermet_source.zip")[[PROG]]
DIR <- file.path("build", PROG); SRC <- file.path(DIR, "src"); BIN <- file.path(DIR, "bin"); ZIP <- file.path(DIR, paste0(PROG, "_source.zip"))
for (d in c(SRC, BIN)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
if (!nzchar(Sys.which("gfortran"))) stop("gfortran not found. On a Mac: brew install gcc")

if (!file.exists(ZIP)) {
  cat("downloading", URL, "\n")
  ok <- tryCatch(download.file(URL, ZIP, mode = "wb", quiet = TRUE,
                               headers = c("User-Agent" = "us-load-impacts research script (github.com/pdez90/us-load-impacts)")) == 0,
                 error = function(e) { message(conditionMessage(e)); FALSE }, warning = function(w) { message(conditionMessage(w)); FALSE })
  if (!ok) { unlink(ZIP); stop("EPA's server refused or the download failed; not retrying. Save the zip by hand to ", ZIP) }
}
unzip(ZIP, exdir = SRC, junkpaths = TRUE)
f <- list.files(SRC, pattern = "\\.(f|f90|for)$", ignore.case = TRUE)
if (!length(f)) stop("no Fortran source found in ", ZIP)
cat(length(f), "source files\n")

# modules first; then everything else, retrying files that were waiting on another file's module, for as long as a pass compiles something
old <- setwd(SRC)
comp <- function(x) system2("gfortran", c("-O2", "-c", "-w", shQuote(x)), stdout = FALSE, stderr = FALSE) == 0
mods <- f[grepl("^mod", f, ignore.case = TRUE)]; todo <- c(mods, setdiff(f, mods))
for (pass in 1:40) {
  done <- vapply(todo, comp, TRUE); cat(sprintf("pass %d: %d compiled, %d waiting\n", pass, sum(done), sum(!done)))
  todo <- todo[!done]; if (!length(todo) || !any(done)) break
}
if (length(todo)) { system2("gfortran", c("-O2", "-c", shQuote(todo[1]))); setwd(old); stop("could not compile: ", paste(todo, collapse = ", ")) }
exe <- file.path("..", "bin", PROG)
linked <- system2("gfortran", c("-O2", "-o", exe, list.files(pattern = "\\.o$"))) == 0
setwd(old); if (!linked) stop("linking failed")
cat("built", file.path(BIN, PROG), "\n")
