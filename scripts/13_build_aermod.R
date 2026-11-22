# =============================================================================
# 13_build_aermod.R  --  download EPA's AERMOD source and compile it with gfortran
# -----------------------------------------------------------------------------
# Usage: Rscript scripts/13_build_aermod.R
# Needs: gfortran (macOS: `brew install gcc`), unzip, network access to EPA's file server.
# Out  : build/aermod/bin/aermod   (build/ is not committed)
# One request to EPA, identified by a User-Agent; on a refusal the script stops and does not retry.
# If the download is refused, save aermod_source.zip by hand from EPA's AERMOD page
# (https://www.epa.gov/scram/air-quality-dispersion-modeling-preferred-and-recommended-models) into build/aermod/ and rerun.
# =============================================================================
URL <- "https://gaftp.epa.gov/Air/aqmg/SCRAM/models/preferred/aermod/aermod_source.zip"
DIR <- "build/aermod"; SRC <- file.path(DIR, "src"); BIN <- file.path(DIR, "bin"); ZIP <- file.path(DIR, "aermod_source.zip")
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

# modules first; then everything else, retrying files that were waiting on another file's module (at most 4 passes)
old <- setwd(SRC)
comp <- function(x) system2("gfortran", c("-O2", "-c", "-w", shQuote(x)), stdout = FALSE, stderr = FALSE) == 0
mods <- f[grepl("^modules\\.", f, ignore.case = TRUE)]; todo <- c(mods, setdiff(f, mods))
for (pass in 1:4) {
  done <- vapply(todo, comp, TRUE); cat(sprintf("pass %d: %d compiled, %d waiting\n", pass, sum(done), sum(!done)))
  todo <- todo[!done]; if (!length(todo)) break
}
if (length(todo)) { system2("gfortran", c("-O2", "-c", shQuote(todo[1]))); setwd(old); stop("could not compile: ", paste(todo, collapse = ", ")) }
exe <- file.path("..", "bin", "aermod")
linked <- system2("gfortran", c("-O2", "-o", exe, list.files(pattern = "\\.o$"))) == 0
setwd(old); if (!linked) stop("linking failed")
cat("built", file.path(BIN, "aermod"), "\n")
