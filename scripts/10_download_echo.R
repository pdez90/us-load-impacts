# =============================================================================
# 10_download_echo.R  --  EPA ECHO bulk files used to find permitted data centers in every state
# -----------------------------------------------------------------------------
# Run from the repo root on a machine with internet access:  Rscript scripts/10_download_echo.R
# Out: data-raw/echo/ICIS-AIR_downloads.zip  (about 64 MB)  ICIS-Air: every Clean Air Act stationary source
#                                                           EPA and the states track, with industry codes and
#                                                           pollutant class (major / synthetic minor / minor)
#      data-raw/echo/echo_exporter.zip       (about 392 MB) ECHO Exporter: one row per facility, with latitude
#                                                           and longitude, joined to ICIS-Air on REGISTRY_ID
#      data-raw/echo/SOURCE                  where and when they came from
# EPA's Data Downloads page offers these files for download and reuse
# (https://echo.epa.gov/tools/data-downloads). echo.epa.gov/robots.txt does not exclude /files/ and asks for
# 10 seconds between requests; this script waits that long and stops on any refusal rather than retrying.
# Files already present are not downloaded again (delete them to refresh).
# =============================================================================
DIR <- "data-raw/echo"; dir.create(DIR, showWarnings = FALSE, recursive = TRUE)
BASE <- "https://echo.epa.gov/files/echodownloads/"
FILES <- c("ICIS-AIR_downloads.zip", "echo_exporter.zip")
UA <- "us-load-impacts research script (https://github.com/pdez90/us-load-impacts)"
options(timeout = 3600)

for (k in seq_along(FILES)) {
  f <- FILES[k]; out <- file.path(DIR, f)
  if (file.exists(out) && file.size(out) > 1e6) { cat("have", out, "\n"); next }
  if (k > 1) Sys.sleep(10)
  cat("downloading", f, "...\n")
  tmp <- paste0(out, ".part")
  ok <- tryCatch(download.file(paste0(BASE, f), tmp, mode = "wb", quiet = FALSE, method = "libcurl",
                               headers = c("User-Agent" = UA)) == 0, error = function(e) { message(conditionMessage(e)); FALSE })
  if (!ok || !file.exists(tmp) || file.size(tmp) < 1e6) {
    unlink(tmp); stop("EPA did not serve ", f, ". Not retrying; check https://echo.epa.gov/tools/data-downloads in a browser.")
  }
  file.rename(tmp, out); cat(sprintf("  %s: %.0f MB\n", f, file.size(out) / 1e6))
}
writeLines(c(sprintf("Downloaded %s from %s", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), BASE),
             paste("  ", FILES, sprintf("%.0f MB", file.size(file.path(DIR, FILES)) / 1e6))), file.path(DIR, "SOURCE"))
cat("done\n")
