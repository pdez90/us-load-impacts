# Writes manifest.json, which Posit Connect Cloud needs to install the R packages.
# Run from the repo root on a machine with CRAN access, after data/app_data.rds exists:
#   Rscript scripts/05_write_manifest.R
stopifnot(file.exists("data/app_data.rds"))
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R",
                         appFiles = c("app.R", "R/calc.R", "methods.html", "data/app_data.rds", "data/build_report.txt",
                                      if (file.exists("data/harvard.rds")) "data/harvard.rds",
                                      if (file.exists("data/onsite_groups.rds")) "data/onsite_groups.rds",
                                      if (file.exists("data/va_permits.rds")) "data/va_permits.rds",
                                      if (file.exists("data/chen2026.rds")) "data/chen2026.rds"))
cat("manifest.json written\n")
