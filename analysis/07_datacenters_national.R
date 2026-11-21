# =============================================================================
# 07_datacenters_national.R  --  data centers with Clean Air Act records, every state (EPA ICIS-Air)
# -----------------------------------------------------------------------------
# Run: Rscript scripts/10_download_echo.R   (once, needs internet)
#      Rscript analysis/07_datacenters_national.R
# In : data-raw/echo/ICIS-AIR_downloads.zip   ICIS-AIR_FACILITIES.csv (and ICIS-AIR_PROGRAMS.csv)
#      data-raw/echo/echo_exporter.zip        coordinates, by REGISTRY_ID
#      data-raw/permits/va/va_permits.csv     (optional) to check how many of Virginia's permits ICIS-Air holds
# Out: analysis/output/dc_national_sites.csv    one row per ICIS-Air source that looks like a data center
#      analysis/output/dc_national_by_state.csv counts by state and pollutant class
#      analysis/output/dc_national_summary.txt
#      analysis/output/dc_national_map.png
#
# What counts as a data center: an industry code for data processing and hosting (NAICS 518210, SIC 7374), or a
# name that says data center or belongs to a data center operator. Crypto-mining names are flagged, since they
# share NAICS 518210. ICIS-Air holds sources that states report to EPA: major sources, synthetic minor sources
# (permits that cap emissions below the major thresholds, as most data centers hold) and some minor sources.
# Data centers under permits by rule, general permits or exemptions are often missing. So this is a list of
# places to look, not a census; the Virginia check below measures how much it misses.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(sf) })
sf_use_s2(FALSE)
OUT <- "analysis/output"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
ECHO <- Sys.getenv("ECHO_DIR", "data-raw/echo")
lines <- character(); say <- function(...) { s <- sprintf(...); lines <<- c(lines, s); cat(s, "\n") }

read_zip_csv <- function(zip, pattern, select = NULL) {
  if (!file.exists(zip)) stop(zip, " not found; run Rscript scripts/10_download_echo.R first")
  nm <- grep(pattern, unzip(zip, list = TRUE)$Name, value = TRUE, ignore.case = TRUE)[1]
  if (is.na(nm)) stop("no file matching ", pattern, " in ", zip)
  td <- tempfile(); dir.create(td); unzip(zip, nm, exdir = td, junkpaths = TRUE)
  f <- file.path(td, basename(nm)); on.exit(unlink(td, recursive = TRUE))
  hdr <- names(fread(f, nrows = 0))
  if (!is.null(select)) { miss <- setdiff(select, hdr); if (length(miss)) message("not in ", basename(nm), ": ", paste(miss, collapse = ", ")); select <- intersect(select, hdr) }
  fread(f, select = select, colClasses = "character", encoding = "Latin-1", showProgress = FALSE)
}

# ---- 1. ICIS-Air sources ------------------------------------------------------------------------------
F <- read_zip_csv(file.path(ECHO, "ICIS-AIR_downloads.zip"), "FACILITIES",
                  c("PGM_SYS_ID", "REGISTRY_ID", "FACILITY_NAME", "STREET_ADDRESS", "CITY", "COUNTY_NAME", "STATE", "ZIP_CODE",
                    "SIC_CODES", "NAICS_CODES", "AIR_POLLUTANT_CLASS_CODE", "AIR_POLLUTANT_CLASS_DESC",
                    "AIR_OPERATING_STATUS_CODE", "AIR_OPERATING_STATUS_DESC"))
stopifnot(all(c("PGM_SYS_ID", "FACILITY_NAME", "STATE") %in% names(F)))
say("Data centers with Clean Air Act records in EPA ICIS-Air (analysis/07_datacenters_national.R)")
say("ICIS-Air sources read: %s", format(nrow(F), big.mark = ","))

OPERATORS <- paste0("DATA ?CENT(ER|RE)|DATACENTER|\\bAMAZON DATA SERVICES|\\bVADATA\\b|EQUINIX|DIGITAL REALTY|\\bQTS\\b|",
                    "\\bVANTAGE\\b|CYRUSONE|SABEY|CORESITE|ALIGNED (DATA|ENERGY)|STACK INFRASTRUCTURE|\\bSWITCH LTD|DUPONT FABROS|",
                    "COMPASS DATA|EDGECONNEX|CLOUDHQ|IRON MOUNTAIN DATA|NTT GLOBAL DATA|RAGINGWIRE|FLEXENTIAL|CYXTERA|",
                    "MICROSOFT.*(DATA|CLOUD)|GOOGLE.*(DATA|LLC)|META PLATFORMS|FACEBOOK|APPLE.*DATA|ORACLE.*(CLOUD|DATA)|",
                    "TIKTOK|BYTEDANCE|COLOVORE|SERVER FARM|SERVERFARM|HYPERSCALE|\\bAWS\\b")
CRYPTO <- "BITCOIN|CRYPTO|MINING|MINER\\b|DIGITAL ASSET|BLOCKCHAIN|RIOT PLATFORMS|MARATHON|CORE SCIENTIFIC|HASH"
nm <- toupper(F$FACILITY_NAME)
F[, by_code := grepl("\\b518210\\b", NAICS_CODES) | grepl("\\b7374\\b", SIC_CODES)]
F[, by_name := grepl(OPERATORS, nm, perl = TRUE)]
F[, crypto := grepl(CRYPTO, nm, perl = TRUE)]
D <- F[by_code | by_name]
D[, matched_on := fifelse(by_code & by_name, "code and name", fifelse(by_code, "industry code", "name"))]
D[, status := fcase(AIR_OPERATING_STATUS_CODE %in% c("OPR", "SEA", "TMP"), "operating",
                    AIR_OPERATING_STATUS_CODE %in% c("CNS", "PLN"), "planned or under construction",
                    AIR_OPERATING_STATUS_CODE == "CLS", "closed", default = "other or unknown")]
D[, class := fcase(AIR_POLLUTANT_CLASS_CODE == "MAJ", "major", AIR_POLLUTANT_CLASS_CODE == "SMI", "synthetic minor",
                   AIR_POLLUTANT_CLASS_CODE == "MIN", "minor", default = "other or unknown")]

# Air programs on record (Title V, state permit program, NSPS IIII, NESHAP ZZZZ ...), when the file has them.
prg <- tryCatch(read_zip_csv(file.path(ECHO, "ICIS-AIR_downloads.zip"), "PROGRAMS\\.csv", c("PGM_SYS_ID", "PROGRAM_CODE")), error = function(e) NULL)
if (!is.null(prg)) D[prg[PGM_SYS_ID %in% D$PGM_SYS_ID, .(programs = paste(sort(unique(PROGRAM_CODE)), collapse = " ")), by = PGM_SYS_ID],
                     on = "PGM_SYS_ID", programs := i.programs]

# ---- 2. coordinates from the ECHO Exporter ---------------------------------------------------------------
X <- tryCatch(read_zip_csv(file.path(ECHO, "echo_exporter.zip"), "ECHO_EXPORTER.*\\.csv",
                           c("REGISTRY_ID", "FAC_LAT", "FAC_LONG", "FAC_FIPS_CODE")), error = function(e) { message(conditionMessage(e)); NULL })
if (!is.null(X) && "REGISTRY_ID" %in% names(D)) {
  X <- unique(X[REGISTRY_ID %in% D$REGISTRY_ID], by = "REGISTRY_ID")
  D[X, on = "REGISTRY_ID", `:=`(lat = as.numeric(i.FAC_LAT), lon = as.numeric(i.FAC_LONG), county_fips = i.FAC_FIPS_CODE)]
} else D[, `:=`(lat = NA_real_, lon = NA_real_, county_fips = NA_character_)]

keep <- intersect(c("STATE", "FACILITY_NAME", "STREET_ADDRESS", "CITY", "COUNTY_NAME", "ZIP_CODE", "PGM_SYS_ID", "REGISTRY_ID",
                    "status", "class", "matched_on", "crypto", "NAICS_CODES", "SIC_CODES", "programs", "lat", "lon", "county_fips"), names(D))
setorder(D, STATE, FACILITY_NAME)
fwrite(D[, ..keep], file.path(OUT, "dc_national_sites.csv"))

# ---- 3. summaries -----------------------------------------------------------------------------------------
cur <- D[status != "closed" & !crypto]
say("Look like data centers: %d (%d by industry code, %d by name only); %d crypto-mining names set aside; %d closed",
    nrow(D), sum(D$by_code), sum(!D$by_code), sum(D$crypto), sum(D$status == "closed"))
say("Not closed, not crypto: %d in %d states; %d major, %d synthetic minor, %d minor, %d other/unknown class; %d with coordinates",
    nrow(cur), uniqueN(cur$STATE), sum(cur$class == "major"), sum(cur$class == "synthetic minor"), sum(cur$class == "minor"),
    sum(cur$class == "other or unknown"), sum(is.finite(cur$lat)))
S <- cur[, .(sites = .N, major = sum(class == "major"), synthetic_minor = sum(class == "synthetic minor"), minor = sum(class == "minor"),
             planned = sum(status == "planned or under construction"), by_name_only = sum(matched_on == "name")), by = STATE][order(-sites)]
fwrite(S, file.path(OUT, "dc_national_by_state.csv"))
say("")
say("BY STATE (not closed, not crypto), most first")
for (i in seq_len(min(25, nrow(S)))) with(S[i], say("  %-3s %4d sites  (major %d, synthetic minor %d, minor %d; planned %d; found by name only %d)",
                                                   STATE, sites, major, synthetic_minor, minor, planned, by_name_only))

# How much does ICIS-Air miss? Virginia, where we hold every permit DEQ lists.
vaf <- "data-raw/permits/va/va_permits.csv"
if (file.exists(vaf)) {
  V <- fread(vaf, colClasses = "character")[current == "yes"]
  # Two keys: house number + ZIP, or house number + first word of the street name (ZIPs differ between
  # Ashburn and Sterling addresses, and some permits give none).
  house <- function(s) sub("^\\s*([0-9]+).*$", "\\1", toupper(s))
  word <- function(s) sub("^\\s*[0-9]+[A-Z]?\\s+(([NSEW]\\.?|NORTH|SOUTH|EAST|WEST)\\s+)?([A-Z0-9]+).*$", "\\3", toupper(s))
  k1 <- function(s, z) paste(house(s), substr(gsub("[^0-9]", "", z), 1, 5)); k2 <- function(s) paste(house(s), word(s))
  hit <- function(src) (k1(V$street, V$zip) %in% k1(src$STREET_ADDRESS, src$ZIP_CODE) & nzchar(gsub("[^0-9]", "", V$zip))) |
                       (k2(V$street) %in% k2(src$STREET_ADDRESS) & grepl("^\\s*[0-9]", V$street))
  in_dc <- hit(D[STATE == "VA"]); in_any <- hit(F[STATE == "VA"])
  say("")
  say("CHECK AGAINST VIRGINIA (%d current DEQ permits; matched on house number and ZIP code or street name)", nrow(V))
  say("  in this data center list: %d (%.0f%%); in ICIS-Air under any industry code: %d (%.0f%%)",
      sum(in_dc), 100 * mean(in_dc), sum(in_any), 100 * mean(in_any))
  say("  ICIS-Air Virginia sources that look like data centers: %d", nrow(D[STATE == "VA" & status != "closed" & !crypto]))
  fwrite(V[!in_any, .(registration, site, locality, street, zip)], file.path(OUT, "dc_national_va_not_in_icis.csv"))
}
say("")
say("Next states to read permits for: %s", paste(head(S$STATE, 10), collapse = ", "))
writeLines(lines, file.path(OUT, "dc_national_summary.txt"))

# ---- 4. map -----------------------------------------------------------------------------------------------
pts <- cur[is.finite(lat) & is.finite(lon) & lon > -125 & lon < -66 & lat > 24 & lat < 50]
if (nrow(pts) && file.exists("data/app_data.rds")) {
  cty <- readRDS("data/app_data.rds")$counties
  st <- suppressWarnings(st_union(st_geometry(cty), by_feature = FALSE))
  png(file.path(OUT, "dc_national_map.png"), width = 2400, height = 1450, res = 200)
  par(mar = c(1, 1, 3, 1))
  plot(st_geometry(cty), border = "grey88", col = "grey98", lwd = 0.2,
       main = sprintf("Data centers with Clean Air Act records in EPA ICIS-Air (%d sites with coordinates)", nrow(pts)))
  plot(st, border = "grey45", add = TRUE, lwd = 0.6)
  col <- c(major = "#a8202f", "synthetic minor" = "#e08214", minor = "#2166ac", "other or unknown" = "#999999")
  points(pts$lon, pts$lat, pch = 21, bg = col[pts$class], col = "white", lwd = 0.4, cex = 0.9)
  legend("bottomleft", pt.bg = col, pch = 21, col = "white", legend = names(col), bty = "n", cex = 0.8, title = "Pollutant class")
  dev.off()
}
cat("wrote", file.path(OUT, c("dc_national_sites.csv", "dc_national_by_state.csv", "dc_national_summary.txt", "dc_national_map.png")), sep = "\n")
