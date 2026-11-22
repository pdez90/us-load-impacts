# =============================================================================
# 12_locate_va_sites.R  --  find coordinates for Virginia permits the Census geocoder could not place
# -----------------------------------------------------------------------------
# Run from the repo root on a machine with internet access:  Rscript scripts/12_locate_va_sites.R
# In : data-raw/permits/va/va_permits.csv, va_permits_geocoded.csv      (scripts/08, 09)
#      data-raw/echo/ICIS-AIR_downloads.zip, echo_exporter.zip          (scripts/10; optional)
#      data-raw/permits/va/hand_locations.csv                           (optional: registration, lon, lat, note)
# Out: data-raw/permits/va/va_located.csv   registration, lon, lat, source, detail  (read by analysis/05)
#
# Many data center streets are new (Gigabit Plaza, Light Speed Plaza) and missing from the Census address file.
# For every current permit without a Census match (or whose listed address is the owner's office), in this order:
#   1. coordinates written in the permit itself
#   2. located by hand (hand_locations.csv, each with a note saying where the point came from)
#   3. EPA's facility record (ECHO Exporter) for the same street address
#   4. OpenStreetMap's geocoder (Nominatim) for the site address
# Site addresses are taken from the permit's full address text, where some permits list each building, before
# its mailing street. Nominatim's usage policy allows small scripted use that identifies itself and sends at most
# one request a second (https://operations.osmfoundation.org/policies/nominatim/); this sends one every 1.5 s,
# caches every answer, and stops if it is refused. analysis/05 keeps a point only if it lies in or within 10 km of
# the county DEQ lists (hand-located points are kept as given).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
DIR <- "data-raw/permits/va"; OUTF <- file.path(DIR, "va_located.csv"); CACHE <- file.path(DIR, "nominatim_cache.csv")
UA <- "us-load-impacts research script (https://github.com/pdez90/us-load-impacts)"
OFFICE <- "13820 SUNRISE VALLEY|13200 WOODLAND PARK|410 TERRY AVE"

P <- fread(file.path(DIR, "va_permits.csv"), colClasses = "character")[current == "yes"]
G <- fread(file.path(DIR, "va_permits_geocoded.csv"), colClasses = "character")
P[G, on = "registration", `:=`(cmatch = i.match, cadr = i.matched_address)]
need <- P[is.na(cmatch) | cmatch != "Match" | grepl(OFFICE, toupper(fcoalesce(cadr, "")))]
cat(nrow(P), "current permits;", nrow(need), "without a usable Census match\n")

# street addresses in a text: "43736 Clubber Lane", "8180, 8190 ... Bethlehem Road" (last number before the name)
SUFFIX <- "Road|Rd|Drive|Dr|Lane|Ln|Plaza|Parkway|Pkwy|Boulevard|Blvd|Way|Circle|Cir|Court|Ct|Street|St|Avenue|Ave|Place|Pl|Terrace|Highway|Hwy|Run|Square|Trail|Pike"
addresses <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(character())
  m <- regmatches(txt, gregexpr(sprintf("[0-9]{2,5}\\s+([A-Z][A-Za-z'.]*\\s+){1,5}(%s)\\b\\.?", SUFFIX), txt, perl = TRUE))[[1]]
  m <- trimws(sub("\\.$", "", m)); unique(m[!grepl(OFFICE, toupper(gsub("\\.", "", m)))])
}
need[, cand := lapply(seq_len(.N), function(i) unique(c(addresses(address[i]), addresses(street[i]))))]
res <- data.table(registration = need$registration, lon = NA_real_, lat = NA_real_, source = NA_character_, detail = NA_character_)
set_loc <- function(i, lon, lat, source, detail) if (is.na(res$lon[i]) && is.finite(lon) && is.finite(lat)) set(res, i, c("lon", "lat", "source", "detail"), list(lon, lat, source, detail))

# 1. coordinates in the permit text
for (i in seq_len(nrow(need))) {
  m <- regmatches(need$address[i], regexpr("(3[6-9]\\.[0-9]{3,})\\s*,\\s*(-7[5-9]|-8[0-3])\\.[0-9]{3,}", need$address[i]))
  if (length(m)) { v <- as.numeric(strsplit(m, ",")[[1]]); set_loc(i, v[2], v[1], "coordinates in the permit", m) }
}
# 2. by hand
HF <- file.path(DIR, "hand_locations.csv")
if (file.exists(HF)) {
  H <- fread(HF, colClasses = list(character = "registration"))
  for (k in seq_len(nrow(H))) { i <- match(H$registration[k], res$registration); if (!is.na(i)) set_loc(i, H$lon[k], H$lat[k], "located by hand", H$note[k]) }
}
# 3. EPA facility record with the same house number and street name
house <- function(s) sub("^\\s*([0-9]+).*$", "\\1", toupper(s))
word <- function(s) sub("^\\s*[0-9]+[A-Z]?\\s+(([NSEW]\\.?|NORTH|SOUTH|EAST|WEST)\\s+)?([A-Z0-9]+).*$", "\\3", toupper(s))
key <- function(s) paste(house(s), word(s))
read_zip_csv <- function(zip, pattern, select) {
  nm <- grep(pattern, unzip(zip, list = TRUE)$Name, value = TRUE, ignore.case = TRUE)[1]
  td <- tempfile(); dir.create(td); on.exit(unlink(td, recursive = TRUE)); unzip(zip, nm, exdir = td, junkpaths = TRUE)
  f <- file.path(td, basename(nm)); fread(f, select = intersect(select, names(fread(f, nrows = 0))), colClasses = "character", encoding = "Latin-1", showProgress = FALSE)
}
ECHO <- Sys.getenv("ECHO_DIR", "data-raw/echo")
if (all(file.exists(file.path(ECHO, c("ICIS-AIR_downloads.zip", "echo_exporter.zip"))))) {
  Fa <- read_zip_csv(file.path(ECHO, "ICIS-AIR_downloads.zip"), "FACILITIES", c("REGISTRY_ID", "FACILITY_NAME", "STREET_ADDRESS", "STATE"))[STATE == "VA"]
  X <- read_zip_csv(file.path(ECHO, "echo_exporter.zip"), "ECHO_EXPORTER.*\\.csv", c("REGISTRY_ID", "FAC_LAT", "FAC_LONG"))
  Fa[unique(X[REGISTRY_ID %in% Fa$REGISTRY_ID], by = "REGISTRY_ID"), on = "REGISTRY_ID", `:=`(lat = as.numeric(i.FAC_LAT), lon = as.numeric(i.FAC_LONG))]
  Fa <- unique(Fa[is.finite(lat)][, k := key(STREET_ADDRESS)], by = "k")
  for (i in which(is.na(res$lon))) for (a in need$cand[[i]]) {
    j <- match(key(a), Fa$k); if (!is.na(j)) { set_loc(i, Fa$lon[j], Fa$lat[j], "EPA facility record", paste0(Fa$FACILITY_NAME[j], ", ", Fa$STREET_ADDRESS[j])); break }
  }
} else cat("EPA files not found in", ECHO, "- step 3 skipped (run scripts/10_download_echo.R)\n")
# 4. OpenStreetMap geocoder
cache <- if (file.exists(CACHE)) fread(CACHE, colClasses = list(character = c("query", "display"))) else data.table(query = character(), lon = numeric(), lat = numeric(), display = character())
nominatim <- function(q) {
  hit <- cache[query == q]; if (nrow(hit)) return(hit[1])
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("install.packages('jsonlite') first")
  Sys.sleep(1.5)
  u <- paste0("https://nominatim.openstreetmap.org/search?format=jsonv2&limit=1&countrycodes=us&q=", utils::URLencode(q, reserved = TRUE))
  tf <- tempfile(); on.exit(unlink(tf))
  ok <- tryCatch(utils::download.file(u, tf, quiet = TRUE, method = "libcurl", headers = c("User-Agent" = UA)) == 0, error = function(e) { message("  ", conditionMessage(e)); FALSE })
  if (!ok) stop("OpenStreetMap's geocoder refused or could not be reached. Stopping without retrying; add the remaining sites to ", HF)
  r <- jsonlite::fromJSON(tf)
  out <- if (length(r) && NROW(r)) data.table(query = q, lon = as.numeric(r$lon[1]), lat = as.numeric(r$lat[1]), display = r$display_name[1]) else data.table(query = q, lon = NA_real_, lat = NA_real_, display = "")
  cache <<- rbind(cache, out); fwrite(cache, CACHE); out
}
if (!nzchar(Sys.getenv("NO_NOMINATIM"))) for (i in which(is.na(res$lon))) for (a in need$cand[[i]]) {
  county <- sub("\\s+Co\\.$", " County", need$locality[i])
  r <- nominatim(paste0(a, ", ", county, ", Virginia"))
  if (is.finite(r$lon)) { set_loc(i, r$lon, r$lat, "OpenStreetMap address", r$display); break }
}

fwrite(res[!is.na(lon)], OUTF)
cat(sprintf("located %d of %d: %s\n", sum(!is.na(res$lon)), nrow(res), paste(res[!is.na(lon), .N, by = source][, sprintf("%s %d", source, N)], collapse = "; ")))
left <- cbind(need[is.na(res$lon), .(registration, site, locality)], tried = vapply(need$cand[is.na(res$lon)], paste, "", collapse = " | "))
if (nrow(left)) { cat("still not located (add to", HF, "with columns registration,lon,lat,note):\n"); print(left, nrows = 100) }
