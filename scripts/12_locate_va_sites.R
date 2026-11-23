# =============================================================================
# 12_locate_va_sites.R  --  find coordinates for Virginia permits the Census geocoder could not place
# -----------------------------------------------------------------------------
# Run from the repo root on a machine with internet access:  Rscript scripts/12_locate_va_sites.R
# In : data-raw/permits/va/va_permits.csv, va_permits_geocoded.csv      (scripts/08, 09)
#      data-raw/echo/ICIS-AIR_downloads.zip, echo_exporter.zip          (scripts/10; optional)
#      data-raw/permits/va/hand_locations.csv        (optional: registration, lon, lat, address, note; coordinates or a
#                                                     corrected street address, with a note saying where it came from)
# Out: data-raw/permits/va/va_located.csv   registration, lon, lat, source, detail  (read by analysis/05)
#
# Many data center streets are new (Gigabit Plaza, Light Speed Plaza) and missing from the Census address file.
# For every current permit without a Census match (or whose listed address is the owner's office), in this order:
#   1. coordinates written in the permit itself
#   2. located by hand (hand_locations.csv, each with a note saying where the point came from)
#   3. EPA's facility record (ECHO Exporter) for the same street address
#   4. Loudoun County's address points (the county's public map service), for permits in Loudoun
#   5. OpenStreetMap's geocoder (Nominatim) for the site address; an answer without the house number is the street,
#      not the building, and is labelled "OpenStreetMap street (building not found)"
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
# 2. by hand: a row gives either coordinates, or a corrected street address that steps 4 and 5 then look up
#    (used where the permit gives no site address or a wrong one). Hand rows replace anything found automatically.
HF <- file.path(DIR, "hand_locations.csv"); by_hand <- rep(FALSE, nrow(res)); hand_note <- rep("", nrow(res))
if (file.exists(HF)) {
  H <- fread(HF, colClasses = list(character = c("registration", "address", "note")))
  for (k in seq_len(nrow(H))) {
    i <- match(H$registration[k], res$registration)
    if (is.na(i)) {                                                  # a permit the Census geocoder placed: add it to the list
      j <- match(H$registration[k], P$registration); if (is.na(j)) next
      need <- rbind(need, P[j][, cand := list(list(character()))], fill = TRUE)
      res <- rbind(res, data.table(registration = H$registration[k], lon = NA_real_, lat = NA_real_, source = NA_character_, detail = NA_character_))
      by_hand <- c(by_hand, FALSE); hand_note <- c(hand_note, ""); i <- nrow(res)
    }
    by_hand[i] <- TRUE; hand_note[i] <- H$note[k]
    set(res, i, c("lon", "lat", "source", "detail"), list(NA_real_, NA_real_, NA_character_, NA_character_))
    if (is.finite(H$lon[k]) && is.finite(H$lat[k])) set_loc(i, H$lon[k], H$lat[k], "located by hand", H$note[k])
    else if (!is.na(H$address[k]) && nzchar(H$address[k])) need$cand[[i]] <- H$address[k]
  }
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
  for (i in which(is.na(res$lon) & !by_hand)) for (a in need$cand[[i]]) {
    j <- match(key(a), Fa$k); if (!is.na(j)) { set_loc(i, Fa$lon[j], Fa$lat[j], "EPA facility record", paste0(Fa$FACILITY_NAME[j], ", ", Fa$STREET_ADDRESS[j])); break }
  }
} else cat("EPA files not found in", ECHO, "- step 3 skipped (run scripts/10_download_echo.R)\n")
# 4. Loudoun County's own address points (its public map service), for permits DEQ lists in Loudoun. One request per house
#    number, 1.5 s apart, cached; on a refusal this step stops without retrying and the script carries on.
LCACHE <- file.path(DIR, "loudoun_address_cache.csv")
LURL <- "https://logis.loudoun.gov/gis/rest/services/COL/LandRecords/MapServer/2/query"
lcache <- if (file.exists(LCACHE)) fread(LCACHE, colClasses = list(character = c("house", "street", "full"))) else data.table(house = character(), street = character(), full = character(), lon = numeric(), lat = numeric())
loudoun_ok <- TRUE
loudoun_parse <- function(js, h) {
  a <- js$features$attributes
  if (is.null(a) || !NROW(a)) return(data.table(house = h, street = "", full = "", lon = NA_real_, lat = NA_real_))
  data.table(house = h, street = toupper(trimws(a$AM_STR_NAME)), full = trimws(a$FULL_ADDRESS), lon = as.numeric(a$AM_POINT_X_DD), lat = as.numeric(a$AM_POINT_Y_DD))
}
loudoun <- function(a) {                                             # mean of the county's points for this number and street, or NULL
  h <- house(a); w <- word(a)
  if (!h %in% lcache$house) {
    if (!loudoun_ok) return(NULL)
    Sys.sleep(1.5); tf <- tempfile(); on.exit(unlink(tf))
    u <- paste0(LURL, "?where=AD_ADDRESS%3D", h, "&outFields=AD_ADDRESS,AM_STR_NAME,FULL_ADDRESS,AM_POINT_X_DD,AM_POINT_Y_DD&returnGeometry=false&f=json")
    ok <- tryCatch(utils::download.file(u, tf, quiet = TRUE, method = "libcurl", headers = c("User-Agent" = UA)) == 0, error = function(e) { message("  ", conditionMessage(e)); FALSE })
    js <- if (ok) tryCatch(jsonlite::fromJSON(tf), error = function(e) NULL) else NULL
    if (is.null(js) || !is.null(js$error)) { loudoun_ok <<- FALSE; cat("Loudoun County's map service refused or could not be read; step 4 stopped\n"); return(NULL) }
    lcache <<- rbind(lcache, loudoun_parse(js, h)); fwrite(lcache, LCACHE)
  }
  m <- lcache[house == h & nzchar(street) & (street == w | startsWith(street, paste0(w, " ")))]
  if (!nrow(m) || length(unique(m$street)) > 1) return(NULL)
  list(lon = mean(m$lon), lat = mean(m$lat), full = m$full[1])
}
if (!nzchar(Sys.getenv("NO_LOUDOUN")) && requireNamespace("jsonlite", quietly = TRUE))
  for (i in which(is.na(res$lon) & grepl("Loudoun", need$locality))) for (a in need$cand[[i]]) {
    r <- loudoun(a); if (!is.null(r)) { set_loc(i, r$lon, r$lat, "Loudoun County address point", r$full); break }
  }
# 5. OpenStreetMap geocoder. A result without the house number is the street, not the building, and is labelled so.
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
if (!nzchar(Sys.getenv("NO_NOMINATIM"))) for (i in which(is.na(res$lon))) {
  county <- sub("\\s+Co\\.$", " County", need$locality[i]); street_only <- NULL
  for (a in need$cand[[i]]) {
    r <- nominatim(paste0(a, ", ", county, ", Virginia")); if (!is.finite(r$lon)) next
    if (grepl(paste0("(^|, )", house(a), ","), r$display)) { set_loc(i, r$lon, r$lat, "OpenStreetMap address", r$display); break }
    if (is.null(street_only)) street_only <- r
  }
  if (is.na(res$lon[i]) && !is.null(street_only)) set_loc(i, street_only$lon, street_only$lat, "OpenStreetMap street (building not found)", street_only$display)
}

# hand rows found by address: recorded as located by hand, with the note and what the address matched
k <- which(by_hand & !is.na(res$lon) & res$source != "located by hand")
if (length(k)) { res[k, detail := paste0(hand_note[k], " [", source, ": ", detail, "]")]; res[k, source := "located by hand"] }
fwrite(res[!is.na(lon)], OUTF)
cat(sprintf("located %d of %d: %s\n", sum(!is.na(res$lon)), nrow(res), paste(res[!is.na(lon), .N, by = source][, sprintf("%s %d", source, N)], collapse = "; ")))
left <- cbind(need[is.na(res$lon), .(registration, site, locality)], tried = vapply(need$cand[is.na(res$lon)], paste, "", collapse = " | "))
if (nrow(left)) { cat("still not located (add to", HF, "with columns registration,lon,lat,address,note):\n"); print(left, nrows = 100) }
