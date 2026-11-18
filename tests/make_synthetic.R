# Synthetic inputs for the pipeline test: fake counties, subregions and an
# eGRID-shaped workbook. Nothing here is real data.
source("scripts/00_config.R"); suppressPackageStartupMessages(library(writexl)); set.seed(1)
sq <- function(x0, y0, dx, dy) st_polygon(list(matrix(c(x0, y0, x0 + dx, y0, x0 + dx, y0 + dy, x0, y0 + dy, x0, y0), ncol = 2, byrow = TRUE)))
g <- expand.grid(i = 0:11, j = 0:7)
cty <- st_sf(STATEFP = sprintf("%02d", c(1, 4, 5)[1 + g$i %/% 4]), GEOID = sprintf("%02d%03d", c(1, 4, 5)[1 + g$i %/% 4], 1 + g$j * 12 + g$i),
             NAME = paste("County", seq_len(nrow(g))), STUSPS = c("AA", "BB", "CC")[1 + g$i %/% 4], STATE_NAME = c("Alpha", "Beta", "Gamma")[1 + g$i %/% 4],
             geometry = st_sfc(Map(function(i, j) sq(-2.4e6 + i * 4e5, -1.6e6 + j * 4e5, 4e5, 4e5), g$i, g$j), crs = LCC))
dir.create(file.path(RAW, "cb_2023_us_county_500k"), recursive = TRUE, showWarnings = FALSE)
st_write(st_transform(cty, 4269), file.path(RAW, "cb_2023_us_county_500k", "cb.shp"), delete_dsn = FALSE, quiet = TRUE)
regs <- c("CAMX", "RFCW", "SRSO")
sr <- st_sf(ZipSubregi = regs, geometry = st_sfc(lapply(0:2, function(b) sq(-2.4e6 + b * 1.6e6, -1.6e6, 1.6e6, 3.2e6)), crs = LCC))
dir.create(file.path(RAW, "egrid2023_subregions"), recursive = TRUE, showWarnings = FALSE)
st_write(st_transform(sr, 4326), file.path(RAW, "egrid2023_subregions", "sub.shp"), delete_dsn = FALSE, quiet = TRUE)
n <- 60
pt <- st_sfc(Map(function(x, y) st_point(c(x, y)), runif(n, -2.3e6, 2.3e6), runif(n, -1.5e6, 1.5e6)), crs = LCC)
ll <- st_coordinates(st_transform(pt, 4326)); x <- st_coordinates(pt)[, 1]
fuel <- sample(c("COAL", "GAS", "NUCLEAR", "WIND", "SOLAR", "HYDRO", "OIL", "BIOMASS"), n, TRUE, c(.2, .35, .08, .12, .1, .05, .05, .05))
comb <- fuel %in% c("COAL", "GAS", "OIL", "BIOMASS")
gen <- round(runif(n, 1e5, 8e6)); hi <- ifelse(comb, gen * runif(n, 7, 11), 0)
P <- data.frame(SEQPLT = 1:n, PSTATABB = "AA", PNAME = paste("Plant", 1:n), ORISPL = 1000 + 1:n,
                SUBRGN = regs[1 + pmin(2, floor((x + 2.4e6) / 1.6e6))], SRNAME = "x", LAT = ll[, 2], LON = ll[, 1],
                PLFUELCT = fuel, NBFACTOR = ifelse(comb, round(runif(n), 2), 0), PLHTIAN = hi, PLNGENAN = gen,
                PLNOXAN = ifelse(comb, hi * 1e-4, 0), PLSO2AN = ifelse(fuel == "COAL", hi * 3e-4, ifelse(comb, hi * 1e-6, 0)),
                PLCO2AN = ifelse(fuel == "COAL", hi * 0.103, ifelse(comb, hi * 0.059, 0)),
                PLGENACL = ifelse(fuel == "COAL", gen, 0), PLGENAGS = ifelse(fuel == "GAS", gen, 0))
P <- rbind(P, transform(P[1, ], ORISPL = 9999, SUBRGN = "AKGD", PNAME = "Alaska plant"))   # must be dropped
two_rows <- function(d) { h <- as.data.frame(t(names(d))); names(h) <- paste("Long name", seq_along(d)); d[] <- lapply(d, as.character); names(d) <- names(h); rbind(h, d) }
srl <- do.call(rbind, lapply(regs, function(r) { d <- P[P$SUBRGN == r, ]
  data.frame(SUBRGN = r, SRCO2RTA = sum(d$PLCO2AN) * 2000 / sum(d$PLNGENAN),
             SRNBCO2 = sum(d$PLCO2AN * d$NBFACTOR) * 2000 / sum(d$PLNGENAN * d$NBFACTOR)) }))
ggl <- data.frame(YEAR = 2023, REGION = c("Eastern", "Western", "ERCOT"), GGRSLOSS = c(0.042, 0.041, 0.042))
write_xlsx(list(Contents = data.frame(a = "synthetic"), PLNT23 = two_rows(P), SRL23 = two_rows(srl), GGL23 = two_rows(ggl)), file.path(RAW, "egrid2023_data.xlsx"))
cat("synthetic R inputs written\n")
