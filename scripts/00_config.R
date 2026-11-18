# =============================================================================
# 00_config.R  --  paths, constants and helpers shared by the build scripts
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(sf) })
options(stringsAsFactors = FALSE, scipen = 999, datatable.integer64 = "double")

ROOT  <- Sys.getenv("USLOAD_ROOT", unset = getwd())
RAW   <- Sys.getenv("USLOAD_RAW",   unset = file.path(ROOT, "data-raw"))
BUILD <- Sys.getenv("USLOAD_BUILD", unset = file.path(ROOT, "build"))
DATA  <- Sys.getenv("USLOAD_DATA",  unset = file.path(ROOT, "data"))
for (d in c(BUILD, DATA)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

msg    <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), sprintf(...)))
assert <- function(ok, ...) if (!isTRUE(ok)) stop(sprintf(...), call. = FALSE)

# The InMAP grid's own projection: Lambert conformal conic on a sphere.
LCC <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370997 +b=6370997 +units=m +no_defs"

SHORT_TON <- 0.90718474          # metric tonnes per short ton
L_PER_GAL <- 3.785411784

# eGRID subregions inside the ISRM domain (contiguous US). AK, HI and PR are out.
NON_CONUS <- c("AKGD", "AKMS", "HIMS", "HIOA", "PRMS")

# Assumed effective release height (stack + plume rise, metres) by eGRID fuel
# category. eGRID carries no stack parameters; script 03 interpolates between
# the ISRM emission layers (about 29 m, 322 m and 917 m mid-height) from these.
# Override with e.g. EFF_HEIGHT_COAL=300.
EFF_HEIGHT <- c(COAL = 400, OIL = 200, GAS = 150, BIOMASS = 100, OFSL = 100, OTHF = 100)
for (k in names(EFF_HEIGHT)) {
  v <- Sys.getenv(paste0("EFF_HEIGHT_", k), unset = NA)
  if (!is.na(v)) EFF_HEIGHT[k] <- as.numeric(v)
}

# Fallback primary PM2.5 emission factors, lb per MMBtu heat input (filterable
# + condensable), used only for plants the eGRID PM2.5 file does not cover.
# Gas: AP-42 Table 1.4-2 (7.6 lb/1e6 scf / 1020 MMBtu). The others are typical
# controlled-unit values and are PLACEHOLDERS: check them against the eGRID PM file.
PM25_EF_LB_MMBTU <- c(COAL = 0.030, OIL = 0.040, GAS = 0.0075, BIOMASS = 0.050, OFSL = 0.020, OTHF = 0.020)

# Fallback operational water CONSUMPTION, litres per MWh net generation, by
# eGRID fuel category (rounded fleet medians after Macknick et al. 2012,
# Environ. Res. Lett. 7 045802). Used only where EIA plant data are missing.
# Hydropower reservoir evaporation is deliberately excluded.
WATER_CONS_L_MWH <- c(COAL = 1900, GAS = 750, OIL = 1100, NUCLEAR = 2500, BIOMASS = 2100,
                      GEOTHERMAL = 1000, OFSL = 1000, OTHF = 1000, SOLAR = 100, WIND = 0, HYDRO = 0)

MIXES <- c("avg", "nb")   # all-generation average; eGRID non-baseload (marginal proxy)

# Single-fuel mixes, used by the app's "set the mix yourself" option: every plant
# of one eGRID primary-fuel category in a subregion, weighted by net generation.
FUEL_GROUPS <- list(coal = "COAL", gas = "GAS", oil = "OIL", biomass = "BIOMASS", otherfossil = c("OFSL", "OTHF"),
                    nuclear = "NUCLEAR", hydro = "HYDRO", wind = "WIND", solar = "SOLAR", geothermal = "GEOTHERMAL")

# eGRID reports grid gross loss by interconnection.
INTERCONNECT <- function(subrgn) ifelse(subrgn == "ERCT", "ERCOT", ifelse(subrgn %in% c("AZNM", "CAMX", "NWPP", "RMPA"), "Western", "Eastern"))
