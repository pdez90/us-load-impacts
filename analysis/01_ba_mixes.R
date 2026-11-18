# =============================================================================
# analysis/01_ba_mixes.R  --  generation-weighted plant mixes by balancing
#                             authority, for the Harvard hyperscale reproduction
# -----------------------------------------------------------------------------
# Guidi, Dominici et al. attribute each data center's electricity to every plant
# in its balancing authority (BA) in proportion to eGRID2023 net generation.
# This builds the same mixes from the plant table of script 02, for the BAs in
# their public file, and checks the CO2 rates against eGRID's own BA sheet.
#
# In : build/plants.csv, data-raw/egrid2023_data.xlsx, data-raw/harvard/data_BA_level.csv
# Out: build/plants_ba.csv  (same layout as plants.csv; 'subrgn' holds the BA code)
#      build/ba_mix.csv     rates per MWh by BA
# =============================================================================
source(file.path(if (nzchar(Sys.getenv("USLOAD_ROOT"))) Sys.getenv("USLOAD_ROOT") else ".", "scripts/00_config.R"))
suppressPackageStartupMessages(library(readxl))
num <- function(x) suppressWarnings(as.numeric(x))
read_sheet <- function(prefix, key) {
  f <- file.path(RAW, "egrid2023_data.xlsx"); sh <- grep(paste0("^", prefix), excel_sheets(f), value = TRUE)[1]
  top <- suppressMessages(read_excel(f, sheet = sh, n_max = 6, col_names = FALSE, .name_repair = "minimal"))
  hdr <- which(apply(top, 1, function(r) key %in% toupper(trimws(as.character(r)))))[1]
  d <- as.data.table(suppressMessages(read_excel(f, sheet = sh, skip = hdr - 1, guess_max = 50000))); setnames(d, toupper(trimws(names(d)))); d
}
hv <- fread(file.path(RAW, "harvard", "data_BA_level.csv"))
pl <- fread(file.path(BUILD, "plants.csv"))
eg <- read_sheet("PLNT", "ORISPL")[, .(orispl = num(ORISPL), bacode = trimws(BACODE), baname = BANAME)]
pl <- merge(pl[, !grep("^k_", names(pl), value = TRUE), with = FALSE], unique(eg, by = "orispl"), by = "orispl", all.x = TRUE)
assert(all(hv$region_B_1 %in% pl$bacode), "BAs missing from eGRID: %s", paste(setdiff(hv$region_B_1, pl$bacode), collapse = ", "))
pl <- pl[bacode %in% hv$region_B_1]
pl[, egrid_subrgn := subrgn][, subrgn := bacode]
pl[, k_avg := 1 / sum(gen_mwh), by = subrgn]                 # every plant in the BA, by net generation
mixb <- pl[, .(n_plants = .N, gen_twh = sum(gen_mwh) / 1e6, co2_kg_mwh = sum(co2_t * k_avg) * 1e3, nox_kg_mwh = sum(nox_t * k_avg) * 1e3,
               so2_kg_mwh = sum(so2_t * k_avg) * 1e3, pm25_kg_mwh = sum(pm25_t * k_avg) * 1e3,
               wcons_l_mwh = sum(wcons_l * k_avg), wwith_l_mwh = sum(wwith_l * k_avg, na.rm = TRUE)), by = .(ba = subrgn)]
ba <- read_sheet("BA", "BACODE")[, .(ba = trimws(BACODE), baname = BANAME, egrid_gen_twh = num(BANGENAN) / 1e6, egrid_co2_g_kwh = num(BACO2RTA) * 0.45359237)]
mixb <- merge(mixb, ba, by = "ba", all.x = TRUE)
mixb[, diff_pct := 100 * (co2_kg_mwh / egrid_co2_g_kwh - 1)]
msg("%d BAs, %d plants. CO2 rate vs eGRID BA sheet: max |diff| %.2f%%; generation max |diff| %.2f%%", nrow(mixb), nrow(pl),
    max(abs(mixb$diff_pct), na.rm = TRUE), max(abs(100 * (mixb$gen_twh / mixb$egrid_gen_twh - 1)), na.rm = TRUE))
fwrite(pl, file.path(BUILD, "plants_ba.csv")); fwrite(mixb[order(-gen_twh)], file.path(BUILD, "ba_mix.csv"))
