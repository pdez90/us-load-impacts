# =============================================================================
# 09_chen2026_comparison.R  --  our tool on the same demand as Chen et al. (2026), state by state
# -----------------------------------------------------------------------------
# Chen Y, Wang S, Senga JRL, White AS, Wong AYH, Freese LM, Bell ML, Selin NE (2026). Spatially distributed air
# quality and health impacts of data center electricity demand in the United States. Environ. Res. Lett. 21 214046.
# https://doi.org/10.1088/1748-9326/aebbd4  (CC BY 4.0)
#
# In : data-raw/chen2026_tableS1.csv   their Supporting Information Table S1, typed in: data center load by state
#                                      (TWh, 2023, from EPRI) and data center-attributable deaths by state where the
#                                      deaths occur (PM2.5 + ozone, GEMM and Turner CRFs; 648 in all)
#      data/app_data.rds
# Out: analysis/output/chen2026_comparison_states.csv, chen2026_comparison_summary.txt, chen2026_comparison.png
#      data/chen2026.rds   what the app's "Chen et al. (2026) comparison" tab shows
#
# Same demand, different chain. Each state's load is split over the eGRID subregions its counties fall in, by
# population (data centers are not spread by population, but within a state this mostly picks one subregion),
# grossed up so total generation matches their 171.0 TWh, and run through the app's grid calculation with the
# average and the non-baseload mix. Deaths are summed by the state where they occur, as in their Table S1.
# Differences come from: which plants respond (their hourly unit-level dispatch across 64 regions vs our eGRID
# subregion mix), the air model (GEOS-Chem with 2023 chemistry vs InMAP ISRM), pollutants (PM2.5 + ozone vs
# PM2.5) and the CRF (GEMM vs Krewski; the app's linearised GEMM and Lepeule shown too).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
source("R/calc.R")
OUT <- "analysis/output"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
lines <- character(); say <- function(...) { s <- sprintf(...); lines <<- c(lines, s); cat(s, "\n") }

app <- readRDS("data/app_data.rds")
C <- fread("data-raw/chen2026_tableS1.csv")
CH_GEN <- 171.0; CH_PM <- 513; CH_O3 <- 137                       # paper: generation (TWh), PM2.5 and ozone deaths
loss <- 1 - sum(C$dc_twh) / CH_GEN                                # gross-up so our generation equals theirs

cty <- as.data.table(sf::st_drop_geometry(app$counties))[, .(fips, state, subrgn, pop)]
st_abb <- c(setNames(state.abb, state.name), "District of Columbia" = "DC")
C[, abb := st_abb[state]]; stopifnot(!anyNA(C$abb))

# each state's load, split over its subregions by population; one representative county per state x subregion
split <- cty[state %in% C$abb, .(pop = sum(pop), fips = fips[which.max(pop)]), by = .(state, subrgn)]
split[, share := pop / sum(pop), by = state]
split[C, on = c(state = "abb"), twh := i.dc_twh * share]
split <- split[twh > 0]

run <- function(scenario, beta) {
  d <- numeric(nrow(cty)); tot <- 0; nox <- 0; so2 <- 0
  for (k in seq_len(nrow(split))) {
    r <- impact_supply(app, split$fips[k], split$twh[k], scenario = scenario, subrgn = split$subrgn[k], loss = loss, beta = beta)
    d <- d + r$county$deaths; tot <- tot + r$totals$gen_twh; nox <- nox + r$totals$nox_t; so2 <- so2 + r$totals$so2_t
  }
  list(by_state = cty[, .(deaths = sum(d[.I])), by = state], gen = tot, nox = nox, so2 = so2, deaths = sum(d))
}
B_K <- CRF$krewski$beta; B_L <- CRF$lepeule$beta
B_G <- CRF$gemm$beta
res <- list(avg_krewski = run("avg", B_K), nb_krewski = run("nb", B_K), nb_gemm = run("nb", B_G), nb_lepeule = run("nb", B_L))

S <- C[, .(state = abb, name = state, dc_twh, chen_deaths = deaths, chen_pm_est = deaths * CH_PM / (CH_PM + CH_O3))]
for (nm in names(res)) S[res[[nm]]$by_state, on = "state", (nm) := i.deaths]
for (nm in names(res)) set(S, which(is.na(S[[nm]])), nm, 0)
fwrite(S[order(-chen_deaths)], file.path(OUT, "chen2026_comparison_states.csv"))

say("Our tool on Chen et al. (2026)'s demand (analysis/09_chen2026_comparison.R)")
say("Demand: %.1f TWh in %d states (their Table S1); generation %.1f TWh (theirs %.1f) with %.1f%% losses and imports",
    sum(C$dc_twh), sum(C$dc_twh > 0), res$nb_krewski$gen, CH_GEN, 100 * loss)
say("")
say("EMISSIONS (kt)                        NOx      SO2")
say("  Chen et al. (dispatch)              61.4     39.1")
say("  ours, average mix                   %5.1f    %5.1f", res$avg_krewski$nox / 1e3, res$avg_krewski$so2 / 1e3)
say("  ours, non-baseload mix              %5.1f    %5.1f", res$nb_krewski$nox / 1e3, res$nb_krewski$so2 / 1e3)
say("")
say("DEATHS PER YEAR")
say("  Chen et al.: PM2.5 %d (GEMM) + ozone %d = %d; PM2.5 with the Medicare CRF (Hao et al.) 565", CH_PM, CH_O3, CH_PM + CH_O3)
say("  ours, average mix, Krewski           %4.0f", res$avg_krewski$deaths)
say("  ours, non-baseload mix, Krewski      %4.0f", res$nb_krewski$deaths)
say("  ours, non-baseload mix, GEMM slope   %4.0f   (as their CRF; linearised at 8 ug/m3)", res$nb_gemm$deaths)
say("  ours, non-baseload mix, Lepeule      %4.0f", res$nb_lepeule$deaths)
say("  ratio, their PM2.5 to ours (non-baseload): %.1f with Krewski, %.1f with the GEMM slope", CH_PM / res$nb_krewski$deaths, CH_PM / res$nb_gemm$deaths)
say("")
sh <- function(x) x / sum(x)
S[, `:=`(chen_share = sh(chen_deaths), our_share = sh(nb_krewski))]
say("WHERE THE DEATHS OCCUR (share of each total; their column includes ozone)")
say("  correlation of state shares, Chen vs ours (non-baseload): %.2f (Spearman %.2f)",
    cor(S$chen_share, S$our_share), cor(S$chen_share, S$our_share, method = "spearman"))
say("  %-16s %6s %8s %8s", "state", "TWh", "Chen %", "ours %")
for (i in head(order(-S$chen_deaths), 15)) say("  %-16s %6.1f %8.1f %8.1f", S$name[i], S$dc_twh[i], 100 * S$chen_share[i], 100 * S$our_share[i])
mw <- c("IA", "NE", "KS", "MO", "MN", "SD", "ND", "WI")
say("  Midwest (%s): Chen %.0f%%, ours %.0f%%", paste(mw, collapse = " "), 100 * sum(S[state %in% mw]$chen_share), 100 * sum(S[state %in% mw]$our_share))
say("  Virginia: %.1f TWh (22%% of the load); deaths in Virginia Chen %.0f%%, ours %.0f%%", S[state == "VA"]$dc_twh,
    100 * S[state == "VA"]$chen_share, 100 * S[state == "VA"]$our_share)
say("  Deaths in states with under 0.5 TWh of data center load: Chen %.0f%%, ours %.0f%%",
    100 * sum(S[dc_twh < 0.5]$chen_share), 100 * sum(S[dc_twh < 0.5]$our_share))
writeLines(lines, file.path(OUT, "chen2026_comparison_summary.txt"))

# For the app: deaths are linear in the CRF slope, so the tab rescales the Krewski runs for the other CRFs.
saveRDS(list(states = as.data.frame(S[, .(state, name, dc_twh, chen_deaths, chen_deaths_lo = C$deaths_lo[match(name, C$state)],
                                          chen_deaths_hi = C$deaths_hi[match(name, C$state)], avg = avg_krewski, nb = nb_krewski)]),
             beta_krewski = B_K, loss = loss, gen = c(avg = res$avg_krewski$gen, nb = res$nb_krewski$gen),
             emis = data.frame(run = c("chen", "avg", "nb"), nox_kt = c(61.4, res$avg_krewski$nox / 1e3, res$nb_krewski$nox / 1e3),
                               so2_kt = c(39.1, res$avg_krewski$so2 / 1e3, res$nb_krewski$so2 / 1e3)),
             chen = list(gen_twh = CH_GEN, pm = CH_PM, o3 = CH_O3, pm_hao = 565, pm_ci = c(469, 557), o3_ci = c(90, 183)),
             built = format(Sys.Date())), "data/chen2026.rds")

png(file.path(OUT, "chen2026_comparison.png"), width = 1500, height = 1400, res = 200)
par(mar = c(4.5, 4.5, 3, 1))
lim <- c(0, max(100 * c(S$chen_share, S$our_share)) * 1.05)
plot(100 * S$chen_share, 100 * S$our_share, pch = 21, bg = "#2166ac", col = "white", cex = 0.6 + sqrt(S$dc_twh) / 2, xlim = lim, ylim = lim,
     xlab = "Chen et al. 2026: share of deaths (%)", ylab = "This tool, non-baseload mix: share of deaths (%)",
     main = "Where data center grid deaths occur, by state\n(same demand; circle size = state's data center load)", cex.main = 0.95, las = 1)
abline(0, 1, lty = 2, col = "grey50")
big <- S$chen_share > 0.03 | S$our_share > 0.03
text(100 * S$chen_share[big], 100 * S$our_share[big], S$state[big], pos = 4, cex = 0.7)
dev.off()
cat("wrote", file.path(OUT, c("chen2026_comparison_states.csv", "chen2026_comparison_summary.txt", "chen2026_comparison.png")), sep = "\n")
