# =============================================================================
# US large-load impact explorer
# -----------------------------------------------------------------------------
# Enter the MW of a new electricity load and where it sits; get annual CO2, the
# downwind PM2.5 increment and attributable deaths (InMAP Source-Receptor
# Matrix), and water use on site and at the power plants.
#
# Reads data/app_data.rds, built by scripts/01-04 (see README.md).
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(bslib); library(leaflet); library(sf); library(DT) })
source("R/calc.R")
sf::sf_use_s2(FALSE)

DATA_FILE <- Sys.getenv("USLOAD_APP_DATA", unset = "data/app_data.rds")
app <- if (file.exists(DATA_FILE)) readRDS(DATA_FILE) else NULL
HV_FILE <- file.path(dirname(DATA_FILE), "harvard.rds")      # written by analysis/02_harvard_extension.R
hv <- if (file.exists(HV_FILE)) readRDS(HV_FILE) else NULL
CH_FILE <- file.path(dirname(DATA_FILE), "chen2026.rds")      # written by analysis/09_chen2026_comparison.R
ch <- if (file.exists(CH_FILE)) readRDS(CH_FILE) else NULL
OG_FILE <- file.path(dirname(DATA_FILE), "onsite_groups.rds")  # written by 04 after 03 --groups: who is exposed, for sources at the site
if (!is.null(app) && file.exists(OG_FILE)) app$og <- readRDS(OG_FILE)
VA_FILE <- file.path(dirname(DATA_FILE), "va_permits.rds")      # written by analysis/06_va_noise.R (after 05)
va <- if (file.exists(VA_FILE)) readRDS(VA_FILE) else NULL
REPO <- "https://github.com/pdez90/us-load-impacts/blob/main/"
MAX_CELLS <- 15000        # most grid cells drawn at once; beyond this the map stays on counties

no_data_ui <- page_fillable(
  title = "US large-load impact explorer",
  card(card_header("No data yet"),
       p("This app needs ", code("data/app_data.rds"), ", which is built once from public data."),
       p("Run the four steps in ", code("README.md"), " (", code("scripts/01_download.sh"), " to ",
         code("scripts/04_bake_app_data.R"), "), commit ", code("data/app_data.rds"), " and redeploy.")))

if (!is.null(app)) {
  cty <- app$counties
  if (is.null(cty$state_name)) cty$state_name <- cty$state
  states <- sort(unique(cty$state_name))
  counties_in <- function(st) { i <- which(cty$state_name == st); i <- i[order(cty$name[i])]; setNames(cty$fips[i], cty$name[i]) }
  default_fips <- if ("51107" %in% cty$fips) "51107" else cty$fips[1]          # Loudoun County, VA
  default_state <- cty$state_name[match(default_fips, cty$fips)]
  mixdf <- as.data.frame(app$mix)
  regions <- sort(unique(mixdf$subrgn))
  region_label <- function(r) {
    nm <- if ("srname" %in% names(mixdf)) mixdf$srname[match(r, mixdf$subrgn)] else NA
    ifelse(is.na(nm) | nm == r, r, paste0(r, " (", nm, ")"))
  }
  GROUP_LABELS <- c(Total = "Everyone", WhiteNoLat = "White, non-Latino", Black = "Black", Native = "Native American",
                    Asian = "Asian", Latino = "Latino")
  FUEL_COLS <- c(coal = "#4d4d4d", gas = "#e08214", oil = "#8c510a", nuclear = "#7b3294", hydro = "#2166ac",
                 biomass = "#66a61e", wind = "#80cdc1", solar = "#fdd835", geothermal = "#d6604d")
  PLANT_COLS <- c(COAL = "#4d4d4d", GAS = "#e08214", OIL = "#8c510a", NUCLEAR = "#7b3294", HYDRO = "#2166ac", BIOMASS = "#66a61e",
                  WIND = "#80cdc1", SOLAR = "#fdd835", GEOTHERMAL = "#d6604d", OFSL = "#999999", OTHF = "#999999")
  has_cells <- !is.null(app$cells); has_plants <- !is.null(app$plants)
  fuel_text <- function(m) {                                     # "67% gas, 29% coal, 3% oil"
    sh <- unlist(m[grep("^share_", names(m))]); names(sh) <- sub("share_", "", names(sh))
    sh <- sort(sh[is.finite(sh) & sh >= 0.02], decreasing = TRUE)
    if (!length(sh)) return("") else paste(sprintf("%.0f%% %s", 100 * sh, names(sh)), collapse = ", ")
  }
}

hv_ui <- function() {
  if (is.null(hv)) return(card(card_header("Not built yet"), p("Run the three commands under 'Reproducing and extending Guidi et al. (2026), the Harvard hyperscale paper' in README.md, then restart the app.")))
  tagList(
    div(style = "max-width: 1000px",
      h4("Guidi et al. (2026): replicating, then extending, their hyperscale data center study"),
      p("Guidi, Dominici and colleagues (Nature Sustainability, 2026) estimated the electricity use and CO2 emissions of 403 US hyperscale data centers",
        "operating between May 2024 and April 2025. This page first checks that this tool reproduces their CO2 result, and then adds two things their study does not contain:",
        "the fine-particle (PM2.5) pollution and deaths from the same electricity, and the water used by the power plants."),
      tags$table(class = "table table-sm", style = "max-width: 900px",
        tags$thead(tags$tr(tags$th("Step"), tags$th("Their study"), tags$th("This tool"))),
        tags$tbody(
          tags$tr(tags$td("1. Electricity each facility uses"), tags$td("Facility capacity x 8,760 h x a load coefficient (0.48 to 0.70; central 0.58)"), tags$td("Taken from their published totals by balancing authority")),
          tags$tr(tags$td("2. Which plants supply it"), tags$td("Every plant in the facility's balancing authority, in proportion to its generation (EPA eGRID2023)"), tags$td("Same rule, rebuilt from eGRID2023 plant records")),
          tags$tr(tags$td("3. CO2"), tags$td("Yes"), tags$td(strong("Reproduced"), " (compared below)")),
          tags$tr(class = "table-success", tags$td("4. SO2, NOx and PM2.5 from those plants"), tags$td("No"), tags$td(strong("Added here"))),
          tags$tr(class = "table-success", tags$td("5. PM2.5 downwind and who is exposed"), tags$td("No"), tags$td(HTML("<strong>Added here</strong>: InMAP Source-Receptor Matrix"))),
          tags$tr(class = "table-success", tags$td("6. Deaths attributable to that PM2.5"), tags$td("No"), tags$td(strong("Added here"))),
          tags$tr(class = "table-success", tags$td("7. Water used by the power plants"), tags$td("No"), tags$td(HTML("<strong>Added here</strong>: EIA plant cooling data")))))),
    layout_columns(fill = FALSE, col_widths = c(4, 8),
      selectInput("hv_u", "Their facility-load scenario", selected = "central",
                  choices = setNames(names(hv$u), sprintf("%s (u = %s)", c(low = "Low", central = "Central", continuity = "Intermediate-high", ai_high = "AI-weighted high")[names(hv$u)], hv$u))),
      div(class = "small text-muted pt-4", "Only the central scenario is public by balancing authority; the others are scaled in proportion, as in their own totals.")),
    h5("Reproduced"),
    layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
      value_box("Electricity (theirs)", textOutput("hv_twh"), "403 facilities in 23 balancing authorities", theme = "primary"),
      value_box("CO2, their result", textOutput("hv_co2_theirs"), "from their public repository", theme = "secondary"),
      value_box("CO2, this tool", textOutput("hv_co2_ours"), textOutput("hv_co2_diff"), theme = "secondary")),
    h5("Added here"),
    layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
      value_box("PM2.5 deaths per year", textOutput("hv_deaths"), textOutput("hv_deaths_sub"), theme = "danger"),
      value_box("Power-plant water consumed", textOutput("hv_water"), textOutput("hv_water_sub"), theme = "info"),
      value_box("Plant emissions per year", textOutput("hv_em"), "SO2, NOx and primary PM2.5 from the attributed generation", theme = "light")),
    layout_columns(col_widths = c(7, 5),
      card(card_header("PM2.5 from the fleet's electricity, by county (annual mean, ng/m3)"), leafletOutput("hv_map", height = 430)),
      card(card_header("Where the electricity is used, and where the deaths fall"), plotOutput("hv_state_plot", height = 430, fill = FALSE))),
    table_card("By balancing authority: their CO2, this tool's CO2, and the additions", DTOutput("hv_ba_tab")),
    layout_columns(col_widths = c(7, 5),
      table_card("By state", DTOutput("hv_state_tab")),
      card(card_header("Exposure by race and ethnicity"), plotOutput("hv_grp_plot", height = 260, fill = FALSE),
           helpText("Population-weighted PM2.5 from the fleet, relative to the average for everyone. Populations are the model's own, from about 2010."))),
    div(class = "small text-muted", style = "max-width: 1000px",
      p(strong("How to read this. "), "The additions keep their method exactly: average mix by balancing authority, and no adjustment for transmission losses (adding 4.2% would raise every figure by 4.4%).",
        "A 'plants that respond to new load' view would give higher numbers. Two hydro utility districts carry no emissions under this method.",
        "On-site cooling water is not included because facility-level data are not public. Deaths use the Krewski function; the Lepeule function gives about 2.2 times as many."),
      p(sprintf("Their totals: github.com/gianguidi/hyperscale-emissions, public/data_BA_level.csv (commit %s). Built %s. Full tables are in analysis/output/.",
                substr(as.character(hv$source), 1, 7), hv$built))))
}

# A card for a data table: sized to its content (not stretched to fill the page), so notes below the table
# never overlap its rows.
table_card <- function(title, ...) card(fill = FALSE, card_header(title), card_body(fillable = FALSE, ...))

ch_ui <- function() {
  if (is.null(ch)) return(card(card_header("Not built yet"), p("Run Rscript analysis/09_chen2026_comparison.R (see README.md), then restart the app.")))
  tagList(
    div(style = "max-width: 1000px",
      h4("Chen et al. (2026): the same demand, through this tool"),
      p("Chen, Selin and colleagues (Environmental Research Letters, 2026) estimated the air pollution and deaths caused by the electricity US data centers drew",
        "from the grid in 2023: 513 deaths a year from PM2.5 and 137 from ozone. They used an hourly model of which power plants run, a full atmospheric chemistry",
        "model and the GEMM health response. This page runs their state-by-state demand through this tool and compares the results step by step."),
      tags$table(class = "table table-sm", style = "max-width: 900px",
        tags$thead(tags$tr(tags$th("Step"), tags$th("Their study"), tags$th("This tool"))),
        tags$tbody(
          tags$tr(tags$td("1. Data center demand by state"), tags$td("EPRI state shares for 2023: 152.3 TWh, 171 TWh of generation"), tags$td(HTML("<strong>Same</strong> (their Table S1), grossed up to the same generation"))),
          tags$tr(tags$td("2. Which plants supply it"), tags$td("Hourly least-cost dispatch of every generator across 64 power regions, with trade between them"),
                  tags$td("eGRID2023 mix of the state's subregion: average of all plants, or the plants that respond to new load (non-baseload)")),
          tags$tr(tags$td("3. Air pollution"), tags$td("GEOS-Chem full chemistry for 2023, about 50 km"), tags$td("InMAP Source-Receptor Matrix (2005 chemistry), 1 to 48 km")),
          tags$tr(tags$td("4. Pollutants counted"), tags$td("PM2.5 and ozone"), tags$td("PM2.5 only")),
          tags$tr(tags$td("5. Health response"), tags$td("GEMM (Burnett et al. 2018)"), tags$td("Krewski, GEMM slope or Lepeule (choose below)")),
          tags$tr(class = "table-success", tags$td("6. Backup diesel generators"), tags$td("Not included"), tags$td(HTML("<strong>Virginia data centers page</strong>: from the air permits")))))),
    layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
      selectInput("ch_mix", "This tool's plants", c("Plants that respond to new load (non-baseload)" = "nb", "Average of all plants" = "avg")),
      selectInput("ch_crf", "This tool's health response", setNames(names(CRF), vapply(CRF, `[[`, "", "label")), selected = "gemm"),
      div(class = "small text-muted pt-4", "Deaths scale with the health response's slope, so these are the same runs rescaled.")),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = c(6, 6, 6, 6), xxl = c(3, 3, 3, 3)),
      value_box("Demand and generation", sprintf("%s TWh", fmt(sum(ch$states$dc_twh), 1)), sprintf("%s TWh generated, in both", fmt(ch$chen$gen_twh)), theme = "primary"),
      value_box("NOx from the power plants", textOutput("ch_nox"), textOutput("ch_nox_sub"), theme = "secondary"),
      value_box("PM2.5 deaths per year", textOutput("ch_deaths"), textOutput("ch_deaths_sub"), theme = "danger"),
      value_box("Where the deaths fall", textOutput("ch_cor"), "correlation of each state's share of deaths, theirs vs this tool", theme = "info")),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Share of deaths in each state: their study vs this tool"), plotOutput("ch_plot", height = 460, fill = FALSE),
           helpText("Circle size: the state's data center load. On the dashed line the two agree. Their shares include ozone deaths (about a fifth).")),
      card(card_header("Totals, step by step"), tableOutput("ch_tab"),
           h6("What differs, and why"),
           tags$ul(class = "small",
             tags$li(strong("Health response. "), "Most of the gap in deaths. GEMM is steeper than Krewski at US pollution levels; with the GEMM option the gap closes from about 1.8 times to 1.2 times."),
             tags$li(strong("The Midwest. "), "They put a larger share of deaths in the Midwest (15% against 10%). Their supplement finds the InMAP matrix understates nitrate formed from NOx there, because its chemistry predates today's ammonia-rich air."),
             tags$li(strong("New York and North Carolina. "), "This tool puts more deaths there. Each state's load is split over its grid regions by population here, which sends New York's into New York City's dense, high-impact region."),
             tags$li(strong("Virginia. "), "Both put only 4 to 5% of the deaths in Virginia, though it uses 22% of the electricity: the generation and the pollution are elsewhere."),
             tags$li(strong("Ozone. "), "Not modelled here; it adds about a quarter to their PM2.5 deaths.")))),
    table_card("By state", DTOutput("ch_state_tab")),
    div(class = "small text-muted", style = "max-width: 1000px",
      p(strong("Source. "), "Chen Y, Wang S, Senga JRL, White AS, Wong AYH, Freese LM, Bell ML, Selin NE (2026). Spatially distributed air quality and health impacts",
        "of data center electricity demand in the United States. Environ. Res. Lett. 21 214046, ",
        tags$a("doi:10.1088/1748-9326/aebbd4", href = "https://doi.org/10.1088/1748-9326/aebbd4", target = "_blank"),
        " (CC BY 4.0). Demand and deaths by state are their Supporting Information Table S1; their full results are due on Zenodo after an embargo.",
        " Code: ", tags$a("analysis/09_chen2026_comparison.R", href = paste0(REPO, "analysis/09_chen2026_comparison.R"), target = "_blank"), ". Built ", ch$built, ".")))
}

va_ui <- function() {
  if (is.null(va)) return(card(card_header("Not built yet"), p("Run analysis/05_va_permits.R and then analysis/06_va_noise.R (see 'Virginia data center air permits' in README.md), then restart the app.")))
  S <- va$sites; num <- function(x) suppressWarnings(as.numeric(x))
  gw <- sum(num(S$total_ekw), na.rm = TRUE) / 1e6; g <- S[is.finite(num(S$deaths_grid)), ]
  pp <- va$people; pop55 <- pp$people[pp$source == "cool" & pp$case == "central" & pp$level_dba == 55 & pp$group == "pop"]
  tagList(
    div(style = "max-width: 1000px",
      h4("Virginia data centers: what their air permits allow"),
      p("Most data centers in Virginia hold an air permit from the state Department of Environmental Quality (DEQ) for their backup diesel generators.",
        sprintf("DEQ lists %d; %d are current here (%d were read; replaced permits are dropped; one is not available, see the notes).", va$n_list, nrow(S), va$n_list - 1L),
        "Each permit gives the engines, their ratings, annual emission limits and operating terms. This page shows what the permits add up to:",
        "the air pollution and deaths under three cases, and a screening estimate of noise from the generators and from cooling equipment.")),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = c(6, 6, 6, 6), xxl = c(3, 3, 3, 3)),
      value_box("Current permits", nrow(S), sprintf("%s engines, %.1f GW of backup generators", fmt(sum(num(S$engines), na.rm = TRUE)), gw), theme = "primary"),
      value_box("Permitted NOx", sprintf("%s t/yr", fmt(sum(num(S$nox_tpy), na.rm = TRUE) * 0.90718474)), "if every site emitted its annual limit (metric tonnes)", theme = "secondary"),
      value_box("PM2.5 deaths per year", sprintf("%s to %s", sig(sum(g$deaths_test), 2), sig(sum(g$deaths_max), 3)),
                sprintf("routine testing to permitted maximum (%d permits with an equipment list)", nrow(g)), theme = "danger"),
      value_box("Cooling noise above 55 dB(A)", sprintf("%s people", sig(pop55, 2)),
                "near the geocoded sites, central case; a rough, generic estimate", theme = "warning")),
    layout_columns(col_widths = c(8, 4),
      card(card_header("Where the permitted sites are"),
        layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
          selectInput("va_col", "Colour sites by", c("Permitted NOx (tons/yr)" = "nox", "Deaths/yr at the permitted maximum" = "deaths", "Generator capacity (MW)" = "mw")),
          selectInput("va_ring", "Noise rings (55 and 45 dB(A))", c("None" = "none", "Generator test, one engine" = "test", "Outage: every generator" = "outage", "Cooling equipment" = "cool"), selected = "cool"),
          radioButtons("va_case", "Noise assumption", c("Quieter" = "quiet", "Central" = "central", "Louder" = "loud"), selected = "central", inline = TRUE)),
        leafletOutput("va_map", height = 560),
        helpText("Circle size: generator capacity. Hollow circles are permits placed at their county's centre because the address could not be located;",
                 "they have no noise rings. Rings are drawn downwind with no barriers; the inner ring is 55 dB(A), the outer 45 dB(A).")),
      card(card_header("Air pollution: three cases"), tableOutput("va_cases"),
           helpText("Routine testing: each engine's permitted testing hours (32 h where none is stated) at 75% load, at EPA Tier 2 rates.",
                    "Grid support adds 50 h per engine in grid emergencies, crediting SCR. Deaths from the PM2.5 these cause (InMAP, Krewski)."),
           card_header("Operating terms in the permits"), tableOutput("va_terms"))),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Noise: people inside each level"), tableOutput("va_noise_people"),
           helpText("People living where the level is at or above the threshold, with every site's cooling (or every site's generators in an outage) added together;",
                    "a test is one engine at one site at a time. Geocoded sites only, so the counts are low, most of all in Loudoun.",
                    "Quieter, central and louder are the enclosure (Level 2, Level 1, none) or the chillers (extra-low-noise, low-noise, standard).")),
      card(card_header("Who lives nearby"), tableOutput("va_who"),
           helpText("Air: population-weighted PM2.5 from every site at its permitted maximum, relative to everyone in the contiguous US.",
                    "Noise: share of people within 45 dB(A) of cooling (central case), relative to Virginia's share.",
                    "Both mostly reflect where in Virginia the sites are (Northern Virginia), not which neighbourhoods are next to them."))),
    table_card("Every current permit", DTOutput("va_tab")),
    div(class = "small text-muted", style = "max-width: 1000px",
      p(strong("Notes. "), "Permits were saved by hand from DEQ's list (DEQ refuses scripted downloads) and read by scripts/08; values that could not be read reliably were read by hand.",
        "Not available: ", va$missing, ". Built ", va$built, "."),
      p("The noise figures are a screening estimate, not a noise study: the permits say nothing about noise, so engine loudness comes from makers' published levels,",
        "the enclosure fitted is unknown, and cooling equipment is estimated from generator capacity. Levels are A-weighted, which understates the low-frequency hum residents report most.",
        "Every assumption, its effect and its source: ", tags$a("analysis/06_va_noise_assumptions.md", href = paste0(REPO, "analysis/06_va_noise_assumptions.md"), target = "_blank"), ".")))
}

ui <- if (is.null(app)) no_data_ui else page_navbar(
  title = "US large-load impact explorer", fillable = FALSE,
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  nav_panel("Explore a site", layout_sidebar(fillable = FALSE,
  sidebar = sidebar(
    width = 380,
    if (isTRUE(app$meta$synthetic)) div(class = "alert alert-danger", strong("Synthetic test data."), " None of these numbers are real."),
    h6("1. Where"),
    radioButtons("loc_mode", NULL, c("Choose a county" = "county", "Enter latitude and longitude" = "latlon"), selected = "county"),
    conditionalPanel("input.loc_mode == 'county'",
      selectInput("state", "State", choices = states, selected = default_state),
      selectizeInput("fips", "County (type to search)", choices = counties_in(default_state), selected = default_fips)),
    conditionalPanel("input.loc_mode == 'latlon'",
      layout_columns(col_widths = c(6, 6),
        numericInput("lat", "Latitude", value = round(cty$lat[match(default_fips, cty$fips)], 4), min = 24, max = 50, step = 0.01),
        numericInput("lon", "Longitude", value = round(cty$lon[match(default_fips, cty$fips)], 4), min = -125, max = -66, step = 0.01)),
      uiOutput("latlon_note")),
    uiOutput("region_note"),
    helpText("You can also click the map to set the location."),
    hr(), h6("2. How big"),
    numericInput("mw", "Load (MW)", value = 100, min = 0, step = 10),
    selectInput("preset", "Type of load", choices = setNames(names(PRESETS), vapply(PRESETS, `[[`, "", "label"))),
    radioButtons("basis", "The MW figure is", c("IT / process load (multiplied by PUE)" = "it", "the whole facility" = "facility"), selected = "it"),
    sliderInput("lf", "Average utilisation (load factor)", min = 0.05, max = 1, value = 0.70, step = 0.05),
    helpText(HTML("The fraction of the capacity shown above (MW) that the site draws on average over a year; 1 means full capacity year-round.",
                  "There is no standard value. Berkeley Lab researchers have used roughly 70% average server utilisation, with an uncertainty",
                  "range of 30&ndash;80%, but server utilisation is not the same as facility power demand. Operators often reserve more capacity",
                  "than they use. Use the facility&rsquo;s measured annual average demand divided by its capacity, if available.")),
    numericInput("pue", "PUE (facility energy / IT energy)", value = 1.2, min = 1, max = 3, step = 0.05),
    numericInput("wue", "On-site water use (L per kWh of IT / process energy)", value = 1.8, min = 0, step = 0.1),
    hr(), h6("3. Where the electricity comes from"),
    radioButtons("scenario", NULL, choices = SCENARIOS, selected = "nb"),
    uiOutput("scenario_help"),
    conditionalPanel("input.scenario == 'custom'", uiOutput("custom_ui")),
    conditionalPanel("input.scenario == 'avg' || input.scenario == 'nb' || input.scenario == 'custom'",
      sliderInput("loss", "Lost in transmission and distribution (%)", min = 0, max = 12, value = 4.2, step = 0.1),
      uiOutput("loss_help"),
      sliderInput("clean", "Share met by new carbon-free supply, hour by hour (%)", min = 0, max = 100, value = 0, step = 5),
      helpText("This is a what-if you set, not data, so it starts at 0. Use it for the share of the load that is covered in every hour",
               "by new carbon-free generation built or contracted for this facility; that share is taken out of the grid supply above.",
               "Annual certificates or contracts that are not matched hour by hour should not be counted here.")),
    conditionalPanel("input.scenario == 'onsite'",
      selectInput("onsite_preset", "Plant type", choices = setNames(names(ONSITE), vapply(ONSITE, `[[`, "", "label")), selected = "sc_scr"),
      layout_columns(col_widths = c(6, 6),
        numericInput("os_co2", "CO2 kg/MWh", 530), numericInput("os_nox", "NOx kg/MWh", 0.04),
        numericInput("os_so2", "SO2 kg/MWh", 0.003), numericInput("os_pm", "PM2.5 kg/MWh", 0.03),
        numericInput("os_water", "Water L/MWh", 0), numericInput("os_h", "Release height m", 60, min = 0)),
      helpText("Illustrative rates: replace them with the permit values. Release height is stack height plus plume rise.")),
    hr(),
    checkboxInput("gs_on", strong("Add backup diesel generators"), FALSE),
    conditionalPanel("input.gs_on",
      selectInput("gs_tier", "Emission standard", choices = setNames(names(GENSETS), vapply(GENSETS, `[[`, "", "label"))),
      layout_columns(col_widths = c(6, 6),
        numericInput("gs_n", "Number of generators", 41, min = 0, step = 1), numericInput("gs_kw", "kW each", 2500, min = 0, step = 100),
        numericInput("gs_hours", "Hours run per year, each", 50, min = 0, step = 10), numericInput("gs_load", "Average load while running (%)", 75, min = 0, max = 100),
        numericInput("gs_h", "Release height m", 10, min = 0)),
      helpText("The count starts at enough 2.5 MW units to carry the facility's full load, plus one spare; change it to the site's actual number. ",
               "Emergency engines may run up to 100 hours a year for testing, maintenance and limited non-emergency use under federal rules ",
               "(40 CFR 60.4211(f)); there is no cap during real outages. Testing often runs at part load. ",
               "Emissions are released at the site, near the ground, and added to whichever supply option is chosen above. ",
               "Rates: EPA limits for engines of 560 kW and over (the whole Tier 2 NMHC+NOx limit is counted as NOx, an upper bound).")),
    hr(),
    accordion(open = FALSE, accordion_panel("Advanced settings",
      selectInput("subrgn", "Grid region", choices = c("Automatic (from the county)" = "", setNames(regions, region_label(regions)))),
      helpText("The region is found automatically from the county. Change it only if you know the site is served from a different region."),
      selectInput("crf", "Health response to PM2.5", choices = setNames(names(CRF), vapply(CRF, `[[`, "", "label"))),
      numericInput("vsl", "Value of a statistical life (million $)", value = 13, min = 0, step = 0.5))),
    downloadButton("dl", "County results (CSV)"),
    if (has_cells) downloadButton("dl_cells", "Grid-cell results (CSV)")
  ),
  layout_columns(fill = FALSE, col_widths = breakpoints(sm = c(6, 6, 6, 6), xxl = c(3, 3, 3, 3)),   # two per row until the window is wide
    value_box("Electricity", textOutput("vb_twh"), textOutput("vb_twh_sub"), theme = "primary"),
    value_box("CO2", textOutput("vb_co2"), textOutput("vb_co2_sub"), theme = "secondary"),
    value_box("PM2.5 deaths", textOutput("vb_deaths"), textOutput("vb_deaths_sub"), theme = "danger"),
    value_box("Water consumed", textOutput("vb_water"), textOutput("vb_water_sub"), theme = "info")),
  navset_card_tab(
    nav_panel("Map", uiOutput("map_note"),
              layout_columns(fill = FALSE, col_widths = c(8, 4),
                             div(class = "small text-muted", textOutput("map_mode", inline = TRUE)),
                             if (has_plants) checkboxInput("show_plants", "Show the power plants behind this mix", FALSE)),
              leafletOutput("map", height = 600)),
    nav_panel("Power sources",
      layout_columns(col_widths = c(6, 6),
        card(card_header("About each supply option"), uiOutput("options_help")),
        card(card_header(textOutput("mix_title", inline = TRUE)), plotOutput("mix_plot", height = 190, fill = FALSE), tableOutput("mix_tab"),
             uiOutput("mix_note"))),
      table_card("Largest plants in this mix", DTOutput("plant_tab"),
           helpText("Share is the fraction of each MWh the plant supplies under the selected grid option. Emissions are the plant's totals for the data year."))),
    nav_panel("Who is exposed",
      layout_columns(col_widths = c(5, 7),
        card(card_header("Where the deaths fall"), tableOutput("where_tab"),
             card_header("Exposure by group"), uiOutput("grp_ui")),
        table_card("Counties with the most attributable deaths", DTOutput("top_tab")))),
    nav_panel("Compare supply options", DTOutput("cmp_tab"),
              helpText("Same load and location under each supply option. The grid rows use the loss and carbon-free share set in the sidebar.")),
    nav_panel("Methods and limitations", includeHTML("methods.html"), h5("Build report"), verbatimTextOutput("qa"))
  ))),
  nav_panel("Virginia data centers", va_ui()),
  nav_panel("Guidi et al. (2026) comparison", hv_ui()),
  nav_panel("Chen et al. (2026) comparison", ch_ui())
)

server <- function(input, output, session) {
  if (is.null(app)) return(invisible())
  nz <- function(x, d) if (is.null(x) || length(x) == 0 || is.na(x)) d else x

  # ---- location: state -> county, or a click on the map ---------------------------------------
  pending <- reactiveVal(default_fips)
  observeEvent(input$state, {
    ch <- counties_in(input$state)
    updateSelectizeInput(session, "fips", choices = ch, selected = if (pending() %in% ch) pending() else ch[[1]])
  }, ignoreInit = TRUE)
  pick_county <- function(id) {
    pending(id); st <- cty$state_name[match(id, cty$fips)]
    if (!identical(st, input$state)) updateSelectInput(session, "state", selected = st) else updateSelectizeInput(session, "fips", selected = id)
  }
  county_at <- function(lon, lat) {                           # row of the county containing a point, or NA
    pt <- st_sfc(st_point(c(lon, lat)), crs = st_crs(cty))
    i <- tryCatch(suppressMessages(st_intersects(pt, cty))[[1]], error = function(e) integer())
    if (length(i)) i[1] else NA_integer_
  }
  observeEvent(input$map_click, {
    if (identical(input$loc_mode, "latlon")) {
      updateNumericInput(session, "lat", value = round(input$map_click$lat, 4)); updateNumericInput(session, "lon", value = round(input$map_click$lng, 4))
    } else { i <- county_at(input$map_click$lng, input$map_click$lat); if (!is.na(i)) pick_county(cty$fips[i]) }
  })
  observeEvent(input$loc_mode, {                              # start the coordinates at the county already chosen
    if (input$loc_mode == "latlon" && input$fips %in% cty$fips) { i <- match(input$fips, cty$fips)
      updateNumericInput(session, "lat", value = round(cty$lat[i], 4)); updateNumericInput(session, "lon", value = round(cty$lon[i], 4)) }
  }, ignoreInit = TRUE)
  site <- reactive({
    if (identical(input$loc_mode, "latlon")) {
      lat <- as.numeric(nz(input$lat, NA)); lon <- as.numeric(nz(input$lon, NA))
      validate(need(is.finite(lat) && is.finite(lon), "Enter a latitude and a longitude."))
      i <- county_at(lon, lat); validate(need(!is.na(i), "That point is outside the contiguous United States."))
      list(i = i, lon = lon, lat = lat)
    } else { req(input$fips %in% cty$fips); i <- match(input$fips, cty$fips); list(i = i, lon = cty$lon[i], lat = cty$lat[i]) }
  })
  site_cell <- reactive(locate_cell(app, site()$lon, site()$lat))
  output$latlon_note <- renderUI({
    s <- site(); k <- site_cell()
    helpText(sprintf("This point is in %s, %s.%s The exact point sets the model grid cell that on-site emissions are released in; grid-supplied results depend on the grid region.",
                     cty$name[s$i], cty$state[s$i], if (!is.na(k)) sprintf(" Model grid cell here: %s km.", app$cells$km[k]) else ""))
  })
  observeEvent(input$preset, {
    p <- PRESETS[[input$preset]]; if (input$preset == "custom") return()
    updateRadioButtons(session, "basis", selected = p$basis); updateSliderInput(session, "lf", value = p$lf)
    updateNumericInput(session, "pue", value = p$pue); updateNumericInput(session, "wue", value = p$wue)
  }, ignoreInit = TRUE)
  observeEvent(input$onsite_preset, {
    o <- ONSITE[[input$onsite_preset]]
    updateNumericInput(session, "os_co2", value = o$co2); updateNumericInput(session, "os_nox", value = o$nox)
    updateNumericInput(session, "os_so2", value = o$so2); updateNumericInput(session, "os_pm", value = o$pm25)
    updateNumericInput(session, "os_water", value = o$water)
  }, ignoreInit = TRUE)

  observeEvent(region(), {
    m <- mixdf[mixdf$subrgn == region(), ][1, ]
    if (!is.null(m$grid_loss) && is.finite(m$grid_loss)) updateSliderInput(session, "loss", value = round(100 * m$grid_loss, 1))
  })
  output$loss_help <- renderUI({
    m <- mixdf[mixdf$subrgn == region(), ][1, ]
    helpText(if (!is.null(m$grid_loss) && is.finite(m$grid_loss))
      sprintf("Electricity lost between the power plants and the meter, so more is generated than is used. The default, %.1f%%, is EPA eGRID2023's grid gross loss for the %s interconnection.", 100 * m$grid_loss, m$interconnect)
      else "Electricity lost between the power plants and the meter, so more is generated than is used.")
  })
  output$custom_ui <- renderUI({
    d <- default_shares(app, region()); input$cs_reset
    d <- d[d >= 0.005]                                       # fuels under 0.5% of generation stay at their current share
    tagList(lapply(names(d), function(f) sliderInput(paste0("cs_", f), FUEL_LABELS[[f]], min = 0, max = 100, value = round(200 * d[[f]]) / 2, step = 0.5, post = "%")),
            actionLink("cs_reset", "Reset to the region's current mix"), uiOutput("cs_sum"))
  })
  shares <- reactive({
    d <- default_shares(app, region())
    v <- vapply(names(d), function(f) if (d[[f]] >= 0.005) as.numeric(nz(input[[paste0("cs_", f)]], 100 * d[[f]])) else 100 * d[[f]], 0)
    if (sum(v) <= 0) d else setNames(v / sum(v), names(d))
  })
  output$cs_sum <- renderUI({
    d <- default_shares(app, region()); big <- names(d)[d >= 0.005]
    v <- vapply(big, function(f) as.numeric(nz(input[[paste0("cs_", f)]], NA)), 0)
    now <- grid_mix(app, region(), "custom", shares())$rates$co2_kg_mwh; base <- grid_mix(app, region(), "custom", d)$rates$co2_kg_mwh
    helpText(sprintf("The sliders add to %s%%; shares are rescaled to 100%%. This mix: %s kg CO2/MWh (the region today: %s). Fuels that supply under 0.5%% of the region's electricity are held at their current share.",
                     if (anyNA(v)) "100" else sig(sum(v), 4), sig(now, 3), sig(base, 3)))
  })

  host <- reactive(cty[site()$i, ])
  region <- reactive(if (nzchar(nz(input$subrgn, ""))) input$subrgn else host()$subrgn)
  energy <- reactive(load_energy(max(nz(input$mw, 0), 0), input$basis, max(nz(input$pue, 1), 1), input$lf))
  args <- reactive({
    e <- energy()
    list(fips = host()$fips, twh = e$twh, it_twh = e$it_twh, subrgn = region(), loss = input$loss / 100,
         clean_share = input$clean / 100, beta = CRF[[input$crf]]$beta, wue = max(nz(input$wue, 0), 0),
         onsite = list(co2 = nz(input$os_co2, 0), nox = nz(input$os_nox, 0), so2 = nz(input$os_so2, 0),
                       pm25 = nz(input$os_pm, 0), water = nz(input$os_water, 0)),
         onsite_height = nz(input$os_h, 60), shares = shares(), site_cell = site_cell(),
         gensets = if (isTRUE(input$gs_on)) list(n = max(nz(input$gs_n, 0), 0), kw = max(nz(input$gs_kw, 0), 0), hours = max(nz(input$gs_hours, 0), 0),
                                                 load = min(max(nz(input$gs_load, 0), 0), 100) / 100, rates = GENSETS[[nz(input$gs_tier, "tier2")]],
                                                 height_m = max(nz(input$gs_h, 10), 0)))
  })
  # generator count follows the facility size until the user types one
  gs_touched <- reactiveVal(FALSE)
  observeEvent(input$gs_n, if (!identical(input$gs_n, gs_auto())) gs_touched(TRUE), ignoreInit = TRUE)
  gs_auto <- reactive(ceiling(energy()$facility_mw * 1000 / max(nz(input$gs_kw, 2500), 1)) + 1)
  observeEvent(gs_auto(), if (!gs_touched()) updateNumericInput(session, "gs_n", value = gs_auto()))
  res <- reactive(do.call(impact, c(list(app = app, scenario = input$scenario), args())))
  mix_row <- function(m) mixdf[mixdf$subrgn == region() & mixdf$mix == m, ]

  output$region_note <- renderUI({
    auto <- !nzchar(nz(input$subrgn, ""))
    div(class = "small alert alert-light py-2 mb-1", strong("Grid region: "), region_label(region()), br(),
        if (auto) "Found automatically from the county." else "Set by hand in Advanced settings.")
  })
  output$scenario_help <- renderUI({
    s <- input$scenario; info <- OPTION_INFO[[s]]
    extra <- if (s %in% c("avg", "nb")) {
      m <- mix_row(s)
      sprintf("In %s this mix is %s, at %s kg CO2 per MWh.", region(), fuel_text(m), sig(m$co2_kg_mwh, 3))
    } else if (s == "custom") sprintf("Starts at the current mix of %s. Move the sliders below.", region())
    div(class = "small alert alert-light py-2", p(class = "mb-1", info$what), if (!is.null(extra)) p(class = "mb-1", strong(extra)),
        p(class = "mb-0 text-muted", info$use, " See the Power sources tab."))
  })

  # ---- value boxes ----------------------------------------------------------------------------
  output$vb_twh <- renderText(sprintf("%s TWh/yr", sig(energy()$twh, 3)))
  output$vb_twh_sub <- renderText({
    t <- res()$totals
    sprintf("%s MW facility; %s TWh generated; %s%% of all contiguous-US generation",
            fmt(energy()$facility_mw), sig(t$gen_twh, 3), sig(100 * t$gen_twh / app$meta$all_gen_twh, 2))
  })
  output$vb_co2 <- renderText(sprintf("%s kt/yr", sig(res()$totals$co2_t / 1e3, 3)))
  output$vb_co2_sub <- renderText({ t <- res()$totals
    paste0(sprintf("NOx %s t, SO2 %s t, primary PM2.5 %s t per year", sig(t$nox_t, 3), sig(t$so2_t, 3), sig(t$pm25_t, 3)),
           if (t$genset_mwh > 0) sprintf("; generators: %s kt CO2, %s t NOx", sig(t$genset_co2_t / 1e3, 2), sig(t$genset_nox_t, 2)) else "") })
  output$vb_deaths <- renderText(sprintf("%s per year", sig(res()$totals$deaths, 2)))
  output$vb_deaths_sub <- renderText({ t <- res()$totals
    paste0(sprintf("$%s million/yr at the chosen VSL; mean increment %s ng/m3 across the US population",
                   sig(t$deaths * nz(input$vsl, 0), 2), sig(t$pw_ugm3 * 1e3, 2)),
           if (t$genset_mwh > 0) sprintf("; %s of the deaths from backup generators (%s MWh/yr)", sig(t$genset_deaths, 2), fmt(t$genset_mwh)) else "") })
  output$vb_water <- renderText({ t <- res()$totals; sprintf("%s ML/yr", sig(t$water_site_ML + t$water_gen_cons_ML, 3)) })
  output$vb_water_sub <- renderText({ t <- res()$totals
    sprintf("%s on site, %s at power plants%s (1 ML = 264,000 US gal)", sig(t$water_site_ML, 3), sig(t$water_gen_cons_ML, 3),
            if (is.na(t$water_gen_with_ML)) "" else sprintf("; %s ML withdrawn there", sig(t$water_gen_with_ML, 3))) })

  # ---- map ------------------------------------------------------------------------------------
  # Counties when zoomed out; the model's own grid cells (1-48 km) once few
  # enough of them are in view.
  output$map <- renderLeaflet({
    q <- parseQueryString(isolate(session$clientData$url_search))
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE, minZoom = 3)) |> addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addMapPane("pm", zIndex = 410) |> addMapPane("top", zIndex = 430)       # site, region and plants stay above the colours
    v <- suppressWarnings(as.numeric(c(q$lng, q$lat, q$zoom)))
    if (length(v) == 3 && all(is.finite(v))) setView(m, v[1], v[2], v[3]) else fitBounds(m, -124.8, 24.5, -66.9, 49.4)
  })
  output$map_note <- renderUI({
    gs <- if (isTRUE(input$gs_on)) " Backup generator emissions are in the totals and the Who is exposed tab, not on this map." else ""
    if (input$scenario == "onsite") div(class = "alert alert-warning", paste0("On-site generation: no downwind map. The map needs the full matrix row for the site, which this version does not store; totals and the Who is exposed tab are available.", gs))
    else if (input$scenario == "clean") div(class = "alert alert-info", paste0("No operational emissions from the electricity supply in this scenario.", gs))
    else if (nzchar(gs)) div(class = "alert alert-light", trimws(gs))
  })
  bounds <- debounce(reactive(input$map_bounds), 350)
  in_view <- reactive({
    b <- bounds(); r <- res()$totals
    if (!has_cells || is.null(b) || !(r$col %in% colnames(app$cells$F)) || r$gen_twh <= 0 || nz(input$map_zoom, 4) < 7) return(NULL)
    px <- 0.05 * (b$east - b$west); py <- 0.05 * (b$north - b$south)
    idx <- which(app$cells$clng > b$west - px & app$cells$clng < b$east + px & app$cells$clat > b$south - py & app$cells$clat < b$north + py)
    if (!length(idx) || length(idx) > MAX_CELLS) NULL else idx
  })
  mode <- reactiveVal("counties")
  observe({ m <- if (is.null(in_view())) "counties" else "cells"; if (!identical(m, isolate(mode()))) mode(m) })
  output$map_mode <- renderText({
    r <- res()$totals
    if (is.na(r$col) || !has_cells) ""
    else if (r$col == "custom") "Showing counties. A mix you set yourself is mapped by county only."
    else if (mode() == "cells") "Showing the model's own grid cells (1 to 48 km). Zoom out for counties."
    else if (nz(input$map_zoom, 4) >= 7) "Showing counties. Zoom in a little more to see the model's grid cells."
    else "Showing counties. Zoom in on an area to see the model's grid cells (1 km in cities, up to 48 km in rural areas)."
  })
  pal_for <- function(r) {                                     # one colour scale for both levels, in ng/m3
    cv <- if (has_cells) cell_values(app, r, 1) else NULL
    top <- if (!is.null(cv)) max(cv$dpm_ugm3) * 1e3 else max(r$county$dpm_ugm3, na.rm = TRUE) * 1e3
    list(top = top, pal = colorNumeric("YlOrRd", domain = log10(top) + c(-3, 0), na.color = "#f0f0f0"))
  }
  observe({                                                    # county layer
    r <- res(); req(mode() == "counties")
    proxy <- leafletProxy("map") |> clearGroup("pm") |> clearControls()
    if (!is.null(r$county) && max(r$county$dpm_ugm3, na.rm = TRUE) > 0) {
      v <- r$county$dpm_ugm3 * 1e3; P <- pal_for(r); lv <- pmin(log10(pmax(v, P$top / 1000)), log10(P$top))
      proxy |>
        addPolygons(data = cty, group = "pm", options = pathOptions(pane = "pm"), weight = 0.3, color = "#888", fillOpacity = 0.8, fillColor = P$pal(lv),
                    label = sprintf("%s, %s: +%s ng/m3, %s deaths/yr", cty$name, cty$state, sig(v, 2), sig(r$county$deaths, 2))) |>
        addLegend("bottomright", pal = P$pal, values = log10(P$top) + c(-3, 0), title = "PM2.5 increment (ng/m3)",
                  labFormat = labelFormat(transform = function(x) signif(10^x, 2)))
    } else {
      proxy |> addPolygons(data = cty, group = "pm", options = pathOptions(pane = "pm"), weight = 0.3, color = "#888", fillOpacity = 0.2, fillColor = "#f0f0f0",
                           label = paste0(cty$name, ", ", cty$state))
    }
  })
  observe({                                                    # grid-cell layer
    idx <- in_view(); req(!is.null(idx)); r <- res(); L <- app$cells
    d <- cell_values(app, r, CRF[[input$crf]]$beta, idx); req(!is.null(d)); v <- d$dpm_ugm3 * 1e3; P <- pal_for(r)
    lv <- pmin(log10(pmax(v, P$top / 1000)), log10(P$top))
    ring <- function(m) as.vector(t(cbind(m[idx, , drop = FALSE], m[idx, 1], NA)))
    leafletProxy("map") |> clearGroup("pm") |> clearControls() |>
      addPolygons(lng = ring(L$lng), lat = ring(L$lat), group = "pm", options = pathOptions(pane = "pm"), stroke = FALSE, fillOpacity = 0.75, fillColor = P$pal(lv),
                  label = sprintf("%s km cell: +%s ng/m3; %s people; %s deaths/yr", d$size_km, sig(v, 2), fmt(d$pop), sig(d$deaths, 2))) |>
      addLegend("bottomright", pal = P$pal, values = log10(P$top) + c(-3, 0), title = "PM2.5 increment (ng/m3)",
                labFormat = labelFormat(transform = function(x) signif(10^x, 2)))
  })
  observe({                                                    # site, region outline, plants
    h <- host(); r <- res()$totals
    proxy <- leafletProxy("map") |> clearGroup("site") |> clearGroup("region") |> clearGroup("plants")
    if (!is.na(r$col)) proxy |> addPolylines(data = app$regions[app$regions$subrgn == r$subrgn, ], group = "region", color = "#2c3e50", weight = 2, dashArray = "4", options = pathOptions(pane = "top"))
    if (has_plants && isTRUE(input$show_plants) && !is.na(r$col)) {
      p <- top_plants(60)
      if (nrow(p)) proxy |> addCircleMarkers(lng = p$lon, lat = p$lat, group = "plants", options = pathOptions(pane = "top"), radius = 3 + 13 * sqrt(p$share), weight = 1, color = "#222",
                                             fillColor = unname(ifelse(is.na(PLANT_COLS[p$fuelcat]), "#999999", PLANT_COLS[p$fuelcat])), fillOpacity = 0.85,
                                             label = sprintf("%s (%s, %s): %.1f%% of each MWh", p$name, tolower(p$fuelcat), p$state, 100 * p$share))
    }
    proxy |> addCircleMarkers(lng = site()$lon, lat = site()$lat, group = "site", options = pathOptions(pane = "top"), radius = 7, color = "#000", fillColor = "#18bc9c", fillOpacity = 1, weight = 2,
                              label = paste0("Site: ", h$name, ", ", h$state))
  })

  # ---- power sources --------------------------------------------------------------------------
  plant_mix <- reactive(if (input$scenario == "nb") "nb" else if (input$scenario %in% c("avg", "custom")) "avg" else "nb")
  top_plants <- function(n) {
    if (!has_plants) return(data.frame())
    p <- app$plants[app$plants$subrgn == region(), ]; p$share <- p[[paste0("share_", plant_mix())]]
    p <- p[p$share > 0, ]; p[order(-p$share), ][seq_len(min(n, nrow(p))), ]
  }
  output$options_help <- renderUI({
    tagList(lapply(names(SCENARIOS), function(nm) { i <- OPTION_INFO[[SCENARIOS[[nm]]]]
      div(class = if (SCENARIOS[[nm]] == input$scenario) "border-start border-4 border-success ps-2 mb-3" else "ps-2 mb-3",
          strong(nm), p(class = "mb-1", i$what), p(class = "mb-1", em(i$use)), p(class = "mb-0 text-muted small", "Keep in mind: ", i$caveat)) }))
  })
  output$mix_title <- renderText(sprintf("What the two grid options mean in %s", region_label(region())))
  output$mix_plot <- renderPlot({
    m <- rbind(mix_row("nb"), mix_row("avg")); sh <- as.matrix(m[, grep("^share_", names(m)), drop = FALSE])
    colnames(sh) <- sub("share_", "", colnames(sh)); sh[!is.finite(sh)] <- 0; sh <- sh[, colSums(sh) > 0.005, drop = FALSE]
    par(mar = c(3, 9.5, 0.5, 8), xpd = NA)
    barplot(t(sh) * 100, horiz = TRUE, names.arg = c("Plants that respond", "Average of all plants"), las = 1, border = NA,
            col = FUEL_COLS[colnames(sh)], xlab = "", xlim = c(0, 100))
    legend("right", inset = c(-0.23, 0), legend = colnames(sh), fill = FUEL_COLS[colnames(sh)], border = NA, bty = "n", cex = 0.9)
  })
  output$mix_tab <- renderTable({
    m <- rbind(mix_row("nb"), mix_row("avg"))
    data.frame(Option = c("Plants that respond to new load", "Average of all plants"), `Plants` = fmt(m$n_plants),
               `CO2 kg/MWh` = sig(m$co2_kg_mwh, 3), `NOx kg/MWh` = sig(m$nox_kg_mwh, 2), `SO2 kg/MWh` = sig(m$so2_kg_mwh, 2),
               `PM2.5 kg/MWh` = sig(m$pm25_kg_mwh, 2), `Water L/MWh` = sig(m$wcons_l_mwh, 3), check.names = FALSE)
  })
  output$mix_note <- renderUI(helpText("Fuel shares of the electricity supplied under each option (EPA eGRID2023). The region is the eGRID subregion that contains the county; utility territories do not follow county lines exactly, so it can be changed under Advanced settings."))
  output$plant_tab <- renderDT({
    p <- top_plants(25); validate(need(nrow(p) > 0, "Plant list not available."))
    datatable(data.frame(Plant = p$name, State = p$state, Fuel = tolower(p$fuelcat), `Share of each MWh (%)` = sig(100 * p$share, 2),
                         `Generation (TWh)` = sig(p$gen_twh, 3), `CO2 kg/MWh` = sig(p$co2_kg_mwh, 3), `SO2 (t/yr)` = sig(p$so2_t, 3),
                         `NOx (t/yr)` = sig(p$nox_t, 3), `PM2.5 (t/yr)` = sig(p$pm25_t, 3), check.names = FALSE),
              rownames = FALSE, options = list(dom = "tp", pageLength = 10, ordering = FALSE, scrollX = TRUE),
              caption = sprintf("%s, %s", region_label(region()), names(SCENARIOS)[match(plant_mix(), SCENARIOS)]))
  })

  # ---- who is exposed ------------------------------------------------------------------------
  output$where_tab <- renderTable({
    t <- res()$totals; h <- host(); validate(need(!is.na(t$deaths_host_county), "Not available for this supply option."))
    data.frame(Where = c(paste0(h$name, ", ", h$state, " (host county)"), paste0("Rest of ", h$state), "Other states", "Total"),
               `Deaths per year` = sig(c(t$deaths_host_county, t$deaths_host_state - t$deaths_host_county, t$deaths - t$deaths_host_state, t$deaths), 2),
               Share = c(sprintf("%.1f%%", 100 * c(t$deaths_host_county, t$deaths_host_state - t$deaths_host_county, t$deaths - t$deaths_host_state) / t$deaths), ""),
               check.names = FALSE)
  })
  output$grp_ui <- renderUI({
    if (!is.null(app$ses)) tagList(
      radioButtons("grp_set", NULL, names(SES_SETS), inline = TRUE),
      plotOutput("ses_plot", height = 260, fill = FALSE), tableOutput("ses_tab"),
      helpText("Population-weighted PM2.5 increment for each group, relative to everyone in the contiguous US. ",
               "Each of about 238,000 census block groups takes the model grid cells it overlaps (1 to 48 km), and is counted with its ",
               "people from the American Community Survey 2019-2023. Income fifths rank block groups by median household income, ",
               "each fifth holding a fifth of the people. Deaths use the model's baseline mortality rates."))
    else tagList(plotOutput("grp_plot", height = 260, fill = FALSE),
                 helpText("Population-weighted PM2.5 increment, relative to the average for everyone (model-grid populations)."))
  })
  ses_rows <- reactive({
    d <- res()$ses
    validate(need(!is.null(d) && d$dpm_ugm3[1] > 0,
                  if (input$scenario == "onsite" && is.null(app$og)) "For on-site generation this needs data/onsite_groups.rds (see the README)."
                  else "No emissions in this supply option."))
    d[d$group %in% SES_SETS[[nz(input$grp_set, names(SES_SETS)[1])]], ]
  })
  output$ses_plot <- renderPlot({
    d <- ses_rows(); r <- rev(d$ratio); lab <- rev(d$label); n <- length(r)
    # labels sit above each bar, so the chart works at any width
    par(mar = c(4, 1, 0.5, 1)); xm <- max(1.3, max(r) * 1.15)
    plot(NA, xlim = c(0, xm), ylim = c(0.4, n + 0.6), yaxt = "n", ylab = "", xlab = "Exposure relative to everyone", bty = "n")
    y <- seq_len(n); rect(0, y - 0.38, r, y - 0.02, col = "#2c3e50", border = NA)
    text(0, y + 0.2, lab, adj = 0, cex = 0.85, xpd = NA); text(r, y - 0.2, sprintf("%.2f", r), pos = 4, cex = 0.85, xpd = NA)
    abline(v = 1, lty = 2)
  })
  output$ses_tab <- renderTable({
    d <- ses_rows()
    data.frame(Group = d$label, People = fmt(d$pop), `PM2.5 increment (ng/m3)` = sig(d$dpm_ugm3 * 1e3, 3),
               `Deaths per year` = sig(d$deaths, 2), `Deaths per million people` = sig(d$deaths_per_million, 2), check.names = FALSE)
  })
  output$grp_plot <- renderPlot({
    g <- res()$groups; validate(need(!is.null(g) && g[["Total"]] > 0, "Not available for this supply option."))
    r <- g[-1] / g[["Total"]]; lab <- ifelse(names(r) %in% names(GROUP_LABELS), GROUP_LABELS[names(r)], names(r))
    par(mar = c(4, 9, 1, 2)); b <- barplot(rev(r), horiz = TRUE, names.arg = rev(lab), las = 1, col = "#2c3e50", border = NA,
                                         xlab = "Exposure relative to the population average", xlim = c(0, max(1.25, max(r) * 1.1)))
    abline(v = 1, lty = 2); text(rev(r), b, sprintf("%.2f", rev(r)), pos = 4, xpd = NA)
  })
  output$top_tab <- renderDT({
    d <- res()$county; validate(need(!is.null(d), "Not available for this supply option."))
    d <- d[order(-d$deaths), ][1:min(25, nrow(d)), ]
    datatable(data.frame(County = paste0(d$name, ", ", d$state), Population = fmt(d$pop), `PM2.5 increment (ng/m3)` = sig(d$dpm_ugm3 * 1e3, 3),
                         `Deaths per year` = sig(d$deaths, 2), check.names = FALSE),
              rownames = FALSE, options = list(dom = "tp", pageLength = 12, ordering = FALSE))
  })

  # ---- comparison -----------------------------------------------------------------------------
  output$cmp_tab <- renderDT({
    t <- do.call(compare_scenarios, c(list(app = app), args()))
    datatable(data.frame(`Supply option` = names(SCENARIOS), `CO2 (kt/yr)` = sig(t$co2_t / 1e3, 3), `NOx (t/yr)` = sig(t$nox_t, 3),
                         `SO2 (t/yr)` = sig(t$so2_t, 3), `Primary PM2.5 (t/yr)` = sig(t$pm25_t, 3), `PM2.5 deaths/yr` = sig(t$deaths, 2),
                         `Water on site (ML/yr)` = sig(t$water_site_ML, 3), `Water at generation (ML/yr)` = sig(t$water_gen_cons_ML, 3), check.names = FALSE),
              rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })
  # ---- Guidi et al. (2026) comparison page (the Harvard hyperscale study) ----------------------------------------------------------------
  if (!is.null(hv)) {
    hk <- reactive(hv$u[[nz(input$hv_u, "central")]] / hv$u_central)        # every quantity scales with the load coefficient
    output$hv_twh <- renderText(sprintf("%s TWh/yr", sig(sum(hv$ba$twh) * hk(), 3)))
    output$hv_co2_theirs <- renderText(sprintf("%s Mt/yr", fmt(sum(hv$ba$co2_mt_harvard) * hk(), 2)))
    output$hv_co2_ours <- renderText(sprintf("%s Mt/yr", fmt(sum(hv$ba$co2_mt) * hk(), 2)))
    output$hv_co2_diff <- renderText(sprintf("difference %.3f%% in total; at most %.3f%% in any balancing authority",
                                             100 * (sum(hv$ba$co2_mt) / sum(hv$ba$co2_mt_harvard) - 1), max(abs(hv$ba$co2_diff_pct[hv$ba$co2_mt_harvard > 0]))))
    output$hv_deaths <- renderText(sprintf("%s to %s", fmt(sum(hv$ba$deaths_krewski) * hk()), fmt(sum(hv$ba$deaths_lepeule) * hk())))
    output$hv_deaths_sub <- renderText(sprintf("Krewski to Lepeule response functions; %s per TWh at Krewski; mean increment %s ng/m3",
                                               sig(sum(hv$ba$deaths_krewski) / sum(hv$ba$twh), 2), sig(hv$groups$pw_ng_m3[hv$groups$group == "Total"] * hk(), 3)))
    output$hv_water <- renderText(sprintf("%s GL/yr", fmt(sum(hv$ba$water_cons_ML) * hk() / 1e3)))
    output$hv_water_sub <- renderText(sprintf("%s billion US gal; %s GL withdrawn. On-site cooling water not included.",
                                              fmt(sum(hv$ba$water_cons_ML) * hk() * 1e6 / 3.785411784 / 1e9, 1), fmt(sum(hv$ba$water_with_ML) * hk() / 1e3)))
    output$hv_em <- renderText(sprintf("%s / %s / %s t", fmt(sum(hv$ba$so2_t) * hk()), fmt(sum(hv$ba$nox_t) * hk()), fmt(sum(hv$ba$pm25_t) * hk())))
    output$hv_map <- renderLeaflet({
      i <- match(cty$fips, hv$county$fips); v <- hv$county$pw_ng_m3[i] * hk(); dth <- hv$county$deaths_krewski[i] * hk()
      top <- max(v, na.rm = TRUE); pal <- colorNumeric("YlOrRd", domain = log10(top) + c(-2, 0), na.color = "#f0f0f0")
      leaflet(options = leafletOptions(preferCanvas = TRUE, minZoom = 3)) |> addProviderTiles(providers$Esri.WorldGrayCanvas) |> fitBounds(-124.8, 24.5, -66.9, 49.4) |>
        addPolygons(data = cty, weight = 0.3, color = "#888", fillOpacity = 0.8, fillColor = pal(pmax(log10(v), log10(top) - 2)),
                    label = sprintf("%s, %s: +%s ng/m3, %s deaths/yr", cty$name, cty$state, sig(v, 2), sig(dth, 2))) |>
        addLegend("bottomright", pal = pal, values = log10(top) + c(-2, 0), title = "ng/m3", labFormat = labelFormat(transform = function(x) signif(10^x, 2)))
    })
    output$hv_state_plot <- renderPlot({
      s <- hv$state; s <- s[order(-(s$hdc_twh / sum(s$hdc_twh) + s$deaths_krewski / sum(s$deaths_krewski))), ][1:14, ]
      m <- rbind(`Share of the fleet's electricity use` = 100 * s$hdc_twh / sum(hv$state$hdc_twh), `Share of the deaths` = 100 * s$deaths_krewski / sum(hv$state$deaths_krewski))
      par(mar = c(4, 3.5, 1, 1)); barplot(m[, ncol(m):1], beside = TRUE, horiz = TRUE, names.arg = rev(s$state), las = 1, col = c("#2c3e50", "#e74c3c"), border = NA, xlab = "% of national total")
      legend("bottomright", legend = rownames(m), fill = c("#2c3e50", "#e74c3c"), border = NA, bty = "n")
    })
    output$hv_ba_tab <- renderDT({
      b <- hv$ba; k <- hk()
      datatable(data.frame(`Balancing authority` = sprintf("%s (%s)", b$ba, b$baname), Facilities = b$n_facilities, `TWh` = sig(b$twh * k, 3),
                           `CO2 theirs (Mt)` = fmt(b$co2_mt_harvard * k, 3), `CO2 this tool (Mt)` = fmt(b$co2_mt * k, 3), `Difference (%)` = sprintf("%.3f", abs(round(b$co2_diff_pct, 3))),
                           `Added: PM2.5 deaths/yr` = sig(b$deaths_krewski * k, 2), `Added: deaths per TWh` = sig(b$deaths_krewski / b$twh, 2),
                           `Added: SO2 (t)` = sig(b$so2_t * k, 3), `Added: water consumed (ML)` = sig(b$water_cons_ML * k, 3), check.names = FALSE),
                rownames = FALSE, options = list(dom = "tp", pageLength = 12, ordering = FALSE, scrollX = TRUE))
    })
    output$hv_state_tab <- renderDT({
      s <- hv$state; k <- hk()
      datatable(data.frame(State = s$state, `Hyperscale facilities` = s$hdc_facilities, `Electricity used there (TWh)` = sig(s$hdc_twh * k, 3),
                           `PM2.5 deaths there/yr` = sig(s$deaths_krewski * k, 2), `Mean increment (ng/m3)` = sig(s$pw_ng_m3 * k, 3), check.names = FALSE),
                rownames = FALSE, options = list(dom = "tp", pageLength = 10, ordering = TRUE))
    })
    output$hv_grp_plot <- renderPlot({
      g <- hv$groups[hv$groups$group != "Total", ]; lab <- ifelse(g$group %in% names(GROUP_LABELS), GROUP_LABELS[g$group], g$group)
      par(mar = c(4, 9, 1, 2)); b <- barplot(rev(g$relative), horiz = TRUE, names.arg = rev(lab), las = 1, col = "#2c3e50", border = NA,
                                           xlab = "Exposure relative to the population average", xlim = c(0, max(1.3, max(g$relative) * 1.1)))
      abline(v = 1, lty = 2); text(rev(g$relative), b, sprintf("%.2f", rev(g$relative)), pos = 4, xpd = NA)
    })
  }

  # ---- Virginia data centers page ---------------------------------------------------------------
  if (!is.null(va)) {
    VS <- va$sites; num <- function(x) suppressWarnings(as.numeric(x))
    VS$mw <- num(VS$total_ekw) / 1e3; VS$nox_tpy <- num(VS$nox_tpy); VS$geo <- grepl("^geocoded", VS$placed)
    yn <- function(x) ifelse(x %in% c(TRUE, "TRUE", "True"), "yes", "no")
    output$va_map <- renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = TRUE)) |> addProviderTiles(providers$Esri.WorldGrayCanvas) |> setView(-77.5, 38.9, 10)
    })
    observe({
      req(input$va_col, input$va_ring, input$va_case)
      v <- switch(input$va_col, nox = VS$nox_tpy, deaths = num(VS$deaths_max), mw = VS$mw)
      pal <- colorNumeric("YlOrRd", domain = range(v, na.rm = TRUE), na.color = "#bbbbbb")
      ttl <- c(nox = "Permitted NOx (short tons/yr)", deaths = "Deaths/yr, permitted max", mw = "Generators (MW)")[[input$va_col]]
      pop <- sprintf("<b>%s</b> (%s)<br>%s, issued %s<br>%s engines, %s MW%s<br>Permitted NOx %s short tons/yr; %s deaths/yr at the maximum<br>SCR on %s engines; grid-emergency use: %s; testing window: %s<br>Placed at: %s",
                     htmltools::htmlEscape(VS$site), VS$registration, VS$locality, VS$issued, fmt(num(VS$engines)), sig(VS$mw, 3),
                     ifelse(is.na(VS$model_family), "", paste0(" (", VS$model_family, ")")), sig(VS$nox_tpy, 3), sig(num(VS$deaths_max), 2),
                     fmt(num(VS$scr_engines)), yn(VS$grid_emergency_use), ifelse(is.na(VS$testing_window) | VS$testing_window == "", "-", VS$testing_window), VS$placed)
      r <- 4 + 10 * sqrt(pmax(ifelse(is.na(VS$mw), 0, VS$mw), 0) / max(VS$mw, na.rm = TRUE))
      p <- leafletProxy("va_map") |> clearGroup("sites") |> clearGroup("rings") |> clearControls()
      if (input$va_ring != "none") {
        for (lv in c(45, 55)) {
          col <- sprintf("d%d_%s_%s", lv, input$va_ring, input$va_case); d <- num(VS[[col]]); k <- which(VS$geo & is.finite(d) & d > 0)
          if (length(k)) p <- p |> addCircles(lng = VS$lon[k], lat = VS$lat[k], radius = d[k], group = "rings", stroke = FALSE,
                                              fillColor = if (lv == 55) "#a8202f" else "#f59e72", fillOpacity = if (lv == 55) 0.25 else 0.15)
        }
      }
      p |> addCircleMarkers(lng = VS$lon, lat = VS$lat, radius = r, group = "sites", color = "#333", weight = 1,
                            fillColor = pal(v), fillOpacity = ifelse(VS$geo, 0.9, 0), popup = pop) |>
        addLegend("bottomright", pal = pal, values = v[is.finite(v)], title = ttl)
    })
    output$va_cases <- renderTable({
      g <- VS[is.finite(num(VS$deaths_grid)), ]
      data.frame(Case = c("Routine testing", "Testing + 50 h grid support", "Permitted maximum", sprintf("Permitted maximum, all %d permits", nrow(VS))),
                 Permits = c(rep(nrow(g), 3), nrow(VS)),
                 `NOx (t/yr)` = fmt(c(sum(num(g$nox_test)), sum(num(g$nox_grid)), sum(g$nox_tpy, na.rm = TRUE) * 0.90718474, sum(VS$nox_tpy, na.rm = TRUE) * 0.90718474)),
                 `Deaths/yr` = sig(c(sum(num(g$deaths_test)), sum(num(g$deaths_grid)), sum(num(g$deaths_max)), sum(num(VS$deaths_max), na.rm = TRUE)), 3), check.names = FALSE)
    }, striped = TRUE, spacing = "s")
    output$va_terms <- renderTable({
      eng <- sum(num(VS$engines), na.rm = TRUE)
      data.frame(Term = c("Engines fitted with SCR", "Running allowed in grid-operator (PJM) emergencies", "Engines capped at 500 h/yr for all purposes",
                          "Testing restricted in ozone season", "Testing allowed only in set hours", "Annual fuel cap"),
                 Permits = c(sprintf("%s engines (%.0f%%)", fmt(sum(num(VS$scr_engines), na.rm = TRUE)), 100 * sum(num(VS$scr_engines), na.rm = TRUE) / eng),
                             sum(VS$grid_emergency_use %in% c(TRUE, "True", "TRUE")), sum(num(VS$hours_all_purposes) == 500, na.rm = TRUE),
                             sum(VS$ozone_season_limit %in% c(TRUE, "True", "TRUE")), sum(!is.na(VS$testing_window) & VS$testing_window != ""),
                             sprintf("%d (%s million gal/yr)", sum(is.finite(num(VS$fuel_gal_yr))), sig(sum(num(VS$fuel_gal_yr), na.rm = TRUE) / 1e6, 3))))
    }, striped = TRUE, spacing = "s")
    output$va_noise_people <- renderTable({
      pp <- va$people[va$people$group == "pop", ]
      key <- c(test = "Generator test, one engine", outage = "Outage: every generator", cool = "Cooling equipment")
      rows <- expand.grid(level = va$levels, source = names(key), stringsAsFactors = FALSE)
      get <- function(src, lv, cs) fmt(signif(pp$people[pp$source == src & pp$level_dba == lv & pp$case == cs], 2))
      data.frame(Source = key[rows$source], Level = paste0(">= ", rows$level, " dB(A)"),
                 Quieter = mapply(get, rows$source, rows$level, "quiet"), Central = mapply(get, rows$source, rows$level, "central"),
                 Louder = mapply(get, rows$source, rows$level, "loud"), check.names = FALSE, row.names = NULL)
    }, striped = TRUE, spacing = "s", align = "llrrr")
    output$va_who <- renderTable({
      g <- c(WhiteNoLat = "White, non-Latino", Black = "Black", Asian = "Asian", Latino = "Latino", income_q1 = "Lowest-income fifth", income_q5 = "Highest-income fifth")
      ag <- va$air_groups; pp <- va$people[va$people$source == "cool" & va$people$case == "central" & va$people$level_dba == 45, ]
      data.frame(Group = unname(g), `Air: exposure vs US` = sprintf("x%.2f", ag$ratio[match(names(g), ag$group)]),
                 `Noise: share within 45 dB(A) vs Virginia` = sprintf("x%.2f", pp$ratio_to_va[match(names(g), pp$group)]), check.names = FALSE)
    }, striped = TRUE, spacing = "s", align = "lrr")
    output$va_tab <- renderDT({
      d <- data.frame(Permit = VS$registration, Site = VS$site, Locality = VS$locality, Issued = VS$issued, Engines = num(VS$engines),
                      MW = round(VS$mw, 1), `Make and model` = VS$model_family, `SCR engines` = num(VS$scr_engines),
                      `Grid emergencies` = yn(VS$grid_emergency_use), `Testing window` = VS$testing_window,
                      `NOx limit (short tons/yr)` = round(VS$nox_tpy, 1), `Deaths/yr at the limit` = signif(num(VS$deaths_max), 2),
                      `Deaths/yr, routine testing` = signif(num(VS$deaths_test), 2),
                      `55 dB(A): test (m)` = num(VS$d55_test_central), `55 dB(A): outage (m)` = num(VS$d55_outage_central),
                      `55 dB(A): cooling (m)` = num(VS$d55_cool_central), Located = ifelse(VS$geo, "address", "county centre"), check.names = FALSE)
      datatable(d[order(-d$`NOx limit (short tons/yr)`), ], rownames = FALSE, filter = "top",
                options = list(pageLength = 15, scrollX = TRUE, dom = "ftip"))
    })
  }

  # ---- Chen et al. (2026) comparison ---------------------------------------------------------------
  if (!is.null(ch)) {
    chk <- reactive(CRF[[nz(input$ch_crf, "gemm")]]$beta / ch$beta_krewski)
    chd <- reactive(ch$states[[nz(input$ch_mix, "nb")]] * chk())                    # this tool's deaths by state
    output$ch_nox <- renderText({ e <- ch$emis; sprintf("%s vs %s kt", fmt(e$nox_kt[e$run == "chen"], 1), fmt(e$nox_kt[e$run == nz(input$ch_mix, "nb")], 1)) })
    output$ch_nox_sub <- renderText({ e <- ch$emis; sprintf("theirs vs this tool; SO2 %s vs %s kt", fmt(e$so2_kt[e$run == "chen"], 1), fmt(e$so2_kt[e$run == nz(input$ch_mix, "nb")], 1)) })
    output$ch_deaths <- renderText(sprintf("%s vs %s", fmt(ch$chen$pm), fmt(sum(chd()))))
    output$ch_deaths_sub <- renderText(sprintf("theirs (GEMM) vs this tool; they add %s from ozone", fmt(ch$chen$o3)))
    output$ch_cor <- renderText(sprintf("%.2f", cor(ch$states$chen_deaths / sum(ch$states$chen_deaths), chd() / sum(chd()))))
    output$ch_plot <- renderPlot({
      S <- ch$states; a <- 100 * S$chen_deaths / sum(S$chen_deaths); b <- 100 * chd() / sum(chd()); lim <- c(0, max(a, b) * 1.08)
      par(mar = c(4.5, 4.5, 1, 1))
      plot(a, b, pch = 21, bg = "#2166ac", col = "white", cex = 0.7 + sqrt(S$dc_twh) / 2, xlim = lim, ylim = lim, las = 1,
           xlab = "Chen et al. 2026: share of deaths (%)", ylab = "This tool: share of deaths (%)")
      abline(0, 1, lty = 2, col = "grey50"); big <- a > 3 | b > 3
      text(a[big], b[big], S$state[big], pos = 4, cex = 0.8)
    })
    output$ch_tab <- renderTable({
      e <- ch$emis; m <- nz(input$ch_mix, "nb"); k <- chk(); base <- ch$states[[m]]
      dk <- function(crf) fmt(sum(base) * CRF[[crf]]$beta / ch$beta_krewski)
      data.frame(Quantity = c("Generation (TWh)", "NOx (kt)", "SO2 (kt)", "PM2.5 deaths, Krewski", "PM2.5 deaths, GEMM", "PM2.5 deaths, Lepeule", "Ozone deaths"),
                 `Their study` = c(fmt(ch$chen$gen_twh), fmt(e$nox_kt[1], 1), fmt(e$so2_kt[1], 1), "", sprintf("%s (%s-%s)", fmt(ch$chen$pm), ch$chen$pm_ci[1], ch$chen$pm_ci[2]), "",
                                   sprintf("%s (%s-%s)", fmt(ch$chen$o3), ch$chen$o3_ci[1], ch$chen$o3_ci[2])),
                 `This tool` = c(fmt(ch$gen[[m]]), fmt(e$nox_kt[e$run == m], 1), fmt(e$so2_kt[e$run == m], 1), dk("krewski"), dk("gemm"), dk("lepeule"), "not modelled"),
                 check.names = FALSE)
    }, striped = TRUE, spacing = "s", align = "lrr")
    output$ch_state_tab <- renderDT({
      S <- ch$states; d <- chd()
      datatable(data.frame(State = S$name, `Data center load (TWh)` = S$dc_twh, `Their deaths (PM2.5 + ozone)` = sprintf("%s (%s-%s)", S$chen_deaths, S$chen_deaths_lo, S$chen_deaths_hi),
                           `Their share (%)` = round(100 * S$chen_deaths / sum(S$chen_deaths), 1), `This tool: PM2.5 deaths` = signif(d, 2),
                           `This tool: share (%)` = round(100 * d / sum(d), 1), check.names = FALSE),
                rownames = FALSE, options = list(pageLength = 15, dom = "ftp", scrollX = TRUE, order = list(list(3, "desc"))))
    })
  }

  output$qa <- renderText(paste(c(sprintf("Built %s from %s and %s.", app$meta$built, app$meta$egrid, app$meta$isrm), app$meta$qa), collapse = "\n"))

  output$dl <- downloadHandler(
    filename = function() sprintf("load_impacts_%s_%sMW_%s_counties.csv", host()$fips, input$mw, input$scenario),
    content = function(file) { d <- res()$county; if (is.null(d)) d <- res()$totals; utils::write.csv(d, file, row.names = FALSE) })
  output$dl_cells <- downloadHandler(
    filename = function() sprintf("load_impacts_%s_%sMW_%s_gridcells.csv", host()$fips, input$mw, input$scenario),
    content = function(file) {
      d <- cell_values(app, res(), CRF[[input$crf]]$beta)
      if (is.null(d)) d <- res()$totals
      utils::write.csv(d, file, row.names = FALSE)
    })
}

shinyApp(ui, server)
