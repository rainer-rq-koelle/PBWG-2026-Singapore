# Prepare PBWG partner data packages (one zip per region pair)
#
# Input : data/EUR-<XXX>-milestone|flights-EUR<XXX>OSN-20250601-20250614.csv (R/build-analytic.R)
# Output: data/share/PBWG-EUR-<XXX>-milestones-20250601-20250614.zip, containing
#           EUR-<XXX>-milestones-20250601-20250614.csv  analytic milestone table
#           EUR-<XXX>-flights-20250601-20250614.csv     one row per flight (linkage summary)
#           EUR-<XXX>-coverage-20250601-20250614.csv    milestone coverage by direction and source
#           DATA-DICTIONARY.csv
#           README.md
# data/share/ is local only (gitignored); packages are distributed outside GitHub.

library(tidyverse)

PERIOD <- "20250601-20250614"
EUR_HUBS <- c("EDDF", "EDDM", "EGLL", "EHAM", "LEMD", "LFPG", "LIRF", "LSZH")
CORE_MST <- c("SOBT", "AOBT", "ATOT", "D40", "D100", "A100", "A40", "ALDT", "AIBT", "SIBT")

REGIONS <- tribble(
  ~REGION, ~NAME,            ~PARTNER, ~PARTNER_NOTE,
  "BRA",   "Brazil",         "BRA",    "Brazilian end from the partner package (DECEA taxi, punctuality and ASMA data).",
  "CHN",   "China",          "CHN",    "Chinese end from the partner sample (official reporting format); the European end as reported by China is kept as out-of-area rows (CONF = CHN-ooA).",
  "ZAF",   "South Africa",   NA,       NA,
  "USA",   "United States",  NA,       NA,
  "CAN",   "Canada",         NA,       NA,
  "JPN",   "Japan",          NA,       NA,
  "THA",   "Thailand",       NA,       NA,
  "SIN",   "Singapore",      NA,       NA
)

dictionary <- tribble(
  ~FIELD, ~TABLE, ~DESCRIPTION,
  "UID", "milestones, flights", "Flight key in this dataset: ADEP-ADES-ICAO24-<first runway/block time yyyymmddHHMM>. Not an official identifier.",
  "FLTID", "milestones, flights", "Aircraft identification (ICAO Doc 4444), registration, or airport-provided FLTID. European rows: filed aircraft identification where linked to the flight plan, otherwise the airport-provided value. Partner rows: as delivered. OSN rows: ADS-B callsign.",
  "REG", "milestones", "Aircraft registration (without hyphen).",
  "TYPE", "milestones", "Aircraft type (ICAO designator).",
  "ICAO24", "milestones, flights", "Aircraft address (24-bit, hex).",
  "ADEP", "milestones, flights", "Aerodrome of departure (ICAO).",
  "ADES", "milestones, flights", "Aerodrome of destination (ICAO).",
  "MST", "milestones", "Milestone code. PBWG core: SOBT, AOBT, ATOT, D40, D100, A100, A40, ALDT, AIBT, SIBT. OSN ring crossings: D<n>/A<n> (n = 40, 50, 60, 100, 110, 120 NM). Vertical profile: TOC, TOD, LVL_START/LVL_END, LVL_CLB_START/END, LVL_DES_START/END, FL<nnn>_CLB/_DES. Other OSN events (runway, surface, first_seen/last_seen) keep their published names.",
  "MST_LBL", "milestones", "Runway (ATOT/ALDT) or stand (AOBT/AIBT) where available; for unmapped OSN ring crossings airport and direction.",
  "TIME", "milestones", "Milestone time, UTC, YYYY-MM-DD HH:MM:SS.",
  "LAT, LON", "milestones", "Position (decimal degrees) for airborne milestones where available.",
  "ALT", "milestones", "Flight level for airborne milestones where available.",
  "SOURCE", "milestones", "EUR = European airport operator data (APDF) incl. ring crossings from the NM trajectory; BRA/CHN/... = partner data; OSN = OpenSky Network via the Open Performance Data Initiative (OPDI) flight events.",
  "SOURCE_ID", "milestones", "Source record id. EUR-<APDS_ID>-<DEP|ARR>; partner ids as delivered; OSN flight-list segment id.",
  "CONF", "milestones", "Out-of-area label only (e.g. CHN-ooA = value reported by China for the European end). Empty otherwise.",
  "MATCH", "milestones", "How the row was attached to the flight: dep+arr / dep_only / arr_only (reference records), icao24+window+fltid / icao24+window (OSN segments; the latter = OSN callsign differs).",
  "SOURCE_ID_DEP, SOURCE_ID_ARR", "flights", "Reference records (EUR or partner) for the departure and arrival end.",
  "T_DEP, T_ARR", "flights", "Departure / arrival anchor (UTC): runway time, or block time if no runway time.",
  "LINK", "flights", "dep+arr = both ends from reference sources; dep_only / arr_only = one end.",
  "N_OSN_SEGMENTS", "flights", "Number of OSN flight-list segments linked to the flight."
)

readme_text <- function(r, flights, ms, cov) {
  dir_tab <- flights |>
    mutate(dir = if_else(ADEP %in% EUR_HUBS, sprintf("Europe -> %s", r$NAME), sprintf("%s -> Europe", r$NAME))) |>
    count(dir) |>
    mutate(line = sprintf("| %s | %s |", dir, format(n, big.mark = ","))) |>
    pull(line)
  partner_line <- if (is.na(r$PARTNER)) {
    sprintf(
      "This sample combines European airport data with open ADS-B data (OSN). Data from %s can be added to complete the %s end of the flights (e.g. block, runway, schedule times and 40/100 NM crossings).",
      r$NAME, r$NAME
    )
  } else {
    sprintf("This sample combines European airport data, partner data from %s and open ADS-B data (OSN). %s", r$NAME, r$PARTNER_NOTE)
  }
  apts <- sort(unique(c(flights$ADEP, flights$ADES)))

  c(
    sprintf("# PBWG milestone sample: Europe - %s (1-14 June 2025)", r$NAME),
    "",
    "Prepared by the EUROCONTROL Performance Review Unit for the ICAO Performance Benchmarking Working Group (PBWG).",
    "Draft sample for discussion, validation and exchange between PBWG members.",
    "",
    partner_line,
    "",
    "## Scope",
    "",
    sprintf("- Period: 1-14 June 2025 (UTC), by runway time at the reported end."),
    sprintf("- European airports: %s.", paste(intersect(EUR_HUBS, apts), collapse = ", ")),
    sprintf("- %s airports: %s.", r$NAME, paste(setdiff(apts, EUR_HUBS), collapse = ", ")),
    "",
    "| Direction | Flights |",
    "|---|---|",
    dir_tab,
    "",
    "## Files",
    "",
    sprintf("- `EUR-%s-milestones-%s.csv`: milestone table, one row per milestone and source (%s rows).", r$REGION, PERIOD, format(nrow(ms), big.mark = ",")),
    sprintf("- `EUR-%s-flights-%s.csv`: one row per flight with the linked source records.", r$REGION, PERIOD),
    sprintf("- `EUR-%s-coverage-%s.csv`: share of flights with each PBWG core milestone, by direction and source.", r$REGION, PERIOD),
    "- `DATA-DICTIONARY.csv`: field definitions.",
    "",
    "## Format and conventions",
    "",
    "The milestone table follows the PBWG milestone-table format (see the PBWG technical note *PBWG Milestone Tables*):",
    "`FLTID, REG, TYPE, ICAO24, ADEP, ADES, MST, MST_LBL, TIME, LAT, LON, ALT, SOURCE, SOURCE_ID, CONF`, plus `UID` (flight key) and `MATCH` (how a row was linked).",
    "",
    "- All times are UTC.",
    "- All milestones of all sources are kept: there is no de-duplication and no source ranking. Where two sources report the same milestone, both rows are included.",
    "- Rows of the same flight share the same `UID`; rows are ordered by `UID` and `TIME`.",
    "- `CONF` only labels out-of-area values (e.g. `CHN-ooA`).",
    "- `FLTID` is the aircraft identification (ICAO Doc 4444), registration, or airport-provided FLTID.",
    "",
    "## How the sources are linked",
    "",
    "1. European records and partner records of the same flight are linked by aircraft address (ICAO24), airport pair and a feasible block-time window.",
    "2. OSN tracks of long-haul flights are often split into several segments at surveillance gaps. Segments are linked by ICAO24 and time window, and must agree with the flight's airports where OSN identifies them.",
    "3. All OSN events of the linked segments are added; ring crossings and vertical-profile events are mapped to PBWG codes (see data dictionary).",
    "",
    "## Notes for use",
    "",
    "- OSN is a volunteer ADS-B receiver network: coverage near some airports and on the ground is limited, and timestamps are not ATC grade. The coverage file shows what is observed at each end.",
    "- European ring crossings (D40/D100, A100/A40) are derived from the NM trajectory and may differ from some of the OSN derived measured position reports.",
    "- This is an analytic dataset (layer 1). Metric-specific selection (e.g. which of several 40 NM crossings to use for ASMA) is a later step.",
    "",
    "## Feedback",
    "",
    "Comments, corrections and complementary data (e.g. block, runway and ring-crossing times at your airports) are very welcome and can be added in a future release.",
    "",
    sprintf("Prepared: %s.", format(Sys.Date(), "%d %B %Y"))
  )
}

coverage_table <- function(ms, flights) {
  n_dir <- flights |> count(DIRECTION = if_else(ADEP %in% EUR_HUBS, "from EUR", "to EUR"), name = "N")
  ms |>
    filter(MST %in% CORE_MST, is.na(CONF)) |>
    distinct(UID, MST, SOURCE) |>
    left_join(flights |> select(UID, ADEP), by = "UID") |>
    count(DIRECTION = if_else(ADEP %in% EUR_HUBS, "from EUR", "to EUR"), MST, SOURCE) |>
    left_join(n_dir, by = "DIRECTION") |>
    mutate(SHARE = round(n / N, 3)) |>
    select(DIRECTION, MST, SOURCE, FLIGHTS = n, SHARE) |>
    arrange(DIRECTION, match(MST, CORE_MST), SOURCE)
}

build_package <- function(r, out_dir = "data/share") {
  tag <- sprintf("EUR-%s-%%s-EUR%sOSN-%s.csv", r$REGION, r$REGION, PERIOD)
  ms <- read_csv(file.path("data", sprintf(tag, "milestone")), col_types = cols(.default = col_character()))
  flights <- read_csv(file.path("data", sprintf(tag, "flights")), col_types = cols(.default = col_character()))

  stage <- file.path(tempdir(), sprintf("PBWG-EUR-%s", r$REGION))
  unlink(stage, recursive = TRUE)
  dir.create(stage, recursive = TRUE)

  f_ms <- sprintf("EUR-%s-milestones-%s.csv", r$REGION, PERIOD)
  f_fl <- sprintf("EUR-%s-flights-%s.csv", r$REGION, PERIOD)
  f_cv <- sprintf("EUR-%s-coverage-%s.csv", r$REGION, PERIOD)
  write_csv(ms, file.path(stage, f_ms), na = "")
  write_csv(flights, file.path(stage, f_fl), na = "")
  write_csv(coverage_table(ms, flights), file.path(stage, f_cv), na = "")
  write_csv(dictionary, file.path(stage, "DATA-DICTIONARY.csv"))
  writeLines(readme_text(r, flights, ms), file.path(stage, "README.md"))

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  zip_path <- normalizePath(file.path(out_dir, sprintf("PBWG-EUR-%s-milestones-%s.zip", r$REGION, PERIOD)), mustWork = FALSE)
  unlink(zip_path)
  withr::with_dir(stage, utils::zip(zip_path, list.files(stage), flags = "-q9X"))
  unlink(stage, recursive = TRUE)

  tibble(REGION = r$REGION, FLIGHTS = nrow(flights), ROWS = nrow(ms),
         CSV_MB = round(file.size(file.path("data", sprintf(tag, "milestone"))) / 1e6, 1),
         ZIP_MB = round(file.size(zip_path) / 1e6, 1))
}

packages <- REGIONS |>
  split(seq_len(nrow(REGIONS))) |>
  map(build_package) |>
  list_rbind()
