# Build international (non-European) PBWG region-pair datasets and packages
#
# Output: data/intl/<R1>-<R2>-milestones-OSN-20250601-20250613.csv  (PBWG milestone table, OSN only)
#         data/intl/<R1>-<R2>-flights-OSN-20250601-20250613.csv     (chained flights + diagnostics)
#         data/intl/INTL-pairs-summary-20250601-20250613.csv        (all region pairs, incl. not observed)
#         data/intl/INTL-chain-exclusions-20250601-20250613.csv     (excluded chains, by reason)
#         data/share/PBWG-<R1>-<R2>-milestones-20250601-20250613.zip
# data/intl/ and data/share/ are local only (gitignored).

source("R/international.R")

INTL_TAG <- "20250601-20250613"
dir.create("data/intl", showWarnings = FALSE)

apts <- read_csv("data/ref-pbwg-airports.csv", show_col_types = FALSE)
REGION_NAMES <- c(
  BRA = "Brazil", CAN = "Canada", CHN = "China", JPN = "Japan",
  SIN = "Singapore", THA = "Thailand", USA = "United States", ZAF = "South Africa"
)

intl <- intl_build(
  open_dataset("data/flight_list/opdi_flight_list/"),
  open_dataset("data/flight_events/opdi_flight_events/"),
  apts
)

# Consistency check per flight: segments, missing ADEP/ADES on intermediate
# pieces, gaps between pieces, callsign consistency
seg_check <- intl$segments |>
  arrange(UID, FIRST_SEEN) |>
  mutate(GAP_MIN = as.numeric(FIRST_SEEN - lag(LAST_SEEN), units = "mins"), .by = UID) |>
  summarise(
    N_SEG = n(),
    N_SEG_NO_ADEP_ADES = sum(is.na(ADEP_OSN) & is.na(ADES_OSN)),
    MAX_GAP_MIN = suppressWarnings(max(GAP_MIN, na.rm = TRUE)),
    N_CALLSIGNS = n_distinct(FLT_ID[nzchar(coalesce(FLT_ID, ""))]),
    .by = UID
  ) |>
  mutate(MAX_GAP_MIN = if_else(is.infinite(MAX_GAP_MIN), 0, round(MAX_GAP_MIN)))

flights <- intl$flights |>
  left_join(seg_check |> select(-N_SEG), by = "UID") |>
  transmute(
    PAIR, UID, ICAO24, FLTID, ADEP, ADES, R_DEP, R_ARR,
    T_FIRST_SEEN = T_START, T_LAST_SEEN = T_END,
    DUR_H = round(DUR_H, 2), GCD_NM = round(GCD_NM), N_SEG,
    N_SEG_NO_ADEP_ADES, MAX_GAP_MIN, N_CALLSIGNS
  )

# All region pairs, including those not observed in OSN
all_pairs <- combn(sort(names(REGION_NAMES)), 2) |>
  t() |>
  as_tibble(.name_repair = ~ c("R1", "R2")) |>
  mutate(PAIR = paste(R1, R2, sep = "-"))

pair_summary <- all_pairs |>
  left_join(
    flights |>
      summarise(
        FLIGHTS = n(),
        AIRPORT_PAIRS = n_distinct(paste(ADEP, ADES)),
        SINGLE_SEGMENT = mean(N_SEG == 1),
        SEG_MEDIAN = median(N_SEG),
        SEG_WITHOUT_ADEP_ADES = sum(N_SEG_NO_ADEP_ADES),
        CALLSIGN_CONSISTENT = mean(N_CALLSIGNS <= 1),
        DUR_H_MEDIAN = round(median(DUR_H), 1),
        .by = PAIR
      ),
    by = "PAIR"
  ) |>
  left_join(
    intl$chains |>
      filter(!EXCLUDE %in% c("destination not a PBWG airport", "same region"), !is.na(EXCLUDE)) |>
      mutate(PAIR = map2_chr(R_DEP, R_ARR, \(a, b) paste(sort(c(a, b)), collapse = "-"))) |>
      count(PAIR, name = "CHAINS_EXCLUDED"),
    by = "PAIR"
  ) |>
  mutate(
    across(c(FLIGHTS, SEG_WITHOUT_ADEP_ADES, CHAINS_EXCLUDED), \(x) coalesce(x, 0L)),
    STATUS = if_else(FLIGHTS > 0, "observed", "not observed in OSN")
  ) |>
  arrange(desc(FLIGHTS))

write_csv(pair_summary, sprintf("data/intl/INTL-pairs-summary-%s.csv", INTL_TAG), na = "")
intl$chains |>
  filter(!is.na(EXCLUDE), !EXCLUDE %in% c("destination not a PBWG airport", "same region")) |>
  mutate(PAIR = map2_chr(R_DEP, R_ARR, \(a, b) paste(sort(c(a, b)), collapse = "-"))) |>
  select(PAIR, ICAO24, ADEP, ADES, FLT0, FLT1, T_START, T_END, DUR_H, MIN_H, MAX_H, N_SEG, EXCLUDE) |>
  write_csv(sprintf("data/intl/INTL-chain-exclusions-%s.csv", INTL_TAG), na = "")

# Per pair files
for (p in pair_summary$PAIR[pair_summary$FLIGHTS > 0]) {
  write_csv(filter(flights, PAIR == p), sprintf("data/intl/%s-flights-OSN-%s.csv", p, INTL_TAG), na = "")
  intl$milestones |>
    filter(PAIR == p) |>
    select(-PAIR) |>
    mutate(TIME = format(TIME, "%Y-%m-%d %H:%M:%S", tz = "UTC")) |>
    write_csv(sprintf("data/intl/%s-milestones-OSN-%s.csv", p, INTL_TAG), na = "")
}

# ---- Packages ---------------------------------------------------------------

CORE_MST <- c("SOBT", "AOBT", "ATOT", "D40", "D100", "A100", "A40", "ALDT", "AIBT", "SIBT")

intl_dictionary <- tribble(
  ~FIELD, ~TABLE, ~DESCRIPTION,
  "UID", "milestones, flights", "Flight key in this dataset: ADEP-ADES-ICAO24-<first OSN observation yyyymmddHHMM>. Not an official identifier.",
  "FLTID", "milestones, flights", "Aircraft identification (ICAO Doc 4444), registration, or airport-provided FLTID. Here: ADS-B callsign observed by OSN.",
  "REG", "milestones", "Aircraft registration (without hyphen).",
  "TYPE", "milestones", "Aircraft type (ICAO designator).",
  "ICAO24", "milestones, flights", "Aircraft address (24-bit, hex).",
  "ADEP", "milestones, flights", "Aerodrome of departure (ICAO).",
  "ADES", "milestones, flights", "Aerodrome of destination (ICAO).",
  "MST", "milestones", "Milestone code. PBWG core: SOBT, AOBT, ATOT, D40, D100, A100, A40, ALDT, AIBT, SIBT. OSN ring crossings: D<n>/A<n> (n = 40, 50, 60, 100, 110, 120 NM). Vertical profile: TOC, TOD, LVL_START/LVL_END, LVL_CLB_START/END, LVL_DES_START/END, FL<nnn>_CLB/_DES. Other OSN events (runway, surface, first_seen/last_seen) keep their published names.",
  "MST_LBL", "milestones", "Runway (ATOT/ALDT) or stand (AOBT/AIBT) where available; for unmapped OSN ring crossings airport and direction.",
  "TIME", "milestones", "Milestone time, UTC, YYYY-MM-DD HH:MM:SS.",
  "LAT, LON", "milestones", "Position (decimal degrees) where available.",
  "ALT", "milestones", "Flight level where available.",
  "SOURCE", "milestones", "OSN = OpenSky Network via the Open Performance Data Initiative (OPDI) flight events. Partner data can be added as further sources.",
  "SOURCE_ID", "milestones", "OSN flight-list segment id.",
  "CONF", "milestones", "Out-of-area label (empty in this release).",
  "MATCH", "milestones", "How the OSN segment was attached: icao24+chain+fltid (callsign equals the flight's callsign) or icao24+chain.",
  "PAIR, R_DEP, R_ARR", "flights", "Region pair and departure/arrival region.",
  "T_FIRST_SEEN, T_LAST_SEEN", "flights", "First and last OSN observation of the flight (UTC).",
  "DUR_H, GCD_NM", "flights", "Observed duration (h) and great-circle distance between the airports (NM).",
  "N_SEG", "flights", "Number of OSN segments chained into the flight.",
  "N_SEG_NO_ADEP_ADES", "flights", "Chained segments where OSN identified neither departure nor destination (intermediate pieces).",
  "MAX_GAP_MIN", "flights", "Longest gap between consecutive OSN segments (min).",
  "N_CALLSIGNS", "flights", "Number of distinct callsigns over the chained segments."
)

intl_readme <- function(p, flights, ms) {
  r <- str_split_1(p, "-")
  n1 <- REGION_NAMES[[r[1]]]
  n2 <- REGION_NAMES[[r[2]]]
  dir_tab <- flights |>
    count(dir = sprintf("%s -> %s", REGION_NAMES[R_DEP], REGION_NAMES[R_ARR])) |>
    mutate(line = sprintf("| %s | %s |", dir, format(n, big.mark = ","))) |>
    pull(line)
  apts_used <- sort(unique(c(flights$ADEP, flights$ADES)))
  c(
    sprintf("# PBWG milestone sample: %s - %s (1-13 June 2025)", n1, n2),
    "",
    "Prepared by the EUROCONTROL Performance Review Unit for the ICAO Performance Benchmarking Working Group (PBWG).",
    "Draft sample for discussion, validation and exchange between PBWG members.",
    "",
    sprintf("This sample is built from open ADS-B data (OSN) only. It is shared with both %s and %s: data from either side can be added to complete the respective end of the flights (e.g. block, runway, schedule times and 40/100 NM crossings).", n1, n2),
    "",
    "## Scope",
    "",
    "- Period: 1-13 June 2025 (UTC), by first OSN observation at the departure airport.",
    sprintf("- PBWG airports observed: %s.", paste(apts_used, collapse = ", ")),
    "",
    "| Direction | Flights |",
    "|---|---|",
    dir_tab,
    "",
    "## Files",
    "",
    sprintf("- `%s-milestones-%s.csv`: milestone table, one row per milestone (%s rows).", p, INTL_TAG, format(nrow(ms), big.mark = ",")),
    sprintf("- `%s-flights-%s.csv`: one row per flight, with the chaining diagnostics.", p, INTL_TAG),
    sprintf("- `%s-coverage-%s.csv`: share of flights with each PBWG core milestone, by direction.", p, INTL_TAG),
    "- `DATA-DICTIONARY.csv`: field definitions.",
    "",
    "## Format and conventions",
    "",
    "The milestone table follows the PBWG milestone-table format (see the PBWG technical note *PBWG Milestone Tables*):",
    "`FLTID, REG, TYPE, ICAO24, ADEP, ADES, MST, MST_LBL, TIME, LAT, LON, ALT, SOURCE, SOURCE_ID, CONF`, plus `UID` (flight key) and `MATCH` (how a row was linked).",
    "",
    "- All times are UTC.",
    "- All OSN events of the flight are kept; repeated events (e.g. several 40 NM crossings) are not removed.",
    "- Rows of the same flight share the same `UID`; rows are ordered by `UID` and `TIME`.",
    "- `FLTID` is the aircraft identification (ICAO Doc 4444), registration, or airport-provided FLTID; here the ADS-B callsign.",
    "",
    "## How the flights are built",
    "",
    "OSN tracks of long-haul flights are often split into several segments at surveillance gaps, and usually only the first segment carries the departure airport and the last the destination. Flights are therefore reconstructed per aircraft (ICAO24) by chaining its segments in time:",
    "",
    "1. Start at a segment departing from a PBWG airport.",
    "2. Follow the aircraft to its next landing; keep the flight if it lands at a PBWG airport of the other region.",
    "3. Exclude chains with an intermediate take-off (a landing was missed), chains whose duration does not fit the great-circle distance, and long chains whose callsign changes flight number (likely two flights).",
    "",
    "The flights file reports, per flight, the number of segments, segments without departure/destination, the longest gap and the number of callsigns.",
    "",
    "## Notes for use",
    "",
    "- OSN is a volunteer ADS-B receiver network: coverage near some airports and on the ground is limited, and timestamps are not ATC grade. The coverage file shows what is observed at each end.",
    "- Flights not observed by OSN at departure (or with a missed landing) are not included. The sample is therefore not a complete traffic count for the pair.",
    "- This is an analytic dataset (layer 1). Metric-specific selection is a later step.",
    "",
    "## Feedback",
    "",
    "Comments, corrections and complementary data (e.g. block, runway and ring-crossing times at your airports) are very welcome and can be added in a future release.",
    "",
    sprintf("Prepared: %s.", format(Sys.Date(), "%d %B %Y"))
  )
}

intl_coverage <- function(ms, flights) {
  n_dir <- flights |> count(DIRECTION = paste(R_DEP, R_ARR, sep = "->"), name = "N")
  ms |>
    filter(MST %in% CORE_MST) |>
    distinct(UID, MST, SOURCE) |>
    left_join(flights |> select(UID, R_DEP, R_ARR), by = "UID") |>
    count(DIRECTION = paste(R_DEP, R_ARR, sep = "->"), MST, SOURCE) |>
    left_join(n_dir, by = "DIRECTION") |>
    mutate(SHARE = round(n / N, 3)) |>
    select(DIRECTION, MST, SOURCE, FLIGHTS = n, SHARE) |>
    arrange(DIRECTION, match(MST, CORE_MST))
}

build_intl_package <- function(p, out_dir = "data/share") {
  fl <- read_csv(sprintf("data/intl/%s-flights-OSN-%s.csv", p, INTL_TAG), col_types = cols(.default = col_character()))
  ms <- read_csv(sprintf("data/intl/%s-milestones-OSN-%s.csv", p, INTL_TAG), col_types = cols(.default = col_character()))

  stage <- file.path(tempdir(), sprintf("PBWG-%s", p))
  unlink(stage, recursive = TRUE)
  dir.create(stage, recursive = TRUE)
  write_csv(ms, file.path(stage, sprintf("%s-milestones-%s.csv", p, INTL_TAG)), na = "")
  write_csv(fl, file.path(stage, sprintf("%s-flights-%s.csv", p, INTL_TAG)), na = "")
  write_csv(intl_coverage(ms, fl), file.path(stage, sprintf("%s-coverage-%s.csv", p, INTL_TAG)), na = "")
  write_csv(intl_dictionary, file.path(stage, "DATA-DICTIONARY.csv"))
  writeLines(intl_readme(p, fl, ms), file.path(stage, "README.md"))

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  zip_path <- normalizePath(file.path(out_dir, sprintf("PBWG-%s-milestones-%s.zip", p, INTL_TAG)), mustWork = FALSE)
  unlink(zip_path)
  withr::with_dir(stage, utils::zip(zip_path, list.files(stage), flags = "-q9X"))
  unlink(stage, recursive = TRUE)
  tibble(PAIR = p, FLIGHTS = nrow(fl), ROWS = nrow(ms), ZIP_MB = round(file.size(zip_path) / 1e6, 1))
}

intl_packages <- map(pair_summary$PAIR[pair_summary$FLIGHTS > 0], build_intl_package) |> list_rbind()
