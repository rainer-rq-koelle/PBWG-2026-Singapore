# Analytic milestone dataset: merge APDF, partner and OSN (OPDI) milestones
#
# Layer 1 of the PBWG analytic chain. Each source keeps its own rows; nothing
# is de-duplicated or overwritten. Rows of the same physical flight share UID.
#
#   1. read_milestones()      reference milestones (APDF, partner) in PBWG format
#   2. flight_sides()         one row per source record (DEP or ARR side)
#   3. link_sides()           DEP side + ARR side -> one flight (UID)
#   4. link_osn_segments()    OSN flight-list segments overlapping each flight
#   5. osn_milestones()       all OSN events of linked segments, PBWG format
#   6. merge_milestones()     bind everything, ordered by UID and TIME

library(tidyverse)
library(arrow)

# Parameters per region pair ----

#' Linkage parameters for a region pair
#'
#' @param min_block_h,max_block_h Feasible AOBT/ATOT -> ALDT/AIBT duration (h).
#' @param pre_min,post_min Buffer (min) before the departure and after the
#'   arrival anchor when collecting OSN segments.
#' @param period UTC dates; flights are retained if a reference anchor falls in it.
bridge_params <- function(min_block_h = 8,
                          max_block_h = 15,
                          pre_min = 60,
                          post_min = 60,
                          period = c("2025-06-01", "2025-06-14")) {
  list(
    min_block_h = min_block_h,
    max_block_h = max_block_h,
    pre_min = pre_min,
    post_min = post_min,
    period = as.Date(period)
  )
}

# Reference milestones ----

#' Read a PBWG milestone CSV (APDF or partner), times in UTC
#'
#' CONF identical to SOURCE (e.g. "EUR" in the APDF extracts) carries no
#' information under the PBWG convention (CONF labels out-of-area rows) and is
#' set to NA.
read_milestones <- function(path) {
  read_csv(
    path,
    col_types = cols(
      TIME = col_datetime(), LAT = col_double(), LON = col_double(),
      ALT = col_double(), .default = col_character()
    ),
    locale = locale(tz = "UTC")
  ) |>
    mutate(
      ICAO24 = toupper(ICAO24),
      CONF = if_else(CONF == SOURCE, NA_character_, CONF)
    )
}

#' Summarise reference milestones to one row per flight side
#'
#' The side is taken from the SOURCE_ID suffix (-DEP / -ARR). The anchor is the
#' runway time if present, otherwise the block time. Out-of-area rows
#' (CONF "-ooA") do not define a side.
flight_sides <- function(ms) {
  ms |>
    filter(!is_ooa(CONF)) |>
    mutate(SIDE = str_extract(SOURCE_ID, "(DEP|ARR)$")) |>
    group_by(SOURCE, SOURCE_ID, SIDE, FLTID, REG, TYPE, ICAO24, ADEP, ADES) |>
    summarise(
      T_ANCHOR = if_else(
        SIDE[1] == "DEP",
        coalesce(first(TIME[MST == "ATOT"]), first(TIME[MST == "AOBT"])),
        coalesce(first(TIME[MST == "ALDT"]), first(TIME[MST == "AIBT"]))
      ),
      .groups = "drop"
    ) |>
    filter(!is.na(T_ANCHOR))
}

#' Link departure and arrival sides into flights
#'
#' A DEP and an ARR side belong to the same flight if ICAO24, ADEP and ADES are
#' equal and the arrival anchor lies min_block_h-max_block_h after the departure
#' anchor. Only mutually unique pairs are linked; unlinked sides remain flights
#' of their own.
#'
#' @return One row per flight with UID, DEP/ARR source ids and anchors.
link_sides <- function(sides, params = bridge_params()) {
  dep <- filter(sides, SIDE == "DEP")
  arr <- filter(sides, SIDE == "ARR")

  pairs <- inner_join(
    dep, arr,
    by = c("ICAO24", "ADEP", "ADES"),
    suffix = c("_DEP", "_ARR"),
    relationship = "many-to-many"
  ) |>
    mutate(BLOCK_H = as.numeric(difftime(T_ANCHOR_ARR, T_ANCHOR_DEP, units = "hours"))) |>
    filter(between(BLOCK_H, params$min_block_h, params$max_block_h)) |>
    add_count(SOURCE_ID_DEP, name = "n_dep") |>
    add_count(SOURCE_ID_ARR, name = "n_arr") |>
    filter(n_dep == 1, n_arr == 1) |>
    transmute(
      ICAO24, ADEP, ADES,
      FLTID = coalesce(FLTID_DEP, FLTID_ARR),
      SOURCE_ID_DEP, SOURCE_ID_ARR,
      T_DEP = T_ANCHOR_DEP, T_ARR = T_ANCHOR_ARR,
      LINK = "dep+arr"
    )

  dep_only <- dep |>
    filter(!SOURCE_ID %in% pairs$SOURCE_ID_DEP) |>
    transmute(ICAO24, ADEP, ADES, FLTID, SOURCE_ID_DEP = SOURCE_ID, T_DEP = T_ANCHOR, LINK = "dep_only")
  arr_only <- arr |>
    filter(!SOURCE_ID %in% pairs$SOURCE_ID_ARR) |>
    transmute(ICAO24, ADEP, ADES, FLTID, SOURCE_ID_ARR = SOURCE_ID, T_ARR = T_ANCHOR, LINK = "arr_only")

  bind_rows(pairs, dep_only, arr_only) |>
    filter(
      as.Date(T_DEP) %within% interval(params$period[1], params$period[2]) |
        as.Date(T_ARR) %within% interval(params$period[1], params$period[2])
    ) |>
    arrange(coalesce(T_DEP, T_ARR)) |>
    mutate(UID = sprintf("%s-%s-%s-%s", ADEP, ADES, ICAO24, format(coalesce(T_DEP, T_ARR), "%Y%m%d%H%M", tz = "UTC"))) |>
    relocate(UID)
}

#' Restrict flights to the airport pairs of a scope-defining milestone file
#'
#' The EUR APDF extract defines the authoritative set of airport pairs (both
#' directions as observed). Partner records outside this set are dropped.
restrict_to_pairs <- function(flights, scope_ms) {
  pairs <- distinct(scope_ms, ADEP, ADES)
  semi_join(flights, pairs, by = c("ADEP", "ADES"))
}

# OSN / OPDI ----

# Parquet timestamps carry no time zone; R would display them in local time.
as_utc <- function(df) {
  mutate(df, across(where(is.POSIXct), \(x) with_tz(x, "UTC")))
}

min_or <- function(x, empty) if (length(x)) as.numeric(min(x)) else empty
max_or <- function(x, empty) if (length(x)) as.numeric(max(x)) else empty

#' Link OSN flight-list segments to flights
#'
#' OSN splits long-haul flights at coverage gaps, so the airport pair is not
#' usable for matching. A segment is linked if its ICAO24 matches and it
#' overlaps the flight's time window [T_DEP - pre_min, T_ARR + post_min]. A
#' missing anchor is replaced by the other anchor -/+ max_block_h; to avoid
#' picking up the aircraft's next/previous leg, segments where OSN identified an
#' airport must agree with ADEP/ADES, and the window is closed at the OSN
#' segment arriving at ADES (or opening at ADEP). LINK_OSN records whether the
#' OSN callsign agrees with the reference callsign.
link_osn_segments <- function(flights, flight_list, params = bridge_params()) {
  win <- flights |>
    transmute(
      UID, ICAO24,
      W_START = coalesce(T_DEP, T_ARR - hours(params$max_block_h)) - minutes(params$pre_min),
      W_END   = coalesce(T_ARR, T_DEP + hours(params$max_block_h)) + minutes(params$post_min)
    )

  icao <- tolower(unique(win$ICAO24))
  t_lo <- min(win$W_START) - hours(24)
  t_hi <- max(win$W_END)

  segs <- flight_list |>
    filter(ICAO24 %in% icao, FIRST_SEEN <= t_hi, FIRST_SEEN >= t_lo) |>
    select(ID, ICAO24, FLT_ID, REGISTRATION, TYPECODE, ADEP_OSN = ADEP, ADES_OSN = ADES, FIRST_SEEN, LAST_SEEN) |>
    collect() |>
    as_utc() |>
    mutate(ICAO24 = toupper(ICAO24), LAST_SEEN = coalesce(LAST_SEEN, FIRST_SEEN))

  inner_join(win, segs, by = "ICAO24", relationship = "many-to-many") |>
    filter(FIRST_SEEN <= W_END, LAST_SEEN >= W_START) |>
    left_join(flights |> select(UID, ADEP, ADES, FLTID, T_DEP, T_ARR), by = "UID") |>
    # Segments with a known OSN airport must agree with the flight's airports
    filter(
      is.na(ADEP_OSN) | ADEP_OSN == ADEP,
      is.na(ADES_OSN) | ADES_OSN == ADES
    ) |>
    # One-sided flights: stop at the OSN segment that closes the flight
    mutate(
      T_CLOSE_ARR = min_or(FIRST_SEEN[ADES_OSN %in% ADES], Inf),
      T_OPEN_DEP  = max_or(FIRST_SEEN[ADEP_OSN %in% ADEP], -Inf),
      .by = UID
    ) |>
    filter(
      !(is.na(T_ARR) & as.numeric(FIRST_SEEN) > T_CLOSE_ARR),
      !(is.na(T_DEP) & as.numeric(FIRST_SEEN) < T_OPEN_DEP)
    ) |>
    # A segment belongs to at most one flight: keep the overlap with the most coverage
    mutate(OVERLAP_S = as.numeric(pmin(LAST_SEEN, W_END) - pmax(FIRST_SEEN, W_START), units = "secs")) |>
    slice_max(OVERLAP_S, by = ID, n = 1, with_ties = FALSE) |>
    mutate(
      LINK_OSN = if_else(
        str_trim(FLT_ID) == FLTID,
        "icao24+window+fltid",
        "icao24+window"
      )
    ) |>
    select(UID, ID, ICAO24, FLT_ID, REGISTRATION, TYPECODE, ADEP_OSN, ADES_OSN, FIRST_SEEN, LAST_SEEN, LINK_OSN)
}

#' Extract all OSN events for linked segments in PBWG milestone format
#'
#' Event types are kept as published (renaming is a later harmonisation step).
#' Events without event_time (about 2% of OPDI events, mostly ring crossings)
#' cannot be sequenced and are dropped.
#' MST_LBL carries the airport/direction for ring crossings and the runway for
#' runway events. ALT is converted from feet to flight level as in APDF.
osn_milestones <- function(osn_links, flights, flight_events) {
  days <- unique(format(c(osn_links$FIRST_SEEN, osn_links$LAST_SEEN), "%Y-%m-%d", tz = "UTC"))

  flight_events |>
    filter(dof %in% days, flight_id %in% osn_links$ID, !is.na(event_time)) |>
    select(id, flight_id, type, event_time, longitude, latitude, altitude, info) |>
    collect() |>
    as_utc() |>
    inner_join(osn_links |> select(UID, ID, FLT_ID, REGISTRATION, TYPECODE, LINK_OSN), by = c("flight_id" = "ID")) |>
    left_join(flights |> select(UID, ICAO24, ADEP, ADES), by = "UID") |>
    mutate(
      apt = str_match(info, '"apt_icao":"([A-Z0-9]{4})"')[, 2],
      dir = str_match(info, '"direction":"([a-z]+)"')[, 2],
      rwy = str_match(info, '"runway":"([^"]+)"')[, 2],
      MST = harmonise_osn_mst(type, apt, dir, ADEP, ADES),
      # Runway only: the airport follows from MST and ADEP/ADES.
      # Unmapped ring crossings keep airport and direction for traceability.
      MST_LBL = case_when(
        str_detect(MST, "^xing-\\d+nm") ~ paste(apt, dir),
        !is.na(rwy) ~ rwy,
        .default = NA_character_
      )
    ) |>
    transmute(
      UID,
      FLTID = str_trim(FLT_ID),
      REG = str_remove_all(REGISTRATION, "-"),
      TYPE = TYPECODE,
      ICAO24, ADEP, ADES,
      MST,
      MST_LBL,
      TIME = event_time,
      LAT = latitude,
      LON = longitude,
      ALT = round(altitude / 100),
      SOURCE = "OSN",
      SOURCE_ID = flight_id,
      CONF = NA_character_,
      MATCH = LINK_OSN
    )
}

#' Map OSN event types to PBWG milestone codes
#'
#' Ring crossings become D<nm> (outbound at ADEP) or A<nm> (inbound at ADES);
#' crossings in another direction keep their OSN name. Vertical-profile events
#' get PBWG-style codes. All other event types keep their published name.
harmonise_osn_mst <- function(type, apt, dir, adep, ades) {
  nm <- str_match(type, "^xing-(\\d+)nm$")[, 2]
  fl <- str_match(type, "^xing-fl(\\d+)$")[, 2]
  case_when(
    !is.na(nm) & apt == adep & dir == "outbound" ~ paste0("D", nm),
    !is.na(nm) & apt == ades & dir == "inbound" ~ paste0("A", nm),
    !is.na(fl) ~ sprintf("FL%03d_%s", as.integer(fl), if_else(dir == "up", "CLB", "DES")),
    .default = recode(
      type,
      "top-of-climb" = "TOC",
      "top-of-descent" = "TOD",
      "level-start" = "LVL_START",
      "level-end" = "LVL_END",
      "level-off-climb-start" = "LVL_CLB_START",
      "level-off-climb-end" = "LVL_CLB_END",
      "level-off-descent-start" = "LVL_DES_START",
      "level-off-descent-end" = "LVL_DES_END"
    )
  )
}

# Merge ----

#' Merge reference and OSN milestones into the analytic dataset
#'
#' Rows are attached to a flight by SOURCE_ID. Out-of-area rows (CONF ending in
#' "-ooA") belong to the same source record as the in-area side, so they are
#' attached via the record id without the -DEP/-ARR suffix.
#' MATCH records how the row was attached: the flight's reference linkage
#' (dep+arr, dep_only, arr_only) or the OSN segment linkage.
merge_milestones <- function(ref_ms, flights, osn_ms) {
  side_uid <- bind_rows(
    flights |> select(UID, LINK, SOURCE_ID = SOURCE_ID_DEP),
    flights |> select(UID, LINK, SOURCE_ID = SOURCE_ID_ARR)
  ) |>
    filter(!is.na(SOURCE_ID)) |>
    mutate(RECORD = str_remove(SOURCE_ID, "-(DEP|ARR)$"))

  in_area <- ref_ms |>
    filter(!is_ooa(CONF)) |>
    inner_join(side_uid |> select(UID, MATCH = LINK, SOURCE_ID), by = "SOURCE_ID")

  ooa <- ref_ms |>
    filter(is_ooa(CONF)) |>
    mutate(RECORD = str_remove(SOURCE_ID, "-(DEP|ARR)$")) |>
    inner_join(side_uid |> select(UID, MATCH = LINK, RECORD), by = "RECORD") |>
    select(-RECORD)

  bind_rows(in_area, ooa, osn_ms) |>
    relocate(UID) |>
    relocate(MATCH, .after = CONF) |>
    arrange(UID, TIME)
}

is_ooa <- function(conf) coalesce(str_ends(conf, "-ooA"), FALSE)
