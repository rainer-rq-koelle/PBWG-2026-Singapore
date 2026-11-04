# International flights between PBWG partner regions (no European end): OSN only
#
# OSN splits long flights into segments at surveillance gaps; only the first
# segment usually carries ADEP and the last ADES. A flight is reconstructed per
# aircraft (ICAO24) by chaining its segments in time:
#   start : segment with ADEP = PBWG airport of region R1
#   end   : first later segment with ADES set (the aircraft's next landing)
#   keep  : ADES = PBWG airport of another region R2, no other landing in
#           between (no intermediate segment with ADES or ADEP set), and an
#           airborne time consistent with the great-circle distance
# The departure piece may itself carry ADES (single-segment flights).
#
# Output per pair (alphabetical, e.g. JPN-USA): flight table and PBWG milestone
# table built from the OSN events of the chained segments (same codes and
# conventions as the EUR region pairs; R/analytic_merge.R).

source("R/analytic_merge.R")

INTL_PERIOD <- c("2025-06-01", "2025-06-13")   # OSN flight events end 2025-06-13

# Plausibility window for runway-to-runway time vs great-circle distance.
# Calibrated on EUR long-haul flights (ATOT->ALDT, 357-499 kt effective, 0.5-99.5%):
# hours in [d/600 + 0.2, d/330 + 1.5], capped at 20 h (longest non-stop flights).
# A tighter upper bound removes chains through a hub where OSN missed the
# intermediate landing (e.g. Asia/Africa -> Gulf hub -> USA).
intl_window_h <- function(d_nm) {
  tibble(MIN_H = d_nm / 600 + 0.2, MAX_H = pmin(d_nm / 330 + 1.5, 20))
}

dist_nm <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 3440.065 * asin(sqrt(pmin(1, a)))
}

#' OSN segments of aircraft seen at PBWG airports (padded period)
intl_segments <- function(flight_list, apts, period = INTL_PERIOD) {
  d0 <- format(as.Date(period[1]) - 1)
  d1 <- format(as.Date(period[2]) + 1)
  seg <- flight_list |>
    filter(DOF >= d0, DOF <= d1) |>
    select(ID, ICAO24, FLT_ID, REGISTRATION, TYPECODE, ADEP, ADES, FIRST_SEEN, LAST_SEEN) |>
    collect() |>
    as_utc() |>
    mutate(
      ICAO24 = toupper(ICAO24),
      FLT_ID = str_trim(FLT_ID),
      LAST_SEEN = coalesce(LAST_SEEN, FIRST_SEEN)
    )
  ac <- seg |> filter(ADEP %in% apts$APT | ADES %in% apts$APT) |> distinct(ICAO24) |> pull()
  seg |>
    filter(ICAO24 %in% ac) |>
    arrange(ICAO24, FIRST_SEEN)
}

#' Chain OSN segments into flights between PBWG regions
#'
#' Returns one row per flight with chain diagnostics and an EXCLUDE reason
#' (NA = kept).
intl_chain <- function(seg, apts, period = INTL_PERIOD) {
  t0 <- as.POSIXct(period[1], tz = "UTC")
  t1 <- as.POSIXct(period[2], tz = "UTC") + days(1)

  seg <- seg |>
    left_join(apts |> select(APT, R_DEP = REGION), by = c("ADEP" = "APT")) |>
    left_join(apts |> select(APT, R_ARR = REGION), by = c("ADES" = "APT")) |>
    mutate(IDX = row_number(), .by = ICAO24)

  starts <- seg |>
    filter(!is.na(R_DEP), FIRST_SEEN >= t0, FIRST_SEEN < t1) |>
    select(ICAO24, IDX0 = IDX, ID0 = ID, ADEP, R_DEP, FLT0 = FLT_ID, T_START = FIRST_SEEN)

  # Next landing of the aircraft at or after the start segment
  landings <- seg |>
    filter(!is.na(ADES)) |>
    select(ICAO24, IDX1 = IDX, ID1 = ID, ADES, R_ARR, FLT1 = FLT_ID, T_END = LAST_SEEN)
  # Next take-off after the start segment (an intermediate departure means a landing was missed)
  takeoffs <- seg |>
    filter(!is.na(ADEP)) |>
    select(ICAO24, IDXT = IDX, T_TO = FIRST_SEEN)

  fl <- starts |>
    left_join(landings, by = join_by(ICAO24, IDX0 <= IDX1), multiple = "first") |>
    left_join(takeoffs, by = join_by(ICAO24, IDX0 < IDXT), multiple = "first") |>
    filter(!is.na(IDX1))

  fl |>
    left_join(apts |> select(APT, LAT0 = LAT, LON0 = LON), by = c("ADEP" = "APT")) |>
    left_join(apts |> select(APT, LAT1 = LAT, LON1 = LON), by = c("ADES" = "APT")) |>
    mutate(
      GCD_NM = dist_nm(LAT0, LON0, LAT1, LON1),
      intl_window_h(GCD_NM),
      DUR_H = as.numeric(T_END - T_START, units = "hours"),
      N_SEG = IDX1 - IDX0 + 1,
      CALLSIGN_SAME = FLT0 == FLT1,
      # Callsign change with a different flight number: likely two flights
      # (missed intermediate landing). Empty callsigns are not judged.
      CALLSIGN_CHANGE = nzchar(FLT0) & nzchar(FLT1) &
        str_extract(FLT0, "\\d+") != str_extract(FLT1, "\\d+") &
        str_sub(FLT0, 1, 3) == str_sub(FLT1, 1, 3) &
        DUR_H > 0.8 * MAX_H,
      EXCLUDE = case_when(
        is.na(R_ARR) ~ "destination not a PBWG airport",
        R_ARR == R_DEP ~ "same region",
        !is.na(IDXT) & IDXT <= IDX1 ~ "intermediate take-off",
        DUR_H < MIN_H ~ "too short for distance",
        DUR_H > MAX_H ~ "too long for distance",
        CALLSIGN_CHANGE ~ "callsign change, long duration",
        .default = NA_character_
      )
    ) |>
    select(-starts_with("LAT"), -starts_with("LON"), -IDXT, -T_TO)
}

#' Segments belonging to the kept flights
intl_flight_segments <- function(flights, seg) {
  seg |>
    mutate(IDX = row_number(), .by = ICAO24) |>
    inner_join(
      flights |> select(UID, ICAO24, IDX0, IDX1),
      by = join_by(ICAO24, between(IDX, IDX0, IDX1))
    ) |>
    select(UID, ID, ICAO24, FLT_ID, REGISTRATION, TYPECODE, ADEP_OSN = ADEP, ADES_OSN = ADES,
           FIRST_SEEN, LAST_SEEN) |>
    mutate(LINK_OSN = "icao24+chain")
}

#' Build all pair datasets
intl_build <- function(flight_list, flight_events, apts, period = INTL_PERIOD) {
  seg <- intl_segments(flight_list, apts, period)
  chains <- intl_chain(seg, apts, period)

  kept <- chains |>
    filter(is.na(EXCLUDE)) |>
    mutate(
      PAIR = map2_chr(R_DEP, R_ARR, \(a, b) paste(sort(c(a, b)), collapse = "-")),
      UID = sprintf("%s-%s-%s-%s", ADEP, ADES, ICAO24, format(T_START, "%Y%m%d%H%M", tz = "UTC")),
      FLTID = coalesce(na_if(FLT0, ""), na_if(FLT1, ""))
    ) |>
    distinct(UID, .keep_all = TRUE)

  segs <- intl_flight_segments(kept, seg)
  # Milestone FLTID consistency: OSN callsign of each segment vs the flight's callsign
  segs <- segs |>
    left_join(kept |> select(UID, FLTID), by = "UID") |>
    mutate(LINK_OSN = if_else(coalesce(FLT_ID == FLTID, FALSE), "icao24+chain+fltid", "icao24+chain")) |>
    select(-FLTID)

  ms <- osn_milestones(segs, kept |> select(UID, ICAO24, ADEP, ADES), flight_events) |>
    left_join(kept |> select(UID, PAIR), by = "UID") |>
    relocate(UID) |>
    arrange(UID, TIME)

  list(chains = chains, flights = kept, segments = segs, milestones = ms)
}
