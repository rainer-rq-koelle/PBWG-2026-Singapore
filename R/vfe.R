# Vertical flight efficiency (VFE) from OSN level segments - exploratory
#
# Definitions (TN-01, VFE chapter):
# - Level segment: OSN LVL_START followed by its LVL_END twin in the same OSN
#   segment (flight-list ID). Unpaired events are counted, not used.
# - Level-off: level segment of >= 20 s (PRU VFE convention).
# - DEP (climb): level segments ending before TOC; ARR (descent): starting after
#   TOD. Without TOC/TOD, all segments within 200 NM are used.
# - Band: distance of the level segment start (end if start has no position)
#   from the ARP, in NM.

library(tidyverse)
library(arrow)

VFE_BANDS <- c(0, 20, 40, 60, 100, 120, 150, 200)
VFE_MIN_LEVEL_S <- 20

band_label <- function(d) {
  cut(d, VFE_BANDS, right = FALSE, labels = sprintf("%03d-%03d", head(VFE_BANDS, -1), VFE_BANDS[-1]))
}

dist_nm <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 3440.065 * asin(sqrt(pmin(1, a)))
}

as_utc <- function(df) mutate(df, across(where(is.POSIXct), \(x) with_tz(x, "UTC")))

#' OSN flights departing from / arriving at an airport
#'
#' Flights with ADEP == ADES (returns, circuits) are excluded: they have no
#' unique climb/descent attribution.
vfe_flights <- function(flight_list, apt, period) {
  bind_rows(
    flight_list |>
      filter(DOF >= period[1], DOF <= period[2], ADEP == apt) |>
      select(ID, ICAO24, FLT_ID, ADEP, ADES, FIRST_SEEN, LAST_SEEN) |>
      collect() |>
      mutate(DIR = "DEP"),
    flight_list |>
      filter(DOF >= period[1], DOF <= period[2], ADES == apt) |>
      select(ID, ICAO24, FLT_ID, ADEP, ADES, FIRST_SEEN, LAST_SEEN) |>
      collect() |>
      mutate(DIR = "ARR")
  ) |>
    filter(coalesce(ADEP != ADES, TRUE)) |>
    select(-ADEP, -ADES) |>
    as_utc() |>
    mutate(APT = apt, .before = 1)
}

#' OSN events needed for VFE, with distance to ARP
vfe_events <- function(flight_events, flights, arp, period) {
  types <- c(
    "level-start", "level-end", "top-of-climb", "top-of-descent",
    "level-off-climb-start", "level-off-descent-start",
    "ATOT", "ALDT", "first_seen", "last_seen"
  )
  days <- format(seq(as.Date(period[1]) - 1, as.Date(period[2]) + 1, by = "day"))

  flight_events |>
    filter(
      dof %in% days,
      flight_id %in% flights$ID,
      type %in% types | grepl("^xing-", type)
    ) |>
    select(flight_id, type, event_time, latitude, longitude, altitude, info) |>
    collect() |>
    as_utc() |>
    filter(!is.na(event_time)) |>
    mutate(DIST = dist_nm(latitude, longitude, arp$ARP_LAT, arp$ARP_LON))
}

#' Pair LVL_START / LVL_END into level segments
#'
#' Returns segments plus a pairing summary (attribute "pairing").
vfe_level_segments <- function(events, flights) {
  lv <- events |>
    filter(type %in% c("level-start", "level-end")) |>
    arrange(flight_id, event_time, desc(type == "level-start")) |>
    mutate(
      nxt_type = lead(type), nxt_time = lead(event_time),
      nxt_dist = lead(DIST), nxt_lat = lead(latitude), nxt_lon = lead(longitude), nxt_alt = lead(altitude),
      prv_type = lag(type),
      .by = flight_id
    )

  pairing <- lv |>
    summarise(
      n_start = sum(type == "level-start"),
      n_end = sum(type == "level-end"),
      start_without_end = sum(type == "level-start" & coalesce(nxt_type, "") != "level-end"),
      end_without_start = sum(type == "level-end" & coalesce(prv_type, "") != "level-start")
    )

  phase <- events |>
    summarise(
      T_TOC = min(event_time[type == "top-of-climb"]),
      T_TOD = max(event_time[type == "top-of-descent"]),
      .by = flight_id
    ) |>
    suppressWarnings() |>
    mutate(across(c(T_TOC, T_TOD), \(x) if_else(is.infinite(x), NA, x)))

  segs <- lv |>
    filter(type == "level-start", nxt_type == "level-end") |>
    transmute(
      ID = flight_id,
      T_START = event_time, T_END = nxt_time,
      DUR_S = as.numeric(T_END - T_START, units = "secs"),
      LVL_FT = altitude,
      DIST_START = DIST, DIST_END = nxt_dist,
      DIST_BAND_REF = coalesce(DIST_START, DIST_END),
      LEVEL_NM = dist_nm(latitude, longitude, nxt_lat, nxt_lon),
      LAT = coalesce(latitude, nxt_lat), LON = coalesce(longitude, nxt_lon)
    ) |>
    inner_join(flights |> select(ID, APT, DIR), by = "ID") |>
    left_join(phase, by = c("ID" = "flight_id")) |>
    filter(
      if_else(DIR == "DEP", is.na(T_TOC) | T_END <= T_TOC, is.na(T_TOD) | T_START >= T_TOD)
    ) |>
    mutate(BAND = band_label(DIST_BAND_REF))

  attr(segs, "pairing") <- pairing
  segs
}

#' Per-flight observation profile (cohorts)
#'
#' Only airborne flights are kept: OSN records without any airborne event
#' (ground vehicles, towing, taxi-only records) are excluded and reported as
#' NON_AIRBORNE.
#' CLOSEST_NM / FARTHEST_NM: distance span to the ARP covered by positioned OSN
#' events of the flight (0 if the flight's own ATOT/ALDT is present). A band
#' counts as observed if it overlaps this span; gaps inside the span (OSN
#' dropouts) are not detected.
#' Cohort (closest observation): A = within 20 NM, B = 20-60 NM,
#' C = 60-200 NM, D = no observation within 200 NM.
vfe_cohorts <- function(events, flights) {
  airborne_types <- c(
    "ATOT", "ALDT", "top-of-climb", "top-of-descent", "level-start", "level-end"
  )
  airborne <- events |>
    filter(type %in% airborne_types | grepl("^xing-", type)) |>
    distinct(flight_id) |>
    pull()

  per_flight <- events |>
    filter(flight_id %in% airborne) |>
    inner_join(flights |> select(ID, DIR), by = c("flight_id" = "ID")) |>
    mutate(
      DIST = if_else((DIR == "DEP" & type == "ATOT") | (DIR == "ARR" & type == "ALDT"), 0, DIST)
    ) |>
    filter(!is.na(DIST)) |>
    summarise(CLOSEST_NM = min(DIST), FARTHEST_NM = max(DIST), .by = flight_id)

  band_lo <- head(VFE_BANDS, -1)
  band_hi <- VFE_BANDS[-1]

  flights |>
    select(APT, ID, DIR) |>
    left_join(per_flight, by = c("ID" = "flight_id")) |>
    mutate(
      COHORT = case_when(
        !ID %in% airborne ~ "X: not airborne",
        is.na(CLOSEST_NM) | CLOSEST_NM >= 200 ~ "D: none within 200 NM",
        CLOSEST_NM < 20 ~ "A: within 20 NM",
        CLOSEST_NM < 60 ~ "B: 20-60 NM",
        .default = "C: 60-200 NM"
      ),
      BANDS_OBS = map2(CLOSEST_NM, FARTHEST_NM, \(lo, hi) {
        if (is.na(lo)) return(character())
        as.character(band_label(band_lo[band_lo < hi & band_hi > lo]))
      })
    )
}

#' Band table: flights observed, level-offs, level time (per flight median/IQR)
vfe_band_table <- function(segs, cohorts) {
  obs <- cohorts |>
    select(APT, ID, DIR, BANDS_OBS) |>
    unnest_longer(BANDS_OBS, values_to = "BAND") |>
    filter(!is.na(BAND))

  lo <- segs |>
    filter(DUR_S >= VFE_MIN_LEVEL_S, !is.na(BAND)) |>
    mutate(BAND = as.character(BAND)) |>
    summarise(N_LO = n(), LVL_S = sum(DUR_S), LVL_NM = sum(LEVEL_NM, na.rm = TRUE), .by = c(APT, ID, DIR, BAND))

  obs |>
    left_join(lo, by = c("APT", "ID", "DIR", "BAND")) |>
    mutate(across(c(N_LO, LVL_S, LVL_NM), \(x) coalesce(x, 0))) |>
    summarise(
      FLIGHTS_OBS = n(),
      FLIGHTS_LO = sum(N_LO > 0),
      # Distribution over flights with at least one level-off in the band
      LVL_MIN_P25 = if (any(N_LO > 0)) quantile(LVL_S[N_LO > 0], 0.25) / 60 else NA_real_,
      LVL_MIN_MED = if (any(N_LO > 0)) median(LVL_S[N_LO > 0]) / 60 else NA_real_,
      LVL_MIN_P75 = if (any(N_LO > 0)) quantile(LVL_S[N_LO > 0], 0.75) / 60 else NA_real_,
      LVL_MIN_SUM = sum(LVL_S) / 60,
      LVL_MIN_MEAN = mean(LVL_S) / 60,
      # Computed last: summarise() overwrites N_LO for later expressions
      N_LO = sum(N_LO),
      .by = c(APT, DIR, BAND)
    ) |>
    relocate(N_LO, .after = FLIGHTS_LO) |>
    arrange(APT, DIR, desc(BAND))
}

#' Roll-up per airport and direction, by cohort
vfe_rollup <- function(segs, cohorts) {
  lo <- segs |>
    filter(DUR_S >= VFE_MIN_LEVEL_S, !is.na(BAND)) |>
    summarise(N_LO = n(), LVL_S = sum(DUR_S), LVL_NM = sum(LEVEL_NM, na.rm = TRUE), .by = ID)

  cohorts |>
    filter(!str_starts(COHORT, "X")) |>
    left_join(lo, by = "ID") |>
    mutate(across(c(N_LO, LVL_S, LVL_NM), \(x) coalesce(x, 0))) |>
    summarise(
      FLIGHTS = n(),
      SHARE_NO_LO = mean(N_LO == 0),
      LVL_MIN_MEAN = mean(LVL_S) / 60,
      LVL_NM_MEAN = mean(LVL_NM),
      LVL_MIN_MED_LO = median(LVL_S[N_LO > 0]) / 60,
      .by = c(APT, DIR, COHORT)
    ) |>
    arrange(APT, DIR, COHORT)
}

#' Map of level-offs and TOC/TOD around an airport, with 20-200 NM rings
#'
#' @param lo Level-offs with DIR, LAT, LON.
#' @param tt TOC (DEP) / TOD (ARR) with DIR, LAT, LON.
#' @param arp One-row ARP table (ARP_LAT, ARP_LON).
vfe_map <- function(lo, tt, arp) {
  rings <- expand_grid(R = VFE_BANDS[-1], a = seq(0, 2 * pi, length.out = 181)) |>
    mutate(
      LAT = arp$ARP_LAT + R / 60 * sin(a),
      LON = arp$ARP_LON + R / 60 * cos(a) / cos(arp$ARP_LAT * pi / 180)
    )
  pts <- bind_rows(
    lo |> transmute(DIR, LAT, LON, LAYER = "Level-offs (>= 20 s)"),
    tt |> transmute(DIR, LAT, LON, LAYER = "TOC (DEP) / TOD (ARR)")
  )

  ggplot(pts, aes(LON, LAT)) +
    geom_path(data = rings, aes(group = R), colour = "grey70", linewidth = 0.2) +
    geom_point(size = 0.05, alpha = 0.15) +
    facet_grid(DIR ~ LAYER) +
    coord_quickmap(xlim = range(rings$LON), ylim = range(rings$LAT)) +
    labs(x = "LON", y = "LAT")
}
