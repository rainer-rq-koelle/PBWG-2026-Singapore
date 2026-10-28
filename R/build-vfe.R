# Build the exploratory VFE dataset for the PBWG reference airports
#
# Output: data/vfe/VFE-<APT>-segments-OSN-20250601-20250613.csv  (level segments)
#         data/vfe/VFE-cohorts|bands|rollup|tocod-OSN-20250601-20250613.csv
#         data/vfe/VFE-issues-OSN-20250601-20250613.csv           (data-engineering items)

source("R/vfe.R")
dir.create("data/vfe", showWarnings = FALSE)

# The flight-events extract ends on 2025-06-13 (flight list: 2025-06-14), so
# the VFE period is 1-13 June 2025.
period <- c("2025-06-01", "2025-06-13")
tag <- "OSN-20250601-20250613"

flight_list <- open_dataset("data/flight_list/opdi_flight_list/")
flight_events <- open_dataset("data/flight_events/opdi_flight_events/")
arp <- read_csv("data/ref-airports-ARP.csv", show_col_types = FALSE)

build_airport <- function(apt) {
  flights <- vfe_flights(flight_list, apt, period)
  events <- vfe_events(flight_events, flights, filter(arp, ICAO == apt), period)
  segs <- vfe_level_segments(events, flights)
  cohorts <- vfe_cohorts(events, flights)

  # Data-engineering checks on the OSN events
  kpi_lo <- events |>
    filter(type %in% c("level-off-climb-start", "level-off-descent-start")) |>
    mutate(
      DUR_S = as.numeric(str_match(info, '"duration_s":([0-9.]+)')[, 2]),
      DIST_NM = as.numeric(str_match(info, '"distance_nm":([0-9.]+)')[, 2])
    )
  issues <- tribble(
    ~ISSUE, ~N, ~DENOMINATOR,
    "LVL_START without LVL_END twin", attr(segs, "pairing")$start_without_end, attr(segs, "pairing")$n_start,
    "LVL_END without LVL_START twin", attr(segs, "pairing")$end_without_start, attr(segs, "pairing")$n_end,
    "Level segment without position (start and end)", sum(is.na(segs$DIST_BAND_REF)), nrow(segs),
    "KPI17/19 level-off with duration > 0 but distance_nm = 0", sum(kpi_lo$DUR_S > 0 & kpi_lo$DIST_NM == 0, na.rm = TRUE), nrow(kpi_lo),
    "KPI17/19 level-off event without position", sum(is.na(kpi_lo$latitude)), nrow(kpi_lo),
    "Ring crossing (xing-*nm) without event_time (source)", NA, NA,
    "200 NM ring crossing (D200/A200) not published", sum(events$type == "xing-200nm"), n_distinct(events$flight_id),
    "Flights without TOC (DEP) / TOD (ARR)",
    sum(!flights$ID[flights$DIR == "DEP"] %in% events$flight_id[events$type == "top-of-climb"]) +
      sum(!flights$ID[flights$DIR == "ARR"] %in% events$flight_id[events$type == "top-of-descent"]),
    nrow(flights)
  ) |>
    mutate(APT = apt, .before = 1)

  write_csv(segs, sprintf("data/vfe/VFE-%s-segments-%s.csv", apt, tag), na = "NA")

  list(
    cohorts = select(cohorts, -BANDS_OBS),
    band = vfe_band_table(segs, cohorts),
    rollup = vfe_rollup(segs, cohorts),
    issues = issues,
    toc_tod = events |>
      filter(type %in% c("top-of-climb", "top-of-descent"), !is.na(latitude)) |>
      inner_join(flights |> select(ID, APT, DIR), by = c("flight_id" = "ID")) |>
      filter((DIR == "DEP" & type == "top-of-climb") | (DIR == "ARR" & type == "top-of-descent")) |>
      select(APT, ID = flight_id, DIR, type, LAT = latitude, LON = longitude, ALT = altitude, DIST)
  )
}

apts <- c("EGLL", "SBGR", "KATL", "CYYZ", "VTBS", "WSSS", "RJAA", "ZBAA")
vfe <- map(set_names(apts), build_airport)

vfe_out <- map(c("cohorts", "band", "rollup", "issues", "toc_tod"), \(k) list_rbind(map(vfe, k))) |>
  set_names(c("cohorts", "band", "rollup", "issues", "toc_tod"))

write_csv(vfe_out$cohorts, sprintf("data/vfe/VFE-cohorts-%s.csv", tag), na = "NA")
write_csv(vfe_out$band, sprintf("data/vfe/VFE-bands-%s.csv", tag), na = "NA")
write_csv(vfe_out$rollup, sprintf("data/vfe/VFE-rollup-%s.csv", tag), na = "NA")
write_csv(vfe_out$toc_tod, sprintf("data/vfe/VFE-tocod-%s.csv", tag), na = "NA")
write_csv(vfe_out$issues, sprintf("data/vfe/VFE-issues-%s.csv", tag), na = "NA")

# Cross-cutting data-engineering items (not per airport)
fe_days <- flight_events |> distinct(dof) |> collect() |> pull()
fl_days <- flight_list |> distinct(DOF) |> collect() |> pull()
ev_no_time <- flight_events |>
  filter(dof >= "2025-06-01", dof <= "2025-06-13") |>
  summarise(n = n(), na = sum(is.na(event_time))) |>
  collect()

tribble(
  ~ITEM, ~DETAIL, ~SEVERITY,
  "Flight events missing for 2025-06-14",
  sprintf("Flight list has DOF 2025-06-14; flight events have no dof = 2025-06-14. Missing days: %s", paste(setdiff(fl_days, fe_days), collapse = ", ")),
  "high",
  "Events without event_time",
  sprintf("%s of %s events (%.1f%%) in 1-13 June 2025, mostly ring crossings and ALDT", ev_no_time$na, ev_no_time$n, 100 * ev_no_time$na / ev_no_time$n),
  "medium",
  "No 200 NM ring crossings",
  "xing-*nm events stop at 120 NM; D200/A200 needed for ASMA/DSMA-200 and VFE banding. TOC/TOD info carries d200_time, so the 200 NM radius is computed internally.",
  "medium",
  "KPI17/19 level-offs with distance_nm = 0",
  "duration_s > 0 but distance_nm = 0; share by airport in VFE-issues file, up to ~70% at some airports. Mostly descent. Level time is unaffected; level distance is not usable.",
  "medium",
  "KPI17/19 level-off events without position",
  "level-off-climb/descent-* events carry no latitude/longitude/altitude; position must be taken from the generic level-start/level-end events.",
  "low",
  "ADES missing in flight list",
  "ADES is NA for ~40% of flight-list rows in June 2025 (partly OSN segmentation); limits arrival-side selection, e.g. only 90 ZBAA arrivals.",
  "medium"
) |>
  write_csv(sprintf("data/vfe/OSN-data-engineering-items-%s.csv", tag))
