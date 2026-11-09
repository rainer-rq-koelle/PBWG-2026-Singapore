# TN-03 – Trajectory milestones (contract points) from NM point profiles
# Range rings D<r>/A<r> around the ARP, TOC/TOD, and FIR crossings (R/tbo-fir.R)
# combined in one long table: ID, MODEL, MST, TIME, LAT, LON, FL, SOURCE.
# Timestamps are UTC.

library(dplyr)
library(tidyr)
library(purrr)

# ARP for the TN-03 development airports (data/ref-airports-ARP.csv lacks them)
tbo_arp <- tibble::tribble(
  ~ICAO,    ~ARP_LAT,   ~ARP_LON,
  "EGLL", 51.470750,  -0.459909,
  "EDDM", 48.353783,  11.786086,
  "EHAM", 52.308613,   4.763889,
  "LEMD", 40.471926,  -3.562640,
  "LTFM", 41.262222,  28.727778,
  "EDDF", 50.033333,   8.570556
)

# Linear interpolation of the first crossing of `level` by the series `d`
# (increasing = TRUE: first time d >= level; FALSE: last time d falls to <= level).
cross_at <- function(d, t, fl, lat, lon, level, outbound = TRUE) {
  n <- length(d)
  if (n < 2) return(tibble())
  i <- if (outbound) which(d[-n] < level & d[-1] >= level)[1]
       else          tail(which(d[-n] > level & d[-1] <= level), 1)
  if (length(i) == 0 || is.na(i)) return(tibble())
  f <- (level - d[i]) / (d[i + 1] - d[i])
  tibble(TIME = t[i] + f * (t[i + 1] - t[i]),
         LAT = lat[i] + f * (lat[i + 1] - lat[i]),
         LON = lon[i] + f * (lon[i + 1] - lon[i]),
         FL  = fl[i] + f * (fl[i + 1] - fl[i]))
}

ring_milestones <- function(prof, arp = tbo_arp, rings = c(40, 100, 200)) {
  prof |>
    left_join(rename(arp, ADEP = ICAO, DEP_LAT = ARP_LAT, DEP_LON = ARP_LON), by = "ADEP") |>
    left_join(rename(arp, ADES = ICAO, ARR_LAT = ARP_LAT, ARR_LON = ARP_LON), by = "ADES") |>
    arrange(MODEL, FLIGHT_ID, TIME_OVER) |>
    mutate(D_DEP = geosphere::distHaversine(cbind(LONGITUDE, LATITUDE), cbind(DEP_LON, DEP_LAT)) / 1852,
           D_ARR = geosphere::distHaversine(cbind(LONGITUDE, LATITUDE), cbind(ARR_LON, ARR_LAT)) / 1852) |>
    group_by(MODEL, ID = FLIGHT_ID) |>
    group_modify(\(g, k) {
      lab <- \(x, m) if (nrow(x)) mutate(x, MST = m) else x
      dep <- map(rings, \(r) lab(cross_at(g$D_DEP, g$TIME_OVER, g$FLIGHT_LEVEL, g$LATITUDE, g$LONGITUDE, r, TRUE), paste0("D", r)))
      arr <- map(rings, \(r) lab(cross_at(g$D_ARR, g$TIME_OVER, g$FLIGHT_LEVEL, g$LATITUDE, g$LONGITUDE, r, FALSE), paste0("A", r)))
      list_rbind(c(dep, arr))
    }) |>
    ungroup() |>
    mutate(SOURCE = "ring")
}

# Simple TOC/TOD: first and last point at the maximum flight level of the profile.
# Placeholder for the VFE algorithm (R/vfe.R); a step climb to a higher level moves TOC.
mst_toc_tod <- function(prof) {
  prof |>
    group_by(MODEL, ID = FLIGHT_ID) |>
    arrange(TIME_OVER, .by_group = TRUE) |>
    filter(FLIGHT_LEVEL == max(FLIGHT_LEVEL)) |>
    slice(c(1, n())) |>
    mutate(MST = c("TOC", "TOD")) |>
    ungroup() |>
    transmute(MODEL, ID, MST, TIME = TIME_OVER, LAT = LATITUDE, LON = LONGITUDE,
              FL = FLIGHT_LEVEL, SOURCE = "vertical")
}

# FIR crossings (fir_crossings() output) into the same format
fir_as_milestones <- function(xc) {
  xc |> transmute(MODEL, ID, MST = paste0("FIR:", CROSSING), TIME, LAT, LON, FL, SOURCE = "fir")
}

mst_order <- c("D40", "D100", "D200", "TOC", "TOD", "A200", "A100", "A40")

build_milestones <- function(prof, xc) {
  bind_rows(ring_milestones(prof), mst_toc_tod(prof), fir_as_milestones(xc)) |>
    arrange(ID, MODEL, TIME)
}

# Contract view: planned (M1) vs actual (M3) per milestone
compare_milestones <- function(mst) {
  mst |>
    pivot_wider(id_cols = c(ID, MST, SOURCE), names_from = MODEL, values_from = c(TIME, LAT, LON, FL)) |>
    mutate(STATUS = case_when(!is.na(TIME_M1) & !is.na(TIME_M3) ~ "delivered",
                              is.na(TIME_M3) ~ "planned-only",
                              .default = "actual-only"),
           DT_MIN = as.numeric(difftime(TIME_M3, TIME_M1, units = "mins")),
           DFL = FL_M3 - FL_M1,
           DPOS_NM = geosphere::distHaversine(cbind(LON_M1, LAT_M1), cbind(LON_M3, LAT_M3)) / 1852)
}
