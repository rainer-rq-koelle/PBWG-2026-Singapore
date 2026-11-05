# TN-03 – Actual ring milestones at the international (INTL) end, with a source hierarchy
#
# Source order per ring milestone (first available wins), recorded in OBS:
#   partner   > osn (OPDI flight-list C40/C100) > cpf (NM CPF, guarded) > m3-observed > m3-extrapolated
# - osn: segment linked by ICAO24 + overlap with the NM flight window; linkage check = ring time must lie
#        inside [AOBT_3 - tol, ARVT_3 + tol] (observed flight window), otherwise rejected (OSN_REJECT).
# - cpf: ring crossing accepted only if the CPF points bracketing the ring are <= max_gap_s apart.
# - m3:  "extrapolated" if position and FL equal M1 (M3 = M1 time-shifted outside the NM area).
# Rings: 40 and 100 NM (OSN has no 200 NM). Timestamps are UTC.

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# Ring crossing with a gap guard; returns TIME plus the bracketing gap (s)
ring_cross_guarded <- function(prof, arp, rings = c(40, 100), side_airport) {
  prof |>
    left_join(rename(arp, APT = ICAO), by = setNames("APT", side_airport)) |>
    mutate(D = geosphere::distHaversine(cbind(LONGITUDE, LATITUDE), cbind(ARP_LON, ARP_LAT)) / 1852) |>
    group_by(ID = FLIGHT_ID) |>
    arrange(TIME_OVER, .by_group = TRUE) |>
    group_modify(\(g, k) {
      n <- nrow(g)
      if (n < 2) return(tibble())
      map(rings, \(r) {
        i <- if (side_airport == "ADES") tail(which(g$D[-n] > r & g$D[-1] <= r), 1)
             else                      which(g$D[-n] < r & g$D[-1] >= r)[1]
        if (length(i) == 0 || is.na(i)) return(tibble())
        f <- (r - g$D[i]) / (g$D[i + 1] - g$D[i])
        tibble(R = r, TIME = g$TIME_OVER[i] + f * (g$TIME_OVER[i + 1] - g$TIME_OVER[i]),
               GAP_S = as.numeric(difftime(g$TIME_OVER[i + 1], g$TIME_OVER[i], units = "secs")))
      }) |> list_rbind()
    }) |>
    ungroup()
}

# flights: ID, ICAO24, ADEP, ADES, INTL, END ("arr"/"dep" = INTL is ADES/ADEP), AOBT_3, ARVT_3
# rings_m: ring_milestones() output for M1 and M3 (MST A40/A100/D40/D100)
# osn_fl:  OPDI flight list rows (ICAO24, ADEP, ADES, FIRST_SEEN, LAST_SEEN, C40_*/C100_*)
# cpf:     NM CPF point profile (FLIGHT_ID, TIME_OVER, LONGITUDE, LATITUDE, ADEP, ADES)
# partner: optional tibble(ID, MST, TIME)
intl_ring_actuals <- function(flights, rings_m, osn_fl, cpf, arp, partner = NULL,
                              win_tol = minutes(30), max_gap_s = 300) {
  mst_of <- \(end, r) paste0(if_else(end == "arr", "A", "D"), r)

  # OSN with linkage check
  osn <- flights |>
    select(ID, ICAO24, INTL, END, AOBT_3, ARVT_3) |>
    inner_join(osn_fl, by = "ICAO24", relationship = "many-to-many") |>
    filter(FIRST_SEEN < ARVT_3 + hours(1), LAST_SEEN > AOBT_3 - hours(1),
           (END == "arr" & ADES == INTL) | (END == "dep" & ADEP == INTL)) |>
    transmute(ID, END, AOBT_3, ARVT_3,
              `40` = if_else(END == "arr", C40_ARR, C40_DEP),
              `100` = if_else(END == "arr", C100_ARR, C100_DEP)) |>
    pivot_longer(c(`40`, `100`), names_to = "R", values_to = "TIME") |>
    filter(!is.na(TIME)) |>
    mutate(MST = mst_of(END, R),
           OK = TIME >= AOBT_3 - win_tol & TIME <= ARVT_3 + win_tol) |>
    group_by(ID, MST) |>
    # several linked segments: keep a passing one; ambiguity (>1 passing) -> reject
    summarise(N_PASS = sum(OK), TIME = first(TIME[OK]), .groups = "drop") |>
    mutate(OBS = "osn", OSN_REJECT = N_PASS != 1) |>
    select(ID, MST, TIME, OBS, OSN_REJECT)

  # CPF with gap guard (INTL side only)
  cpf_side <- cpf |> semi_join(flights, by = c("FLIGHT_ID" = "ID"))
  cpf_r <- bind_rows(
    ring_cross_guarded(filter(cpf_side, FLIGHT_ID %in% flights$ID[flights$END == "arr"]), arp, side_airport = "ADES") |>
      mutate(MST = paste0("A", R)),
    ring_cross_guarded(filter(cpf_side, FLIGHT_ID %in% flights$ID[flights$END == "dep"]), arp, side_airport = "ADEP") |>
      mutate(MST = paste0("D", R))) |>
    filter(GAP_S <= max_gap_s) |>
    transmute(ID, MST, TIME, OBS = "cpf")

  # M3, flagged extrapolated when identical to M1
  m3 <- rings_m |>
    filter(MST %in% c("A40", "A100", "D40", "D100")) |>
    select(ID, MODEL, MST, TIME, LAT, LON, FL) |>
    pivot_wider(names_from = MODEL, values_from = c(TIME, LAT, LON, FL)) |>
    semi_join(flights, by = "ID") |>
    mutate(DPOS = geosphere::distHaversine(cbind(LON_M1, LAT_M1), cbind(LON_M3, LAT_M3)) / 1852,
           OBS = if_else(DPOS < 0.1 & abs(FL_M3 - FL_M1) < 1, "m3-extrapolated", "m3-observed")) |>
    transmute(ID, MST, TIME = TIME_M3, OBS)

  # keep only rings on the INTL side
  side <- flights |>
    transmute(ID, SIDE = if_else(END == "arr", "A", "D")) 
  cand <- bind_rows(
    if (!is.null(partner)) mutate(partner, OBS = "partner"),
    filter(osn, !OSN_REJECT) |> select(-OSN_REJECT),
    cpf_r, m3) |>
    inner_join(side, by = "ID") |>
    filter(substr(MST, 1, 1) == SIDE) |>
    mutate(RANK = match(OBS, c("partner", "osn", "cpf", "m3-observed", "m3-extrapolated")))

  list(
    actual = cand |> group_by(ID, MST) |> slice_min(RANK, n = 1, with_ties = FALSE) |> ungroup() |>
      select(ID, MST, TIME, OBS),
    candidates = cand,
    osn_rejected = filter(osn, OSN_REJECT)
  )
}
