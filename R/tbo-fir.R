# TN-03 – FIR boundary milestones from NM airspace profiles (M1 = FTFM, M3 = CTFM)
#
# Rules
# - FIR and UIR of the same unit are merged (REGION = first 4 letters of AIRSPACE_ID),
#   so only lateral boundary crossings remain (FIR/UIR split is a vertical boundary).
# - Visits shorter than `min_dwell_s` are dropped (boundary clipping), neighbouring
#   visits of the same REGION are then merged again.
# - M1/M3 are matched per visit (k-th visit of a REGION), not per REGION name.
# Timestamps are UTC.

library(dplyr)
library(tidyr)
library(lubridate)

fir_extract <- function(con, wef, til, ids, chunk_size = 900) {
  purrr::map(c(M1 = "FTFM", M3 = "CTFM"), \(p) {
    tbl <- eurocontrol::airspace_profiles_tidy(con, wef = wef, til = til,
                                               airspace = "FIR", profile = p)
    split(ids, ceiling(seq_along(ids) / chunk_size)) |>
      purrr::map(\(chunk) tbl |> filter(ID %in% !!chunk) |> collect()) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind(names_to = "MODEL") |>
    mutate(across(c(ENTRY_TIME, EXIT_TIME), \(x) force_tz(x, "UTC")))
}

merge_runs <- function(x) {
  x |>
    group_by(ID, MODEL) |>
    arrange(ENTRY_TIME, .by_group = TRUE) |>
    mutate(RUN = cumsum(REGION != lag(REGION, default = ""))) |>
    group_by(ID, MODEL, RUN, REGION) |>
    summarise(ENTRY_TIME = first(ENTRY_TIME), ENTRY_LON = first(ENTRY_LON),
              ENTRY_LAT = first(ENTRY_LAT), ENTRY_FL = first(ENTRY_FL),
              EXIT_TIME = last(EXIT_TIME), EXIT_LON = last(EXIT_LON),
              EXIT_LAT = last(EXIT_LAT), EXIT_FL = last(EXIT_FL),
              .groups = "drop") |>
    select(-RUN)
}

fir_milestones <- function(asp, min_dwell_s = 60) {
  asp |>
    mutate(REGION = substr(AIRSPACE_ID, 1, 4)) |>
    merge_runs() |>
    filter(as.numeric(difftime(EXIT_TIME, ENTRY_TIME, units = "secs")) >= min_dwell_s) |>
    merge_runs() |>
    group_by(ID, MODEL) |>
    arrange(ENTRY_TIME, .by_group = TRUE) |>
    mutate(SEQ = row_number(), N_SEQ = n()) |>
    group_by(ID, MODEL, REGION) |>
    mutate(VISIT = row_number()) |>
    ungroup()
}

# Distinct crossings: each REGION counted once (first entry), re-entries along boundaries
# collapsed. A crossing is the transition PREV>REGION at the first entry into REGION.
# The first region of a flight is the departure (no crossing).
fir_crossings <- function(ms) {
  ms |>
    group_by(ID, MODEL, REGION) |>
    slice_min(ENTRY_TIME, n = 1, with_ties = FALSE) |>
    group_by(ID, MODEL) |>
    arrange(ENTRY_TIME, .by_group = TRUE) |>
    mutate(FROM = lag(REGION), CROSSING = paste(FROM, REGION, sep = ">")) |>
    ungroup() |>
    filter(!is.na(FROM)) |>
    select(ID, MODEL, CROSSING, FROM, TO = REGION, TIME = ENTRY_TIME,
           LON = ENTRY_LON, LAT = ENTRY_LAT, FL = ENTRY_FL)
}

fir_crossing_compare <- function(xc) {
  w <- xc |>
    pivot_wider(id_cols = c(ID, CROSSING, FROM, TO), names_from = MODEL,
                values_from = c(TIME, LON, LAT, FL))
  w |>
    mutate(STATUS = case_when(!is.na(TIME_M1) & !is.na(TIME_M3) ~ "matched",
                              is.na(TIME_M3) ~ "planned-only",
                              .default = "actual-only"),
           DT_MIN = as.numeric(difftime(TIME_M3, TIME_M1, units = "mins")),
           DFL = FL_M3 - FL_M1,
           DPOS_NM = geosphere::distHaversine(cbind(LON_M1, LAT_M1), cbind(LON_M3, LAT_M3)) / 1852)
}

fir_compare <- function(ms) {
  seqs <- ms |>
    group_by(ID, MODEL) |>
    summarise(SEQ_STR = paste(REGION, collapse = ">"), .groups = "drop") |>
    pivot_wider(names_from = MODEL, values_from = SEQ_STR, names_prefix = "SEQ_") |>
    mutate(SEQ_MATCH = !is.na(SEQ_M1) & SEQ_M1 == SEQ_M3)

  pts <- ms |>
    select(ID, MODEL, REGION, VISIT, SEQ, N_SEQ, ENTRY_TIME, EXIT_TIME, ENTRY_FL, EXIT_FL) |>
    pivot_wider(id_cols = c(ID, REGION, VISIT), names_from = MODEL,
                values_from = c(SEQ, N_SEQ, ENTRY_TIME, EXIT_TIME, ENTRY_FL, EXIT_FL)) |>
    mutate(
      MATCHED  = !is.na(ENTRY_TIME_M1) & !is.na(ENTRY_TIME_M3),
      # first entry = take-off, last exit = landing: flag so they can be separated from boundaries
      ROLE     = case_when(SEQ_M3 == 1 ~ "dep", SEQ_M3 == N_SEQ_M3 ~ "arr", .default = "enr"),
      DT_ENTRY = as.numeric(difftime(ENTRY_TIME_M3, ENTRY_TIME_M1, units = "mins")),
      DT_EXIT  = as.numeric(difftime(EXIT_TIME_M3, EXIT_TIME_M1, units = "mins"))
    )
  list(seq = seqs, pts = pts)
}
