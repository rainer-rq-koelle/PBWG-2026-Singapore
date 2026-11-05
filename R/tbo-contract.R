# TN-03 – Canonical trajectory-contract table
#
# One row per flight x contract point (milestone). Plan (REF) and actual (ACT) side by side, each with its source,
# so delivery measures are computed uniformly and any provider (NM, OSN, partner) can add rows or fill columns.
#
# Columns
#   UID, FLTID, ADEP, ADES, REGION_PAIR      flight identity (UID = SAM_ID in the EUR demo)
#   SEQ, MST, MST_CLASS, SIDE                contract point, order along the flight; MST_CLASS = ring|vertical|fir;
#                                            SIDE = EUR|INTL|ENR (whose airspace / observation domain)
#   REF_MODEL, REF_TIME, REF_LAT, REF_LON, REF_FL        reference (M1 now; M2 when RTFM available)
#   ACT_TIME, ACT_LAT, ACT_LON, ACT_FL, ACT_SRC, ACT_OBS  actual; ACT_OBS = observed|proxy|extrapolated|missing
#   STATUS                                   delivered|planned-only|actual-only|not-observed
#   DT_MIN, DFL, DPOS_NM                     offsets ACT - REF (only for STATUS = delivered)
#   DDT_MIN                                  change of DT since previous delivered point (segment gain/loss)
#
# ACT_SRC values: NM-M3, NM-CPF, OSN, PARTNER-<ISO3>; ACT_OBS maps them to observation quality.
# Timestamps are UTC.

library(dplyr)
library(tidyr)

act_obs_class <- function(src) {
  case_when(src %in% c("PARTNER", "NM-M3", "NM-CPF", "OSN") ~ "observed",
            src == "NM-M3-EXTRAP" ~ "extrapolated",
            src == "PROXY" ~ "proxy",
            .default = "missing")
}

# mst_cmp: compare_milestones() output (ID, MST, SOURCE, TIME_M1/M3, LAT_*, LON_*, FL_*)
# intl:    intl_ring_actuals()$actual (ID, MST, TIME, OBS) – overrides ACT at INTL rings
# flights: ID, AIRCRAFT_ID, ADEP, ADES, plus intl codes vector
contract_table <- function(mst_cmp, flights, intl_codes, intl = NULL) {
  ct <- mst_cmp |>
    left_join(select(flights, ID, FLTID = AIRCRAFT_ID, ADEP, ADES), by = "ID") |>
    mutate(
      MST_CLASS = case_when(SOURCE == "fir" ~ "fir", MST %in% c("TOC", "TOD") ~ "vertical", .default = "ring"),
      SIDE = case_when(
        MST_CLASS == "ring" & substr(MST, 1, 1) == "D" & ADEP %in% intl_codes ~ "INTL",
        MST_CLASS == "ring" & substr(MST, 1, 1) == "A" & ADES %in% intl_codes ~ "INTL",
        MST_CLASS == "ring" ~ "EUR",
        # FIR crossing into a non-EUR region (ICAO prefixes as in TN-02) = outside the NM observation domain
        MST_CLASS == "fir" & !grepl("^(E|L|GC|GM|BI|BK|UD|UG|UK)", sub(".*>", "", MST)) ~ "INTL",
        .default = "ENR"),
      REGION_PAIR = paste(if_else(ADEP %in% intl_codes, "INTL", "EUR"),
                          if_else(ADES %in% intl_codes, "INTL", "EUR"), sep = "-"),
      REF_MODEL = "M1",
      ACT_SRC = if_else(is.na(TIME_M3), NA_character_, "NM-M3"),
      # M3 identical to M1 outside the NM area = time-shifted plan, not an observation
      ACT_SRC = if_else(SIDE == "INTL" & !is.na(DPOS_NM) & DPOS_NM < 0.1 & abs(DFL) < 1, "NM-M3-EXTRAP", ACT_SRC)) |>
    transmute(UID = ID, FLTID, ADEP, ADES, REGION_PAIR, MST, MST_CLASS, SIDE, REF_MODEL,
              REF_TIME = TIME_M1, REF_LAT = LAT_M1, REF_LON = LON_M1, REF_FL = FL_M1,
              ACT_TIME = TIME_M3, ACT_LAT = LAT_M3, ACT_LON = LON_M3, ACT_FL = FL_M3, ACT_SRC)

  if (!is.null(intl)) {
    src_map <- c(partner = "PARTNER", osn = "OSN", cpf = "NM-CPF",
                 "m3-observed" = "NM-M3", "m3-extrapolated" = "NM-M3-EXTRAP")
    ov <- intl |> transmute(UID = ID, MST, I_TIME = TIME, I_SRC = unname(src_map[OBS]))
    ct <- ct |>
      left_join(ov, by = c("UID", "MST")) |>
      mutate(swap = !is.na(I_SRC) & I_SRC %in% c("PARTNER", "OSN", "NM-CPF"),
             ACT_TIME = if_else(swap, I_TIME, ACT_TIME),
             # ring sources give time only; position/FL of the ring crossing are not reported
             across(c(ACT_LAT, ACT_LON, ACT_FL), \(x) if_else(swap, NA_real_, x)),
             ACT_SRC = if_else(swap, I_SRC, ACT_SRC)) |>
      select(-I_TIME, -I_SRC, -swap)
  }

  ct |>
    mutate(
      ACT_OBS = act_obs_class(ACT_SRC),
      STATUS = case_when(is.na(REF_TIME) & !is.na(ACT_TIME) ~ "actual-only",
                         !is.na(REF_TIME) & is.na(ACT_TIME) ~ "planned-only",
                         ACT_OBS != "observed" ~ "not-observed",
                         .default = "delivered"),
      DT_MIN  = if_else(STATUS == "delivered", as.numeric(difftime(ACT_TIME, REF_TIME, units = "mins")), NA_real_),
      DFL     = if_else(STATUS == "delivered", ACT_FL - REF_FL, NA_real_),
      DPOS_NM = if_else(STATUS == "delivered",
                        geosphere::distHaversine(cbind(REF_LON, REF_LAT), cbind(ACT_LON, ACT_LAT)) / 1852, NA_real_)) |>
    group_by(UID) |>
    arrange(coalesce(REF_TIME, ACT_TIME), .by_group = TRUE) |>
    mutate(SEQ = row_number(), PREV_DT = lag(DT_MIN)) |>
    fill(PREV_DT, .direction = "down") |>
    mutate(DDT_MIN = if_else(STATUS == "delivered", DT_MIN - PREV_DT, NA_real_)) |>
    select(-PREV_DT) |>
    ungroup() |>
    relocate(SEQ, .after = REGION_PAIR)
}

# Flight-level delivery summary (one row per flight)
contract_summary <- function(ct) {
  ct |>
    group_by(UID, FLTID, ADEP, ADES, REGION_PAIR) |>
    summarise(
      N_POINTS = n(),
      DELIVERED = sum(STATUS == "delivered"),
      NOT_OBSERVED = sum(STATUS == "not-observed"),
      DEVIATED = sum(STATUS %in% c("planned-only", "actual-only")),
      OBS_SHARE = DELIVERED / N_POINTS,
      DT_FIRST = first(DT_MIN[STATUS == "delivered"]),
      DT_LAST = last(DT_MIN[STATUS == "delivered"]),
      DT_CHANGE = DT_LAST - DT_FIRST,
      INTL_OBS = any(SIDE == "INTL" & STATUS == "delivered"),
      .groups = "drop")
}
