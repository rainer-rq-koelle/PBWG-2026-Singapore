# Turnaround study: layer 3 (classification and metrics) on the analytic datasets
# built by R/turnaround.R. All times UTC; durations and delays in minutes, signed
# (negative delay = early).

library(dplyr)

# Exploratory thresholds (scheduled ground time, min), see TN-02 section 6
ta_thresholds <- list(
  eur_turn_max  = 240,   # EUR-EUR: rotation if STT <= 240
  intl_turn_max = 360,   # INTL: rotation if STT <= 360
  park_min      = 420,   # STT > 420 or overnight: parked / not a turn
  mtt_quantile  = 0.05   # empirical minimum turn time per class x aircraft type
)

# Turnaround class from the remote class of the inbound and outbound leg
ta_class <- function(arr_class, dep_class) {
  case_when(
    is.na(dep_class)                            ~ "no-dep",
    arr_class != "INTL" & dep_class != "INTL"   ~ "EUR-EUR",
    arr_class == "INTL" & dep_class == "INTL"   ~ "INTL-INTL",
    TRUE                                        ~ "mixed"
  )
}

# Hub time zones (IANA; DST handled by tzdb). Local time is a descriptor only:
# all analytics run in UTC. Extend when adding hubs (incl. partner airports).
hub_tz <- c(
  EGLL = "Europe/London", EHAM = "Europe/Amsterdam", EDDF = "Europe/Berlin",
  EDDM = "Europe/Berlin", LFPG = "Europe/Paris", LSZH = "Europe/Zurich",
  LTFM = "Europe/Istanbul", LIRF = "Europe/Rome", LEMD = "Europe/Madrid",
  LPPT = "Europe/Lisbon"
)

# Local calendar date at the hub (per-row tz; vectorised over hubs)
local_date <- function(t, hub) {
  out <- as.Date(rep(NA_character_, length(t)))
  for (h in unique(hub)) {
    i <- which(hub == h)
    out[i] <- as.Date(format(t[i], "%Y-%m-%d", tz = hub_tz[[h]]))
  }
  out
}

local_hour <- function(t, hub) {
  out <- rep(NA_integer_, length(t))
  for (h in unique(hub)) {
    i <- which(hub == h)
    out[i] <- as.integer(format(t[i], "%H", tz = hub_tz[[h]]))
  }
  out
}

ta_classify <- function(tr, th = ta_thresholds) {
  if (any(!tr$HUB %in% names(hub_tz))) stop("Missing hub_tz for: ",
    paste(setdiff(unique(tr$HUB), names(hub_tz)), collapse = ", "))
  tr |>
    mutate(
      TA_CLASS = ta_class(ARR_CLASS, DEP_CLASS),
      STT = GROUND_SCH_MIN,
      ATT = GROUND_ACT_MIN,
      # Descriptors only: overnight in local time; classification uses STT
      OVERNIGHT_LT = local_date(AOBT, HUB) > local_date(AIBT, HUB),
      AIBT_LT_HOUR = local_hour(AIBT, HUB),
      TURN_TYPE = case_when(
        PAIR_QC != "next-dep"                              ~ "no-turn",
        is.na(STT) | is.na(ATT) | STT <= 0 | ATT <= 0       ~ "invalid",
        STT > th$park_min                                  ~ "parked",
        TA_CLASS == "EUR-EUR" & STT <= th$eur_turn_max     ~ "turn",
        TA_CLASS != "EUR-EUR" & STT <= th$intl_turn_max    ~ "turn",
        TRUE                                               ~ "long-turn"
      )
    )
}

# Turnaround metrics (local, at the hub)
#   ARR_DLY  = AIBT - SIBT            arrival (in-block) delay
#   DEP_DLY  = AOBT - SOBT            departure (off-block) delay
#   TT_DEV   = ATT - STT              turnaround time deviation
#   ABSORBED = ARR_DLY - DEP_DLY      delay absorbed (>0) or added (<0) on the ground
#   MTT      = empirical minimum turn time (class x type quantile of ATT, turns only)
#   SLACK    = STT - MTT              scheduled buffer
#   PROP_EXP = max(0, ARR_DLY - SLACK)  delay expected to propagate given the buffer
#   PROP_OBS = min(max(ARR_DLY, 0), max(DEP_DLY, 0))  delay carried over (observed, upper bound)
ta_metrics <- function(tr, th = ta_thresholds) {
  tr <- ta_classify(tr, th)
  mtt <- tr |>
    filter(TURN_TYPE == "turn") |>
    group_by(TA_CLASS, TYPE) |>
    summarise(N_TYPE = n(), MTT = quantile(ATT, th$mtt_quantile, names = FALSE), .groups = "drop")
  tr |>
    left_join(mtt, by = c("TA_CLASS", "TYPE")) |>
    mutate(
      ARR_DLY  = as.numeric(difftime(AIBT, SIBT, units = "mins")),
      DEP_DLY  = as.numeric(difftime(AOBT, SOBT, units = "mins")),
      TT_DEV   = ATT - STT,
      ABSORBED = ARR_DLY - DEP_DLY,
      SLACK    = STT - MTT,
      PROP_EXP = pmax(0, ARR_DLY - SLACK),
      PROP_OBS = pmin(pmax(ARR_DLY, 0), pmax(DEP_DLY, 0)),
      ARR_LATE15 = ARR_DLY > 15,
      DEP_LATE15 = DEP_DLY > 15
    )
}

# Scheduled vs planned vs actual block time on legs with APDF at both ends and an NM link.
# Uses the build object (ta_build() return) for NM fields.
ta_timeline <- function(legs, nm_fields) {
  legs |>
    filter(IN_PERIOD, REMOTE_SRC == "APDF") |>
    inner_join(nm_fields, by = "UID") |>
    mutate(DIR = if_else(LEG == "IN", paste("Into", HUB), paste("Out of", HUB))) |>
    group_by(HUB, DIR) |>
    summarise(
      N             = n(),
      SCHED_BLOCK   = median(as.numeric(difftime(SIBT, SOBT, units = "mins")), na.rm = TRUE),
      M1_OBT_TO_LDG = median(TAXI_TIME_1 + FLT_DUR_1, na.rm = TRUE),
      ACT_BLOCK     = median(as.numeric(difftime(AIBT, AOBT, units = "mins")), na.rm = TRUE),
      TAXI_IN       = median(as.numeric(difftime(AIBT, ALDT, units = "mins")), na.rm = TRUE),
      FLOWN_MINUS_FILED = median(FLT_DUR_3 - FLT_DUR_1, na.rm = TRUE),
      DEP_DLY       = median(as.numeric(difftime(AOBT, SOBT, units = "mins")), na.rm = TRUE),
      ARR_DLY       = median(as.numeric(difftime(AIBT, SIBT, units = "mins")), na.rm = TRUE),
      .groups = "drop"
    )
}

# Propagation chain: upstream departure -> hub arrival -> hub departure -> downstream arrival
#   D0 = AOBT_up - SOBT_up      upstream departure delay   (remote)
#   D1 = AIBT_hub - SIBT_hub    hub arrival delay           (= ARR_DLY)
#   D2 = AOBT_hub - SOBT_hub    hub departure delay         (= DEP_DLY)
#   D3 = AIBT_dn - SIBT_dn      downstream arrival delay    (remote)
#   REC_UP = D0 - D1  recovered on the inbound leg (block buffer)
#   REC_HUB = D1 - D2 absorbed at the hub (= ABSORBED)
#   REC_DN = D2 - D3  recovered on the outbound leg
ta_chain <- function(tm, legs) {
  up <- legs |> select(ARR_UID = UID, UP_SRC = REMOTE_SRC, UP_SOBT = SOBT, UP_AOBT = AOBT)
  dn <- legs |> select(DEP_UID = UID, DN_SRC = REMOTE_SRC, DN_AIBT = AIBT, DN_SIBT = SIBT)
  tm |>
    left_join(up, by = "ARR_UID") |>
    left_join(dn, by = "DEP_UID") |>
    mutate(
      D0 = as.numeric(difftime(UP_AOBT, UP_SOBT, units = "mins")),
      D1 = ARR_DLY,
      D2 = DEP_DLY,
      D3 = as.numeric(difftime(DN_AIBT, DN_SIBT, units = "mins")),
      REC_UP  = D0 - D1,
      REC_HUB = D1 - D2,
      REC_DN  = D2 - D3,
      CHAIN_SRC = case_when(
        UP_SRC == "APDF" & DN_SRC == "APDF"                         ~ "APDF",
        UP_SRC %in% c("APDF", "NM") & DN_SRC %in% c("APDF", "NM")   ~ "with-NM",
        TRUE                                                        ~ "incomplete"
      )
    )
}

# Summary of one link of the chain: delay in (x) -> delay out (y)
link_summary <- function(x, y, late = 15) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) == 0) return(tibble())
  tibble(
    N          = length(x),
    IN_MED     = median(x),
    OUT_MED    = median(y),
    REC_MED    = median(x - y),
    # Of the flights late (> 15) at the start: share still late at the end
    LATE_CARRY = sum(x > late & y > late) / sum(x > late),
    # Positive delay minutes at the end per positive delay minute at the start
    DLY_RATIO  = sum(pmax(y, 0)) / sum(pmax(x, 0)),
    # Slope of y on x for late starts (marginal propagation per minute of delay)
    SLOPE_LATE = if (sum(x > late) >= 30) unname(coef(lm(y[x > late] ~ x[x > late]))[2]) else NA_real_
  )
}

ta_chain_summary <- function(ch, by = c("HUB", "TA_CLASS")) {
  ch <- ch |> filter(TURN_TYPE == "turn")
  links <- list(
    "1 upstream leg (D0->D1)"  = c("D0", "D1"),
    "2 hub turn (D1->D2)"      = c("D1", "D2"),
    "3 downstream leg (D2->D3)" = c("D2", "D3"),
    "chain (D0->D3)"           = c("D0", "D3")
  )
  imap(links, \(v, nm) {
    src_col <- switch(substr(nm, 1, 1), "1" = "UP_SRC", "2" = NULL, "3" = "DN_SRC", "c" = "CHAIN_SRC")
    g <- c(by, src_col)
    ch |>
      group_by(across(all_of(g))) |>
      reframe(link_summary(.data[[v[1]]], .data[[v[2]]])) |>
      rename(any_of(c(SRC = "UP_SRC", SRC = "DN_SRC", SRC = "CHAIN_SRC"))) |>
      mutate(LINK = nm, .before = 1)
  }) |>
    list_rbind() |>
    mutate(SRC = coalesce(SRC, "hub")) |>
    filter(SRC %in% c("hub", "APDF", "NM", "with-NM"))
}

# Summary per class: on-time shares, absorption and propagation
ta_summary <- function(tm, by = "TA_CLASS") {
  tm |>
    filter(TURN_TYPE == "turn") |>
    group_by(across(all_of(by))) |>
    summarise(
      N            = n(),
      STT_MED      = median(STT),
      ATT_MED      = median(ATT),
      SLACK_MED    = median(SLACK, na.rm = TRUE),
      ARR_DLY_MED  = median(ARR_DLY),
      DEP_DLY_MED  = median(DEP_DLY),
      ARR_OTP15    = mean(!ARR_LATE15),
      DEP_OTP15    = mean(!DEP_LATE15),
      # Of the arrivals > 15 min late: share that also departed > 15 min late
      LATE_CARRY   = sum(ARR_LATE15 & DEP_LATE15) / sum(ARR_LATE15),
      # Of the departures > 15 min late: share preceded by a > 15 min late arrival
      DEP_REACT    = sum(ARR_LATE15 & DEP_LATE15) / sum(DEP_LATE15),
      # Delay minutes: share of positive arrival delay recovered on the ground
      ABSORB_RATIO = 1 - sum(PROP_OBS) / sum(pmax(ARR_DLY, 0)),
      .groups = "drop"
    )
}
