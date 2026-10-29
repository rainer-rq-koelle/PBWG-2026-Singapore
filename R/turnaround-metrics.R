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

# Turnaround metrics (local, at the hub). All delays signed (negative = early).
#   ARR_DLY  = AIBT - SIBT            arrival (in-block) delay
#   DEP_DLY  = AOBT - SOBT            departure (off-block) delay
#   DDI_G    = ATT - STT              Delay Difference Indicator - Group (AIU/CODA naming)
#   GTO_FLAG = ATT > STT              Ground Time Overshoot (share over a set of turns = GTO)
#   TDI      = DEP_DLY - max(0, ARR_DLY)  Turnaround Delay Indicator (early arrivals neutralised)
#   ABSORBED = ARR_DLY - DEP_DLY      delay absorbed (>0) or added (<0) on the ground (= -DDI_G)
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
      DDI_G    = ATT - STT,
      GTO_FLAG = ATT > STT,
      TDI      = DEP_DLY - pmax(0, ARR_DLY),
      ABSORBED = ARR_DLY - DEP_DLY,
      SLACK    = STT - MTT,
      PROP_EXP = pmax(0, ARR_DLY - SLACK),
      PROP_OBS = pmin(pmax(ARR_DLY, 0), pmax(DEP_DLY, 0)),
      ARR_LATE15 = ARR_DLY > 15,
      DEP_LATE15 = DEP_DLY > 15,
      # Operating day (local, from OPDAY_START_H) of the scheduled arrival
      OPDAY = op_day(SIBT, HUB)
    )
}

# Local operating day: calendar date of (t - OPDAY_START_H) in the hub's time zone.
# Reference is the scheduled time (SIBT/SOBT), so delay does not move a flight to another day.
OPDAY_START_H <- 4
op_day <- function(t, hub, start_h = OPDAY_START_H) local_date(t - start_h * 3600, hub)

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
#   DDI_F_UP = D1 - D0, DDI_F_DN = D3 - D2  Delay Difference Indicator - Flight per leg (> 0: delay added)
#   BTO_UP / BTO_DN = actual block time > scheduled block time (share over flights = BTO)
#   ATFM_ARR / ATFM_DEP = NM ATFM delay of the inbound / outbound flight (0 if not regulated)
ta_chain <- function(tm, legs, attr = NULL) {
  up <- legs |> select(ARR_UID = UID, UP_SRC = REMOTE_SRC, UP_SOBT = SOBT, UP_AOBT = AOBT)
  dn <- legs |> select(DEP_UID = UID, DN_SRC = REMOTE_SRC, DN_AIBT = AIBT, DN_SIBT = SIBT)
  ch <- tm |>
    left_join(up, by = "ARR_UID") |>
    left_join(dn, by = "DEP_UID") |>
    mutate(
      D0 = as.numeric(difftime(UP_AOBT, UP_SOBT, units = "mins")),
      D1 = ARR_DLY,
      D2 = DEP_DLY,
      D3 = as.numeric(difftime(DN_AIBT, DN_SIBT, units = "mins")),
      DDI_F_UP = D1 - D0,
      DDI_F_DN = D3 - D2,
      BTO_UP = as.numeric(difftime(AIBT, UP_AOBT, units = "mins")) >
               as.numeric(difftime(SIBT, UP_SOBT, units = "mins")),
      BTO_DN = as.numeric(difftime(DN_AIBT, AOBT, units = "mins")) >
               as.numeric(difftime(DN_SIBT, SOBT, units = "mins")),
      CHAIN_SRC = case_when(
        UP_SRC == "APDF" & DN_SRC == "APDF"                         ~ "APDF",
        UP_SRC %in% c("APDF", "NM") & DN_SRC %in% c("APDF", "NM")   ~ "with-NM",
        TRUE                                                        ~ "incomplete"
      )
    )
  if (!is.null(attr)) {
    # NM: ATFM_DELAY is empty for non-regulated flights -> 0 when the flight is linked to NM
    atfm <- attr |>
      transmute(UID, ATFM = if_else(is.na(FLT_STATUS), NA_real_, coalesce(as.numeric(ATFM_DELAY), 0)))
    ch <- ch |>
      left_join(atfm |> rename(ARR_UID = UID, ATFM_ARR = ATFM), by = "ARR_UID") |>
      left_join(atfm |> rename(DEP_UID = UID, ATFM_DEP = ATFM), by = "DEP_UID")
  }
  ch
}

# Summary of one link of the chain: delay in (x) -> delay out (y)
#   DDI_F / DDI_G = mean(y - x) (CODA/AIU convention, > 0: delay added); BTO/GTO = share(overshoot)
link_summary <- function(x, y, over = NULL, late = 15) {
  ok <- !is.na(x) & !is.na(y)
  if (!is.null(over)) over <- over[ok]
  x <- x[ok]; y <- y[ok]
  if (length(x) == 0) return(tibble())
  tibble(
    N          = length(x),
    IN_MED     = median(x),
    OUT_MED    = median(y),
    DDI_MEAN   = mean(y - x),
    DDI_MED    = median(y - x),
    OVERSHOOT  = if (is.null(over)) NA_real_ else mean(over, na.rm = TRUE),
    # Of the flights late (> 15) at the start: share still late at the end
    LATE_CARRY = sum(x > late & y > late) / sum(x > late),
    # Positive delay minutes at the end per positive delay minute at the start
    DLY_RATIO  = sum(pmax(y, 0)) / sum(pmax(x, 0))
  )
}

ta_chain_summary <- function(ch, by = c("HUB", "TA_CLASS")) {
  ch <- ch |> filter(TURN_TYPE == "turn")
  links <- list(
    "1 upstream leg (D0->D1)"   = c("D0", "D1", "BTO_UP"),
    "2 hub turn (D1->D2)"       = c("D1", "D2", "GTO_FLAG"),
    "3 downstream leg (D2->D3)" = c("D2", "D3", "BTO_DN"),
    "chain (D0->D3)"            = c("D0", "D3", NA)
  )
  imap(links, \(v, nm) {
    src_col <- switch(substr(nm, 1, 1), "1" = "UP_SRC", "2" = NULL, "3" = "DN_SRC", "c" = "CHAIN_SRC")
    g <- c(by, src_col)
    ch |>
      group_by(across(all_of(g))) |>
      reframe(link_summary(.data[[v[1]]], .data[[v[2]]], if (is.na(v[3])) NULL else .data[[v[3]]])) |>
      rename(any_of(c(SRC = "UP_SRC", SRC = "DN_SRC", SRC = "CHAIN_SRC"))) |>
      mutate(LINK = nm, .before = 1)
  }) |>
    list_rbind() |>
    mutate(SRC = coalesce(SRC, "hub")) |>
    filter(SRC %in% c("hub", "APDF", "NM", "with-NM"))
}

# Daily view per local operating day (turns only; chain indicators on EUR-EUR turns).
# Descriptive: puts the period summaries into perspective, no thresholds applied.
ta_daily <- function(ch, by = c("HUB", "OPDAY")) {
  t <- ch |> filter(TURN_TYPE == "turn")
  eur <- t |> filter(TA_CLASS == "EUR-EUR")
  local <- t |>
    group_by(across(all_of(by))) |>
    summarise(
      N_TURNS   = n(),
      ARR_OTP15 = mean(!ARR_LATE15),
      DEP_OTP15 = mean(!DEP_LATE15),
      ARR_DLY   = mean(ARR_DLY),
      DEP_DLY   = mean(DEP_DLY),
      DDI_G     = mean(DDI_G),
      GTO       = mean(GTO_FLAG),
      TDI       = mean(TDI),
      .groups = "drop"
    )
  chain <- eur |>
    group_by(across(all_of(by))) |>
    summarise(
      N_EUR     = n(),
      BTO_UP    = mean(BTO_UP, na.rm = TRUE),
      DDI_F_UP  = mean(DDI_F_UP, na.rm = TRUE),
      BTO_DN    = mean(BTO_DN, na.rm = TRUE),
      DDI_F_DN  = mean(DDI_F_DN, na.rm = TRUE),
      .groups = "drop"
    )
  out <- left_join(local, chain, by = by)
  if ("ATFM_DEP" %in% names(t)) {
    atfm <- t |>
      group_by(across(all_of(by))) |>
      summarise(
        ATFM_DEP_SHARE = mean(ATFM_DEP > 0, na.rm = TRUE),
        ATFM_DEP_MEAN  = mean(ATFM_DEP, na.rm = TRUE),
        ATFM_ARR_MEAN  = mean(ATFM_ARR, na.rm = TRUE),
        .groups = "drop"
      )
    out <- left_join(out, atfm, by = by)
  }
  out |> mutate(WDAY = lubridate::wday(OPDAY, label = TRUE, week_start = 1, locale = "C"), .after = OPDAY)
}

# ATFM breakdown: local and chain indicators split by whether the departing flight
# (or the inbound flight) carried ATFM delay. ATFM delay is an internationally used concept.
ta_atfm_split <- function(ch, by = "HUB") {
  ch |>
    filter(TURN_TYPE == "turn", !is.na(ATFM_ARR), !is.na(ATFM_DEP)) |>
    mutate(ATFM = case_when(
      ATFM_ARR > 0 & ATFM_DEP > 0 ~ "inbound and outbound",
      ATFM_ARR > 0                ~ "inbound only",
      ATFM_DEP > 0                ~ "outbound only",
      TRUE                        ~ "none"
    ) |> factor(levels = c("none", "inbound only", "outbound only", "inbound and outbound"))) |>
    group_by(across(all_of(c(by, "ATFM")))) |>
    summarise(
      N         = n(),
      ATFM_ARR  = mean(ATFM_ARR),
      ATFM_DEP  = mean(ATFM_DEP),
      ARR_DLY   = mean(ARR_DLY),
      DEP_DLY   = mean(DEP_DLY),
      DEP_OTP15 = mean(!DEP_LATE15),
      DDI_G     = mean(DDI_G),
      TDI       = mean(TDI),
      DDI_F_DN  = mean(DDI_F_DN[TA_CLASS == "EUR-EUR"], na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    mutate(SHARE = N / sum(N), .after = N) |>
    ungroup()
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
      DDI_G        = mean(DDI_G),
      GTO          = mean(GTO_FLAG),
      TDI          = mean(TDI),
      # Of the arrivals > 15 min late: share that also departed > 15 min late
      LATE_CARRY   = sum(ARR_LATE15 & DEP_LATE15) / sum(ARR_LATE15),
      # Of the departures > 15 min late: share preceded by a > 15 min late arrival
      DEP_REACT    = sum(ARR_LATE15 & DEP_LATE15) / sum(DEP_LATE15),
      # Delay minutes: share of positive arrival delay recovered on the ground
      ABSORB_RATIO = 1 - sum(PROP_OBS) / sum(pmax(ARR_DLY, 0)),
      .groups = "drop"
    )
}
