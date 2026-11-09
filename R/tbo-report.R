# TN-03 – report helpers for the European and international chapters
# Reads the local TN-03 data products (data/tbo/, not tracked) and returns tables and figures.
# Timestamps are UTC.

library(dplyr)
library(tidyr)
library(ggplot2)
library(arrow)

tbo_dir <- function(...) here::here("data", "tbo", ...)
tbo_has_data <- function() file.exists(tbo_dir("TBO-contract-dev-M1M2-20250601-20250614.parquet"))

mst_levels <- c("D40", "D100", "D200", "TOC", "TOD", "A200", "A100", "A40")
pair_levels <- c("EGLL-EDDM", "EDDM-EGLL", "EHAM-LEMD", "LEMD-EHAM", "EDDF-LTFM", "LTFM-EDDF")

tbo_load <- function() {
  list(
    flights = read_parquet(tbo_dir("TBO-dev-flights-20250601-20250614.parquet")),
    prof    = read_parquet(tbo_dir("TBO-M1M3-dev-profiles-20250601-20250614.parquet")),
    m2      = read_parquet(tbo_dir("TBO-M2-RTFM-20250601-20250614.parquet")),
    ct      = read_parquet(tbo_dir("TBO-contract-dev-M1M2-20250601-20250614.parquet")),
    dec     = read_parquet(tbo_dir("TBO-M123-ring-decomposition-20250601-20250614.parquet")),
    route   = read_parquet(tbo_dir("TBO-M2-vs-M1-route-check-20250601-20250614.parquet")),
    firx    = read_parquet(tbo_dir("TBO-dev-fir-crossings-M1M3-20250601-20250614.parquet")),
    firseq  = read_parquet(tbo_dir("TBO-dev-fir-sequence-20250601-20250614.parquet")),
    rr_cand = read_parquet(tbo_dir("TBO-reroute-candidates-20250601-20250614.parquet")),
    rr_prof = read_parquet(tbo_dir("TBO-reroute-showcase-M123-20250601-20250614.parquet")),
    rr_keys = read_parquet(tbo_dir("TBO-reroute-showcase-keys.parquet")),
    rr_enr  = read_parquet(tbo_dir("TBO-reroute-showcase-enroute-M1M2-20250601-20250614.parquet")),
    rr_ct   = read_parquet(tbo_dir("TBO-contract-reroute-showcase-M1M2-20250601-20250614.parquet")),
    ct_intl = read_parquet(tbo_dir("TBO-contract-INTL-20250601-20250607.parquet")),
    intl_fl = read_parquet(tbo_dir("TBO-intl-flights-20250601-20250607.parquet")),
    intl_cand = read_parquet(tbo_dir("TBO-intl-ring-candidates-20250601-20250607.parquet"))
  )
}

# ---- European chapter --------------------------------------------------------------------------

tab_sample <- function(d) {
  d$flights |>
    mutate(PAIR = factor(PAIR, pair_levels)) |>
    group_by(PAIR) |>
    summarise(Flights = n(), Regulated = sum(REGULATED),
              `ATFM delay > 0` = sum(REGULATED & ATFM_DELAY > 0),
              `Median ATFM delay (min)` = median(ATFM_DELAY[REGULATED & ATFM_DELAY > 0]),
              .groups = "drop")
}

# Time offset at the rings against M1 and M2 per pair (regulated flights)
tab_pair_refs <- function(d, rings = c("D40", "A40")) {
  d$ct |>
    filter(REGULATED, MST %in% rings, STATUS == "delivered") |>
    left_join(select(d$flights, UID = ID, PAIR), by = "UID") |>
    group_by(PAIR = factor(PAIR, pair_levels), MST = factor(MST, rings), REF_MODEL) |>
    summarise(med = median(DT_MIN), iqr = IQR(DT_MIN), .groups = "drop") |>
    pivot_wider(names_from = REF_MODEL, values_from = c(med, iqr)) |>
    arrange(PAIR, MST) |>
    select(PAIR, MST, med_M1, iqr_M1, med_M2, iqr_M2)
}

# Revision / execution split at the rings
tab_decomp <- function(d) {
  d$dec |>
    mutate(GROUP = if_else(DELAY_GRP == "delay>0", "ATFM delay > 0", "ATFM delay = 0"),
           MST = factor(MST, setdiff(mst_levels, c("TOC", "TOD")))) |>
    group_by(GROUP, MST) |>
    summarise(n = n(), Total = median(TOTAL), Revision = median(REVISION), Execution = median(EXECUTION),
              `Execution IQR` = IQR(EXECUTION), .groups = "drop") |>
    arrange(GROUP, MST)
}

tab_route_check <- function(d) {
  d$route |>
    mutate(ROUTE = factor(ROUTE, c("same route", "minor (points differ)", "reroute")),
           GROUP = if_else(DELAY_GRP == "delay>0", "ATFM delay > 0", "ATFM delay = 0")) |>
    count(GROUP, ROUTE) |>
    pivot_wider(names_from = ROUTE, values_from = n, values_fill = 0)
}

tab_fir_seq <- function(d) {
  d$firseq |>
    left_join(select(d$flights, ID, PAIR), by = "ID") |>
    group_by(PAIR = factor(PAIR, pair_levels)) |>
    summarise(Flights = n(), `Same FIR sequence (%)` = 100 * mean(SEQ_MATCH), .groups = "drop")
}

# Time offset (vs M1 and M2) along the milestone chain for one flight
plot_chain <- function(d, id) {
  d$ct |>
    filter(UID == id, STATUS == "delivered", MST_CLASS %in% c("ring", "fir")) |>
    mutate(REF = if_else(REF_MODEL == "M1", "filed plan (M1)", "regulated plan (M2)")) |>
    ggplot(aes(REF_TIME, DT_MIN, colour = REF)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_line() +
    geom_point() +
    labs(x = "time at reference milestone (UTC)", y = "time offset actual − reference (min)", colour = NULL) +
    theme(legend.position = "bottom")
}

# Lateral picture of one flight: M1, M2, M3
plot_map3 <- function(prof, ids, labeller = NULL) {
  prof |>
    filter(FLIGHT_ID %in% ids) |>
    arrange(MODEL, FLIGHT_ID, TIME_OVER) |>
    mutate(MODEL = recode(MODEL, M1 = "filed (M1)", M2 = "regulated (M2)", M3 = "flown (M3)")) |>
    ggplot(aes(LONGITUDE, LATITUDE, colour = MODEL)) +
    geom_path(linewidth = 0.6) +
    facet_wrap(~FLIGHT_ID, scales = "free", labeller = labeller %||% "label_value") +
    coord_quickmap() +
    labs(x = NULL, y = NULL, colour = NULL) +
    theme(legend.position = "bottom")
}

tab_reroute_scan <- function(d) {
  d$rr_cand |>
    mutate(apt = factor(apt)) |>
    count(apt, name = "Reroute candidates") |>
    arrange(desc(`Reroute candidates`))
}

tab_reroute_delivery <- function(d) {
  d$rr_enr |>
    filter(!is.na(REROUTE)) |>
    mutate(GROUP = if_else(REROUTE, "reroute (> 5 NM en route)", "route-string change only")) |>
    group_by(GROUP, REF = REF_MODEL) |>
    summarise(n = n(), `Cross-track mean (NM)` = median(XTRK_MEAN), `Cross-track P95 (NM)` = median(XTRK_P95),
              `Time offset (min)` = median(DT_ENR_MED), `Time offset IQR (min)` = IQR(DT_ENR_MED), .groups = "drop")
}

# ---- International chapter ---------------------------------------------------------------------

intl_codes_tn <- c("WSSS", "SBGR", "ZBAA", "KATL", "CYYZ", "RJAA", "VTBS")

tab_intl_sources <- function(d) {
  d$ct_intl |>
    filter(MST_CLASS == "ring", SIDE == "INTL", MST %in% c("A100", "D100")) |>
    mutate(INTL = if_else(substr(MST, 1, 1) == "A", ADES, ADEP),
           SRC = recode(ACT_SRC, OSN = "OpenSky", `NM-CPF` = "NM surveillance (CPF)", `NM-M3` = "NM flown (M3)",
                        `NM-M3-EXTRAP` = "not observed")) |>
    count(INTL, MST, SRC) |>
    group_by(INTL, MST) |> mutate(share = n / sum(n)) |> ungroup() |>
    select(-n) |>
    pivot_wider(names_from = SRC, values_from = share, values_fill = 0) |>
    arrange(INTL, MST)
}

tab_intl_delivery <- function(d) {
  d$ct_intl |>
    filter(MST_CLASS == "ring", STATUS == "delivered") |>
    mutate(INTL = if_else(ADEP %in% intl_codes_tn, ADEP, ADES),
           DIR = if_else(ADEP %in% intl_codes_tn, "to Europe", "from Europe")) |>
    group_by(INTL, DIR, END = if_else(SIDE == "INTL", "partner end", "European end")) |>
    summarise(n = n(), dt = median(DT_MIN), iqr = IQR(DT_MIN), .groups = "drop") |>
    pivot_wider(names_from = END, values_from = c(n, dt, iqr))
}

tab_intl_fir_obs <- function(d) {
  d$ct_intl |>
    filter(MST_CLASS == "fir") |>
    count(SIDE, STATUS) |>
    pivot_wider(names_from = STATUS, values_from = n, values_fill = 0)
}

# ---- Partner contribution: Thailand (THA) --------------------------------------------------------

tha_load <- function() read_parquet(tbo_dir("TBO-THA-flights-wide-20250601-20250930.parquet"))
tha_has_data <- function() file.exists(tbo_dir("TBO-THA-flights-wide-20250601-20250930.parquet"))

tab_tha_overview <- function(th) {
  th |>
    filter(FLOW != "other") |>
    mutate(FLOW = factor(FLOW, c("Thailand–Singapore", "Thailand–Japan", "Thailand domestic"))) |>
    group_by(FLOW) |>
    summarise(Flights = n(),
              `Aerodrome pairs` = n_distinct(paste(ADEP, ADES)),
              `Departures with ring crossings (%)` = 100 * mean(!is.na(D100[substr(ADEP, 1, 2) == "VT"])),
              `Arrivals with ring crossings (%)` = 100 * mean(!is.na(A100[substr(ADES, 1, 2) == "VT"])),
              `In-block time reported (%)` = 100 * mean(!is.na(AIBT[substr(ADES, 1, 2) == "VT"])),
              .groups = "drop")
}

# Segment durations at the Thai end of the flight (P15 / median / P85, minutes)
tab_tha_segments <- function(th) {
  mins <- \(a, b) as.numeric(difftime(a, b, units = "mins"))
  th |>
    filter(FLOW %in% c("Thailand–Singapore", "Thailand–Japan")) |>
    mutate(`Taxi-out (AOBT–ATOT)` = mins(ATOT, AOBT), `Departure 0–40 NM (ATOT–D40)` = mins(D40, ATOT),
           `Departure 40–100 NM (D40–D100)` = mins(D100, D40), `Arrival 100–40 NM (A100–A40)` = mins(A40, A100),
           `Arrival 40 NM–landing (A40–ALDT)` = mins(ALDT, A40), `Taxi-in (ALDT–AIBT)` = mins(AIBT, ALDT)) |>
    pivot_longer(c(starts_with("Taxi"), starts_with("Departure"), starts_with("Arrival")), names_to = "Segment", values_to = "MIN") |>
    filter(!is.na(MIN), MIN > 0, MIN < 120) |>
    mutate(Segment = factor(Segment, c("Taxi-out (AOBT–ATOT)", "Departure 0–40 NM (ATOT–D40)", "Departure 40–100 NM (D40–D100)",
                                       "Arrival 100–40 NM (A100–A40)", "Arrival 40 NM–landing (A40–ALDT)", "Taxi-in (ALDT–AIBT)"))) |>
    group_by(FLOW, Segment) |>
    summarise(n = n(), P15 = quantile(MIN, .15), Median = median(MIN), P85 = quantile(MIN, .85), .groups = "drop")
}

plot_tha_arrival <- function(th) {
  mins <- \(a, b) as.numeric(difftime(a, b, units = "mins"))
  th |>
    filter(FLOW %in% c("Thailand–Singapore", "Thailand–Japan"), substr(ADES, 1, 2) == "VT") |>
    transmute(FLOW, ADES, `100–40 NM` = mins(A40, A100), `40 NM–landing` = mins(ALDT, A40)) |>
    pivot_longer(-c(FLOW, ADES), names_to = "Segment", values_to = "MIN") |>
    filter(!is.na(MIN), MIN > 0, MIN < 60) |>
    ggplot(aes(MIN, colour = ADES)) +
    geom_density() +
    facet_grid(FLOW ~ Segment) +
    labs(x = "duration (min)", y = NULL, colour = "Arrival aerodrome") +
    theme(legend.position = "bottom", axis.text.y = element_blank())
}
