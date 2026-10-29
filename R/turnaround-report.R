# Report helpers for TN-02 (tables and figures on the layer-2 turnaround datasets).

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(arrow)
library(ggplot2)
library(gt)

# gt with output-specific sizing (HTML, PDF/LaTeX, Word)
gt_doc <- function(data, ...) {
  tab <- gt::gt(data, ...)
  if (isTRUE(knitr::is_latex_output())) {
    tab |> tab_options(table.font.size = "7pt", latex.use_longtable = TRUE)
  } else if (identical(knitr::pandoc_to(), "docx")) {
    tab |> tab_options(table.font.size = gt::px(9))
  } else {
    tab |> tab_options(table.font.size = gt::px(13), table.width = gt::pct(100))
  }
}

ta_path <- function(hub, what, period, ext = "parquet", dir = "data/turnaround") {
  file.path(dir, sprintf("TA-%s-%s-%s.%s", hub, what, period, ext))
}

# Load one hub and derive metrics and chain (layer 3)
ta_load <- function(hub, period, dir = "data/turnaround") {
  p <- \(what, ext = "parquet") ta_path(hub, what, period, ext, dir)
  legs <- read_parquet(p("legs"))
  attr <- read_parquet(p("flight-attributes"))
  tm   <- ta_metrics(read_parquet(p("turnarounds")))
  ch   <- ta_chain(tm, legs, attr)
  list(
    hub    = hub,
    legs   = legs,
    attr   = attr,
    params = read_csv(p("proxy-params", "csv"), show_col_types = FALSE),
    cover  = read_csv(p("coverage", "csv"), show_col_types = FALSE),
    study  = read_csv(p("study-airports", "csv"), show_col_types = FALSE),
    tm     = tm,
    ch     = ch,
    daily  = ta_daily(ch)
  )
}

# ---- tables -------------------------------------------------------------------

tbl_coverage <- function(cover) {
  cover |>
    group_by(LEG, REMOTE_SRC) |> summarise(N = sum(N), .groups = "drop") |>
    group_by(LEG) |> mutate(SHARE = N / sum(N)) |> ungroup() |>
    pivot_wider(names_from = LEG, values_from = c(N, SHARE), values_fill = 0) |>
    gt_doc() |>
    cols_label(REMOTE_SRC = "Remote end", N_IN = "Inbound", N_OUT = "Outbound",
               SHARE_IN = "Inbound %", SHARE_OUT = "Outbound %") |>
    fmt_percent(starts_with("SHARE"), decimals = 1) |> fmt_integer(starts_with("N_"))
}

tbl_blanks <- function(cover) {
  cover |>
    filter(REMOTE_SRC == "BLANK") |>
    count(MATCH, wt = N, name = "N") |>
    mutate(SHARE = N / sum(cover$N)) |>
    arrange(desc(N)) |>
    gt_doc() |> cols_label(MATCH = "Reason for blank", SHARE = "Share of hub flights") |>
    fmt_integer(N) |> fmt_percent(SHARE, decimals = 2)
}

tbl_turn_types <- function(tm) {
  tm |>
    count(TA_CLASS, TURN_TYPE) |>
    group_by(TA_CLASS) |> mutate(N = sum(n), SHARE = n / N) |> ungroup() |>
    select(-n) |>
    pivot_wider(names_from = TURN_TYPE, values_from = SHARE, values_fill = 0) |>
    select(TA_CLASS, N, any_of(c("turn", "long-turn", "parked", "invalid", "no-turn"))) |>
    mutate(CLASS_SHARE = N / sum(N), .after = N) |>
    gt_doc() |>
    cols_label(TA_CLASS = "Turnaround class", CLASS_SHARE = "Share of arrivals") |>
    fmt_integer(N) |> fmt_percent(-c(TA_CLASS, N), decimals = 1)
}

tbl_local <- function(tm, by = "TA_CLASS") {
  first_lbl <- c(TA_CLASS = "Class", HUB = "Hub")[[by[1]]]
  ta_summary(tm, by) |>
    filter(.data[[by[length(by)]]] != "no-dep") |>
    select(all_of(by), N, STT_MED, ATT_MED, SLACK_MED, ARR_DLY_MED, DEP_DLY_MED,
           ARR_OTP15, DEP_OTP15, DDI_G, GTO, TDI, LATE_CARRY, DEP_REACT) |>
    gt_doc() |>
    cols_label(.list = setNames(list(first_lbl), by[1])) |>
    cols_label(STT_MED = "STT", ATT_MED = "ATT", SLACK_MED = "Slack",
               ARR_DLY_MED = "Arr. delay", DEP_DLY_MED = "Dep. delay",
               ARR_OTP15 = "Arr. ≤ 15", DEP_OTP15 = "Dep. ≤ 15",
               DDI_G = "DDI-G", GTO = "GTO", TDI = "TDI",
               LATE_CARRY = "Late arr. → late dep.", DEP_REACT = "Late dep. after late arr.") |>
    fmt_integer(N) |>
    fmt_number(c(STT_MED, ATT_MED, SLACK_MED, ARR_DLY_MED, DEP_DLY_MED), decimals = 0) |>
    fmt_number(c(DDI_G, TDI), decimals = 1) |>
    fmt_percent(c(ARR_OTP15, DEP_OTP15, GTO, LATE_CARRY, DEP_REACT), decimals = 0) |>
    tab_spanner("Median (min)", c(STT_MED, ATT_MED, SLACK_MED, ARR_DLY_MED, DEP_DLY_MED)) |>
    tab_spanner("Turnaround indicators", c(DDI_G, GTO, TDI))
}

chain_links <- function(ch, class = "EUR-EUR") {
  ta_chain_summary(ch, by = c("HUB", "TA_CLASS")) |>
    filter(TA_CLASS == class) |>
    select(-any_of("TA_CLASS"))
}

tbl_links <- function(cl, links) {
  cl |>
    filter(LINK %in% links) |>
    select(LINK, SRC, N, IN_MED, OUT_MED, DDI_MEAN, OVERSHOOT, LATE_CARRY, DLY_RATIO) |>
    gt_doc(groupname_col = "LINK") |>
    cols_label(SRC = "Remote source", IN_MED = "Delay in", OUT_MED = "Delay out",
               DDI_MEAN = "DDI (mean)", OVERSHOOT = "BTO / GTO",
               LATE_CARRY = "Late carry", DLY_RATIO = "Delay ratio") |>
    fmt_integer(N) |> fmt_number(c(IN_MED, OUT_MED), decimals = 0) |>
    fmt_number(DDI_MEAN, decimals = 1) |>
    fmt_percent(c(OVERSHOOT, LATE_CARRY), decimals = 0) |> fmt_number(DLY_RATIO, decimals = 2) |>
    sub_missing(OVERSHOOT, missing_text = "–") |>
    tab_spanner("Median (min)", c(IN_MED, OUT_MED))
}

tbl_atfm <- function(ch, by = "HUB") {
  ta_atfm_split(ch, by) |>
    gt_doc(groupname_col = if (by == "HUB") NULL else by) |>
    cols_label(ATFM = "ATFM delay on", SHARE = "Share", ATFM_ARR = "ATFM arr.", ATFM_DEP = "ATFM dep.",
               ARR_DLY = "Arr. delay", DEP_DLY = "Dep. delay", DEP_OTP15 = "Dep. ≤ 15",
               DDI_G = "DDI-G", TDI = "TDI", DDI_F_DN = "DDI-F down") |>
    fmt_integer(N) |> fmt_percent(c(SHARE, DEP_OTP15), decimals = 0) |>
    fmt_number(c(ATFM_ARR, ATFM_DEP, ARR_DLY, DEP_DLY, DDI_G, TDI, DDI_F_DN), decimals = 1) |>
    tab_spanner("Mean (min)", c(ATFM_ARR, ATFM_DEP, ARR_DLY, DEP_DLY, DDI_G, TDI, DDI_F_DN))
}

# ---- figures -----------------------------------------------------------------

plot_stt <- function(tm) {
  ggplot(tm |> filter(PAIR_QC == "next-dep", TA_CLASS %in% c("EUR-EUR", "INTL-INTL"),
                      STT > 0, STT < 900), aes(STT)) +
    geom_histogram(binwidth = 5) +
    geom_vline(xintercept = c(240, 360, 420), linetype = "dashed") +
    facet_wrap(~TA_CLASS, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = seq(0, 900, 120)) +
    labs(x = "Scheduled ground time SOBT − SIBT (min)", y = "Turnarounds")
}

plot_delays <- function(tm) {
  tm |>
    filter(TURN_TYPE == "turn", TA_CLASS %in% c("EUR-EUR", "INTL-INTL")) |>
    select(TA_CLASS, `Arrival delay (AIBT − SIBT)` = ARR_DLY, `Departure delay (AOBT − SOBT)` = DEP_DLY) |>
    pivot_longer(-TA_CLASS) |>
    filter(between(value, -60, 120)) |>
    ggplot(aes(value)) +
    geom_histogram(binwidth = 2) +
    geom_vline(xintercept = c(0, 15), linetype = "dashed") +
    facet_grid(TA_CLASS ~ name, scales = "free_y") +
    labs(x = "Delay (min, negative = early)", y = "Turnarounds")
}

link_curves <- function(ch, class = "EUR-EUR", min_n = 20) {
  d <- ch |> filter(TURN_TYPE == "turn", TA_CLASS == class)
  bind_rows(
    d |> transmute(HUB, SRC = UP_SRC, LINK = "1 upstream leg", x = D0, y = D1),
    d |> transmute(HUB, SRC = "hub", LINK = "2 hub turn", x = D1, y = D2),
    d |> transmute(HUB, SRC = DN_SRC, LINK = "3 downstream leg", x = D2, y = D3)
  ) |>
    filter(SRC %in% c("APDF", "NM", "hub"), !is.na(x), !is.na(y), between(x, -30, 120)) |>
    mutate(XB = floor(x / 5) * 5 + 2.5) |>
    group_by(HUB, LINK, SRC, XB) |>
    summarise(Y = mean(y), N = n(), .groups = "drop") |>
    filter(N >= min_n)
}

plot_links <- function(ch) {
  ggplot(link_curves(ch), aes(XB, Y, colour = SRC)) +
    geom_abline(linetype = "dashed") +
    geom_line() +
    facet_wrap(~LINK) +
    labs(x = "Delay at start of link (min, 5-min bins)", y = "Mean delay at end of link (min)",
         colour = "Source")
}

# Daily timeline (local operating day): punctuality, turnaround and leg indicators.
# Weekends shaded; dashed line = median over days.
daily_long <- function(daily) {
  daily |>
    transmute(HUB, OPDAY, WDAY,
              `Dep. ≤ 15 (%)` = 100 * DEP_OTP15, `Arr. ≤ 15 (%)` = 100 * ARR_OTP15,
              `Dep. delay (min)` = DEP_DLY, `TDI (min)` = TDI, `DDI-G (min)` = DDI_G,
              `DDI-F down (min)` = DDI_F_DN, `ATFM dep. (min)` = ATFM_DEP_MEAN) |>
    pivot_longer(-c(HUB, OPDAY, WDAY), names_to = "IND") |>
    mutate(IND = factor(IND, levels = unique(IND)))
}

plot_daily <- function(daily) {
  d <- daily_long(daily)
  we <- daily |> filter(WDAY %in% c("Sat", "Sun")) |> distinct(OPDAY)
  ref <- d |> group_by(IND) |> summarise(M = median(value, na.rm = TRUE))
  ggplot(d, aes(OPDAY, value)) +
    geom_rect(data = we, aes(xmin = OPDAY - 0.5, xmax = OPDAY + 0.5, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey85") +
    geom_hline(data = ref, aes(yintercept = M), linetype = "dashed") +
    geom_line() +
    facet_wrap(~IND, ncol = 1, scales = "free_y", strip.position = "left") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
    labs(x = "Local operating day (from 04:00 local; grey = weekend)", y = NULL) +
    theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1))
}

plot_weekday <- function(daily_all) {
  daily_long(daily_all) |>
    filter(IND %in% c("Dep. ≤ 15 (%)", "TDI (min)", "DDI-G (min)", "ATFM dep. (min)")) |>
    group_by(HUB, IND, WDAY) |>
    summarise(M = median(value, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(WDAY, M, group = HUB)) +
    geom_line(colour = "grey40") +
    facet_wrap(~IND, scales = "free_y") +
    labs(x = NULL, y = "Median over days (one line per hub)")
}
