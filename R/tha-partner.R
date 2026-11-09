# AEROTHAI partner milestones (Thailand) – link to OSN-reconstructed international flights (TN-01 §7.5)
# Partner file has no REG/ICAO24: link on FLTID + ADEP + ADES + time (first OSN observation vs partner ATOT).

THA_FILE <- "THA-SIN-JPN-DOM-milestones-20250601-20250930.csv"
THA_CORE <- c("AOBT", "ATOT", "D40", "D100", "A100", "A40", "ALDT", "AIBT")

tha_read <- function(dir = here::here("data", "tbo")) {
  readr::read_csv(file.path(dir, THA_FILE), show_col_types = FALSE) |>
    dplyr::mutate(TIME = lubridate::with_tz(TIME, "UTC"))
}

tha_flow <- function(adep, ades) {
  dplyr::case_when(
    startsWith(adep, "VT") & startsWith(ades, "VT") ~ "Thai domestic",
    startsWith(adep, "WS") | startsWith(ades, "WS") ~ "Thailand–Singapore",
    startsWith(adep, "RJ") | startsWith(ades, "RJ") ~ "Thailand–Japan",
    TRUE ~ "other"
  )
}

tha_flights <- function(tha) {
  tha |>
    dplyr::summarise(
      FLTID = dplyr::first(FLTID), ADEP = dplyr::first(ADEP), ADES = dplyr::first(ADES),
      T_DEP = dplyr::coalesce(TIME[MST == "ATOT"][1], TIME[MST == "D40"][1]),
      T_ARR = dplyr::coalesce(TIME[MST == "ALDT"][1], TIME[MST == "A40"][1]),
      .by = SOURCE_ID
    ) |>
    dplyr::mutate(FLOW = tha_flow(ADEP, ADES))
}

# Share of flights with each milestone, per flow
tha_completeness <- function(tha) {
  fl <- tha_flights(tha)
  tha |>
    dplyr::distinct(SOURCE_ID, MST) |>
    dplyr::left_join(dplyr::select(fl, SOURCE_ID, FLOW), by = "SOURCE_ID") |>
    dplyr::count(FLOW, MST) |>
    dplyr::left_join(dplyr::count(fl, FLOW, name = "flights"), by = "FLOW") |>
    dplyr::mutate(share = n / flights) |>
    dplyr::select(-n) |>
    tidyr::pivot_wider(names_from = MST, values_from = share) |>
    dplyr::select(FLOW, flights, dplyr::any_of(THA_CORE))
}

# One-to-one link of OSN chains (pairs with THA) to AEROTHAI records, window ±90 min
tha_link_osn <- function(tha, pairs = c("SIN-THA", "JPN-THA"), tag = "20250601-20250613", win_min = 90) {
  osn <- purrr::map(pairs, \(p) readr::read_csv(
    here::here("data", "intl", sprintf("%s-flights-OSN-%s.csv", p, tag)), show_col_types = FALSE)) |>
    purrr::list_rbind() |>
    dplyr::mutate(dplyr::across(c(T_FIRST_SEEN, T_LAST_SEEN), \(x) lubridate::with_tz(x, "UTC")))
  lk <- osn |>
    dplyr::inner_join(tha_flights(tha), by = c("FLTID", "ADEP", "ADES"), relationship = "many-to-many") |>
    # Thai departures: partner ATOT vs first OSN observation; Thai arrivals: partner ALDT vs last OSN observation
    dplyr::mutate(dt = dplyr::coalesce(
      as.numeric(difftime(T_DEP, T_FIRST_SEEN, units = "mins")),
      as.numeric(difftime(T_ARR, T_LAST_SEEN, units = "mins"))
    )) |>
    dplyr::filter(!is.na(dt), abs(dt) <= win_min) |>
    dplyr::filter(dplyr::n() == 1, .by = UID) |>
    dplyr::filter(dplyr::n() == 1, .by = SOURCE_ID) |>
    dplyr::mutate(THAI_END = dplyr::if_else(startsWith(ADEP, "VT"), "departure", "arrival"))
  list(osn = osn, link = lk)
}

# Milestone coverage at the Thai end: OSN alone, AEROTHAI, merged
tha_osn_coverage <- function(tha, lk, pairs = c("SIN-THA", "JPN-THA"), tag = "20250601-20250613") {
  om <- purrr::map(pairs, \(p) readr::read_csv(
    here::here("data", "intl", sprintf("%s-milestones-OSN-%s.csv", p, tag)),
    show_col_types = FALSE, col_types = readr::cols(.default = "c"))) |>
    purrr::list_rbind() |>
    dplyr::filter(UID %in% lk$UID, MST %in% THA_CORE) |>
    dplyr::distinct(UID, MST) |>
    dplyr::mutate(OSN = TRUE)
  th <- tha |> dplyr::distinct(SOURCE_ID, MST) |> dplyr::mutate(AEROTHAI = TRUE)
  ends <- tibble::tibble(
    THAI_END = rep(c("departure", "arrival"), each = 4),
    MST = c("AOBT", "ATOT", "D40", "D100", "A100", "A40", "ALDT", "AIBT")
  )
  lk |>
    dplyr::select(UID, SOURCE_ID, THAI_END) |>
    dplyr::inner_join(ends, by = "THAI_END", relationship = "many-to-many") |>
    dplyr::left_join(om, by = c("UID", "MST")) |>
    dplyr::left_join(th, by = c("SOURCE_ID", "MST")) |>
    dplyr::mutate(dplyr::across(c(OSN, AEROTHAI), \(x) dplyr::coalesce(x, FALSE)), merged = OSN | AEROTHAI) |>
    dplyr::summarise(flights = dplyr::n(), dplyr::across(c(OSN, AEROTHAI, merged), mean), .by = c(THAI_END, MST))
}

# Runway-time agreement OSN − AEROTHAI at the Thai end (s)
tha_agreement <- function(tha, lk, pairs = c("SIN-THA", "JPN-THA"), tag = "20250601-20250613") {
  om <- purrr::map(pairs, \(p) readr::read_csv(
    here::here("data", "intl", sprintf("%s-milestones-OSN-%s.csv", p, tag)),
    show_col_types = FALSE, col_types = readr::cols(.default = "c"))) |>
    purrr::list_rbind() |>
    dplyr::filter(UID %in% lk$UID, MST %in% c("ATOT", "ALDT")) |>
    dplyr::mutate(TIME = lubridate::ymd_hms(TIME, tz = "UTC")) |>
    dplyr::summarise(t_osn = min(TIME), .by = c(UID, MST))
  om |>
    dplyr::inner_join(dplyr::select(lk, UID, SOURCE_ID, THAI_END), by = "UID") |>
    dplyr::filter((THAI_END == "departure" & MST == "ATOT") | (THAI_END == "arrival" & MST == "ALDT")) |>
    dplyr::inner_join(
      tha |> dplyr::filter(MST %in% c("ATOT", "ALDT")) |> dplyr::summarise(t_th = min(TIME), .by = c(SOURCE_ID, MST)),
      by = c("SOURCE_ID", "MST")
    ) |>
    dplyr::mutate(d = as.numeric(t_osn - t_th, units = "secs")) |>
    dplyr::summarise(n = dplyr::n(), median_s = median(d), p05_s = quantile(d, 0.05), p95_s = quantile(d, 0.95), .by = MST)
}
