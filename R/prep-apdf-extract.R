# Prepare EUR APDF milestone extracts per region (PBWG milestone-table format)
#
# Source : hub APDF (apdf-<year>.zip in PBWG_DATA_DIR, unzipped to data-src/apdf-<year>/)
#          NM flight tables (PBWG_DATA_DIR/NM-flight-tables/) for ICAO24 and FLTID
# Output : data/EUR-<XXX>-milestone-test-<period>.csv
#
# Flights between the 8 EUR hubs and the region's airports. One record per APDF
# movement (SOURCE_ID = EUR-<APDS_ID>-<DEP|ARR>):
#   DEP: SOBT, AOBT, ATOT (runway), D40, D100 (position, FL)
#   ARR: A100, A40 (position, FL), ALDT (runway), AIBT (stand), SIBT
# Inclusion: runway time (ATOT/ALDT) within the period (UTC).
#
# FLTID: aircraft identification (ICAO Doc 4444) from the flight plan where the
# APDF record links uniquely to it (SAM_ID), otherwise the airport-provided
# FLTID. The APDF label stays traceable via the APDS_ID in SOURCE_ID.
# ICAO24: aircraft address from the flight plan via SAM_ID.

library(tidyverse)
library(arrow)

EUR_HUBS <- c("EDDF", "EDDM", "EGLL", "EHAM", "LEMD", "LFPG", "LIRF", "LSZH")

as_utc <- function(df) mutate(df, across(where(is.POSIXct), \(x) with_tz(x, "UTC")))

apdf_hub_file <- function(hub, apdf_zip, src_dir) {
  m <- grep(paste0("^", hub, "_"), unzip(apdf_zip, list = TRUE)$Name, value = TRUE)
  if (length(m) != 1) stop("No unique APDF file for ", hub)
  f <- file.path(src_dir, m)
  if (!file.exists(f)) unzip(apdf_zip, files = m, exdir = src_dir)
  f
}

#' Read hub APDF movements to/from the region's airports
read_apdf_region <- function(region_apts, period, apdf_zip, src_dir, hubs = EUR_HUBS) {
  t0 <- as.POSIXct(period[1], tz = "UTC")
  t1 <- as.POSIXct(period[2], tz = "UTC") + days(1)
  map(hubs, \(h) {
    open_dataset(apdf_hub_file(h, apdf_zip, src_dir)) |>
      filter(ADEP_ICAO %in% region_apts | ADES_ICAO %in% region_apts) |>
      select(
        APDS_ID, IM_SAMAD_ID, AP_C_FLTID, AP_C_REG, ARCTYP, ADEP_ICAO, ADES_ICAO, SRC_PHASE,
        SCHED_TIME_UTC, BLOCK_TIME_UTC, MVT_TIME_UTC, AP_C_RWY, AP_C_STND,
        C40_CROSS_TIME, C40_CROSS_LAT, C40_CROSS_LON, C40_CROSS_FL,
        C100_CROSS_TIME, C100_CROSS_LAT, C100_CROSS_LON, C100_CROSS_FL
      ) |>
      collect()
  }) |>
    list_rbind() |>
    as_utc() |>
    filter(MVT_TIME_UTC >= t0, MVT_TIME_UTC < t1)
}

#' Flight-plan identity (aircraft identification, address) by SAM_ID
read_fpl_identity <- function(sam_ids, nm_files) {
  map(nm_files, \(f) {
    open_dataset(f) |>
      filter(ID %in% sam_ids) |>
      select(ID, AIRCRAFT_ID, AIRCRAFT_ADDRESS) |>
      collect()
  }) |>
    list_rbind() |>
    distinct() |>
    add_count(ID) |>
    filter(n == 1) |>
    select(-n)
}

#' APDF movements -> PBWG milestone table (long)
apdf_to_milestones <- function(apdf, fpl) {
  apdf |>
    add_count(IM_SAMAD_ID, name = "N_SAM") |>
    left_join(fpl, by = c("IM_SAMAD_ID" = "ID")) |>
    mutate(
      SAM_OK = !is.na(IM_SAMAD_ID) & N_SAM == 1,
      FLTID = if_else(SAM_OK & !is.na(AIRCRAFT_ID) & AIRCRAFT_ID != "", AIRCRAFT_ID, AP_C_FLTID),
      ICAO24 = if_else(SAM_OK, toupper(AIRCRAFT_ADDRESS), NA_character_),
      SOURCE_ID = sprintf("EUR-%.0f-%s", APDS_ID, SRC_PHASE)
    ) |>
    reframe(
      FLTID, REG = AP_C_REG, TYPE = ARCTYP, ICAO24, ADEP = ADEP_ICAO, ADES = ADES_ICAO,
      tibble(
        MST = if (SRC_PHASE == "DEP") c("SOBT", "AOBT", "ATOT", "D40", "D100")
              else c("A100", "A40", "ALDT", "AIBT", "SIBT"),
        MST_LBL = if (SRC_PHASE == "DEP") c(NA, NA, AP_C_RWY, NA, NA)
                  else c(NA, NA, AP_C_RWY, AP_C_STND, NA),
        TIME = if (SRC_PHASE == "DEP") c(SCHED_TIME_UTC, BLOCK_TIME_UTC, MVT_TIME_UTC, C40_CROSS_TIME, C100_CROSS_TIME)
               else c(C100_CROSS_TIME, C40_CROSS_TIME, MVT_TIME_UTC, BLOCK_TIME_UTC, SCHED_TIME_UTC),
        LAT = if (SRC_PHASE == "DEP") c(NA, NA, NA, C40_CROSS_LAT, C100_CROSS_LAT) else c(C100_CROSS_LAT, C40_CROSS_LAT, NA, NA, NA),
        LON = if (SRC_PHASE == "DEP") c(NA, NA, NA, C40_CROSS_LON, C100_CROSS_LON) else c(C100_CROSS_LON, C40_CROSS_LON, NA, NA, NA),
        ALT = if (SRC_PHASE == "DEP") c(NA, NA, NA, C40_CROSS_FL, C100_CROSS_FL) else c(C100_CROSS_FL, C40_CROSS_FL, NA, NA, NA)
      ),
      SOURCE = "EUR", SOURCE_ID,
      .by = c(APDS_ID, SRC_PHASE)
    ) |>
    select(-APDS_ID, -SRC_PHASE) |>
    filter(!is.na(TIME)) |>
    mutate(CONF = NA_character_) |>
    arrange(SOURCE_ID, TIME)
}

write_milestones <- function(ms, path) {
  ms |>
    mutate(TIME = format(TIME, "%Y-%m-%d %H:%M:%S", tz = "UTC")) |>
    write_csv(path, na = "NA")
}

#' Build the EUR APDF extract for one region
prep_apdf_extract <- function(region, region_apts, period = c("2025-06-01", "2025-06-14"),
                              data_dir = Sys.getenv("PBWG_DATA_DIR"), year = 2025,
                              src_dir = sprintf("data-src/apdf-%d", year), write = TRUE) {
  apdf_zip <- file.path(data_dir, sprintf("apdf-%d.zip", year))
  nm_files <- list.files(file.path(data_dir, "NM-flight-tables"), sprintf("NM-FLT-%d-Q[23]", year), full.names = TRUE)

  apdf <- read_apdf_region(region_apts, period, apdf_zip, src_dir)
  fpl <- read_fpl_identity(unique(na.omit(apdf$IM_SAMAD_ID)), nm_files)
  ms <- apdf_to_milestones(apdf, fpl)

  if (write) {
    write_milestones(ms, sprintf("data/EUR-%s-milestone-test-%s-%s.csv", region,
                                 format(as.Date(period[1]), "%Y%m%d"), format(as.Date(period[2]), "%Y%m%d")))
  }
  invisible(ms)
}
