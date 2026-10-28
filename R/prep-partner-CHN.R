# Extract the Chinese partner payload into the PBWG milestone format.
#
# Input : data-src/milestone-China.csv (wide: one row per flight)
#         FLTID, REG, TYPE, ADEP, ADES, SOBT, NEW_AOBT, ATOT, ALDT, NEW_AIBT, SIBT
# Output: data/CHN-EUR-milestone-CHN-20250529-20250617.csv
#
# Findings (TN-01, section on China):
# - Times are UTC. European-end times agree with EUR APDF to the minute, and
#   Chinese-end runway times agree with OSN. No local-time correction applied.
# - Five values are bare dates ("2025-06-03"); they are read as 00:00:00 UTC.
# - NEW_AOBT / NEW_AIBT are AOBT / AIBT.
# - ICAO24 is not part of the official reporting format. It is filled from REG
#   via the OPDI/OSN flight list, falling back to the EUR APDF extracts, using
#   only registrations with a unique ICAO24.
# - Both ends are retained. Following the PBWG convention, the European end
#   reported by CHN is labelled CONF = "CHN-ooA" (out-of-area); the Chinese end
#   has CONF = NA.

library(tidyverse)
library(arrow)

src_path <- "data-src/milestone-China.csv"
out_path <- "data/CHN-EUR-milestone-CHN-20250529-20250617.csv"

chn <- read_csv(src_path, col_types = cols(.default = col_character())) |>
  select(-1) |>
  rename(AOBT = NEW_AOBT, AIBT = NEW_AIBT) |>
  mutate(
    ROW = row_number(),
    across(SOBT:SIBT, \(x) parse_date_time(x, c("Ymd HMS", "Ymd"), tz = "UTC"))
  )

unique_icao24 <- function(df) {
  df |>
    distinct(REG, ICAO24) |>
    filter(!is.na(REG), !is.na(ICAO24)) |>
    add_count(REG) |>
    filter(n == 1) |>
    select(-n)
}

icao_apdf <- list.files("data", "milestone-test", full.names = TRUE) |>
  map(\(f) read_csv(f, col_types = cols(.default = col_character()))) |>
  list_rbind() |>
  mutate(ICAO24 = toupper(ICAO24)) |>
  unique_icao24()

icao_osn <- open_dataset("data/flight_list/opdi_flight_list/") |>
  filter(DOF >= "2025-05-29", DOF <= "2025-06-17") |>
  distinct(ICAO24, REGISTRATION) |>
  collect() |>
  transmute(REG = str_remove_all(REGISTRATION, "-"), ICAO24 = toupper(ICAO24)) |>
  unique_icao24()

chn <- chn |>
  left_join(icao_osn, by = "REG") |>
  left_join(icao_apdf, by = "REG", suffix = c("", "_APDF")) |>
  mutate(ICAO24 = coalesce(ICAO24, ICAO24_APDF)) |>
  select(-ICAO24_APDF)

chn_payload <- chn |>
  pivot_longer(SOBT:SIBT, names_to = "MST", values_to = "TIME", values_drop_na = TRUE) |>
  mutate(
    SIDE = if_else(MST %in% c("SOBT", "AOBT", "ATOT"), "DEP", "ARR"),
    # PBWG convention: the European end reported by CHN is out-of-area
    in_area = (SIDE == "DEP" & str_starts(ADEP, "Z")) | (SIDE == "ARR" & str_starts(ADES, "Z"))
  ) |>
  transmute(
    FLTID, REG, TYPE, ICAO24, ADEP, ADES, MST,
    MST_LBL = NA_character_,
    TIME,
    LAT = NA_real_, LON = NA_real_, ALT = NA_real_,
    SOURCE = "CHN",
    SOURCE_ID = sprintf("CHN-%04d-%s", ROW, SIDE),
    CONF = if_else(in_area, NA_character_, "CHN-ooA")
  ) |>
  arrange(SOURCE_ID, TIME)

chn_payload |>
  mutate(TIME = format(TIME, "%Y-%m-%d %H:%M:%S", tz = "UTC")) |>
  write_csv(out_path, na = "NA")
