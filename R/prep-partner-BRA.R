# Extract the Brazilian partner payload into the PBWG milestone format.
#
# Input : data-src/BRA-EUR-milestones-20250530-20250615.zip (DECEA/BRA package,
#         see README.md inside the zip for provenance and matching rules)
# Output: data/BRA-EUR-milestone-BRA-20250530-20250615.csv
#
# Only Brazil-sourced milestones are retained. The package's European APDF rows
# are dropped because APDF enters the analytic merge from our own extracts.
#
# Field mapping to the PBWG schema:
#   SOURCE    = "BRA"
#   SOURCE_ID = "BRA-<package FLIGHT_ID hash>-<DEP|ARR>" (flight-side key, as for APDF;
#               the hash maps back to FLIGHT_ID and the per-milestone provenance in the package)
#   CONF      = NA (in-area; the package's Brazilian rows are all at the Brazilian end)
#   TIME      = UTC (punctuality times already converted from BRT in the package)

library(tidyverse)

zip_path <- "data-src/BRA-EUR-milestones-20250530-20250615.zip"
out_path <- "data/BRA-EUR-milestone-BRA-20250530-20250615.csv"

bra_raw <- read_csv(
  unz(zip_path, "bra-eur-milestones/milestones.csv"),
  col_types = cols(.default = col_character())
)

bra_payload <- bra_raw |>
  filter(str_starts(SOURCE, "BRA_")) |>
  transmute(
    FLTID,
    REG,
    TYPE = ARCTYP,
    ICAO24 = toupper(ICAO24),
    ADEP,
    ADES,
    MST,
    MST_LBL,
    TIME = ymd_hms(TIME, tz = "UTC"),
    LAT = NA_real_,
    LON = NA_real_,
    ALT = NA_real_,
    SOURCE = "BRA",
    SOURCE_ID = paste("BRA", str_remove(FLIGHT_ID, "^BE25-"), SOURCE_ROLE, sep = "-"),
    # PBWG convention: CONF only labels out-of-area rows; all rows here are in-area
    CONF = NA_character_
  ) |>
  arrange(SOURCE_ID, TIME)

stopifnot(!anyNA(bra_payload$TIME))

# Same timestamp format as the APDF milestone CSVs
bra_payload |>
  mutate(TIME = format(TIME, "%Y-%m-%d %H:%M:%S", tz = "UTC")) |>
  write_csv(out_path, na = "NA")
