# TN-03 – Model 2 (RTFM) point profiles from the Databricks data product
# dp_all_ft_flights / output_flights, column rtfm_all_ft_pt_list (array of point structs, exploded in Databricks).
#
# Expected input (one row per point, as exported from Databricks):
#   flight_id, version_date, aircraft_id, departure_aerodrome_icao_id, arrival_aerodrome_icao_id, seq,
#   time_over, point_id, air_route, flight_level, <distance>, <point_type>, <coords "DDMMSSNDDDMMSSE">, ...
# Struct fields (printSchema 2026-11-05): time_over (timestamp), pt_id, rte_id, pt_fl (int), pt_dist (int),
#   pt_type (A aerodrome, W waypoint, N navaid, D procedure point, V vertical-profile point), pt_geo
#   ("DDMMSSNDDDMMSSE"), pt_dist_rel, pt_visible.
# Flight key: Databricks flight_id is NOT the PRISME SAM_ID; linkage key to be confirmed.
# Output: same layout as eurocontrol::point_profiles_tidy() so it plugs into ring/FIR/contract code (MODEL = "M2").
# Timestamps are UTC.

library(dplyr)

# "502736N0042710E" -> c(lat, lon) in decimal degrees
parse_dms <- function(x) {
  g <- stringr::str_match(x, "^(\\d{2})(\\d{2})(\\d{2})([NS])(\\d{3})(\\d{2})(\\d{2})([EW])$")
  lat <- as.numeric(g[, 2]) + as.numeric(g[, 3]) / 60 + as.numeric(g[, 4]) / 3600
  lon <- as.numeric(g[, 6]) + as.numeric(g[, 7]) / 60 + as.numeric(g[, 8]) / 3600
  cbind(LATITUDE  = ifelse(g[, 5] == "S", -lat, lat),
        LONGITUDE = ifelse(g[, 9] == "W", -lon, lon))
}

rtfm_to_profile <- function(x, cols = c(time = "time_over", point = "pt_id", route = "rte_id",
                                        fl = "pt_fl", coords = "pt_geo", type = "pt_type")) {
  ll <- parse_dms(x[[cols[["coords"]]]])
  tibble(
    FLIGHT_ID    = x$flight_id,
    TIME_OVER    = lubridate::force_tz(as.POSIXct(x[[cols[["time"]]]], tz = "UTC"), "UTC"),
    LONGITUDE    = ll[, "LONGITUDE"],
    LATITUDE     = ll[, "LATITUDE"],
    FLIGHT_LEVEL = as.numeric(x[[cols[["fl"]]]]),
    POINT_ID     = coalesce(na_if(x[[cols[["point"]]]], ""), "NO_POINT"),
    AIR_ROUTE    = coalesce(na_if(x[[cols[["route"]]]], ""), "NO_ROUTE"),
    POINT_TYPE   = x[[cols[["type"]]]],
    # pt_dist: cumulative distance along the route (km, checked on sample EBCI-LGAV)
    DIST_KM      = as.numeric(x$pt_dist),
    SEQ_ID       = x$seq,
    CALLSIGN     = x$aircraft_id,
    ADEP         = x$departure_aerodrome_icao_id,
    ADES         = x$arrival_aerodrome_icao_id,
    MODEL_TYPE   = "RTFM"
  ) |>
    arrange(FLIGHT_ID, SEQ_ID)
}
