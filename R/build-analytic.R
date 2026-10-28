# Build the analytic milestone datasets (layer 1) per region pair
#
# Sources : EUR APDF milestones, partner milestones, OSN/OPDI flight events
# Output  : data/EUR-<XXX>-milestone-EUR<XXX>OSN-20250601-20250614.csv
#           data/EUR-<XXX>-flights-EUR<XXX>OSN-20250601-20250614.csv (linkage table)

source("R/analytic_merge.R")

flight_list <- open_dataset("data/flight_list/opdi_flight_list/")
flight_events <- open_dataset("data/flight_events/opdi_flight_events/")

build_region_pair <- function(region, partner_file, prm) {
  apdf_ms <- read_milestones(sprintf("data/EUR-%s-milestone-test-20250601-20250614.csv", region))
  ref_ms <- bind_rows(apdf_ms, read_milestones(partner_file))

  # Scope: airport pairs of the EUR APDF extract are authoritative
  flights <- ref_ms |>
    flight_sides() |>
    link_sides(prm) |>
    restrict_to_pairs(apdf_ms)

  osn_links <- link_osn_segments(flights, flight_list, prm)
  osn_ms <- osn_milestones(osn_links, flights, flight_events)
  analytic <- merge_milestones(ref_ms, flights, osn_ms)

  tag <- sprintf("EUR-%s-%%s-EUR%sOSN-20250601-20250614.csv", region, region)

  analytic |>
    mutate(TIME = format(TIME, "%Y-%m-%d %H:%M:%S", tz = "UTC")) |>
    write_csv(file.path("data", sprintf(tag, "milestone")), na = "NA")

  flights |>
    left_join(count(osn_links, UID, name = "N_OSN_SEGMENTS"), by = "UID") |>
    mutate(N_OSN_SEGMENTS = coalesce(N_OSN_SEGMENTS, 0L)) |>
    write_csv(file.path("data", sprintf(tag, "flights")), na = "NA")

  invisible(list(flights = flights, osn_links = osn_links, analytic = analytic))
}

region_pairs <- list(
  BRA = list(
    partner_file = "data/BRA-EUR-milestone-BRA-20250530-20250615.csv",
    prm = bridge_params(min_block_h = 5, max_block_h = 16)
  ),
  CHN = list(
    partner_file = "data/CHN-EUR-milestone-CHN-20250529-20250617.csv",
    prm = bridge_params(min_block_h = 6, max_block_h = 16)
  )
)

results <- imap(region_pairs, \(x, region) build_region_pair(region, x$partner_file, x$prm))
