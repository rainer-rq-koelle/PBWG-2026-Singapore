# TN-03 – apply the conformance method (R/tbo-conformance.R) to real flights
# Projects each flight onto a local azimuthal-equidistant plane (NM) centred on the reference route,
# then runs tbo_deviation()/tbo_summary(). Timestamps are UTC; t in minutes since the reference start.

library(dplyr)
library(purrr)

to_plane <- function(prof, lon0, lat0, t0) {
  xy <- sf::sf_project(from = "EPSG:4326",
                       to = sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +units=m", lat0, lon0),
                       pts = cbind(prof$LONGITUDE, prof$LATITUDE))
  tibble(x = xy[, 1] / 1852, y = xy[, 2] / 1852,
         t = as.numeric(difftime(prof$TIME_OVER, t0, units = "mins")),
         fl = prof$FLIGHT_LEVEL) |>
    distinct(t, .keep_all = TRUE) |>
    arrange(t)
}

# One flight against one reference model; returns the tbo_summary() row (or NULL on failure)
conformance_flight <- function(prof, id, ref_model, act_model = "M3", rfl = NULL) {
  r <- prof |> filter(FLIGHT_ID == id, MODEL == ref_model) |> arrange(TIME_OVER)
  a <- prof |> filter(FLIGHT_ID == id, MODEL == act_model) |> arrange(TIME_OVER)
  if (nrow(r) < 5 || nrow(a) < 5) return(NULL)
  lon0 <- mean(range(r$LONGITUDE)); lat0 <- mean(range(r$LATITUDE)); t0 <- min(r$TIME_OVER)
  ref <- to_plane(r, lon0, lat0, t0)
  act <- to_plane(a, lon0, lat0, t0)
  if (!is.null(rfl)) attr(ref, "rfl") <- rfl
  out <- tryCatch({
    dev <- suppressWarnings(tbo_deviation(act, ref))
    suppressWarnings(tbo_summary(dev, act, ref))
  }, error = function(e) NULL)
  if (is.null(out)) return(NULL)
  mutate(out, ID = id, REF_MODEL = ref_model, .before = 1)
}

conformance_batch <- function(prof, ids, refs = c("M1", "M2"), rfl = NULL) {
  grid <- tidyr::expand_grid(ID = ids, REF = refs)
  pmap(grid, \(ID, REF) {
    if (!any(prof$FLIGHT_ID == ID & prof$MODEL == REF)) return(NULL)
    conformance_flight(prof, ID, REF, rfl = rfl[[as.character(ID)]])
  }) |> list_rbind()
}

# Continuous profile of one flight (for figures): dt, d, dfl along reference progress
conformance_profile <- function(prof, id, ref_model) {
  r <- prof |> filter(FLIGHT_ID == id, MODEL == ref_model) |> arrange(TIME_OVER)
  a <- prof |> filter(FLIGHT_ID == id, MODEL == "M3") |> arrange(TIME_OVER)
  lon0 <- mean(range(r$LONGITUDE)); lat0 <- mean(range(r$LATITUDE)); t0 <- min(r$TIME_OVER)
  dev <- suppressWarnings(tbo_deviation(to_plane(a, lon0, lat0, t0), to_plane(r, lon0, lat0, t0)))
  as_tibble(dev) |> mutate(ID = id, REF_MODEL = ref_model)
}
