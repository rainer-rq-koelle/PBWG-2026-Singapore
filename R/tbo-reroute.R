# TN-03 – Reroute definition and multi-reference contract comparison
#
# Reroute (agreed definition): the regulated trajectory (M2) deviates from the filed trajectory (M1)
# by more than `thr_nm` laterally EN ROUTE, i.e. excluding the first/last `term_nm` of M1.
# Route-string / waypoint differences alone are not sufficient (often SID/STAR changes only).

library(dplyr)
library(tidyr)
library(purrr)

# Lateral distance profile of trajectory b from reference a (both densified at 1 NM), en-route part of a only
lateral_profile <- function(a, b, term_nm = 40) {
  if (nrow(a) < 2 || nrow(b) < 2) return(NULL)
  A <- densify(prep(a)); A <- A[A$s > term_nm & A$s < max(A$s) - term_nm, ]
  B <- densify(prep(b))
  if (nrow(A) < 2) return(NULL)
  apply(geosphere::distm(cbind(A$lon, A$lat), cbind(B$lon, B$lat)), 1, min) / 1852
}

is_reroute <- function(m1, m2, thr_nm = 5, term_nm = 40) {
  d <- lateral_profile(m2, m1, term_nm)   # M2 points measured from M1? -> use M1 as reference path
  if (is.null(d)) return(NA)
  max(d) > thr_nm
}

# prof: long profile table with MODEL in M1/M2/M3, FLIGHT_ID, TIME_OVER, LONGITUDE, LATITUDE, FLIGHT_LEVEL
reroute_flags <- function(prof, thr_nm = 5, term_nm = 40) {
  ids <- prof |> filter(MODEL == "M2") |> distinct(FLIGHT_ID) |> pull()
  map(ids, \(i) {
    g <- \(m) prof |> filter(MODEL == m, FLIGHT_ID == i) |> arrange(TIME_OVER)
    d <- lateral_profile(g("M1"), g("M2"), term_nm)
    tibble(ID = i,
           M2M1_MAX_NM = if (is.null(d)) NA_real_ else max(d),
           M2M1_MEAN_NM = if (is.null(d)) NA_real_ else mean(d))
  }) |>
    list_rbind() |>
    mutate(REROUTE = M2M1_MAX_NM > thr_nm)
}

# Contract comparison against several references. mst: milestone table with MODEL M1/M2/M3.
# Returns one row per flight x milestone x reference (REF_MODEL M1 or M2) with the ACT = M3 offsets.
compare_refs <- function(mst, refs = c("M1", "M2")) {
  act <- mst |> filter(MODEL == "M3") |> select(ID, MST, SOURCE, A_TIME = TIME, A_LAT = LAT, A_LON = LON, A_FL = FL)
  map(refs, \(r) {
    mst |> filter(MODEL == r) |>
      select(ID, MST, SOURCE, R_TIME = TIME, R_LAT = LAT, R_LON = LON, R_FL = FL) |>
      full_join(act, by = c("ID", "MST", "SOURCE")) |>
      mutate(REF_MODEL = r)
  }) |>
    list_rbind() |>
    # flights without the reference model (e.g. no M2) are dropped for that reference
    semi_join(mst |> distinct(ID, MODEL), by = c("ID", "REF_MODEL" = "MODEL")) |>
    mutate(STATUS = case_when(!is.na(R_TIME) & !is.na(A_TIME) ~ "delivered",
                              is.na(A_TIME) ~ "planned-only", .default = "actual-only"),
           DT_MIN = as.numeric(difftime(A_TIME, R_TIME, units = "mins")),
           DFL = A_FL - R_FL,
           DPOS_NM = geosphere::distHaversine(cbind(R_LON, R_LAT), cbind(A_LON, A_LAT)) / 1852)
}
