# TN-03 – synthetic test and demo cases for the conformance method (planar NM, minutes, FL)

library(dplyr)
library(purrr)

# Vertical profile: climb, cruise (optional step), descent; returns function(s, L).
fl_prof <- function(cruise = 350, climb_nm = 120, desc_nm = 120,
                    step_at = Inf, step_to = cruise, step_nm = 10) {
  force(cruise); force(step_to)
  f <- function(s, L) {
    ramp <- pmin(pmax((s - step_at) / step_nm, 0), 1)
    pmax(pmin(cruise * s / climb_nm,
              cruise + (step_to - cruise) * ramp,
              max(cruise, step_to) * (L - s) / desc_nm), 0)
  }
  attr(f, "breaks") <- function(L) {
    b <- c(climb_nm, L - desc_nm, step_at, step_at + step_nm)
    b[is.finite(b) & b > 0 & b < L]
  }
  f
}

# Generate a trajectory along waypoints `wp` (x, y). v: NM/min (number or function of s).
# sample = "time": surveillance-like points every dt_s seconds (Model 3);
# sample = "points": sparse plan points at vertices, every 50 NM and FL breaks (Model 1/2).
synth_traj <- function(wp, fl_fun = fl_prof(), v = 7, t0 = 0,
                       sample = c("time", "points"), dt_s = 30) {
  sample <- match.arg(sample)
  sp <- path_len(wp$x, wp$y); L <- max(sp)
  sg <- seq(0, L, by = 0.05)
  vel <- if (is.function(v)) v(sg) else rep(v, length(sg))
  tg <- t0 + c(0, cumsum(diff(sg) / vel[-1]))
  if (sample == "time") {
    tt <- unique(c(seq(t0, max(tg), by = dt_s / 60), max(tg)))
    s_at <- approx(tg, sg, tt)$y
  } else {
    s_at <- sort(unique(round(c(sp, seq(0, L, 50), attr(fl_fun, "breaks")(L), L), 6)))
    tt <- approx(sg, tg, s_at)$y
  }
  tibble(t = tt, x = approx(sp, wp$x, s_at)$y, y = approx(sp, wp$y, s_at)$y,
         fl = fl_fun(s_at, L))
}

wp <- function(...) {
  m <- matrix(c(...), ncol = 2, byrow = TRUE)
  tibble(x = m[, 1], y = m[, 2])
}

synth_cases <- function() {
  straight <- wp(0, 0, 200, 0, 400, 0, 600, 0)
  ref <- synth_traj(straight, sample = "points")
  act <- function(path = straight, ...) synth_traj(path, ...)
  slow <- function(s) ifelse(s > 150, 6.65, 7)
  hold <- wp(0, 0, 300, 0, 320, 0, 320, -4, 300, -4, 300, 0,
             320, 0, 320, -4, 300, -4, 300, 0.01, 600, 0)

  tribble(
    ~case, ~question, ~ref, ~act,
    "C01 identical", "baseline: all deviations zero", ref, act(),
    "C02 clock shift +10", "departure delay only", ref, act(t0 = 10),
    "C03 airborne slowdown", "delay accumulated en route (drift)", ref, act(v = slow),
    "C04 parallel offset 5 NM", "lateral offset, timing nearly unchanged", ref,
      act(wp(0, 0, 60, 0, 80, 5, 520, 5, 540, 0, 600, 0)),
    "C05 shortcut", "plan dogleg, flown direct: shorter, early", synth_traj(
      wp(0, 0, 200, 0, 300, 15, 400, 0, 600, 0), sample = "points"), act(),
    "C06 detour 40 NM", "route change beyond D_max", ref,
      act(wp(0, 0, 250, 0, 300, 40, 350, 0, 600, 0)),
    "C07 step climb", "FL330 then FL350 at mid-route", ref,
      act(fl_fun = fl_prof(cruise = 330, step_at = 300, step_to = 350)),
    "C08 capped FL310", "flown below plan throughout", ref,
      act(fl_fun = fl_prof(cruise = 310, climb_nm = 106, desc_nm = 106)),
    "C09 above FL390", "flown above plan throughout", ref,
      act(fl_fun = fl_prof(cruise = 390, climb_nm = 134, desc_nm = 134)),
    "C10 late descent", "TOD 30 NM later, steeper descent", ref,
      act(fl_fun = fl_prof(desc_nm = 90)),
    "C11 holding", "2 laps (~14 min) at 300 NM", ref, act(hold),
    "C12 surveillance gap", "no data 250-350 NM", ref,
      act() |> filter(x < 250 | x > 350),
    "C13 same-time view", "clock shift +10 seen at fixed time", ref, act(t0 = 10),
    "C16 capped reference", "plan already capped FL310 (RFL350), flown as planned",
      structure(synth_traj(straight, fl_prof(310, 106, 106), sample = "points"), rfl = 350),
      synth_traj(straight, fl_prof(310, 106, 106)),
    "C17a sparse 2 min", "C03 reported every 2 min", ref, act(v = slow, dt_s = 120),
    "C17b sparse 5 min", "C03 reported every 5 min", ref, act(v = slow, dt_s = 300)
  )
}

# Synthetic airspace: A | B | C split at x = 150 and 430 (off the A200 ring at x = 400);
# D = box north of the route (y > 20, 200 < x < 350).
fir_demo <- function(x, y) {
  ifelse(y > 20 & x > 200 & x < 350, "D", ifelse(x < 150, "A", ifelse(x < 430, "B", "C")))
}

# Event-view cases: C14 FIR sequence/milestones under a detour, C15 segment-wise ddt.
mst_cases <- function() {
  straight <- wp(0, 0, 200, 0, 400, 0, 600, 0)
  ref <- synth_traj(straight, sample = "points")
  v15 <- \(s) ifelse(s < 100, 7, ifelse(s < 400, 7.3, ifelse(s > 500, 6, 7)))
  tribble(
    ~case, ~question, ~ref, ~act,
    "C14 detour via FIR D", "extra FIR, changed sequence, FIR time offsets", ref,
      synth_traj(wp(0, 0, 250, 0, 300, 40, 350, 0, 600, 0)),
    "C15 late, recover, lose", "+8 min off-block, gain 1.76 en route, lose 2.38 in descent", ref,
      synth_traj(straight, v = v15, t0 = 8)
  ) |>
    mutate(
      seq_R = map(ref, \(r) fir_sequence(r, fir_demo)),
      seq_A = map(act, \(a) fir_sequence(a, fir_demo)),
      seq_match = map2_lgl(seq_R, seq_A, identical),
      mst = map2(ref, act, \(r, a) mst_compare(r, a, fir_demo))
    )
}

run_cases <- function(cases = synth_cases(), ...) {
  cases |>
    mutate(
      dev = map2(act, ref, \(a, r) tbo_deviation(a, r, ...)),
      summ = pmap(list(dev, act, ref), \(d, a, r) tbo_summary(d, a, r))
    )
}

# Revision vs execution split on M1 -> M2 -> M3 at common progress gates.
revision_case <- function(gates = c(100, 300, 500)) {
  straight <- wp(0, 0, 600, 0)
  m1 <- synth_traj(straight, sample = "points")
  m2 <- synth_traj(straight, sample = "points", t0 = 15)
  m3 <- synth_traj(straight, t0 = 18, v = \(s) ifelse(s > 150, 6.8, 7))
  tg <- function(tr) gate_times(tbo_deviation(tr, m1), gates)
  t1 <- tg(m1); t2 <- tg(m2); t3 <- tg(m3)
  tibble(gate_nm = gates, total = t3 - t1, revision = t2 - t1, execution = t3 - t2,
         closes = abs(total - revision - execution) < 1e-9)
}

# Expected behaviour, stated before running; tolerance-based checks.
check_cases <- function(res) {
  s <- res |> select(case, summ) |> tidyr::unnest(summ)
  g <- \(cs, col) s[[col]][s$case == cs]
  near <- \(a, b, tol = 0.1) abs(a - b) <= tol
  tribble(
    ~case, ~expectation, ~pass,
    "C01 identical", "dt, d, ddist, dTOC = 0; |dfl| < 0.05 FL (corner cut at TOC/TOD); coverage 1",
      near(g("C01 identical", "dt_exit"), 0, 0.01) && near(g("C01 identical", "d_max_abs"), 0, 0.01) &&
      near(g("C01 identical", "dfl_mean"), 0, 0.05) && near(g("C01 identical", "coverage"), 1, 1e-6) &&
      near(g("C01 identical", "ddist_nm"), 0, 0.01) && near(g("C01 identical", "dTOC_min"), 0, 0.02),
    "C02 clock shift +10", "dt = +10 everywhere, drift 0, d = dfl = 0",
      near(g("C02 clock shift +10", "dt_entry"), 10) && near(g("C02 clock shift +10", "drift"), 0) &&
      near(g("C02 clock shift +10", "d_max_abs"), 0, 0.01),
    "C03 airborne slowdown", "dt_entry ~ 0, drift = 410 NM valid span * (1/6.65 - 1/7) = 3.08 min",
      near(g("C03 airborne slowdown", "dt_entry"), 0) && near(g("C03 airborne slowdown", "drift"), 3.08, 0.1),
    "C04 parallel offset 5 NM", "median |d| = 5; |dt| < 0.2 min",
      near(g("C04 parallel offset 5 NM", "d_med_abs"), 5) && abs(g("C04 parallel offset 5 NM", "dt_exit")) < 0.2,
    "C05 shortcut", "ddist = 200 - 2*sqrt(100^2+15^2) = -2.24 NM; dt_exit = -0.32 min",
      near(g("C05 shortcut", "ddist_nm"), -2.24) && near(g("C05 shortcut", "dt_exit"), -0.32, 0.05),
    "C06 detour 40 NM", "one off-route span; ddist > 0; dt at re-join = dt_exit (~4 min)",
      g("C06 detour 40 NM", "n_spans") == 1 && g("C06 detour 40 NM", "ddist_nm") > 0 &&
      near(g("C06 detour 40 NM", "dt_rejoin"), g("C06 detour 40 NM", "dt_exit"), 0.05),
    "C07 step climb", "dfl < 0; symmetric share < 1; RP4 share = symmetric (below)",
      g("C07 step climb", "dfl_mean") < 0 && g("C07 step climb", "share_sym") < 0.9,
    "C08 capped FL310", "symmetric and RP4 shares both low",
      g("C08 capped FL310", "share_sym") < 0.5 && g("C08 capped FL310", "share_rp4") < 0.5,
    "C09 above FL390", "RP4 share ~ 1 while symmetric share low",
      g("C09 above FL390", "share_rp4") > 0.95 && g("C09 above FL390", "share_sym") < 0.5,
    "C10 late descent", "dTOD = (120 - 90) * 340/350 = +29.1 NM; dfl > 0",
      near(g("C10 late descent", "dTOD_nm"), 29.1, 0.5) && g("C10 late descent", "dfl_mean") > 0,
    "C11 holding", "drift ~ +13.7 min (96 NM at 7 NM/min); d stays small",
      near(g("C11 holding", "drift"), 96 / 7, 0.3) && g("C11 holding", "d_max_abs") < 5,
    "C12 surveillance gap", "last/first report at 248.5/353.5 NM: 105 NM gap, 75 NM excess; coverage = 1 - 75/520 = 0.856",
      near(g("C12 surveillance gap", "coverage"), 1 - 75 / 520, 0.005) &&
      near(g("C12 surveillance gap", "dt_exit"), 0, 0.01),
    "C13 same-time view", "dt = +10 min <=> ds = -70 NM (10 min x 7 NM/min)",
      near(g("C13 same-time view", "dt_entry"), 10) && near(g("C13 same-time view", "ds_med"), -70, 0.5),
    "C16 capped reference", "RP4 = sym = 1 although cruise ~ 300 NM below RFL",
      near(g("C16 capped reference", "share_rp4"), 1, 1e-6) &&
      near(g("C16 capped reference", "share_sym"), 1, 1e-6) &&
      g("C16 capped reference", "cruise_below_rfl_nm") > 250,
    "C17a sparse 2 min", "drift = 3.08; coverage 1 (14 NM gaps < 30)",
      near(g("C17a sparse 2 min", "drift"), 3.08, 0.05) && near(g("C17a sparse 2 min", "coverage"), 1, 1e-6),
    "C17b sparse 5 min", "drift = 3.08; gaps 33.25 NM at 6.65 NM/min, 3.25 NM excess: coverage = 1 - 3.25/33.25 = 0.90",
      near(g("C17b sparse 5 min", "drift"), 3.08, 0.05) && near(g("C17b sparse 5 min", "coverage"), 0.902, 0.01)
  )
}

check_mst <- function(m) {
  c14 <- m$mst[[1]]; c15 <- m$mst[[2]]
  dt_at <- \(tab, k) tab$dt[tab$MST == k]
  tribble(
    ~case, ~expectation, ~pass,
    "C14 detour via FIR D", "sequence A-B-C vs A-B-D-B-C; mismatch flagged",
      identical(m$seq_R[[1]], c("A", "B", "C")) && identical(m$seq_A[[1]], c("A", "B", "D", "B", "C")) &&
      !m$seq_match[[1]],
    "C14 detour via FIR D", "FIR_D_IN#1 and FIR_B_IN#2 ONLY_ACT; FIR_C entry ~ +4 min late",
      all(c("FIR_D_IN#1", "FIR_B_IN#2") %in% c14$MST[c14$status == "ONLY_ACT"]) &&
      near(dt_at(c14, "FIR_C_IN#1"), 4.0, 0.15),
    "C15 late, recover, lose", "dt: DEP 8, FIR_C entry 6.24, ARR 8.62",
      near(dt_at(c15, "DEP"), 8, 0.01) && near(dt_at(c15, "FIR_C_IN#1"), 6.24, 0.02) &&
      near(dt_at(c15, "ARR"), 8.62, 0.02),
    "C15 late, recover, lose", "sum of segment ddt = dt(ARR) - dt(DEP)",
      near(sum(c15$ddt, na.rm = TRUE), dt_at(c15, "ARR") - dt_at(c15, "DEP"), 1e-6)
  )
}
