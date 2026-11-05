# TN-03 – reference-anchored trajectory conformance (concept implementation)
#
# Trajectories are data frames with columns x, y (NM, planar), t (min), fl (flight level).
# Real data: project lat/lon to a local azimuthal-equidistant plane per flight before use.
# Sign conventions: dt > 0 = actual late; d > 0 = actual right of reference track;
# dfl > 0 = actual above reference; ds > 0 = actual ahead of reference at the same clock time.

library(tibble)
library(dplyr)

path_len <- function(x, y) c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))

ring_fun <- function(ctr, r) function(x, y) sqrt((x - ctr[1])^2 + (y - ctr[2])^2) < r

# (1) Densify the (sparse) reference: linear in x/y, time and FL, fixed arc-length step.
# Planar coordinates: straight lines here correspond to great circles only within a local projection.
ref_densify <- function(ref, step = 1) {
  s0 <- path_len(ref$x, ref$y)
  sg <- unique(c(seq(0, max(s0), by = step), max(s0)))
  tibble(
    s = sg,
    x = approx(s0, ref$x, sg)$y,
    y = approx(s0, ref$y, sg)$y,
    t = approx(s0, ref$t, sg)$y,
    fl = approx(s0, ref$fl, sg)$y
  )
}

# (1) Densify the actual trajectory at a fixed time step (linear in x/y and FL).
act_densify <- function(act, dt_s = 30) {
  tg <- unique(c(seq(min(act$t), max(act$t), by = dt_s / 60), max(act$t)))
  tibble(t = tg, x = approx(act$t, act$x, tg)$y, y = approx(act$t, act$y, tg)$y,
         fl = approx(act$t, act$fl, tg)$y)
}

# (2, 3) Monotone projection onto reference segments with signed cross-track distance.
# Search window anchored at the running maximum of progress: [s_max - back, s_max + fwd].
# The window prevents snapping to distant parts of the route; `back` lets holding and vectoring
# legs be projected locally, while progress itself never decreases.
project_forward <- function(trk, refd, back = 30, fwd = 60) {
  n <- nrow(refd)
  ax <- refd$x[-n]; ay <- refd$y[-n]; s0 <- refd$s[-n]
  dx <- diff(refd$x); dy <- diff(refd$y)
  L2 <- dx^2 + dy^2; L <- sqrt(L2)
  s_raw <- d <- numeric(nrow(trk))
  s_max <- 0
  for (i in seq_len(nrow(trk))) {
    k <- which(s0 >= s_max - back & s0 <= s_max + fwd)
    px <- trk$x[i]; py <- trk$y[i]
    u <- pmin(pmax(((px - ax[k]) * dx[k] + (py - ay[k]) * dy[k]) / L2[k], 0), 1)
    dist <- sqrt((px - ax[k] - u * dx[k])^2 + (py - ay[k] - u * dy[k])^2)
    j <- which.min(dist)
    cross <- dx[k[j]] * (py - ay[k[j]]) - dy[k[j]] * (px - ax[k[j]])
    s_raw[i] <- s0[k[j]] + u[j] * L[k[j]]
    d[i] <- -sign(cross) * dist[j]
    s_max <- max(s_max, s_raw[i])
  }
  tibble(s_raw = s_raw, s = cummax(s_raw), d = d)
}

# (5) Off-route spans: maximal runs with |d| > d_in that reach |d| > d_max and are at least
# min_len NM long (actual path). Hysteresis (d_in) makes the span cover the whole excursion.
off_route_spans <- function(trk, d_max = 20, d_in = 5, min_len = 10) {
  out <- tibble(i0 = integer(), i1 = integer())
  r <- rle(abs(trk$d) > d_in)
  ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
  P <- path_len(trk$x, trk$y)
  for (j in which(r$values)) {
    idx <- starts[j]:ends[j]
    if (max(abs(trk$d[idx])) > d_max && P[ends[j]] - P[starts[j]] >= min_len) {
      out <- add_row(out, i0 = starts[j], i1 = ends[j])
    }
  }
  out |>
    mutate(
      s_start = trk$s[pmax(i0 - 1, 1)], s_end = trk$s[pmin(i1 + 1, nrow(trk))],
      t_start = trk$t[pmax(i0 - 1, 1)], t_end = trk$t[pmin(i1 + 1, nrow(trk))],
      len_act = P[pmin(i1 + 1, nrow(trk))] - P[pmax(i0 - 1, 1)],
      extra_nm = len_act - (s_end - s_start)
    )
}

# Continuous deviations on a common reference-progress grid.
# Returns the grid (one row per reference step) with attributes:
#   track – densified actual trajectory (time-ordered) with s, d and the same-time offset ds;
#   spans – off-route spans; rings – terminal cut (s on R, t on A).
tbo_deviation <- function(act, ref, step = 1, dt_s = 30, d_max = 20, d_in = 5, min_len = 10,
                          term = 40, gap_max = 30, back = 30, fwd = 60) {
  refd <- ref_densify(ref, step)
  trk <- act_densify(act, dt_s)
  trk <- bind_cols(trk, project_forward(trk, refd, back, fwd))
  # Same-time view; NA where the reference is not airborne at that clock time
  trk$ds <- trk$s - approx(refd$t, refd$s, trk$t)$y

  # (6) Terminal cut by each trajectory's own ring crossings
  adep <- c(refd$x[1], refd$y[1]); ades <- c(refd$x[nrow(refd)], refd$y[nrow(refd)])
  first_exit <- \(tr, ctr) filter(seg_events(tr, ring_fun(ctr, term)), from == "TRUE")$t[1]
  last_entry <- \(tr, ctr) { e <- filter(seg_events(tr, ring_fun(ctr, term)), to == "TRUE")$t; e[length(e)] }
  rings <- list(
    sD_R = approx(refd$t, refd$s, first_exit(refd, adep))$y,
    sA_R = approx(refd$t, refd$s, last_entry(refd, ades))$y,
    tD_A = first_exit(trk, adep), tA_A = last_entry(trk, ades)
  )

  spans <- off_route_spans(trk, d_max, d_in, min_len)

  # (4) Common s grid: actual time, level and lateral offset interpolated at each reference step
  g <- refd |> rename(t_R = t, fl_R = fl, x_R = x, y_R = y)
  g$t <- approx(trk$s, trk$t, g$s, ties = min)$y
  g$fl <- approx(trk$s, trk$fl, g$s, ties = mean)$y
  g$d <- approx(trk$s, trk$d, g$s, ties = mean)$y
  g$dt <- g$t - g$t_R
  g$dfl <- g$fl - g$fl_R
  g$eligible <- !is.na(g$t) & g$s >= rings$sD_R & g$s <= rings$sA_R &
    g$t >= rings$tD_A & g$t <= rings$tA_A
  g$off_route <- vapply(g$s, \(s) any(s > spans$s_start & s < spans$s_end), logical(1))

  # Observed: the actual flight passed s within gap_max / 2 NM (along its path) of a real report.
  # Only the part of a reporting gap that exceeds gap_max counts as unobserved.
  P <- path_len(act$x, act$y)
  Pt <- approx(act$t, P, g$t)$y
  k <- pmin(findInterval(g$t, act$t, rightmost.closed = TRUE), length(P) - 1)
  dist_rep <- pmin(Pt - P[pmax(k, 1)], P[pmax(k, 1) + 1] - Pt)
  g$observed <- is.na(g$t) | dist_rep <= gap_max / 2

  g$valid <- g$eligible & !g$off_route
  # (5) No point-wise time or level offset inside off-route spans
  g$dt[g$off_route] <- NA
  g$dfl[g$off_route] <- NA

  in_span_t <- vapply(trk$t, \(t) any(t > spans$t_start & t < spans$t_end), logical(1))
  trk$eligible <- trk$t >= rings$tD_A & trk$t <= rings$tA_A
  trk$valid <- trk$eligible & !in_span_t & trk$s >= rings$sD_R & trk$s <= rings$sA_R
  if (nrow(spans)) {
    # Re-join: first grid step after the span where the flight is back on the reference (|d| < 1 NM)
    spans$dt_rejoin <- vapply(spans$s_end, \(se) g$dt[which(g$s > se & !g$off_route & abs(g$d) < 1)[1]],
                              numeric(1))
  }

  attr(g, "track") <- trk
  attr(g, "spans") <- spans
  attr(g, "rings") <- rings
  g
}

# TOC/TOD: interpolated first/last crossing of (max level - tol FL), in time and reference progress.
toc_tod <- function(tr, tol = 10) {
  lev <- max(tr$fl) - tol
  i <- which(tr$fl >= lev)
  a <- min(i); b <- max(i)
  cross <- function(j, k) {
    if (j < 1 || k > nrow(tr)) return(c(tr$t[max(1, j)], tr$s[max(1, j)]))
    f <- (lev - tr$fl[j]) / (tr$fl[k] - tr$fl[j])
    c(tr$t[j] + f * (tr$t[k] - tr$t[j]), tr$s[j] + f * (tr$s[k] - tr$s[j]))
  }
  up <- cross(a - 1, a); dn <- cross(b + 1, b)
  tibble(TOC_t = up[1], TOD_t = dn[1], TOC_s = up[2], TOD_s = dn[2])
}

# Time at which a trajectory reaches reference progress `gates` (event view).
gate_times <- function(dev, gates) {
  approx(dev$s, dev$t, gates, ties = min)$y
}

# Flight-level summary. band = 10 FL (1000 ft).
# gap_max (NM): longest interpolated stretch still counted as observed.
# Flight-level summary on the common grid. band = 10 FL (1000 ft).
# Shares are weighted by reference progress (equal weight per grid step); holds add no weight.
tbo_summary <- function(dev, act, ref, step = 1, band = 10) {
  refd <- ref_densify(ref, step)
  L <- max(refd$s)
  trk <- attr(dev, "track"); spans <- attr(dev, "spans"); rings <- attr(dev, "rings")
  v <- dev[dev$valid, ]
  wsh <- function(cond) if (nrow(v)) mean(cond) else NA_real_
  rt <- toc_tod(refd)
  # Actual TOC/TOD on the actual trajectory itself (time-ordered), located in reference progress
  at <- toc_tod(trk)
  # Requested cruise level (RFL): defaults to the reference maximum
  rfl <- attr(ref, "rfl") %||% max(ref$fl)
  # (7) Extra distance by zone: departure (to own D40), en route, arrival (from own A40)
  P <- path_len(trk$x, trk$y)
  dep_A <- approx(trk$t, P, rings$tD_A)$y
  arr_A <- max(P) - approx(trk$t, P, rings$tA_A)$y
  ddist <- max(P) - L
  tibble(
    dt_entry = v$dt[1],
    dt_exit = v$dt[nrow(v)],
    drift = v$dt[nrow(v)] - v$dt[1],
    d_med_abs = median(abs(v$d)),
    d_max_abs = max(abs(trk$d[trk$eligible])),
    dfl_mean = mean(v$dfl),
    share_sym = wsh(abs(v$dfl) < band),
    share_rp4 = wsh(v$dfl > -band),
    cruise_below_rfl_nm = step * sum(v$fl_R >= max(refd$fl) - band & v$fl < rfl - band),
    ds_med = median(trk$ds[trk$valid], na.rm = TRUE),
    n_spans = nrow(spans),
    route_change_nm = sum(spans$s_end - spans$s_start),
    span_extra_nm = sum(spans$extra_nm),
    dt_rejoin = if (nrow(spans)) spans$dt_rejoin[nrow(spans)] else NA_real_,
    coverage = mean(dev$observed[dev$eligible]),
    ddist_nm = ddist,
    ddist_dep = dep_A - rings$sD_R,
    ddist_arr = arr_A - (L - rings$sA_R),
    ddist_enr = ddist - (dep_A - rings$sD_R) - (arr_A - (L - rings$sA_R)),
    dTOC_min = at$TOC_t - rt$TOC_t,
    dTOD_min = at$TOD_t - rt$TOD_t,
    dTOD_nm = at$TOD_s - rt$TOD_s,
    n_backward = sum(diff(trk$s_raw) < -0.5)
  )
}

# ---- Milestone / event engine (planar) --------------------------------------------------------

# Label changes between consecutive points, located by bisection on the linear segment.
seg_events <- function(tr, label_fun, iter = 30) {
  lab <- as.character(label_fun(tr$x, tr$y))
  k <- which(lab[-1] != lab[-nrow(tr)])
  if (!length(k)) {
    return(tibble(t = numeric(), x = numeric(), y = numeric(), fl = numeric(),
                  from = character(), to = character()))
  }
  bind_rows(lapply(k, function(i) {
    p <- function(u, col) tr[[col]][i] + u * (tr[[col]][i + 1] - tr[[col]][i])
    lo <- 0; hi <- 1
    for (j in seq_len(iter)) {
      mid <- (lo + hi) / 2
      if (as.character(label_fun(p(mid, "x"), p(mid, "y"))) == lab[i]) lo <- mid else hi <- mid
    }
    u <- (lo + hi) / 2
    tibble(t = p(u, "t"), x = p(u, "x"), y = p(u, "y"), fl = p(u, "fl"),
           from = lab[i], to = lab[i + 1])
  }))
}

# Milestones on one trajectory: DEP/ARR (end points), D/A rings, FIR entries (numbered), TOC/TOD.
milestone_table <- function(tr, adep, ades, fir_fun,
                            rings_d = c(40, 100, 200), rings_a = c(200, 100, 40)) {
  ring <- ring_fun
  pick <- function(ev, first) if (nrow(ev)) ev[if (first) 1 else nrow(ev), ] else NULL
  d <- lapply(rings_d, \(r) pick(filter(seg_events(tr, ring(adep, r)), from == "TRUE"), TRUE))
  a <- lapply(rings_a, \(r) pick(filter(seg_events(tr, ring(ades, r)), to == "TRUE"), FALSE))
  rings <- bind_rows(
    bind_rows(d) |> mutate(MST = paste0("D", rings_d[!vapply(d, is.null, TRUE)])),
    bind_rows(a) |> mutate(MST = paste0("A", rings_a[!vapply(a, is.null, TRUE)]))
  )
  fir <- seg_events(tr, fir_fun) |>
    mutate(MST = paste0("FIR_", to, "_IN#", ave(seq_along(to), to, FUN = seq_along)))
  tt <- toc_tod(mutate(tr, s = path_len(x, y)))
  at_t <- \(t0, m) tibble(MST = m, t = t0, x = approx(tr$t, tr$x, t0)$y,
                          y = approx(tr$t, tr$y, t0)$y, fl = approx(tr$t, tr$fl, t0)$y)
  bind_rows(
    at_t(tr$t[1], "DEP"), rings, fir, at_t(tt$TOC_t, "TOC"), at_t(tt$TOD_t, "TOD"),
    at_t(tr$t[nrow(tr)], "ARR")
  ) |>
    select(MST, t, x, y, fl) |>
    arrange(t)
}

fir_sequence <- function(tr, fir_fun) c(fir_fun(tr$x[1], tr$y[1]), seg_events(tr, fir_fun)$to)

# Event view: join milestones by key; segment-wise ddt between consecutive common milestones.
mst_compare <- function(ref, act, fir_fun) {
  adep <- c(ref$x[1], ref$y[1]); ades <- c(ref$x[nrow(ref)], ref$y[nrow(ref)])
  r <- milestone_table(ref, adep, ades, fir_fun)
  a <- milestone_table(act, adep, ades, fir_fun)
  full_join(r, a, by = "MST", suffix = c("_R", "_A")) |>
    mutate(
      status = case_when(is.na(t_R) ~ "ONLY_ACT", is.na(t_A) ~ "ONLY_REF", TRUE ~ "BOTH"),
      dt = t_A - t_R,
      dpos = sqrt((x_A - x_R)^2 + (y_A - y_R)^2),
      dfl = fl_A - fl_R
    ) |>
    arrange(coalesce(t_R, t_A)) |>
    mutate(ddt = if_else(status == "BOTH", dt - lag_both(dt, status), NA_real_))
}

lag_both <- function(dt, status) {
  out <- rep(NA_real_, length(dt)); prev <- NA_real_
  for (i in seq_along(dt)) if (status[i] == "BOTH") { out[i] <- prev; prev <- dt[i] }
  out
}
