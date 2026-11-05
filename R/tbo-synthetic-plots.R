# TN-03 – figures for the synthetic cases (expects tbo-conformance.R and tbo-synthetic.R sourced)

library(ggplot2)
library(tidyr)

pick <- function(res, cs) res[res$case == cs, ]

# C02/C13: one clock shift, two views (time offset by progress, distance offset by clock time)
plot_space_time <- function(res) {
  dev <- pick(res, "C13 same-time view")$dev[[1]]
  d <- filter(dev, valid)
  tr <- filter(attr(dev, "track"), valid)
  bind_rows(
    tibble(view = "Same position: time offset (min) vs distance along plan (NM)", x = d$s, y = d$dt),
    tibble(view = "Same clock time: distance offset (NM) vs time (min)", x = tr$t, y = tr$ds)
  ) |>
    ggplot(aes(x, y)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line() +
    facet_wrap(vars(view), ncol = 1, scales = "free") +
    labs(x = NULL, y = NULL)
}

# C09/C16: vertical profiles against the reference, with the +/-1000 ft band
plot_vertical <- function(res, band = 10) {
  cs <- c("C09 above FL390", "C16 capped reference")
  prof <- map_dfr(cs, function(k) {
    r <- pick(res, k)
    refd <- ref_densify(r$ref[[1]])
    bind_rows(
      tibble(case = k, traj = "Reference", s = refd$s, fl = refd$fl),
      tibble(case = k, traj = "Actual", s = r$dev[[1]]$s, fl = r$dev[[1]]$fl) |> filter(!is.na(fl))
    )
  })
  lab <- res |> filter(case %in% cs) |> select(case, summ) |> unnest(summ) |>
    transmute(case, label = sprintf("RP4 share %.2f\nsymmetric share %.2f", share_rp4, share_sym))
  rfl <- tibble(case = cs, rfl = c(NA, 350))
  ref_band <- prof |> filter(traj == "Reference")
  ggplot(prof, aes(s, fl)) +
    geom_ribbon(data = ref_band, aes(ymin = fl - band, ymax = fl + band), alpha = 0.2) +
    geom_hline(data = rfl, aes(yintercept = rfl), linetype = "dashed", na.rm = TRUE) +
    geom_line(aes(linetype = traj)) +
    geom_text(data = lab, aes(x = 300, y = 120, label = label), inherit.aes = FALSE, size = 3.2) +
    facet_wrap(vars(case)) +
    labs(x = "Distance along reference (NM)", y = "Flight level", linetype = NULL)
}

# C14: plan view with the synthetic FIRs and milestone positions
plot_fir_detour <- function(m) {
  r <- m$ref[[1]]; a <- m$act[[1]]; tab <- m$mst[[1]]
  firs <- tibble(
    fir = c("A", "B", "C", "D"),
    xmin = c(-20, 150, 430, 200), xmax = c(150, 430, 620, 350),
    ymin = c(-30, -30, -30, 20), ymax = c(60, 60, 60, 60)
  )
  pts <- bind_rows(
    tab |> filter(!is.na(t_R)) |> transmute(MST, traj = "Reference", x = x_R, y = y_R),
    tab |> filter(!is.na(t_A)) |> transmute(MST, traj = "Actual", x = x_A, y = y_A)
  ) |>
    filter(grepl("FIR", MST)) |>
    mutate(label = sub("FIR_(.)_IN#(.)", "\\1 in #\\2", MST))
  ggplot() +
    geom_rect(data = firs, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fir),
              alpha = 0.25, colour = "grey50") +
    geom_path(data = r, aes(x, y, linetype = "Reference")) +
    geom_path(data = a, aes(x, y, linetype = "Actual")) +
    geom_point(data = pts, aes(x, y, shape = traj)) +
    geom_text(data = filter(pts, traj == "Actual"), aes(x, y, label = label),
              nudge_y = -9, size = 2.8) +
    coord_equal(ylim = c(-30, 60)) +
    theme(legend.position = "bottom") +
    labs(x = "x (NM)", y = "y (NM)", fill = "FIR", linetype = NULL, shape = NULL)
}

# C15: time offset at common milestones
plot_milestone_chain <- function(m) {
  tab <- m$mst[[2]] |> filter(status == "BOTH")
  ggplot(tab, aes(t_R, dt)) +
    geom_step() +
    geom_point() +
    geom_text(aes(label = MST), angle = 90, hjust = -0.2, size = 2.8) +
    scale_y_continuous(limits = c(5.5, 10)) +
    labs(x = "Reference time (min after reference off-block)", y = "Time offset (min, + = late)")
}
