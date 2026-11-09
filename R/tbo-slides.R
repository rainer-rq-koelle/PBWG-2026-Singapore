# TN-03 – figures specific to the PBWG presentation (concept schematics)
library(ggplot2)
library(dplyr)

# Schematic: reference and actual trajectory reduced to contract points (offsets at points)
plot_concept_points <- function() {
  ref <- tibble(x = c(0, 60, 200, 400, 540, 600), y = c(0, 20, 40, 40, 20, 0))
  act <- tibble(x = c(0, 55, 190, 300, 420, 545, 600), y = c(0, 18, 35, 45, 38, 25, 0))
  pts <- tibble(x = c(40, 100, 300, 500, 560), lab = c("D40", "D100", "FIR entry", "A100", "A40"))
  ggplot() +
    geom_path(data = ref, aes(x, y, colour = "reference (plan)"), linewidth = 1) +
    geom_path(data = act, aes(x, y, colour = "actual (flown)"), linewidth = 1, linetype = 2) +
    geom_vline(data = pts, aes(xintercept = x), colour = "grey60", linetype = 3) +
    geom_text(data = pts, aes(x, 52, label = lab), size = 3.5) +
    annotate("text", x = 300, y = -8, size = 3.5,
             label = "at each point: Δt = t(actual) − t(plan);  Δposition;  ΔFL") +
    scale_colour_manual(values = c("reference (plan)" = "#1f4e79", "actual (flown)" = "#c55a11")) +
    labs(x = "progress along the route", y = NULL, colour = NULL) +
    coord_cartesian(ylim = c(-12, 56)) +
    theme_minimal(base_size = 16) +
    theme(axis.text = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
}

# Revision / execution: median offsets at rings (from tab_decomp)
plot_rev_exec <- function(d) {
  tab_decomp(d) |>
    filter(GROUP == "ATFM delay > 0") |>
    select(MST, Revision, Execution) |>
    tidyr::pivot_longer(-MST) |>
    ggplot(aes(MST, value, fill = name)) +
    geom_col() +
    geom_hline(yintercept = 0, colour = "grey40") +
    scale_fill_manual(values = c(Revision = "#1f4e79", Execution = "#c55a11")) +
    labs(x = "milestone", y = "median time offset (min)", fill = NULL,
         subtitle = "Regulated flights with ATFM delay: total = revision + execution") +
    theme_minimal(base_size = 16) + theme(legend.position = "bottom")
}

# Observation coverage by partner airport (100 NM ring) – stacked bars
plot_intl_coverage <- function(d) {
  tab_intl_sources(d) |>
    tidyr::pivot_longer(-c(INTL, MST), names_to = "Source") |>
    mutate(Source = factor(Source, c("OpenSky", "NM surveillance (CPF)", "NM flown (M3)", "not observed")),
           MST = if_else(MST == "A100", "arrival from Europe", "departure to Europe")) |>
    ggplot(aes(INTL, value, fill = Source)) +
    geom_col() +
    facet_wrap(~MST) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("OpenSky" = "#2e75b6", "NM surveillance (CPF)" = "#9dc3e6",
                                 "NM flown (M3)" = "#bdd7ee", "not observed" = "#c00000")) +
    labs(x = NULL, y = "share of flights", fill = NULL) +
    theme_minimal(base_size = 16) + theme(legend.position = "bottom")
}
