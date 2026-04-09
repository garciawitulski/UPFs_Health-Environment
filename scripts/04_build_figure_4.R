suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(forcats)
  library(patchwork)
})

bootstrap_script_dir <- function() {
  for (i in rev(seq_along(sys.frames()))) {
    if (!is.null(sys.frames()[[i]]$ofile)) return(dirname(normalizePath(sys.frames()[[i]]$ofile, winslash = "/", mustWork = FALSE)))
  }
  args <- commandArgs(trailingOnly = FALSE)
  hit <- args[grepl("^--file=", args)]
  if (length(hit)) return(dirname(normalizePath(sub("^--file=", "", hit[1]), winslash = "/", mustWork = FALSE)))
  getwd()
}
source(file.path(bootstrap_script_dir(), "00_config.R"), local = TRUE)

summary_env <- read_pkg_csv("summary_health_environment_upf_scenarios.csv")

pareto_df <- summary_env %>%
  mutate(
    basket = recode(
      replacement_target,
      NDG_ARG = "NDG",
      NOVA1 = "NOVA 1",
      NOVA3 = "NOVA 3",
      NOVA1_NOVA3_mix = "NOVA1/3 mix",
      none = "No replacement"
    ),
    structure = recode(
      scenario_model,
      isocaloric_replacement = "Isocaloric",
      isoweight_replacement = "Isoweight",
      no_replacement = "No sub."
    ),
    scenario_label = recode(
      scenario,
      red_10 = "10% reduction",
      red_20 = "20% reduction",
      red_50 = "50% reduction",
      ndg_gapa_conservative = "NDG conservative",
      ndg_NDG_conservative = "NDG conservative",
      ndg_gapa_central = "NDG central",
      ndg_NDG_central = "NDG central",
      ndg_gapa_strict = "NDG strict",
      ndg_NDG_strict = "NDG strict"
    ),
    point_id = paste(scenario, scenario_model, replacement_target, sep = "__"),
    deaths = deaths_averted,
    ghg = -ghg_kg_co2eq_avoided_pct_of_baseline,
    water = -water_l_avoided_pct_of_baseline,
    land = -land_m2_avoided_pct_of_baseline,
    eutro = -eutro_g_po4eq_avoided_pct_of_baseline
  )

pal_basket <- c(
  "NDG" = "#1D3557",
  "NOVA 1" = "#2A9D8F",
  "NOVA 3" = "#C57B1C",
  "NOVA1/3 mix" = "#8D6A9F",
  "No replacement" = "#7C8797"
)
col_zone_better <- "#EEF7F4"
col_zone_worse <- "#FCF3EF"
col_front <- "#334155"

pareto_front <- function(df, x, y) {
  d <- df[order(-df[[x]]), ]
  keep <- logical(nrow(d))
  best <- Inf
  for (i in seq_len(nrow(d))) {
    if (d[[y]][i] < best) {
      keep[i] <- TRUE
      best <- d[[y]][i]
    }
  }
  d[keep, ]
}

fmt_env_change <- function(x) {
  ifelse(abs(x) < 0.05, "0", sprintf("%+.1f", round(x, 1)))
}

label_frontier <- function(scenario_label, structure, basket) {
  short_structure <- recode(
    structure,
    "Isocaloric" = "Iso",
    "Isoweight" = "Iso-wt",
    "No sub." = "No sub."
  )
  paste0(sub("^NDG ", "", scenario_label), "\n", short_structure, " | ", basket)
}

front_ids <- function(df, yvar) {
  d <- df[order(-df$deaths), ]
  keep <- logical(nrow(d))
  best <- Inf
  for (i in seq_len(nrow(d))) {
    if (d[[yvar]][i] < best) {
      keep[i] <- TRUE
      best <- d[[yvar]][i]
    }
  }
  d$point_id[keep]
}

union_ids <- unique(unlist(lapply(c("ghg", "water", "land", "eutro"), function(v) front_ids(pareto_df, v))))
highlight_df <- pareto_df[pareto_df$point_id %in% union_ids, ]

p5_scatter <- function(df, yvar, ylab, tag, show_shape_legend = FALSE, show_x = TRUE, show_annotations = TRUE) {
  front <- pareto_front(df, "deaths", yvar) %>%
    arrange(deaths) %>%
    mutate(front_label = label_frontier(scenario_label, structure, basket))
  emph <- highlight_df %>%
    mutate(front_label = label_frontier(scenario_label, structure, basket)) %>%
    arrange(deaths)

  x_span <- diff(range(df$deaths, na.rm = TRUE))
  if (!is.finite(x_span) || x_span == 0) x_span <- max(df$deaths, na.rm = TRUE)
  y_span <- diff(range(df[[yvar]], na.rm = TRUE))
  if (!is.finite(y_span) || y_span == 0) y_span <- max(abs(df[[yvar]]), na.rm = TRUE)
  if (!is.finite(y_span) || y_span == 0) y_span <- 1

  front_labels <- emph %>%
    arrange(desc(.data[[yvar]]), deaths) %>%
    mutate(
      label_x = max(df$deaths, na.rm = TRUE) + x_span * 0.09,
      label_y = if (n() == 1) {
        .data[[yvar]] + ifelse(.data[[yvar]] <= 0, y_span * 0.08, -y_span * 0.08)
      } else {
        seq(
          from = max(.data[[yvar]], na.rm = TRUE) + y_span * 0.10,
          to = min(.data[[yvar]], na.rm = TRUE) - y_span * 0.10,
          length.out = n()
        )
      }
    ) %>%
    arrange(deaths, .data[[yvar]])

  ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, fill = col_zone_better, alpha = 0.15) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf, fill = col_zone_worse, alpha = 0.15) +
    geom_hline(yintercept = 0, colour = col_muted, linewidth = 0.32, linetype = "dashed") +
    geom_point(
      data = df,
      aes(deaths, .data[[yvar]], fill = basket, shape = structure),
      inherit.aes = FALSE,
      colour = alpha("#475569", 0.5),
      size = 2.1,
      stroke = 0.2,
      alpha = 0.45
    ) +
    geom_step(
      data = front,
      aes(deaths, .data[[yvar]], group = 1),
      inherit.aes = FALSE,
      colour = col_front,
      linewidth = 0.95,
      direction = "vh"
    ) +
    geom_point(
      data = emph,
      aes(deaths, .data[[yvar]], fill = basket, shape = structure),
      colour = alpha(col_ink, 0.9),
      size = 3.6,
      stroke = 0.4
    ) +
    geom_curve(
      data = front_labels,
      aes(x = deaths, y = .data[[yvar]], xend = label_x - x_span * 0.012, yend = label_y),
      inherit.aes = FALSE,
      curvature = 0.12,
      linewidth = 0.3,
      colour = alpha(col_muted, 0.75)
    ) +
    geom_label(
      data = front_labels,
      aes(x = label_x, y = label_y, label = front_label),
      inherit.aes = FALSE,
      hjust = 0,
      size = 2.35,
      linewidth = 0.12,
      label.r = unit(0.08, "lines"),
      fill = alpha("white", 0.94),
      colour = col_ink
    ) +
    {if (show_annotations) list(
      annotate("text", x = min(df$deaths) * 1.02, y = min(df[[yvar]], na.rm = TRUE) * 0.96, label = "Lower environmental burden", hjust = 0, vjust = 1, size = 2.45, colour = col_muted),
      annotate("text", x = min(df$deaths) * 1.02, y = max(df[[yvar]], na.rm = TRUE) * 1.08, label = "Higher environmental burden", hjust = 0, vjust = 1, size = 2.45, colour = col_muted)
    ) else NULL} +
    scale_fill_manual(
      values = pal_basket,
      name = "Basket",
      guide = if (show_shape_legend) guide_legend(override.aes = list(shape = 21, size = 3.2, alpha = 1)) else "none"
    ) +
    scale_shape_manual(values = c(Isocaloric = 21, Isoweight = 22, `No sub.` = 24), name = "Structure") +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0.04, 0.75))) +
    labs(tag = tag, title = NULL, x = if (show_x) "Deaths averted/year" else NULL, y = ylab) +
    coord_cartesian(clip = "off") +
    theme_nf() +
    theme(
      legend.position = if (show_shape_legend) "right" else "none",
      legend.key.size = unit(3, "mm"),
      axis.text.x = if (show_x) element_text() else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank()
    )
}

fig4a <- p5_scatter(pareto_df, "ghg", "GHG change (%)", "a", show_x = FALSE, show_annotations = TRUE)
fig4b <- p5_scatter(pareto_df, "water", "Water change (%)", "b", show_x = FALSE, show_annotations = FALSE)
fig4c <- p5_scatter(pareto_df, "land", "Land change (%)", "c", show_x = TRUE, show_annotations = FALSE, show_shape_legend = TRUE)
fig4d <- p5_scatter(pareto_df, "eutro", "Eutrophication change (%)", "d", show_x = TRUE, show_annotations = FALSE)

heatmap_df <- highlight_df %>%
  mutate(row_label = label_frontier(scenario_label, structure, basket)) %>%
  arrange(deaths, basket, structure) %>%
  mutate(row_label = factor(row_label, levels = rev(unique(row_label)))) %>%
  select(row_label, Deaths = deaths, GHG = ghg, Water = water, Land = land, Eutro = eutro) %>%
  pivot_longer(Deaths:Eutro, names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  mutate(desirability = if (first(metric) == "Deaths") rescale(value) else rescale(-value)) %>%
  ungroup() %>%
  mutate(
    label = ifelse(metric == "Deaths", comma(round(value)), fmt_env_change(value)),
    label_colour = ifelse(desirability > 0.6, "white", col_ink),
    metric = factor(metric, levels = c("Deaths", "GHG", "Water", "Land", "Eutro"))
  )

fig4e <- ggplot(heatmap_df, aes(metric, row_label, fill = desirability)) +
  geom_tile(colour = "white", linewidth = 0.9) +
  geom_text(aes(label = label, colour = label_colour), size = 2.35, lineheight = 0.92) +
  scale_fill_gradientn(colours = c("#F8FAFC", "#DCEFE7", "#0F766E"), limits = c(0, 1), guide = "none") +
  scale_colour_identity() +
  labs(tag = "e", x = NULL, y = NULL) +
  theme_nf() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = rel(0.76), lineheight = 0.95),
    plot.margin = margin(6, 10, 6, 6)
  )

fig <- ((fig4a | fig4b) / (fig4c | fig4d) / fig4e) +
  plot_layout(guides = "collect", heights = c(1, 1, 0.85)) &
  theme(legend.position = "bottom", legend.key.size = unit(3, "mm"))

save_pair(fig, "Figure_4", width_mm = 183, height_mm = 240)
message("Saved Figure_4 to ", FIG_DIR)
