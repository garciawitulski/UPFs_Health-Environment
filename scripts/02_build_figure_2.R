suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
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

col_text <- col_ink
pal_env_low <- "#0F766E"
pal_env_mid <- "#FFFBEB"
pal_env_high <- "#B91C1C"
pal_base_low <- "#F8FAFC"
pal_base_high <- "#2F7F6E"

theme_bundle <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(colour = col_text, family = "sans"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = col_grid, linewidth = 0.35),
      axis.title = element_text(size = rel(0.98)),
      axis.text = element_text(size = rel(0.92)),
      strip.text = element_text(face = "bold", size = rel(0.95)),
      legend.title = element_text(face = "bold", size = rel(0.9)),
      legend.text = element_text(size = rel(0.9)),
      plot.margin = margin(14, 18, 14, 18)
    )
}

baseline_env <- read_pkg_csv("env_impact_curated_table_main_totals_shares.csv")
summary_env <- read_pkg_csv("summary_health_environment_upf_scenarios.csv")

baseline_row <- baseline_env %>% filter(component == "NOVA4_UPF")
baseline_panel <- data.frame(
  indicator = c("GHG", "Land", "Water", "Eutro"),
  value = c(
    baseline_row$ghg_share_pct,
    baseline_row$land_share_pct,
    baseline_row$water_share_pct,
    baseline_row$eutro_share_pct
  ),
  stringsAsFactors = FALSE
) %>%
  mutate(
    label = paste0(number(value, accuracy = 0.1), "%"),
    indicator = factor(indicator, levels = c("GHG", "Land", "Water", "Eutro"))
  )

scenario_levels <- c("10% reduction", "20% reduction", "50% reduction",
                     "NDG conservative", "NDG central", "NDG strict")

env_tradeoff <- summary_env %>%
  mutate(
    canon = canonical_scenario(scenario),
    panel = case_when(
      scenario_model == "no_replacement" & replacement_target == "none" ~ "No substitution",
      scenario_model == "isocaloric_replacement" & replacement_target == "NDG_ARG" ~ "Isocaloric NDG basket",
      scenario_model == "isoweight_replacement" & replacement_target == "NDG_ARG" ~ "Isoweight NDG basket",
      TRUE ~ NA_character_
    ),
    scenario = factor(scenario_label(canon), levels = rev(scenario_levels))
  ) %>%
  filter(!is.na(panel), !is.na(scenario)) %>%
  pivot_longer(
    cols = c(
      ghg_kg_co2eq_avoided_pct_of_baseline,
      land_m2_avoided_pct_of_baseline,
      water_l_avoided_pct_of_baseline,
      eutro_g_po4eq_avoided_pct_of_baseline
    ),
    names_to = "indicator",
    values_to = "avoided_pct"
  ) %>%
  mutate(
    net = -avoided_pct,
    label = ifelse(abs(net) < 0.05, "0", sprintf("%+.1f", round(net, 1))),
    indicator = recode(
      indicator,
      ghg_kg_co2eq_avoided_pct_of_baseline = "GHG",
      land_m2_avoided_pct_of_baseline = "Land",
      water_l_avoided_pct_of_baseline = "Water",
      eutro_g_po4eq_avoided_pct_of_baseline = "Eutro"
    ),
    indicator = factor(indicator, levels = c("GHG", "Land", "Water", "Eutro")),
    panel = factor(panel, levels = c("No substitution", "Isocaloric NDG basket", "Isoweight NDG basket"))
  )

p_top <- ggplot(baseline_panel, aes(x = indicator, y = "Baseline UPF share", fill = value)) +
  geom_tile(colour = "#D1D5DB", linewidth = 0.7, width = 0.96, height = 0.88) +
  geom_text(aes(label = label, colour = value > 14), size = 3.6, fontface = "bold") +
  scale_fill_gradient(low = pal_base_low, high = pal_base_high, limits = c(0, 22), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = col_text), guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_bundle(11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    plot.margin = margin(6, 18, 2, 18)
  )

p_bottom <- ggplot(env_tradeoff, aes(x = indicator, y = scenario, fill = net)) +
  geom_tile(colour = "white", linewidth = 0.9) +
  geom_text(aes(label = label, colour = abs(net) > 8.5), size = 2.7, fontface = "bold", lineheight = 0.92) +
  facet_grid(. ~ panel) +
  scale_fill_gradient2(
    low = pal_env_low,
    mid = pal_env_mid,
    high = pal_env_high,
    midpoint = 0,
    limits = c(-22, 30),
    breaks = c(-20, -10, 0, 10, 20, 30),
    labels = label_number(accuracy = 1),
    name = "Net change (%)",
    guide = guide_colourbar(barwidth = 12, barheight = 0.45, title.position = "top")
  ) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = col_text), guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_bundle(11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    strip.text.x = element_text(hjust = 0),
    legend.position = "bottom",
    plot.margin = margin(2, 18, 12, 18)
  )

fig <- p_top / p_bottom + plot_layout(heights = c(0.46, 1.6))
save_pair(fig, "Figure_2", width_mm = 255, height_mm = 205)
message("Saved Figure_2 to ", FIG_DIR)
