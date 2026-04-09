# 18_figuras_submission_alternative.R
# Alternative submission-oriented figure set built directly from the current
# pipeline outputs. Outputs are written to output/figures_alternative.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
})

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
out_dir <- file.path(root, "output", "figures_alternative")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dpi <- 400

col_text <- "#111827"
col_muted <- "#6B7280"
col_grid <- "#E5E7EB"
col_men <- "#2B6CB0"
col_women <- "#C84D7A"
col_prop <- "#2B6CB0"
col_guideline <- "#0F766E"
col_no_sub <- "#2563EB"
col_iso_ndg <- "#D35F2A"
col_heat_low <- "#179970"
col_heat_mid <- "#F8F6F1"
col_heat_high <- "#D35F2A"

theme_submission <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(colour = col_text, family = "sans"),
      plot.title = element_text(face = "bold", size = rel(1.25), hjust = 0),
      plot.subtitle = element_text(size = rel(0.95), colour = col_muted, hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = col_grid, linewidth = 0.35),
      axis.title = element_text(size = rel(0.98)),
      axis.text = element_text(size = rel(0.92)),
      strip.text = element_text(face = "bold", size = rel(0.95)),
      legend.title = element_text(face = "bold", size = rel(0.9)),
      legend.text = element_text(size = rel(0.9)),
      plot.margin = margin(14, 16, 14, 16)
    )
}

save_all <- function(plot_obj, path_base, width_mm, height_mm) {
  ggsave(paste0(path_base, ".png"), plot_obj, width = width_mm, height = height_mm, units = "mm", dpi = dpi, bg = "white")
  ggsave(paste0(path_base, ".tiff"), plot_obj, width = width_mm, height = height_mm, units = "mm", dpi = dpi, compression = "lzw", bg = "white")
  ggsave(paste0(path_base, ".pdf"), plot_obj, width = width_mm, height = height_mm, units = "mm", device = cairo_pdf, bg = "white")
}

parse_ci_mean <- function(x) {
  x <- trimws(as.character(x))
  pattern <- "^([0-9]+\\.?[0-9]*)\\s*\\(([0-9]+\\.?[0-9]*),\\s*([0-9]+\\.?[0-9]*)\\)$"
  reg <- regexec(pattern, x)
  matches <- regmatches(x, reg)
  vapply(matches, function(m) if (length(m) == 4) as.numeric(m[2]) else NA_real_, numeric(1))
}

canonical_scenario <- function(x) {
  x <- trimws(as.character(x))
  out <- x
  out[x %in% c("ndg_gapa_conservative", "ndg_NDG_conservative")] <- "ndg_conservative"
  out[x %in% c("ndg_gapa_central", "ndg_NDG_central")] <- "ndg_central"
  out[x %in% c("ndg_gapa_strict", "ndg_NDG_strict")] <- "ndg_strict"
  out
}

scenario_label <- function(x) {
  labels <- c(
    red_10 = "10% reduction",
    red_20 = "20% reduction",
    red_50 = "50% reduction",
    ndg_conservative = "NDG conservative",
    ndg_central = "NDG central",
    ndg_strict = "NDG strict"
  )
  unname(labels[x])
}

scenario_short <- function(x) {
  labels <- c(
    red_10 = "10%",
    red_20 = "20%",
    red_50 = "50%",
    ndg_conservative = "NDG cons.",
    ndg_central = "NDG central",
    ndg_strict = "NDG strict"
  )
  unname(labels[x])
}

scenario_type <- function(x) {
  ifelse(grepl("^red_", x), "Proportional", "Guideline-aligned")
}

scenario_order <- c("ndg_strict", "ndg_central", "ndg_conservative", "red_50", "red_20", "red_10")
age_order <- c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69")

tab1 <- read.csv(file.path(root, "output", "Tabla1_UPF_por_energia_Argentina.csv"), stringsAsFactors = FALSE, check.names = FALSE)
tab2 <- read.csv(file.path(root, "output", "Tabla2_Muertes_atribuibles_Argentina.csv"), stringsAsFactors = FALSE, check.names = FALSE)
scenario_health <- read.csv(file.path(root, "output", "scenario_health_economic_summary.csv"), stringsAsFactors = FALSE)
summary_env <- read.csv(file.path(root, "output", "summary_health_environment_upf_scenarios.csv"), stringsAsFactors = FALSE)

tab1 <- tab1 %>%
  filter(`Age groups, years` != "Total") %>%
  transmute(
    age = gsub("--", "-", `Age groups, years`, fixed = TRUE),
    upf_men = parse_ci_mean(`Men, % (95% CI)`),
    upf_women = parse_ci_mean(`Women, % (95% CI)`)
  )

tab2 <- tab2 %>%
  filter(`Age groups, years` != "Total") %>%
  transmute(
    age = `Age groups, years`,
    men_deaths = as.numeric(`Men, deaths attr`),
    women_deaths = as.numeric(`Women, deaths attr`)
  )

age_df <- left_join(tab2, tab1, by = "age") %>%
  mutate(age = factor(age, levels = age_order)) %>%
  arrange(age)

health_df <- scenario_health %>%
  mutate(
    canon = canonical_scenario(scenario),
    scenario = scenario_label(canon),
    short = scenario_short(canon),
    type = scenario_type(canon),
    scenario = factor(scenario, levels = scenario_label(scenario_order))
  ) %>%
  select(canon, scenario, short, type, everything())

frontier_df <- summary_env %>%
  mutate(
    canon = canonical_scenario(scenario),
    scenario = scenario_label(canon),
    short = scenario_short(canon),
    type = scenario_type(canon),
    substitution = case_when(
      scenario_model == "no_replacement" & replacement_target == "none" ~ "No substitution",
      scenario_model == "isocaloric_replacement" & replacement_target == "NDG_ARG" ~ "Isocaloric NDG basket",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(substitution)) %>%
  mutate(
    scenario = factor(scenario, levels = scenario_label(scenario_order)),
    substitution = factor(substitution, levels = c("No substitution", "Isocaloric NDG basket"))
  ) %>%
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
    net_change_pct = -avoided_pct,
    indicator = recode(
      indicator,
      ghg_kg_co2eq_avoided_pct_of_baseline = "GHG",
      land_m2_avoided_pct_of_baseline = "Land use",
      water_l_avoided_pct_of_baseline = "Water use",
      eutro_g_po4eq_avoided_pct_of_baseline = "Eutrophication"
    ),
    indicator = factor(indicator, levels = c("GHG", "Land use", "Water use", "Eutrophication"))
  )

heatmap_rows <- tribble(
  ~scenario_model, ~replacement_target, ~row_label,
  "no_replacement", "none", "No substitution",
  "isocaloric_replacement", "NOVA3", "Isocaloric, NOVA 3",
  "isocaloric_replacement", "NOVA1_NOVA3_mix", "Isocaloric, NOVA 1/3 mix",
  "isocaloric_replacement", "NDG_ARG", "Isocaloric, NDG basket",
  "isocaloric_replacement", "NOVA1", "Isocaloric, NOVA 1",
  "isoweight_replacement", "NDG_ARG", "Isoweight, NDG basket"
)

heatmap_df <- summary_env %>%
  mutate(canon = canonical_scenario(scenario)) %>%
  filter(canon == "ndg_central") %>%
  inner_join(heatmap_rows, by = c("scenario_model", "replacement_target")) %>%
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
    net_change_pct = -avoided_pct,
    indicator = recode(
      indicator,
      ghg_kg_co2eq_avoided_pct_of_baseline = "GHG",
      land_m2_avoided_pct_of_baseline = "Land use",
      water_l_avoided_pct_of_baseline = "Water use",
      eutro_g_po4eq_avoided_pct_of_baseline = "Eutrophication"
    ),
    indicator = factor(indicator, levels = c("GHG", "Land use", "Water use", "Eutrophication")),
    row_label = factor(row_label, levels = rev(heatmap_rows$row_label))
  )

# Figure 1: butterfly chart with central UPF labels
gap <- 220
butterfly_bars <- bind_rows(
  age_df %>%
    transmute(age, sex = "Men", deaths = men_deaths, upf = upf_men, xmin = -(gap + men_deaths), xmax = -gap),
  age_df %>%
    transmute(age, sex = "Women", deaths = women_deaths, upf = upf_women, xmin = gap, xmax = gap + women_deaths)
) %>%
  mutate(
    y = rev(seq_along(age_order))[match(as.character(age), age_order)],
    fill = ifelse(sex == "Men", col_men, col_women),
    deaths_label = comma(deaths),
    upf_label = sprintf("%.1f%%", upf)
  )

fig1 <- ggplot() +
  geom_rect(
    data = butterfly_bars,
    aes(xmin = xmin, xmax = xmax, ymin = y - 0.36, ymax = y + 0.36, fill = sex),
    colour = NA
  ) +
  geom_segment(
    aes(x = -gap, xend = -gap, y = 0.5, yend = 8.5),
    linewidth = 0.4, colour = col_grid
  ) +
  geom_segment(
    aes(x = gap, xend = gap, y = 0.5, yend = 8.5),
    linewidth = 0.4, colour = col_grid
  ) +
  geom_text(
    data = butterfly_bars,
    aes(x = ifelse(sex == "Men", xmin - 55, xmax + 55), y = y, label = deaths_label),
    size = 3.5, colour = col_muted
  ) +
  geom_text(
    data = butterfly_bars,
    aes(x = ifelse(sex == "Men", -gap / 2, gap / 2), y = y, label = upf_label, colour = sex),
    size = 3.0, fontface = "bold", show.legend = FALSE
  ) +
  geom_text(aes(x = -1150, y = 8.85, label = "Men"), fontface = "bold", size = 4.4, colour = col_men) +
  geom_text(aes(x = 1150, y = 8.85, label = "Women"), fontface = "bold", size = 4.4, colour = col_women) +
  geom_text(aes(x = 0, y = 8.85, label = "UPF share"), fontface = "bold", size = 4.1, colour = col_text) +
  scale_fill_manual(values = c("Men" = col_men, "Women" = col_women), guide = "none") +
  scale_colour_manual(values = c("Men" = col_men, "Women" = col_women)) +
  scale_y_continuous(
    breaks = seq_along(age_order),
    labels = rev(age_order),
    expand = expansion(mult = c(0.03, 0.07))
  ) +
  scale_x_continuous(
    breaks = c(-(gap + c(1500, 1000, 500, 0)), gap + c(0, 500, 1000, 1500)),
    labels = c("1,500", "1,000", "500", "0", "0", "500", "1,000", "1,500"),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_cartesian(xlim = c(-2250, 2250), clip = "off") +
  labs(
    title = "Attributable burden by sex and age",
    subtitle = "Bars show attributable deaths; central labels show UPF share of total energy",
    x = "Attributable deaths",
    y = "Age group (years)"
  ) +
  theme_submission(12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(colour = col_grid),
    axis.ticks.x = element_blank()
  )

save_all(fig1, file.path(out_dir, "alt_fig1_butterfly_burden_exposure"), 183, 118)

# Figure 2: policy ladder with multiple outcomes
policy_df <- health_df %>%
  transmute(
    scenario,
    type,
    metric = "Deaths averted",
    value = deaths_averted,
    lo = deaths_averted_lo,
    hi = deaths_averted_hi
  ) %>%
  bind_rows(
    health_df %>%
      transmute(
        scenario,
        type,
        metric = "YLL averted",
        value = yll_averted,
        lo = yll_averted_lo,
        hi = yll_averted_hi
      )
  ) %>%
  bind_rows(
    health_df %>%
      transmute(
        scenario,
        type,
        metric = "Averted PVLE\n(billion intl.$ PPP)",
        value = pvle_averted_billion_intl_ppp,
        lo = pvle_averted_billion_intl_ppp_lo,
        hi = pvle_averted_billion_intl_ppp_hi
      )
  ) %>%
  mutate(
    metric = factor(metric, levels = c("Deaths averted", "YLL averted", "Averted PVLE\n(billion intl.$ PPP)")),
    label = case_when(
      metric == "Deaths averted" ~ comma(round(value)),
      metric == "YLL averted" ~ comma(round(value)),
      TRUE ~ number(value, accuracy = 0.01)
     )
   ) %>%
  group_by(metric) %>%
  mutate(
    text_x = hi + max(hi, na.rm = TRUE) * 0.045
  ) %>%
  ungroup()

fig2 <- ggplot(policy_df, aes(x = value, y = scenario, colour = type)) +
  geom_segment(aes(x = lo, xend = hi, yend = scenario), linewidth = 0.8, colour = col_muted) +
  geom_point(size = 2.9) +
  geom_text(aes(x = text_x, label = label), hjust = 0, size = 3.05, colour = col_text) +
  facet_grid(. ~ metric, scales = "free_x") +
  scale_colour_manual(values = c("Proportional" = col_prop, "Guideline-aligned" = col_guideline), name = NULL) +
  scale_x_continuous(labels = label_comma()) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Policy scenarios across health and economic outcomes",
    subtitle = "Points show medians and horizontal lines show 95% uncertainty intervals",
    x = NULL,
    y = NULL
  ) +
  theme_submission(11.5) +
  theme(
    strip.text.x = element_text(hjust = 0),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    panel.spacing.x = unit(1.9, "lines"),
    plot.margin = margin(14, 32, 14, 16)
  )

save_all(fig2, file.path(out_dir, "alt_fig2_policy_ladder"), 236, 118)

# Figure 3: frontier plot
label_df <- frontier_df %>%
  filter(
    (substitution == "Isocaloric NDG basket" & canon %in% c("red_10", "red_20", "red_50", "ndg_conservative", "ndg_central", "ndg_strict")) |
      (substitution == "No substitution" & canon %in% c("red_50", "ndg_central", "ndg_strict"))
  )

fig3 <- ggplot(frontier_df, aes(x = deaths_averted, y = net_change_pct, colour = substitution, shape = type)) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", colour = col_muted) +
  geom_path(aes(group = interaction(indicator, substitution)), linewidth = 0.7, alpha = 0.75) +
  geom_point(size = 2.8, stroke = 0.8) +
  geom_text(
    data = label_df,
    aes(label = short),
    size = 3.0,
    colour = col_text,
    nudge_y = ifelse(label_df$substitution == "Isocaloric NDG basket", 1.4, -1.4),
    check_overlap = TRUE,
    show.legend = FALSE
  ) +
  facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c("No substitution" = col_no_sub, "Isocaloric NDG basket" = col_iso_ndg), name = "Substitution assumption") +
  scale_shape_manual(values = c("Proportional" = 16, "Guideline-aligned" = 17), name = "Scenario type") +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = "Health-environment frontiers under two substitution assumptions",
    subtitle = "Negative values indicate net reductions vs baseline",
    x = "Deaths averted (median)",
    y = "Net environmental change (%)"
  ) +
  theme_submission(11.5) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.major = element_line(colour = col_grid, linewidth = 0.3)
  )

save_all(fig3, file.path(out_dir, "alt_fig3_frontier_small_multiples"), 210, 160)

# Figure 4: heatmap
fig4 <- ggplot(heatmap_df, aes(x = indicator, y = row_label, fill = net_change_pct)) +
  geom_tile(colour = "white", linewidth = 0.9) +
  geom_text(
    aes(label = ifelse(abs(net_change_pct) < 0.05, "0.0", ifelse(net_change_pct > 0, paste0("+", number(net_change_pct, accuracy = 0.1)), number(net_change_pct, accuracy = 0.1)))),
    size = 4.4,
    fontface = "bold",
    colour = ifelse(abs(heatmap_df$net_change_pct) >= 11, "white", col_text)
  ) +
  scale_fill_gradient2(
    low = col_heat_low,
    mid = col_heat_mid,
    high = col_heat_high,
    midpoint = 0,
    limits = c(-25, 25),
    labels = label_number(accuracy = 1),
    name = "Net change (%)"
  ) +
  labs(
    title = "Environmental trade-offs across substitution structures",
    subtitle = "NDG central scenario (target UPF = 3.63%)",
    x = NULL,
    y = NULL
  ) +
  theme_submission(12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    legend.position = "right"
  )

save_all(fig4, file.path(out_dir, "alt_fig4_environment_heatmap"), 210, 150)

cat("Alternative submission-oriented figures written to:\n")
cat(" - ", file.path(out_dir, "alt_fig1_butterfly_burden_exposure.(png|tiff|pdf)"), "\n", sep = "")
cat(" - ", file.path(out_dir, "alt_fig2_policy_ladder.(png|tiff|pdf)"), "\n", sep = "")
cat(" - ", file.path(out_dir, "alt_fig3_frontier_small_multiples.(png|tiff|pdf)"), "\n", sep = "")
cat(" - ", file.path(out_dir, "alt_fig4_environment_heatmap.(png|tiff|pdf)"), "\n", sep = "")
