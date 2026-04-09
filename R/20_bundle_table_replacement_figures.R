# 20_bundle_table_replacement_figures.R
# Curated bundle of figures intended to replace the main manuscript tables.
# Outputs are written to output/table_replacement_bundle, together with a copy
# of this script and a short manifest README.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(patchwork)
})

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
bundle_dir <- file.path(root, "output", "table_replacement_bundle")
dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)

# Rebuild the figure pools used for the curated selection.
source(file.path(root, "R", "18_figuras_submission_alternative.R"), local = new.env(parent = globalenv()))
source(file.path(root, "R", "19_figuras_submission_options.R"), local = new.env(parent = globalenv()))
source(file.path(root, "R", "22_ed1_replacement_baskets.R"), local = new.env(parent = globalenv()))

dpi <- 400

col_text <- "#111827"
col_muted <- "#6B7280"
col_grid <- "#E5E7EB"
col_low <- "#179970"
col_mid <- "#F8F6F1"
col_high <- "#D35F2A"
col_seq <- "#0F766E"
col_prop <- "#2B6CB0"
col_guideline <- "#0F766E"

theme_bundle <- function(base_size = 11.5) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(colour = col_text, family = "sans"),
      plot.title = element_text(face = "bold", size = rel(1.22), hjust = 0),
      plot.subtitle = element_text(size = rel(0.95), colour = col_muted, hjust = 0),
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

save_all <- function(plot_obj, path_base, width_mm, height_mm) {
  ggsave(paste0(path_base, ".png"), plot_obj, width = width_mm, height = height_mm, units = "mm", dpi = dpi, bg = "white")
  ggsave(paste0(path_base, ".tiff"), plot_obj, width = width_mm, height = height_mm, units = "mm", dpi = dpi, compression = "lzw", bg = "white")
  ggsave(paste0(path_base, ".pdf"), plot_obj, width = width_mm, height = height_mm, units = "mm", device = cairo_pdf, bg = "white")
}

copy_formats <- function(src_base, dest_base) {
  exts <- c(".png", ".tiff", ".pdf")
  for (ext in exts) {
    file.copy(paste0(src_base, ext), paste0(dest_base, ext), overwrite = TRUE)
  }
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

fmt_signed <- function(x, accuracy = 0.1) {
  ifelse(
    abs(x) < (accuracy / 2),
    "0.0",
    ifelse(
      x > 0,
      paste0("+", number(x, accuracy = accuracy)),
      number(x, accuracy = accuracy)
    )
  )
}

smart_axis_label <- function(x) {
  ifelse(
    abs(x) >= 1000,
    paste0(number(x / 1000, accuracy = 1, trim = TRUE), "k"),
    ifelse(
      abs(x) >= 1,
      number(x, accuracy = 0.1, trim = TRUE),
      number(x, accuracy = 0.01, trim = TRUE)
    )
  )
}

parse_ci_mean <- function(x) {
  x <- trimws(as.character(x))
  pattern <- "^([0-9]+\\.?[0-9]*)\\s*\\(([0-9]+\\.?[0-9]*),\\s*([0-9]+\\.?[0-9]*)\\)$"
  reg <- regexec(pattern, x)
  matches <- regmatches(x, reg)
  vapply(matches, function(m) if (length(m) == 4) as.numeric(m[2]) else NA_real_, numeric(1))
}

scenario_order <- c("red_10", "red_20", "red_50", "ndg_conservative", "ndg_central", "ndg_strict")
scenario_levels <- scenario_label(scenario_order)

# Figure 01: aligned dumbbell panels replacing Tables 1 and 2
tab1 <- read.csv(file.path(root, "output", "Tabla1_UPF_por_energia_Argentina.csv"), stringsAsFactors = FALSE, check.names = FALSE)
tab2 <- read.csv(file.path(root, "output", "Tabla2_Muertes_atribuibles_Argentina.csv"), stringsAsFactors = FALSE, check.names = FALSE)

sex_cols <- c("Men" = "#2B6CB0", "Women" = "#C84D7A")
age_order <- c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69")

exposure_long <- tab1 %>%
  filter(`Age groups, years` != "Total") %>%
  transmute(
    age = factor(gsub("--", "-", `Age groups, years`, fixed = TRUE), levels = rev(age_order)),
    Men = parse_ci_mean(`Men, % (95% CI)`),
    Women = parse_ci_mean(`Women, % (95% CI)`)
  ) %>%
  pivot_longer(cols = c(Men, Women), names_to = "sex", values_to = "value")

exposure_segments <- exposure_long %>%
  pivot_wider(names_from = sex, values_from = value)

exp_x_min <- floor(min(exposure_long$value, na.rm = TRUE))
exp_x_max <- ceiling(max(exposure_long$value, na.rm = TRUE))

deaths_long <- tab2 %>%
  filter(`Age groups, years` != "Total") %>%
  transmute(
    age = factor(`Age groups, years`, levels = rev(age_order)),
    Men = as.numeric(`Men, deaths attr`),
    Women = as.numeric(`Women, deaths attr`)
  ) %>%
  pivot_longer(cols = c(Men, Women), names_to = "sex", values_to = "value")

deaths_segments <- deaths_long %>%
  pivot_wider(names_from = sex, values_from = value)

summary_df <- bind_rows(
  data.frame(
    panel = "UPF share of total energy",
    text = "Overall\nMen 15.9%   Women 17.2%   Total 16.7%",
    x = 1,
    stringsAsFactors = FALSE
  ),
  data.frame(
    panel = "Attributable deaths",
    text = "Overall\nMen 5,699   Women 4,124   Total 9,823",
    x = 2,
    stringsAsFactors = FALSE
  )
)

p01_summary <- ggplot(summary_df, aes(x = x, y = 1)) +
  geom_label(
    aes(label = text),
    fill = "#F8FAFC",
    colour = col_text,
    linewidth = 0.25,
    label.r = unit(0.14, "lines"),
    size = 3.35,
    fontface = "bold",
    lineheight = 1.0
  ) +
  scale_x_continuous(limits = c(0.55, 2.45), breaks = NULL) +
  scale_y_continuous(limits = c(0.7, 1.3), breaks = NULL) +
  labs(
    title = "UPF exposure and attributable mortality show distinct sex patterns across age groups",
    subtitle = "Top strip summarises overall values; lower panels compare men and women within each age group"
  ) +
  theme_void(base_size = 11.5) +
  theme(
    text = element_text(colour = col_text, family = "sans"),
    plot.title = element_text(face = "bold", size = 13.5, hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10.6, colour = col_muted, hjust = 0, margin = margin(b = 4)),
    plot.margin = margin(14, 18, 6, 18)
  )

p01_exposure <- ggplot(exposure_segments, aes(y = age)) +
  annotate("rect", xmin = exp_x_min, xmax = exp_x_max, ymin = -Inf, ymax = Inf, fill = "#F8FAFC", alpha = 0.9) +
  geom_segment(aes(x = Men, xend = Women, yend = age), linewidth = 1.1, colour = alpha(col_muted, 0.7), lineend = "round") +
  geom_point(data = exposure_long, aes(x = value, colour = sex), size = 3.0) +
  scale_colour_manual(values = sex_cols, name = NULL) +
  scale_x_continuous(limits = c(exp_x_min, exp_x_max + 0.3), breaks = seq(exp_x_min, exp_x_max, by = 1), labels = function(x) paste0(x, "%")) +
  labs(title = "UPF share of total energy", x = NULL, y = NULL) +
  theme_bundle(11.1) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.title = element_text(size = rel(1.02)),
    axis.text.y = element_text(face = "bold")
  )

p01_deaths <- ggplot(deaths_segments, aes(y = age)) +
  annotate("rect", xmin = 0, xmax = 1900, ymin = -Inf, ymax = Inf, fill = "#F8FAFC", alpha = 0.9) +
  geom_segment(aes(x = Men, xend = Women, yend = age), linewidth = 1.1, colour = alpha(col_muted, 0.7), lineend = "round") +
  geom_point(data = deaths_long, aes(x = value, colour = sex), size = 3.0) +
  geom_text(
    data = deaths_long %>% group_by(age) %>% slice_max(order_by = value, n = 1, with_ties = FALSE) %>% ungroup(),
    aes(x = value, label = comma(value)),
    hjust = -0.1,
    nudge_x = 55,
    size = 3.0,
    colour = col_text
  ) +
  scale_colour_manual(values = sex_cols, guide = "none") +
  scale_x_continuous(labels = label_comma()) +
  coord_cartesian(clip = "off") +
  labs(title = "Attributable deaths", x = NULL, y = NULL) +
  theme_bundle(11.1) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = rel(1.02)),
    axis.text.y = element_blank(),
    plot.margin = margin(14, 30, 14, 8)
  )

fig01 <- p01_summary / (p01_exposure | p01_deaths) +
  plot_layout(heights = c(0.44, 1), widths = c(1, 1.18))

save_all(fig01, file.path(bundle_dir, "fig01_tables1_2_butterfly_exposure_burden"), 255, 190)

# Figure 02: Table 3 replacement
yll_df <- read.csv(file.path(root, "output", "yll_allcause_por_estrato.csv"), stringsAsFactors = FALSE)
life_df <- read.csv(file.path(root, "output", "Tabla6_Esperanza_vida_UPF.csv"), stringsAsFactors = FALSE)
cost_sex_df <- read.csv(file.path(root, "output", "resumen_costos_indirectos_por_sexo.csv"), stringsAsFactors = FALSE)
cost_total_df <- read.csv(file.path(root, "output", "resumen_costos_indirectos.csv"), stringsAsFactors = FALSE)

yll_summary <- bind_rows(
  yll_df %>% filter(sexo_m == 1) %>% summarise(group = "Men", yll = sum(YLL)),
  yll_df %>% filter(sexo_m == 0) %>% summarise(group = "Women", yll = sum(YLL)),
  yll_df %>% summarise(group = "Total", yll = sum(YLL))
)

life_summary <- life_df %>%
  transmute(
    group = recode(tipo, "Hombres" = "Men", "Mujeres" = "Women", "Total" = "Total"),
    e30_actual = e30_actual,
    e30_no_upf = e30_sin_UPF,
    e30_gain = ganancia_anos
  )

cost_summary <- bind_rows(
  cost_sex_df %>% transmute(group = sexo, pvle = costo_PPP_millones_intl_USD / 1000, cost_per_death = (costo_PPP_millones_intl_USD * 1000) / deaths_attr_total),
  cost_total_df %>% transmute(group = "Total", pvle = costo_PPP_millones_intl_USD / 1000, cost_per_death = (costo_PPP_millones_intl_USD * 1000) / deaths_attr_total)
)

table3_long <- left_join(yll_summary, life_summary, by = "group") %>%
  left_join(cost_summary, by = "group") %>%
  transmute(
    group,
    `YLL attributable` = yll,
    `e30 actual` = e30_actual,
    `e30 without\nUPF deaths` = e30_no_upf,
    `Gain in e30\n(years)` = e30_gain,
    `PVLE\n(billion intl.$ PPP)` = pvle,
    `Cost per death\n(thousand intl.$ PPP)` = cost_per_death
  ) %>%
  pivot_longer(-group, names_to = "metric", values_to = "value") %>%
  mutate(
    metric_key = recode(
      metric,
      `YLL attributable` = "yll",
      `e30 actual` = "e30_actual",
      `e30 without\nUPF deaths` = "e30_no_upf",
      `Gain in e30\n(years)` = "gain",
      `PVLE\n(billion intl.$ PPP)` = "pvle",
      `Cost per death\n(thousand intl.$ PPP)` = "cost_per_death"
    ),
    group = factor(group, levels = c("Women", "Men", "Total")),
    label = case_when(
      metric_key == "yll" ~ comma(round(value)),
      metric_key %in% c("e30_actual", "e30_no_upf") ~ number(value, accuracy = 0.01),
      metric_key == "gain" ~ number(value, accuracy = 0.01),
      metric_key == "pvle" ~ number(value, accuracy = 0.02),
      TRUE ~ number(value, accuracy = 1)
    ),
    metric = factor(
      metric,
      levels = c(
        "YLL attributable",
        "e30 actual",
        "e30 without\nUPF deaths",
        "Gain in e30\n(years)",
        "PVLE\n(billion intl.$ PPP)",
        "Cost per death\n(thousand intl.$ PPP)"
      )
    )
  ) %>%
  group_by(metric_key) %>%
  mutate(score = value / max(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(label_colour = ifelse(score > 0.58, "white", col_text))

fig02 <- ggplot(table3_long, aes(x = metric, y = group, fill = score)) +
  geom_tile(colour = "white", linewidth = 0.9) +
  geom_text(aes(label = label), size = 3.1, colour = table3_long$label_colour, lineheight = 0.94) +
  scale_fill_gradient(low = col_mid, high = col_seq, limits = c(0, 1), guide = "none") +
  labs(
    title = "All-cause burden attributable to UPF consumption",
    subtitle = "Scaled within each metric; labels show the underlying values",
    x = NULL,
    y = NULL
  ) +
  theme_bundle(11.3) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", lineheight = 0.92),
    axis.text.y = element_text(face = "bold")
  )

save_all(fig02, file.path(bundle_dir, "fig02_table3_allcause_burden_matrix"), 235, 105)

# Figure 03: Table 4 replacement
scenario_health <- read.csv(file.path(root, "output", "scenario_health_economic_summary.csv"), stringsAsFactors = FALSE)

scenario_df <- scenario_health %>%
  mutate(
    canon = canonical_scenario(scenario),
    scenario = factor(scenario_label(canon), levels = rev(scenario_levels)),
    type = ifelse(grepl("^red_", canon), "Proportional", "Guideline-aligned")
  )

fig03_df <- bind_rows(
  scenario_df %>% transmute(scenario, type, metric = "Deaths averted", value = deaths_averted, lo = deaths_averted_lo, hi = deaths_averted_hi),
  scenario_df %>% transmute(scenario, type, metric = "YLL averted", value = yll_averted, lo = yll_averted_lo, hi = yll_averted_hi),
  scenario_df %>% transmute(scenario, type, metric = "Gain in e30\n(years)", value = e30_gain_years, lo = NA_real_, hi = NA_real_),
  scenario_df %>% transmute(scenario, type, metric = "PVLE averted\n(billion intl.$ PPP)", value = pvle_averted_billion_intl_ppp, lo = pvle_averted_billion_intl_ppp_lo, hi = pvle_averted_billion_intl_ppp_hi)
) %>%
  mutate(
    metric = factor(metric, levels = c("Deaths averted", "YLL averted", "Gain in e30\n(years)", "PVLE averted\n(billion intl.$ PPP)")),
    label = case_when(
      metric == "Deaths averted" ~ comma(round(value)),
      metric == "YLL averted" ~ comma(round(value)),
      metric == "Gain in e30\n(years)" ~ number(value, accuracy = 0.01),
      TRUE ~ number(value, accuracy = 0.01)
    )
  ) %>%
  group_by(metric) %>%
  mutate(text_x = ifelse(is.na(hi), value, hi) + max(ifelse(is.na(hi), value, hi), na.rm = TRUE) * 0.045) %>%
  ungroup()

fig03 <- ggplot(fig03_df, aes(x = value, y = scenario, colour = type)) +
  geom_segment(
    data = fig03_df %>% filter(!is.na(lo), !is.na(hi)),
    aes(x = lo, xend = hi, y = scenario, yend = scenario),
    inherit.aes = FALSE,
    linewidth = 0.8,
    colour = col_muted
  ) +
  geom_point(size = 2.8) +
  geom_text(aes(x = text_x, label = label), hjust = 0, size = 3.0, colour = col_text) +
  facet_grid(. ~ metric, scales = "free_x") +
  scale_colour_manual(values = c("Proportional" = col_prop, "Guideline-aligned" = col_guideline), name = NULL) +
  scale_x_continuous(labels = smart_axis_label) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Scenario-specific health and economic gains from reducing UPF consumption",
    subtitle = "Points show medians; horizontal lines show 95% uncertainty intervals when available",
    x = NULL,
    y = NULL
  ) +
  theme_bundle(11.1) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text.x = element_text(hjust = 0),
    legend.position = "top",
    panel.spacing.x = grid::unit(2.1, "lines"),
    axis.text.x = element_text(size = rel(0.84)),
    plot.margin = margin(14, 34, 14, 16)
  )

save_all(fig03, file.path(bundle_dir, "fig03_table4_policy_gains_ladder"), 270, 122)

# Figure 04: Table 5 replacement
baseline_env <- read.csv(file.path(root, "output", "env_impact_curated_table_main_totals_shares.csv"), stringsAsFactors = FALSE)
summary_env <- read.csv(file.path(root, "output", "summary_health_environment_upf_scenarios.csv"), stringsAsFactors = FALSE)

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

pal_env_low <- "#0F766E"
pal_env_mid <- "#FFFBEB"
pal_env_high <- "#B91C1C"
pal_base_low <- "#F8FAFC"
pal_base_high <- "#2F7F6E"

p4a <- ggplot(baseline_panel, aes(x = indicator, y = "Baseline UPF share", fill = value)) +
  geom_tile(colour = "#D1D5DB", linewidth = 0.7, width = 0.96, height = 0.88) +
  geom_text(
    aes(label = label, colour = value > 14),
    size = 3.6,
    fontface = "bold"
  ) +
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

p4b <- ggplot(env_tradeoff, aes(x = indicator, y = scenario, fill = net)) +
  geom_tile(colour = "white", linewidth = 0.9) +
  geom_text(
    aes(label = label, colour = abs(net) > 8.5),
    size = 2.7,
    fontface = "bold",
    lineheight = 0.92
  ) +
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
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bundle(11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    strip.text.x = element_text(hjust = 0),
    legend.position = "bottom",
    plot.margin = margin(2, 18, 12, 18)
  )

fig04 <- p4a / p4b +
  plot_layout(heights = c(0.46, 1.6))

save_all(fig04, file.path(bundle_dir, "fig04_table5_environment_policy_heatmap"), 255, 205)

# Figure 05: complementary composition figure
copy_formats(
  file.path(root, "output", "figures_final", "extended_data", "ED1_replacement_baskets"),
  file.path(bundle_dir, "fig05_complement_replacement_baskets_alluvial")
)

# Copy this script into the bundle for convenience
self_script <- file.path(root, "R", "20_bundle_table_replacement_figures.R")
file.copy(self_script, file.path(bundle_dir, "build_table_replacement_bundle.R"), overwrite = TRUE)

# Short manifest for the bundle
manifest_lines <- c(
  "# Table Replacement Figure Bundle",
  "",
  "This folder contains the curated figure set selected to replace the main manuscript tables.",
  "",
  "Selected mapping:",
  "- `fig01_tables1_2_butterfly_exposure_burden.*`: replaces Table 1 and Table 2",
  "- `fig02_table3_allcause_burden_matrix.*`: replaces Table 3",
  "- `fig03_table4_policy_gains_ladder.*`: replaces Table 4",
  "- `fig04_table5_environment_policy_heatmap.*`: replaces Table 5 with a baseline strip plus scenario trade-off panels",
  "- `fig05_complement_replacement_baskets_alluvial.*`: complementary basket-composition figure based on Table S7 food groups",
  "",
  "Run from the project root:",
  "```r",
  "source(\"R/20_bundle_table_replacement_figures.R\")",
  "```"
)
writeLines(manifest_lines, con = file.path(bundle_dir, "README_bundle.md"))

cat("Curated table-replacement bundle written to:\n")
cat(" - ", bundle_dir, "\n", sep = "")
cat("Files:\n")
cat(" - fig01_tables1_2_butterfly_exposure_burden.(png|tiff|pdf)\n")
cat(" - fig02_table3_allcause_burden_matrix.(png|tiff|pdf)\n")
cat(" - fig03_table4_policy_gains_ladder.(png|tiff|pdf)\n")
cat(" - fig04_table5_environment_policy_heatmap.(png|tiff|pdf)\n")
cat(" - fig05_complement_replacement_baskets_alluvial.(png|tiff|pdf)\n")
cat(" - build_table_replacement_bundle.R\n")
cat(" - README_bundle.md\n")
