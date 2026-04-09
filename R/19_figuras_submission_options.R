# 19_figuras_submission_options.R
# Additional submission-oriented figure options built directly from the current
# pipeline outputs. Outputs are written to output/figures_options.

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggalluvial)
  library(dplyr)
  library(tidyr)
  library(scales)
})

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
out_dir <- file.path(root, "output", "figures_options")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dpi <- 400

col_text <- "#111827"
col_muted <- "#6B7280"
col_grid <- "#E5E7EB"
col_score_low <- "#B91C1C"
col_score_mid <- "#F8F6F1"
col_score_high <- "#0F766E"
col_base_effect <- "#334155"
col_replacement_effect <- "#D97706"
col_no_sub <- "#1D4ED8"
col_ndg_basket <- "#C2410C"

family_cols <- c(
  "Grains /\nbaked" = "#8C6A43",
  "Vegetables /\npulses" = "#4C956C",
  "Fruit" = "#E09F3E",
  "Dairy /\neggs" = "#5C8FB5",
  "Other animal-\nsource" = "#D9796A",
  "Ruminant\nmeats" = "#9E2A2B",
  "Fats, nuts\nand seeds" = "#7B6DAD"
)

scenario_cols <- c(
  "10% reduction" = "#7CB7E8",
  "20% reduction" = "#4292C6",
  "50% reduction" = "#08519C",
  "NDG conservative" = "#F4A582",
  "NDG central" = "#D6604D",
  "NDG strict" = "#B2182B"
)

theme_options <- function(base_size = 11.5) {
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

canonical_scenario <- function(x) {
  x <- trimws(as.character(x))
  out <- x
  out[x %in% c("ndg_gapa_conservative", "ndg_NDG_conservative")] <- "ndg_conservative"
  out[x %in% c("ndg_gapa_central", "ndg_NDG_central")] <- "ndg_central"
  out[x %in% c("ndg_gapa_strict", "ndg_NDG_strict")] <- "ndg_strict"
  out
}

scenario_lookup <- c(
  red_10 = "10% reduction",
  red_20 = "20% reduction",
  red_50 = "50% reduction",
  ndg_conservative = "NDG conservative",
  ndg_central = "NDG central",
  ndg_strict = "NDG strict"
)

scenario_short_lookup <- c(
  red_10 = "10%",
  red_20 = "20%",
  red_50 = "50%",
  ndg_conservative = "NDG cons.",
  ndg_central = "NDG central",
  ndg_strict = "NDG strict"
)

scenario_label <- function(x) unname(scenario_lookup[x])
scenario_short <- function(x) unname(scenario_short_lookup[x])

fmt_signed <- function(x, accuracy = 0.1) {
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(
      abs(x) < (accuracy / 2),
      "0.0",
      ifelse(
        x > 0,
        paste0("+", number(x, accuracy = accuracy)),
        number(x, accuracy = accuracy)
      )
    )
  )
}

scenario_order <- c("red_10", "red_20", "red_50", "ndg_conservative", "ndg_central", "ndg_strict")
scenario_levels <- scenario_label(scenario_order)
indicator_levels <- c("GHG", "Land use", "Water use", "Eutrophication")

scenario_health <- read.csv(file.path(root, "output", "scenario_health_economic_summary.csv"), stringsAsFactors = FALSE)
summary_env <- read.csv(file.path(root, "output", "summary_health_environment_upf_scenarios.csv"), stringsAsFactors = FALSE)
basket_shares <- read.csv(file.path(root, "output", "env_replacement_basket_shares_curated.csv"), stringsAsFactors = FALSE)

health_df <- scenario_health %>%
  mutate(
    canon = canonical_scenario(scenario),
    scenario = factor(scenario_label(canon), levels = scenario_levels),
    short = scenario_short(canon)
  ) %>%
  arrange(scenario)

env_long <- summary_env %>%
  mutate(
    canon = canonical_scenario(scenario),
    scenario = factor(scenario_label(canon), levels = scenario_levels),
    short = scenario_short(canon),
    substitution = case_when(
      scenario_model == "no_replacement" & replacement_target == "none" ~ "No substitution",
      scenario_model == "isocaloric_replacement" & replacement_target == "NDG_ARG" ~ "Isocaloric NDG basket",
      scenario_model == "isocaloric_replacement" & replacement_target == "NOVA1" ~ "Isocaloric NOVA 1",
      scenario_model == "isocaloric_replacement" & replacement_target == "NOVA3" ~ "Isocaloric NOVA 3",
      scenario_model == "isocaloric_replacement" & replacement_target == "NOVA1_NOVA3_mix" ~ "Isocaloric NOVA 1/3 mix",
      scenario_model == "isoweight_replacement" & replacement_target == "NDG_ARG" ~ "Isoweight NDG basket",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(substitution)) %>%
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
    indicator = factor(indicator, levels = indicator_levels)
  )

# Figure 1: dumbbell plot between no substitution and NDG basket
dumbbell_df <- env_long %>%
  filter(substitution %in% c("No substitution", "Isocaloric NDG basket")) %>%
  mutate(
    substitution = factor(substitution, levels = c("No substitution", "Isocaloric NDG basket")),
    scenario = factor(scenario, levels = rev(scenario_levels))
  ) %>%
  select(indicator, scenario, substitution, net_change_pct) %>%
  tidyr::pivot_wider(names_from = substitution, values_from = net_change_pct) %>%
  mutate(
    gap = `Isocaloric NDG basket` - `No substitution`,
    change_label = ifelse(
      abs(gap) < 0.05,
      "minimal shift",
      paste0(ifelse(gap > 0, "+", ""), number(gap, accuracy = 0.1), " pp")
    )
  )

fig1 <- ggplot(dumbbell_df, aes(y = scenario)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "#ECFDF5", alpha = 0.7) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#FFF7ED", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, colour = col_muted) +
  geom_segment(
    aes(x = `No substitution`, xend = `Isocaloric NDG basket`, yend = scenario),
    linewidth = 1.15,
    colour = alpha(col_muted, 0.7),
    lineend = "round"
  ) +
  geom_point(aes(x = `No substitution`, colour = "No substitution"), size = 3.0) +
  geom_point(aes(x = `Isocaloric NDG basket`, colour = "Isocaloric NDG basket"), size = 3.0) +
  geom_text(
    aes(x = `Isocaloric NDG basket`, label = change_label),
    nudge_x = 0.9,
    hjust = 0,
    size = 2.95,
    colour = col_text
  ) +
  facet_wrap(~ indicator, scales = "free_x", ncol = 2) +
  scale_colour_manual(
    values = c("No substitution" = col_no_sub, "Isocaloric NDG basket" = col_ndg_basket),
    name = NULL
  ) +
  labs(
    title = "Environmental trade-offs depend on the substitution pathway",
    subtitle = "Each row compares the same scenario under no substitution versus an isocaloric NDG basket;\nlabels show the shift between assumptions",
    x = "Net environmental change (%)",
    y = NULL
  ) +
  theme_options(11.5) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.spacing = grid::unit(1.25, "lines"),
    legend.position = "top",
    plot.margin = margin(14, 40, 14, 18)
  ) +
  coord_cartesian(clip = "off")

save_all(fig1, file.path(out_dir, "opt_fig1_dumbbell_substitution_tradeoffs"), 235, 180)
save_all(fig1, file.path(out_dir, "opt_fig1_slopegraph_substitution_tradeoffs"), 235, 180)

# Figure 2: policy matrix with actual values and metric-specific scaling
health_tiles <- bind_rows(
  health_df %>%
    transmute(
      scenario,
      domain = "Health / economy",
      metric = "Deaths\naverted",
      metric_key = "Deaths averted",
      signed_value = deaths_averted,
      label = comma(round(deaths_averted))
    ),
  health_df %>%
    transmute(
      scenario,
      domain = "Health / economy",
      metric = "YLL\naverted",
      metric_key = "YLL averted",
      signed_value = yll_averted,
      label = comma(round(yll_averted))
    ),
  health_df %>%
    transmute(
      scenario,
      domain = "Health / economy",
      metric = "PVLE\naverted\n(billion intl.$ PPP)",
      metric_key = "PVLE averted",
      signed_value = pvle_averted_billion_intl_ppp,
      label = number(pvle_averted_billion_intl_ppp, accuracy = 0.01)
    )
)

env_tiles <- env_long %>%
  filter(substitution %in% c("No substitution", "Isocaloric NDG basket")) %>%
  transmute(
    scenario,
    domain = ifelse(substitution == "No substitution", "Environment\nNo substitution", "Environment\nIsocaloric NDG basket"),
    metric = recode(
      as.character(indicator),
      "Land use" = "Land\nuse",
      "Water use" = "Water\nuse",
      "Eutrophication" = "Eutro."
    ),
    metric_key = as.character(indicator),
    signed_value = -net_change_pct,
    label = fmt_signed(net_change_pct, accuracy = 0.1)
  )

tile_df <- bind_rows(health_tiles, env_tiles) %>%
  mutate(
    domain = factor(
      domain,
      levels = c("Health / economy", "Environment\nNo substitution", "Environment\nIsocaloric NDG basket")
    ),
    metric = factor(
      metric,
      levels = c(
        "Deaths\naverted",
        "YLL\naverted",
        "PVLE\naverted\n(billion intl.$ PPP)",
        "GHG",
        "Land\nuse",
        "Water\nuse",
        "Eutro."
      )
    )
  ) %>%
  group_by(metric_key) %>%
  mutate(score = signed_value / max(abs(signed_value), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(label_colour = ifelse(abs(score) > 0.58, "white", col_text))

fig2 <- ggplot(tile_df, aes(x = metric, y = scenario, fill = score)) +
  geom_tile(colour = "white", linewidth = 0.9) +
  geom_text(aes(label = label), size = 3.05, colour = tile_df$label_colour, lineheight = 0.95) +
  facet_grid(. ~ domain, scales = "free_x", space = "free_x") +
  scale_fill_gradient2(
    low = col_score_low,
    mid = col_score_mid,
    high = col_score_high,
    midpoint = 0,
    limits = c(-1, 1),
    labels = label_number(accuracy = 0.1),
    name = "Relative\nperformance"
  ) +
  scale_y_discrete(limits = rev(scenario_levels)) +
  labs(
    title = "Policy profile matrix across health, economic, and environmental outcomes",
    subtitle = "Tile color is scaled within each metric; labels show the underlying values",
    x = NULL,
    y = NULL
  ) +
  theme_options(11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", lineheight = 0.92),
    strip.text.x = element_text(hjust = 0),
    panel.spacing.x = grid::unit(1.2, "lines"),
    legend.position = "right"
  )

save_all(fig2, file.path(out_dir, "opt_fig2_policy_profile_matrix"), 270, 150)

# Figure 3: decomposition of the environmental response under the NDG central scenario
profile_order <- c(
  "No substitution",
  "Isocaloric NDG basket",
  "Isoweight NDG basket",
  "Isocaloric NOVA 1/3 mix",
  "Isocaloric NOVA 1",
  "Isocaloric NOVA 3"
)

decomp_source <- env_long %>%
  filter(canon == "ndg_central", substitution %in% profile_order) %>%
  mutate(substitution = factor(substitution, levels = profile_order))

base_effects <- decomp_source %>%
  filter(substitution == "No substitution") %>%
  transmute(indicator, base_change = net_change_pct)

decomp_totals <- decomp_source %>%
  left_join(base_effects, by = "indicator") %>%
  mutate(
    upf_removed_effect = base_change,
    replacement_mix_effect = net_change_pct - base_change
  )

decomp_bars <- bind_rows(
  decomp_totals %>%
    transmute(substitution, indicator, component = "UPF removed effect", value = upf_removed_effect),
  decomp_totals %>%
    transmute(substitution, indicator, component = "Replacement mix effect", value = replacement_mix_effect)
) %>%
  mutate(component = factor(component, levels = c("UPF removed effect", "Replacement mix effect")))

decomp_labels <- decomp_totals %>%
  transmute(substitution, indicator, total = net_change_pct) %>%
  group_by(indicator) %>%
  mutate(
    offset = max(abs(total), na.rm = TRUE) * 0.08,
    label_y = total + ifelse(total >= 0, offset, -offset),
    label = fmt_signed(total, accuracy = 0.1)
  ) %>%
  ungroup()

fig3 <- ggplot(decomp_bars, aes(x = substitution, y = value, fill = component)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, colour = col_muted) +
  geom_col(width = 0.72) +
  geom_point(
    data = decomp_labels,
    aes(x = substitution, y = total),
    inherit.aes = FALSE,
    size = 1.9,
    colour = col_text
  ) +
  geom_text(
    data = decomp_labels,
    aes(x = substitution, y = label_y, label = label),
    inherit.aes = FALSE,
    size = 3.05,
    colour = col_text
  ) +
  facet_wrap(~ indicator, scales = "free_y", ncol = 2) +
  coord_flip(clip = "off") +
  scale_x_discrete(limits = rev(profile_order)) +
  scale_fill_manual(
    values = c("UPF removed effect" = col_base_effect, "Replacement mix effect" = col_replacement_effect),
    name = NULL
  ) +
  labs(
    title = "How the replacement mix modifies the environmental response",
    subtitle = "NDG central scenario: total net change equals the no-substitution effect plus the incremental replacement effect",
    x = NULL,
    y = "Net environmental change (%)"
  ) +
  theme_options(11.2) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.spacing = grid::unit(1.2, "lines")
  )

save_all(fig3, file.path(out_dir, "opt_fig3_decomposition_environmental_response"), 235, 175)

# Figure 4: alluvial comparison of basket composition
alluvial_df <- basket_shares %>%
  transmute(
    env_group,
    family = case_when(
      env_group == "Fruits" ~ "Fruit",
      env_group %in% c("Vegetables (outdoor)", "Legumes and pulses", "Starchy vegetables") ~ "Vegetables /\npulses",
      env_group %in% c("Cereals (without rice)", "Rice", "Pasta", "Baked products") ~ "Grains /\nbaked",
      env_group %in% c("Dairy products", "Eggs") ~ "Dairy /\neggs",
      env_group %in% c("Beef", "Lamb and mutton") ~ "Ruminant\nmeats",
      env_group %in% c("Other meats", "Poultry", "Pork", "Fish and seafood") ~ "Other animal-\nsource",
      env_group %in% c("Oil crops", "Nuts and seeds") ~ "Fats, nuts\nand seeds",
      TRUE ~ "Other"
    ),
    `Observed\nnon-UPF` = baseline_non_upf_share_pct,
    `NOVA 1\nbasket` = nova1_share_pct,
    `NOVA 3\nbasket` = nova3_share_pct,
    `NOVA 1/3\nmix` = nova1_nova3_mix_share_pct,
    `NDG\nbasket` = NDG_target_share_pct
  ) %>%
  pivot_longer(
    cols = c(`Observed\nnon-UPF`, `NOVA 1\nbasket`, `NOVA 3\nbasket`, `NOVA 1/3\nmix`, `NDG\nbasket`),
    names_to = "basket",
    values_to = "share"
  ) %>%
  group_by(basket, family) %>%
  summarise(share = sum(share), .groups = "drop") %>%
  mutate(
    basket = factor(
      basket,
      levels = c("Observed\nnon-UPF", "NOVA 1\nbasket", "NOVA 3\nbasket", "NOVA 1/3\nmix", "NDG\nbasket")
    ),
    family = factor(
      family,
      levels = c(
        "Grains /\nbaked",
        "Vegetables /\npulses",
        "Fruit",
        "Dairy /\neggs",
        "Other animal-\nsource",
        "Ruminant\nmeats",
        "Fats, nuts\nand seeds"
      )
    )
  )

basket_levels_vec <- levels(alluvial_df$basket)
family_levels_vec <- levels(alluvial_df$family)
alluvial_fill_values <- c(
  stats::setNames(rep("#F5F1EB", length(basket_levels_vec)), basket_levels_vec),
  family_cols
)

total_alluvial <- sum(alluvial_df$share)

basket_label_df <- alluvial_df %>%
  group_by(basket) %>%
  summarise(total = sum(share), .groups = "drop") %>%
  arrange(basket) %>%
  mutate(
    top = total_alluvial - cumsum(dplyr::lag(total, default = 0)),
    bottom = top - total,
    mid = (top + bottom) / 2,
    label = as.character(basket)
  )

family_label_df <- alluvial_df %>%
  group_by(family) %>%
  summarise(
    total = sum(share),
    min_share = min(share),
    max_share = max(share),
    .groups = "drop"
  ) %>%
  arrange(family) %>%
  mutate(
    top = total_alluvial - cumsum(dplyr::lag(total, default = 0)),
    bottom = top - total,
    mid = (top + bottom) / 2,
    range_label = paste0(number(min_share, accuracy = 0.1), "-", number(max_share, accuracy = 0.1), "%"),
    display_name = recode(
      as.character(family),
      "Ruminant\nmeats" = "Ruminant",
      "Fats, nuts\nand seeds" = "Fats/nuts",
      .default = as.character(family)
    ),
    is_small = total < 30,
    name_colour = unname(family_cols[as.character(family)])
  )

top_flow_labels <- alluvial_df %>%
  arrange(basket, family) %>%
  left_join(basket_label_df %>% select(basket, top), by = "basket") %>%
  group_by(basket) %>%
  mutate(
    flow_top = top - cumsum(dplyr::lag(share, default = 0)),
    flow_bottom = flow_top - share,
    flow_mid = (flow_top + flow_bottom) / 2
  ) %>%
  slice_max(order_by = share, n = 2, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    short_family = recode(
      as.character(family),
      "Grains /\nbaked" = "Grains/baked",
      "Vegetables /\npulses" = "Vegetables/pulses",
      "Dairy /\neggs" = "Dairy/eggs",
      "Other animal-\nsource" = "Other animal foods",
      "Ruminant\nmeats" = "Ruminant meats",
      "Fats, nuts\nand seeds" = "Fats/nuts/seeds",
      "Fruit" = "Fruit"
    ),
    label = paste0(short_family, " ", number(share, accuracy = 0.1), "%"),
    text_colour = case_when(
      family == "Fruit" ~ col_text,
      TRUE ~ "white"
    )
  )

fig4 <- ggplot(
  alluvial_df,
  aes(axis1 = basket, axis2 = family, y = share)
) +
  geom_alluvium(
    aes(fill = family),
    width = 0.24,
    alpha = 0.9,
    knot.pos = 0.38,
    colour = alpha("white", 0.5),
    decreasing = FALSE
  ) +
  geom_stratum(
    aes(fill = after_stat(stratum)),
    width = 0.24,
    colour = "#D1D5DB",
    linewidth = 0.6
  ) +
  geom_text(
    data = basket_label_df,
    aes(x = 1, y = mid, label = label),
    inherit.aes = FALSE,
    size = 3.0,
    fontface = "bold",
    colour = col_text
  ) +
  geom_text(
    data = family_label_df %>% filter(!is_small),
    aes(x = 2, y = mid, label = display_name, colour = family),
    inherit.aes = FALSE,
    size = 2.9,
    lineheight = 0.92,
    fontface = "bold"
  ) +
  geom_text(
    data = family_label_df %>% filter(!is_small),
    aes(x = 2.08, y = mid, label = range_label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 2.7,
    colour = col_muted
  ) +
  geom_segment(
    data = family_label_df %>% filter(is_small) %>% arrange(desc(mid)) %>% mutate(label_y = c(36, 15)[seq_len(n())]),
    aes(x = 2.12, xend = 2.18, y = mid, yend = label_y),
    inherit.aes = FALSE,
    linewidth = 0.45,
    colour = col_muted
  ) +
  geom_text(
    data = family_label_df %>% filter(is_small) %>% arrange(desc(mid)) %>% mutate(label_y = c(36, 15)[seq_len(n())]),
    aes(x = 2.2, y = label_y, label = paste0(display_name, "\n", range_label), colour = family),
    inherit.aes = FALSE,
    hjust = 0,
    size = 2.55,
    lineheight = 0.9
  ) +
  geom_label(
    data = top_flow_labels,
    aes(x = 1.18, y = flow_mid, label = label, fill = family),
    inherit.aes = FALSE,
    hjust = 0,
    size = 2.55,
    linewidth = 0.15,
    label.r = unit(0.08, "lines"),
    colour = top_flow_labels$text_colour,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = alluvial_fill_values,
    breaks = family_levels_vec,
    labels = gsub("\n", " ", family_levels_vec),
    name = "Food family"
  ) +
  scale_colour_manual(values = family_cols, guide = "none") +
  scale_x_discrete(
    limits = c("Basket", "Food family"),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    title = "Replacement baskets differ sharply in their food-group composition",
    subtitle = "Ribbon widths show within-basket shares; callouts mark the two largest components in each basket,\nand right-side labels show the range across baskets",
    x = NULL,
    y = NULL
  ) +
  theme_options(11.2) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face = "bold"),
    legend.position = "top",
    legend.box = "vertical",
    plot.margin = margin(14, 52, 14, 18)
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1, colour = NA), nrow = 2, byrow = TRUE)) +
  coord_cartesian(clip = "off")

save_all(fig4, file.path(out_dir, "opt_fig4_replacement_baskets_alluvial"), 245, 190)

cat("Additional submission-oriented figure options written to:\n")
cat(" - ", file.path(out_dir, "opt_fig1_dumbbell_substitution_tradeoffs.(png|tiff|pdf)"), "\n", sep = "")
cat(" - ", file.path(out_dir, "opt_fig1_slopegraph_substitution_tradeoffs.(png|tiff|pdf)"), "\n", sep = "")
cat(" - ", file.path(out_dir, "opt_fig2_policy_profile_matrix.(png|tiff|pdf)"), "\n", sep = "")
cat(" - ", file.path(out_dir, "opt_fig3_decomposition_environmental_response.(png|tiff|pdf)"), "\n", sep = "")
cat(" - ", file.path(out_dir, "opt_fig4_replacement_baskets_alluvial.(png|tiff|pdf)"), "\n", sep = "")
