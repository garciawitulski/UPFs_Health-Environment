# 22_ed1_replacement_baskets.R
# ---------------------------------------------------------------------------
# Builds a cleaner replacement-basket figure directly from Table S7 Panel A,
# preserving the exact food groups reported in the appendix.
# Outputs to output/figures_final/extended_data/ and can also be copied into
# other submission bundles.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(forcats)
  library(patchwork)
})

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
OUT <- file.path(root, "output")
DEST <- file.path(OUT, "figures_final", "extended_data")
dir.create(DEST, showWarnings = FALSE, recursive = TRUE)

col_ink <- "#111827"
col_muted <- "#6B7280"
col_grid <- "#E5E7EB"
col_seq_low <- "#F8F6F1"
col_seq_high <- "#0F766E"
col_div_low <- "#B91C1C"
col_div_mid <- "#F8F6F1"
col_div_high <- "#0F766E"

theme_nf <- function(base = 9.2) {
  theme_minimal(base_size = base) +
    theme(
      text = element_text(colour = col_ink),
      plot.title = element_text(face = "bold", size = rel(1.05), hjust = 0),
      plot.subtitle = element_text(colour = col_muted, size = rel(0.86), hjust = 0),
      panel.grid.major = element_line(colour = col_grid, linewidth = 0.25),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = col_ink, size = rel(0.8)),
      strip.text = element_text(face = "bold", size = rel(0.88)),
      plot.margin = margin(8, 10, 8, 10)
    )
}

save_pair <- function(plot_obj, base_name, width_mm = 185, height_mm = 220) {
  for (ext in c(".pdf", ".png", ".tiff")) {
    f <- file.path(DEST, paste0(base_name, ext))
    if (ext == ".pdf") {
      ggsave(f, plot_obj, width = width_mm, height = height_mm, units = "mm", dpi = 400, device = cairo_pdf)
    } else if (ext == ".tiff") {
      ggsave(f, plot_obj, width = width_mm, height = height_mm, units = "mm", dpi = 400, compression = "lzw")
    } else {
      ggsave(f, plot_obj, width = width_mm, height = height_mm, units = "mm", dpi = 400)
    }
    message("  saved ", basename(f))
  }
}

panel_a_path <- file.path(OUT, "table_s7_scenario_composition_panelA.csv")
if (!file.exists(panel_a_path)) {
  stop("Missing Table S7 Panel A CSV: ", panel_a_path)
}

panel_a <- read.csv(panel_a_path, stringsAsFactors = FALSE, check.names = FALSE) %>%
  transmute(
    food_group = as.character(food_group),
    `Observed non-UPF` = as.numeric(observed_non_upf),
    `NOVA 1` = as.numeric(nova1),
    `NOVA 3` = as.numeric(nova3),
    `NOVA 1/NOVA 3 mix` = as.numeric(nova1_nova3_mix),
    `NDG target` = as.numeric(NDG_target)
  )

food_group_levels <- panel_a$food_group
basket_levels <- c("Observed non-UPF", "NOVA 1", "NOVA 3", "NOVA 1/NOVA 3 mix", "NDG target")
basket_labels <- c(
  "Observed non-UPF" = "Observed\nnon-UPF basket",
  "NOVA 1" = "NOVA 1",
  "NOVA 3" = "NOVA 3",
  "NOVA 1/NOVA 3 mix" = "NOVA 1/NOVA 3 mix",
  "NDG target" = "NDG target"
)

food_group_labels <- c(
  "Fruits" = "Fruits",
  "Vegetables (outdoor)" = "Vegetables\n(outdoor)",
  "Legumes and pulses" = "Legumes\nand pulses",
  "Cereals (without rice)" = "Cereals\n(no rice)",
  "Rice" = "Rice",
  "Pasta" = "Pasta",
  "Baked products" = "Baked\nproducts",
  "Starchy vegetables" = "Starchy\nvegetables",
  "Dairy products" = "Dairy\nproducts",
  "Beef" = "Beef",
  "Lamb and mutton" = "Lamb and\nmutton",
  "Other meats" = "Other\nmeats",
  "Poultry" = "Poultry",
  "Pork" = "Pork",
  "Fish and seafood" = "Fish and\nseafood",
  "Eggs" = "Eggs",
  "Oil crops" = "Oil crops",
  "Nuts and seeds" = "Nuts and\nseeds"
)

long <- panel_a %>%
  pivot_longer(-food_group, names_to = "basket", values_to = "share_pct") %>%
  mutate(
    food_group = factor(food_group, levels = rev(food_group_levels)),
    basket = factor(basket, levels = basket_levels),
    label = ifelse(share_pct < 0.05, "0", number(share_pct, accuracy = 0.1)),
    label_colour = ifelse(share_pct >= 15, "white", col_ink)
  )

baseline_vec <- panel_a %>%
  select(food_group, baseline = `Observed non-UPF`)

delta <- panel_a %>%
  pivot_longer(-food_group, names_to = "basket", values_to = "share_pct") %>%
  filter(basket != "Observed non-UPF") %>%
  left_join(baseline_vec, by = "food_group") %>%
  mutate(
    delta_pp = share_pct - baseline,
    food_group = factor(food_group, levels = rev(food_group_levels)),
    basket = factor(basket, levels = basket_levels[basket_levels != "Observed non-UPF"]),
    label = ifelse(abs(delta_pp) < 0.05, "0", sprintf("%+.1f", round(delta_pp, 1))),
    label_colour = ifelse(abs(delta_pp) >= 12, "white", col_ink)
  )

p1 <- ggplot(long, aes(basket, food_group, fill = share_pct)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = label, colour = label_colour), size = 2.15, lineheight = 0.92) +
  scale_fill_gradient(
    low = col_seq_low,
    high = col_seq_high,
    limits = c(0, max(long$share_pct, na.rm = TRUE)),
    breaks = c(0, 10, 20, 30, 40, 50),
    labels = label_number(accuracy = 1, suffix = "%"),
    name = "Energy share"
  ) +
  scale_colour_identity() +
  scale_x_discrete(labels = function(x) unname(basket_labels[x])) +
  scale_y_discrete(labels = function(x) unname(food_group_labels[x])) +
  labs(
    tag = "a",
    title = "Energy share of each Table S7 food group across replacement baskets",
    x = NULL,
    y = NULL
  ) +
  theme_nf() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = rel(0.76), lineheight = 0.95),
    legend.position = "bottom"
  )

p2 <- ggplot(delta, aes(basket, food_group, fill = delta_pp)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = label, colour = label_colour), size = 2.12, lineheight = 0.92) +
  scale_fill_gradient2(
    low = col_div_low,
    mid = col_div_mid,
    high = col_div_high,
    midpoint = 0,
    limits = c(-30, 30),
    breaks = c(-30, -15, 0, 15, 30),
    labels = function(x) paste0(number(x, accuracy = 1), " pp"),
    name = "Change vs\nObserved"
  ) +
  scale_colour_identity() +
  scale_x_discrete(labels = function(x) unname(basket_labels[x])) +
  scale_y_discrete(labels = function(x) unname(food_group_labels[x])) +
  labs(
    tag = "b",
    title = "Difference from the observed non-UPF basket",
    x = NULL,
    y = NULL
  ) +
  theme_nf() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = rel(0.76), lineheight = 0.95),
    legend.position = "bottom"
  )

fig <- p1 / p2 +
  plot_layout(heights = c(1.05, 1))

save_pair(fig, "ED1_replacement_baskets")
