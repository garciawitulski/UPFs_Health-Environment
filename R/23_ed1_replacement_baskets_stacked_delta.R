# 23_ed1_replacement_baskets_stacked_delta.R
# ---------------------------------------------------------------------------
# Alternative replacement-basket figure based on Table S7 food groups:
#   (a) stacked composition bars
#   (b) delta heatmap vs observed non-UPF basket
# Outputs to output/figures_final/alternatives/
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
DEST <- file.path(OUT, "figures_final", "alternatives")
dir.create(DEST, showWarnings = FALSE, recursive = TRUE)

col_ink <- "#111827"
col_muted <- "#6B7280"
col_grid <- "#E5E7EB"
col_div_low  <- "#C2410C"   # burnt orange
col_div_mid  <- "#F8FAFC"   # off white
col_div_high <- "#1F3A4D"   # deep slate (matches main figures)

theme_nf <- function(base = 9.3) {
  theme_minimal(base_size = base) +
    theme(
      text = element_text(colour = col_ink),
      plot.title = element_text(face = "bold", size = rel(1.05), hjust = 0),
      plot.subtitle = element_text(colour = col_muted, size = rel(0.86), hjust = 0),
      panel.grid.major = element_line(colour = col_grid, linewidth = 0.25),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = col_ink, size = rel(0.82)),
      strip.text = element_text(face = "bold", size = rel(0.9)),
      legend.title = element_text(face = "bold", size = rel(0.9)),
      legend.text = element_text(size = rel(0.8)),
      plot.margin = margin(8, 10, 8, 10)
    )
}

save_pair <- function(plot_obj, base_name, width_mm = 195, height_mm = 225) {
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

basket_levels <- c("Observed non-UPF", "NOVA 1", "NOVA 3", "NOVA 1/NOVA 3 mix", "NDG target")
basket_labels <- c(
  "Observed non-UPF" = "Observed non-UPF basket",
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

## Semantic palette: food groups coloured by category family for readability
## - Plants & vegetables: greens
## - Fruits / nuts:       warm yellows / tans
## - Cereals & starches:  ambers / wheat
## - Dairy & eggs:        cool light tones
## - Meats:               reds
## - Fish:                steel blue
## Editorial palette: cohesive cool-to-warm ramp grouped by food family.
## Plants = teal/green family, grains = sand/ochre, dairy/eggs = pale slate,
## meats = coral/brick ramp, fish = deep slate (anchors to main figs).
group_cols <- c(
  # Vegetables & legumes (teal-green family)
  "Vegetables (outdoor)" = "#1F7A6F",
  "Starchy vegetables"   = "#7FB8A8",
  "Legumes and pulses"   = "#2E8B76",
  "Oil crops"            = "#AFD0B8",
  # Fruits & nuts (soft coral/peach)
  "Fruits"               = "#F4A582",
  "Nuts and seeds"       = "#D98B6A",
  # Cereals, rice, pasta, baked (soft lavender/mauve family)
  "Cereals (without rice)" = "#B8A9D0",
  "Rice"                   = "#E4DEF0",
  "Pasta"                  = "#9482B8",
  "Baked products"         = "#6B5A94",
  # Dairy & eggs (pale sky)
  "Dairy products" = "#CFE2EC",
  "Eggs"           = "#F3D98B",
  # Meats (rose to wine ramp)
  "Beef"            = "#A63A50",
  "Lamb and mutton" = "#6B1F33",
  "Pork"            = "#E7959E",
  "Poultry"         = "#F1B6B1",
  "Other meats"     = "#C85A6A",
  # Fish (deep slate — anchors to main figure palette)
  "Fish and seafood" = "#1F3A4D"
)

## Compute readable text colour per fill from relative luminance (WCAG-ish)
## so we don't have to hand-list dark groups.
text_on_fill <- function(hex) {
  rgb_mat <- col2rgb(hex) / 255
  lum <- 0.2126 * rgb_mat[1, ] + 0.7152 * rgb_mat[2, ] + 0.0722 * rgb_mat[3, ]
  ifelse(lum < 0.55, "white", col_ink)
}
group_text_colour <- setNames(text_on_fill(group_cols), names(group_cols))

## Order food groups by semantic family (matches palette grouping), so the
## stacked bars read top-to-bottom by category instead of an arbitrary mix.
food_group_levels <- c(
  # Vegetables & legumes
  "Vegetables (outdoor)", "Starchy vegetables",
  "Legumes and pulses", "Oil crops",
  # Fruits & nuts
  "Fruits", "Nuts and seeds",
  # Cereals & starches
  "Cereals (without rice)", "Rice", "Pasta", "Baked products",
  # Dairy & eggs
  "Dairy products", "Eggs",
  # Meats
  "Beef", "Lamb and mutton", "Pork", "Poultry", "Other meats",
  # Fish
  "Fish and seafood"
)
## Drop any group not present in the data, preserve order
food_group_levels <- food_group_levels[food_group_levels %in% panel_a$food_group]

long <- panel_a %>%
  pivot_longer(-food_group, names_to = "basket", values_to = "share_pct") %>%
  mutate(
    basket = factor(basket, levels = basket_levels),
    food_group = factor(food_group, levels = food_group_levels),
    label = ifelse(share_pct >= 10, number(share_pct, accuracy = 0.1), NA_character_)
  )

stacked_df <- long %>%
  arrange(basket, food_group) %>%
  group_by(basket) %>%
  mutate(
    xmax = cumsum(share_pct),
    xmin = xmax - share_pct,
    xmid = (xmin + xmax) / 2,
    text_colour = unname(group_text_colour[as.character(food_group)])
  ) %>%
  ungroup() %>%
  mutate(
    basket = fct_rev(basket)
  )

baseline_vec <- panel_a %>%
  select(food_group, baseline = `Observed non-UPF`)

delta <- panel_a %>%
  pivot_longer(-food_group, names_to = "basket", values_to = "share_pct") %>%
  filter(basket != "Observed non-UPF") %>%
  left_join(baseline_vec, by = "food_group") %>%
  mutate(
    delta_pp = share_pct - baseline,
    basket = factor(basket, levels = basket_levels[basket_levels != "Observed non-UPF"]),
    food_group = factor(food_group, levels = rev(food_group_levels)),
    label = ifelse(abs(delta_pp) < 0.05, "0", sprintf("%+.1f", round(delta_pp, 1))),
    label_colour = ifelse(abs(delta_pp) >= 18, "white", col_ink)
  )

p1 <- ggplot(stacked_df) +
  geom_rect(
    aes(
      xmin = xmin, xmax = xmax,
      ymin = as.numeric(basket) - 0.34,
      ymax = as.numeric(basket) + 0.34,
      fill = food_group
    ),
    colour = "white",
    linewidth = 0.28
  ) +
  geom_text(
    aes(x = xmid, y = as.numeric(basket), label = label, colour = text_colour),
    size = 2.05,
    fontface = "bold",
    na.rm = TRUE
  ) +
  scale_fill_manual(
    values = group_cols,
    breaks = food_group_levels,
    labels = function(x) unname(food_group_labels[x]),
    name = "Food group"
  ) +
  scale_colour_identity() +
  scale_y_continuous(
    breaks = seq_len(nlevels(stacked_df$basket)),
    labels = unname(basket_labels[rev(basket_levels)])
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = label_percent(scale = 1)
  ) +
  labs(
    tag = "a",
    title = "Composition of each replacement basket",
    x = "% of basket energy",
    y = NULL
  ) +
  theme_nf() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.25),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.width = unit(5.5, "mm"),
    legend.key.height = unit(3.2, "mm"),
    legend.spacing.x = unit(2, "mm"),
    axis.text.y = element_text(size = rel(0.85)),
    axis.text.x = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(ncol = 6, byrow = TRUE,
                             title.position = "top"))

p2 <- ggplot(delta, aes(basket, food_group, fill = delta_pp)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = label, colour = label_colour), size = 2.05, lineheight = 0.92) +
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
  plot_layout(heights = c(0.95, 1.12))

save_pair(fig, "ED1_replacement_baskets_stacked_delta")
