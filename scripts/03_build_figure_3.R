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

col_div_low  <- "#C2410C"
col_div_mid  <- "#F8FAFC"
col_div_high <- "#1F3A4D"

panel_a <- read_pkg_csv("replacement_basket_composition.csv") %>%
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

group_cols <- c(
  "Vegetables (outdoor)" = "#1F7A6F",
  "Starchy vegetables"   = "#7FB8A8",
  "Legumes and pulses"   = "#2E8B76",
  "Oil crops"            = "#AFD0B8",
  "Fruits"               = "#F4A582",
  "Nuts and seeds"       = "#D98B6A",
  "Cereals (without rice)" = "#B8A9D0",
  "Rice"                   = "#E4DEF0",
  "Pasta"                  = "#9482B8",
  "Baked products"         = "#6B5A94",
  "Dairy products" = "#CFE2EC",
  "Eggs"           = "#F3D98B",
  "Beef"            = "#A63A50",
  "Lamb and mutton" = "#6B1F33",
  "Pork"            = "#E7959E",
  "Poultry"         = "#F1B6B1",
  "Other meats"     = "#C85A6A",
  "Fish and seafood" = "#1F3A4D"
)

text_on_fill <- function(hex) {
  rgb_mat <- col2rgb(hex) / 255
  lum <- 0.2126 * rgb_mat[1, ] + 0.7152 * rgb_mat[2, ] + 0.0722 * rgb_mat[3, ]
  ifelse(lum < 0.55, "white", col_ink)
}
group_text_colour <- setNames(text_on_fill(group_cols), names(group_cols))

food_group_levels <- c(
  "Vegetables (outdoor)", "Starchy vegetables",
  "Legumes and pulses", "Oil crops",
  "Fruits", "Nuts and seeds",
  "Cereals (without rice)", "Rice", "Pasta", "Baked products",
  "Dairy products", "Eggs",
  "Beef", "Lamb and mutton", "Pork", "Poultry", "Other meats",
  "Fish and seafood"
)
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
  mutate(basket = fct_rev(basket))

baseline_vec <- panel_a %>% select(food_group, baseline = `Observed non-UPF`)

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
    aes(xmin = xmin, xmax = xmax, ymin = as.numeric(basket) - 0.34, ymax = as.numeric(basket) + 0.34, fill = food_group),
    colour = "white",
    linewidth = 0.28
  ) +
  geom_text(aes(x = xmid, y = as.numeric(basket), label = label, colour = text_colour), size = 2.05, fontface = "bold", na.rm = TRUE) +
  scale_fill_manual(values = group_cols, breaks = food_group_levels, labels = function(x) unname(food_group_labels[x]), name = "Food group") +
  scale_colour_identity() +
  scale_y_continuous(breaks = seq_len(nlevels(stacked_df$basket)), labels = unname(basket_labels[rev(basket_levels)])) +
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), labels = label_percent(scale = 1)) +
  labs(tag = "a", title = "Composition of each replacement basket", x = "% of basket energy", y = NULL) +
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
  guides(fill = guide_legend(ncol = 6, byrow = TRUE, title.position = "top"))

p2 <- ggplot(delta, aes(basket, food_group, fill = delta_pp)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = label, colour = label_colour), size = 2.05, lineheight = 0.92) +
  scale_fill_gradient2(low = col_div_low, mid = col_div_mid, high = col_div_high, midpoint = 0,
                       limits = c(-30, 30), breaks = c(-30, -15, 0, 15, 30),
                       labels = function(x) paste0(number(x, accuracy = 1), " pp"), name = "Change vs\nObserved") +
  scale_colour_identity() +
  scale_x_discrete(labels = function(x) unname(basket_labels[x])) +
  scale_y_discrete(labels = function(x) unname(food_group_labels[x])) +
  labs(tag = "b", title = "Difference from the observed non-UPF basket", x = NULL, y = NULL) +
  theme_nf() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = rel(0.76), lineheight = 0.95),
    legend.position = "bottom"
  )

fig <- p1 / p2 + plot_layout(heights = c(0.95, 1.12))
save_pair(fig, "Figure_3", width_mm = 195, height_mm = 225)
message("Saved Figure_3 to ", FIG_DIR)
