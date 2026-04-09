# 24_results_section_figures.R
# ---------------------------------------------------------------------------
# Four manuscript figures, one per Results paragraph that previously cited
# only an Online Appendix table:
#   Fig_R1_upf_heatmap        -> Table S9  (UPF % energy by sex x age)
#   Fig_R2_attributable_pyramid -> Table S10 (attributable deaths + PAF)
#   Fig_R3_burden_lollipop    -> Table S11 (YLL, e30, indirect costs by sex)
#   Fig_R4_scenario_ladder    -> Table S12 (counterfactual scenarios, 95% UI)
# ---------------------------------------------------------------------------

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

parse_ci <- function(x) {
  m <- regmatches(x, regexec("([0-9.]+)\\s*\\(([0-9.]+),\\s*([0-9.]+)\\)", x))
  out <- do.call(rbind, lapply(m, function(v) {
    if (length(v) < 4) c(NA, NA, NA) else as.numeric(v[2:4])
  }))
  colnames(out) <- c("est", "lo", "hi")
  as.data.frame(out)
}

# ---------------------------------------------------------------------------
# FIG R1: Heatmap sex x age, UPF % of total energy (Table S9)
# ---------------------------------------------------------------------------
t1_full <- read_pkg_csv("Tabla1_UPF_por_energia_Argentina.csv")
names(t1_full)[1] <- "age_group"
t1 <- t1_full[t1_full$age_group != "Total", ]
t1_tot <- t1_full[t1_full$age_group == "Total", ]
mean_men_t1   <- parse_ci(t1_tot$`Men, % (95% CI)`)$est
mean_women_t1 <- parse_ci(t1_tot$`Women, % (95% CI)`)$est

men_ci   <- parse_ci(t1$`Men, % (95% CI)`);   men_ci$sex   <- "Men";   men_ci$age   <- t1$age_group
women_ci <- parse_ci(t1$`Women, % (95% CI)`); women_ci$sex <- "Women"; women_ci$age <- t1$age_group
hd <- bind_rows(men_ci, women_ci) %>%
  mutate(
    age = factor(age, levels = unique(t1$age_group)),
    sex = factor(sex, levels = c("Women", "Men")),  # men on top
    label_val = sprintf("%.1f", est),
    label_ci  = sprintf("[%.1f, %.1f]", lo, hi)
  )

tot_ci_men   <- parse_ci(t1_tot$`Men, % (95% CI)`)
tot_ci_women <- parse_ci(t1_tot$`Women, % (95% CI)`)
mean_df_t1 <- data.frame(
  sex = factor(c("Men", "Women"), levels = c("Women", "Men")),
  est = c(tot_ci_men$est,  tot_ci_women$est),
  lo  = c(tot_ci_men$lo,   tot_ci_women$lo),
  hi  = c(tot_ci_men$hi,   tot_ci_women$hi)
) %>%
  mutate(label_val = sprintf("%.1f", est),
         label_ci  = sprintf("[%.1f, %.1f]", lo, hi))

n_age <- nlevels(hd$age)
pal_r1 <- c("#F7FBFC", "#EAF4F7", "#D8EBF1", "#BEDCE6",
            "#9CCBD8", "#74B6C8", "#4E97AF", "#2E748E", "#184C67")

fig_r1 <- ggplot(hd, aes(age, sex, fill = est)) +
  geom_tile(colour = "white", linewidth = 1.0) +
  geom_text(aes(label = label_val, colour = est > 17),
            size = 3.1, fontface = "bold", nudge_y = 0.12) +
  geom_text(aes(label = label_ci, colour = est > 17),
            size = 2.0, nudge_y = -0.20) +
  # Mean column (separate, with CI)
  annotate("segment",
           x = n_age + 0.58, xend = n_age + 0.58,
           y = 0.5, yend = 2.5,
           colour = col_muted, linewidth = 0.4, linetype = "22") +
  geom_tile(data = mean_df_t1,
            aes(x = n_age + 1.15, y = sex, fill = est),
            width = 0.9, height = 0.9,
            colour = col_ink, linewidth = 0.35,
            inherit.aes = FALSE) +
  geom_text(data = mean_df_t1,
            aes(x = n_age + 1.15, y = sex, label = label_val,
                colour = est > 17),
            inherit.aes = FALSE,
            fontface = "bold", size = 3.1, nudge_y = 0.12) +
  geom_text(data = mean_df_t1,
            aes(x = n_age + 1.15, y = sex, label = label_ci,
                colour = est > 17),
            inherit.aes = FALSE, size = 2.0, nudge_y = -0.20) +
  annotate("text", x = n_age + 1.15, y = 0.30,
           label = "Overall", fontface = "bold",
           size = 2.85, colour = col_ink) +
  coord_cartesian(xlim = c(0.45, n_age + 1.70),
                  ylim = c(0.05, 2.65), clip = "off", expand = FALSE) +
  scale_fill_gradientn(
    colours = pal_r1,
    limits = c(12, 21),
    name   = "UPF share of total dietary energy (%)",
    breaks = c(13, 15, 17, 19, 21),
    guide  = guide_colourbar(barwidth = unit(60, "mm"),
                             barheight = unit(2.8, "mm"),
                             title.position = "top",
                             ticks.colour = col_ink,
                             frame.colour = col_ink,
                             frame.linewidth = 0.3)
  ) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = col_ink),
                      guide = "none") +
  labs(x = "Age group (years)", y = NULL) +
  theme_nf() +
  theme(
    panel.grid      = element_blank(),
    axis.text.x     = element_text(face = "bold"),
    axis.text.y     = element_text(face = "bold", size = rel(0.95)),
    axis.ticks      = element_blank(),
    plot.caption    = element_text(colour = col_muted, size = rel(0.78),
                                   hjust = 0, margin = margin(t = 4)),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold", size = rel(0.85))
  )

# ---------------------------------------------------------------------------
# FIG R2: Bilateral attributable-deaths pyramid + PAF overlay (Table S10)
# ---------------------------------------------------------------------------
t2_full <- read_pkg_csv("Tabla2_Muertes_atribuibles_Argentina.csv")
names(t2_full)[1] <- "age_group"
t2 <- t2_full[t2_full$age_group != "Total", ]
t2_tot <- t2_full[t2_full$age_group == "Total", ]
mean_men_t2_paf   <- as.numeric(t2_tot$`Men, PAF %`)
mean_women_t2_paf <- as.numeric(t2_tot$`Women, PAF %`)
total_men_t2   <- as.numeric(t2_tot$`Men, deaths attr`)
total_women_t2 <- as.numeric(t2_tot$`Women, deaths attr`)

pyr <- bind_rows(
  data.frame(age = t2$age_group, sex = "Men",
             deaths = -t2$`Men, deaths attr`,   paf = t2$`Men, PAF %`),
  data.frame(age = t2$age_group, sex = "Women",
             deaths =  t2$`Women, deaths attr`, paf = t2$`Women, PAF %`)
) %>%
  mutate(age = factor(age, levels = unique(t2$age_group)),
         sex = factor(sex, levels = c("Men", "Women")))

dmax <- max(abs(pyr$deaths))
xbreaks <- pretty(c(-dmax, dmax), n = 6)

# Editorial population pyramid: central age rail, no redundant labels,
# minimal gridlines, small PAF marker dots aligned on the outer edge.
pyr <- pyr %>%
  group_by(sex) %>%
  mutate(share = abs(deaths) / sum(abs(deaths))) %>%
  ungroup()

# gap in the middle reserved for age-group labels
gap <- dmax * 0.28
pyr2 <- pyr %>%
  mutate(
    xstart = ifelse(sex == "Men", -gap, gap),
    xend   = ifelse(sex == "Men", -gap + deaths, gap + deaths),
    yidx   = as.numeric(age)
  )

label_threshold <- dmax * 0.18
label_offset <- dmax * 0.03
paf_label_offset <- dmax * 0.032

pyr2 <- pyr2 %>%
  mutate(
    label = comma(abs(round(deaths))),
    label_inside = abs(deaths) >= label_threshold,
    label_x = case_when(
      sex == "Men" & !label_inside ~ xend - label_offset,
      sex == "Women" & !label_inside ~ xend + label_offset,
      TRUE ~ xend
    ),
    label_hjust = case_when(
      sex == "Men" & label_inside ~ -0.15,
      sex == "Women" & label_inside ~ 1.15,
      sex == "Men" ~ 1,
      TRUE ~ 0
    )
  )

# PAF lollipops placed outside the bars, but close enough to avoid leaving
# a visually large left margin in the composite figure.
paf_max <- max(pyr$paf)
paf_span <- dmax * 0.20
pyr2 <- pyr2 %>%
  mutate(paf_x = ifelse(sex == "Men",
                        -(gap + dmax) - paf_span,
                         (gap + dmax) + paf_span))

outer_breaks <- pretty(c(0, dmax), n = 4)
outer_breaks <- outer_breaks[outer_breaks > 0 & outer_breaks <= dmax]

x_limit <- (gap + dmax) + paf_span + dmax * 0.045  # compact room for PAF labels
n_age_pyr <- nlevels(pyr$age)

fig_r2 <- ggplot(pyr2) +
  # reference gridlines
  geom_segment(data = data.frame(b = outer_breaks),
               aes(x = -(gap + b), xend = -(gap + b),
                   y = 0.4, yend = n_age_pyr + 0.6),
               colour = col_grid, linewidth = 0.25) +
  geom_segment(data = data.frame(b = outer_breaks),
               aes(x =  (gap + b), xend =  (gap + b),
                   y = 0.4, yend = n_age_pyr + 0.6),
               colour = col_grid, linewidth = 0.25) +
  # main bars
  geom_rect(aes(xmin = xstart, xmax = xend,
                ymin = yidx - 0.36, ymax = yidx + 0.36,
                fill = sex), colour = NA) +
  # Count labels stay inside only when the bar is wide enough; otherwise they
  # move just outside the bar to avoid clipping in the lower-age rows.
  geom_text(
    data = pyr2 %>% filter(label_inside),
    aes(x = label_x, y = yidx, label = label, hjust = label_hjust),
    size = 2.25, colour = "white", fontface = "bold"
  ) +
  geom_text(
    data = pyr2 %>% filter(!label_inside),
    aes(x = label_x, y = yidx, label = label, hjust = label_hjust),
    size = 2.05, colour = col_ink, fontface = "bold"
  ) +
  # central age labels
  geom_text(data = data.frame(age = levels(pyr$age),
                               yidx = seq_along(levels(pyr$age))),
            aes(x = 0, y = yidx, label = age),
            fontface = "bold", size = 2.5, colour = col_rule,
            inherit.aes = FALSE) +
  # PAF lollipop: thin stem + small dot beyond the bars
  geom_segment(aes(x = ifelse(sex == "Men",
                              -(gap + dmax),
                               (gap + dmax)),
                   xend = paf_x,
                   y = yidx, yend = yidx),
               colour = col_muted, linewidth = 0.25) +
  geom_point(aes(x = paf_x, y = yidx, fill = sex),
             shape = 21, size = 1.7, stroke = 0.3, colour = col_ink) +
  geom_text(aes(x = paf_x + ifelse(sex == "Men", -paf_label_offset, paf_label_offset),
                y = yidx,
                label = sprintf("%.0f%%", paf),
                hjust = ifelse(sex == "Men", 1, 0)),
            size = 1.8, colour = col_muted) +
  # bottom axis numbers
  geom_text(data = data.frame(b = outer_breaks),
            aes(x = -(gap + b), y = 0.05,
                label = comma(b)),
            size = 1.9, colour = col_muted, inherit.aes = FALSE) +
  geom_text(data = data.frame(b = outer_breaks),
            aes(x =  (gap + b), y = 0.05,
                label = comma(b)),
            size = 1.9, colour = col_muted, inherit.aes = FALSE) +
  # sex headers
  annotate("text", x = -(gap + dmax) * 0.50,
           y = n_age_pyr + 1.95,
           label = "MEN", hjust = 0.5, vjust = 0.5,
           size = 3.0, fontface = "bold", colour = col_men) +
  annotate("text", x = (gap + dmax) * 0.50,
           y = n_age_pyr + 1.95,
           label = "WOMEN", hjust = 0.5, vjust = 0.5,
           size = 3.0, fontface = "bold", colour = col_women) +
  annotate("text", x = -(gap + dmax) * 0.50,
           y = n_age_pyr + 1.40,
           label = sprintf("%s deaths  \u00b7  PAF %.1f%%",
                           comma(round(total_men_t2)), mean_men_t2_paf),
           size = 2.05, fontface = "italic", colour = col_muted) +
  annotate("text", x = (gap + dmax) * 0.50,
           y = n_age_pyr + 1.40,
           label = sprintf("%s deaths  \u00b7  PAF %.1f%%",
                           comma(round(total_women_t2)), mean_women_t2_paf),
           size = 2.05, fontface = "italic", colour = col_muted) +
  scale_fill_manual(values = c(Men = col_men, Women = col_women),
                    guide = "none") +
  scale_x_continuous(limits = c(-x_limit, x_limit),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.1, n_age_pyr + 2.4),
                     expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_void(base_size = 9.3) +
  theme(plot.margin = margin(6, 4, 6, 4))

# ---------------------------------------------------------------------------
# FIG R3: Lollipop comparison men vs women on YLL, e30 gain, indirect costs
# ---------------------------------------------------------------------------
# Source values reported in the manuscript text and Tabla7/cost outputs.
# We hard-code totals here so the figure matches the published numbers.
burden <- tibble::tribble(
  ~metric_id, ~metric_label,                    ~unit,                 ~Men,   ~Women,
  "yll",     "Years of life lost",              "thousand YLL",        203.601, 146.086,
  "e30",     "Life-expectancy gain at age 30", "years",                0.35,    0.22,
  "cost",    "Indirect economic cost averted", "billion intl$ (PPP)",  0.61,    0.23
) %>%
  mutate(metric_label = factor(metric_label, levels = metric_label),
         total = Men + Women)

burden_long <- burden %>%
  pivot_longer(c(Men, Women), names_to = "sex", values_to = "value") %>%
  group_by(metric_label) %>%
  mutate(rel = value / max(value)) %>%
  ungroup() %>%
  mutate(sex = factor(sex, levels = c("Men", "Women")))

# Editorial paired bars per metric: three rows, each with a large metric
# header, unit subtitle, paired M/W bars normalised to the row maximum,
# value labels at the bar tip, and a compact stats footer (total, delta, ratio).
metrics_order <- c("Years of life lost",
                   "Life-expectancy gain at age 30",
                   "Indirect economic cost averted")
unit_by_metric <- c(
  "Years of life lost"              = "thousand YLL",
  "Life-expectancy gain at age 30" = "years",
  "Indirect economic cost averted" = "billion intl$ (PPP)"
)

burden_pair <- burden %>%
  mutate(metric_label = factor(metric_label, levels = metrics_order)) %>%
  rowwise() %>%
  mutate(mmax = max(Men, Women)) %>%
  ungroup() %>%
  mutate(row = as.numeric(metric_label))

# Long-form, with offsets so men sit above women inside each row
burden_pair_long <- burden_pair %>%
  pivot_longer(c(Men, Women), names_to = "sex", values_to = "value") %>%
  mutate(
    sex = factor(sex, levels = c("Men", "Women")),
    sub_off = ifelse(sex == "Men", -0.09, 0.19),
    y_pair = row + sub_off,
    norm   = value / mmax,
    share  = value / (value + ifelse(sex == "Men",
                                      burden_pair$Women[match(metric_label,
                                                               burden_pair$metric_label)],
                                      burden_pair$Men[match(metric_label,
                                                             burden_pair$metric_label)]))
  )

row_sep <- data.frame(y = seq_len(nrow(burden_pair) - 1) + 0.5)

fig_r3 <- ggplot(burden_pair_long) +
  # faint reference rail (full row width)
  geom_rect(aes(xmin = 0, xmax = 1.0,
                ymin = y_pair - 0.13, ymax = y_pair + 0.13),
            fill = "#F1F3F5", colour = NA) +
  # main bar
  geom_rect(aes(xmin = 0, xmax = norm,
                ymin = y_pair - 0.13, ymax = y_pair + 0.13,
                fill = sex), colour = NA) +
  # value at the tip of each bar
  geom_text(aes(x = norm, y = y_pair,
                label = sprintf("%.2f", value),
                colour = sex),
            hjust = -0.18, size = 2.55, fontface = "bold") +
  # sex label at the start
  geom_text(aes(x = -0.015, y = y_pair,
                label = substr(as.character(sex), 1, 1),
                colour = sex),
            hjust = 1, size = 2.4, fontface = "bold") +
  # share % inside the bar (only when bar is wide enough)
  geom_text(aes(x = norm / 2, y = y_pair,
                label = ifelse(norm > 0.30,
                               sprintf("%.0f%%", 100 * share), "")),
            colour = "white", size = 2.0, fontface = "bold") +
  # metric header (bold, above each row)
  geom_text(data = burden_pair,
            aes(x = 0, y = row - 0.40, label = as.character(metric_label)),
            hjust = 0, size = 2.95, fontface = "bold", colour = col_ink,
            inherit.aes = FALSE) +
  # unit (top right)
  geom_text(data = burden_pair,
            aes(x = 1.18, y = row - 0.40,
                label = unit_by_metric[as.character(metric_label)]),
            hjust = 1, size = 2.1, fontface = "italic",
            colour = col_muted, inherit.aes = FALSE) +
  # stats footer: below the pair of bars
  geom_text(data = burden_pair,
            aes(x = 0, y = row + 0.40,
                label = sprintf("Total %.2f  \u00b7  M - W %.2f  \u00b7  M:W %.2f",
                                Men + Women, Men - Women, Men / Women)),
            hjust = 0, size = 2.05, fontface = "italic",
            colour = col_muted, inherit.aes = FALSE) +
  # row separators (hairline)
  geom_hline(data = row_sep, aes(yintercept = y),
             colour = col_grid, linewidth = 0.3) +
  scale_fill_manual(values = c(Men = col_men, Women = col_women),
                    guide = "none") +
  scale_colour_manual(values = c(Men = col_men, Women = col_women),
                      guide = "none") +
  scale_x_continuous(limits = c(-0.05, 1.22), expand = c(0, 0)) +
  scale_y_reverse(limits = c(3.75, 0.55), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_void(base_size = 9.3) +
  theme(plot.margin = margin(6, 4, 6, 4))

# ---------------------------------------------------------------------------
# FIG R4: Scenario impact ladder (Table S12)
# Three small-multiples panels (deaths / YLL / cost averted), six rows for the
# six scenarios ordered by intensity. Per row: faint background bar = % of max,
# 95% UI as a thick line, central estimate as a large dot, value labels.
# A faint curved spline connects scenario centres to convey the intensity
# gradient between proportional reductions and NDG-aligned targets.
# ---------------------------------------------------------------------------
sehs <- read_pkg_csv("scenario_health_economic_summary.csv")

scen_levels <- c("red_10", "red_20", "red_50",
                 "ndg_gapa_conservative", "ndg_gapa_central", "ndg_gapa_strict")
scen_labels <- c(
  red_10 = "10% proportional reduction",
  red_20 = "20% proportional reduction",
  red_50 = "50% proportional reduction",
  ndg_gapa_conservative = "NDG conservative (target 5.45%)",
  ndg_gapa_central      = "NDG central (target 3.63%)",
  ndg_gapa_strict       = "NDG strict (target 1.81%)"
)
fam_levels <- c("Proportional reduction", "NDG-aligned target")
fam_pal <- c("Proportional reduction" = "#7E94B5",
             "NDG-aligned target"     = "#1B5E47")

scen_long <- sehs %>%
  filter(scenario %in% scen_levels) %>%
  mutate(
    scenario = factor(scenario, levels = scen_levels),
    family = factor(ifelse(grepl("^ndg", scenario),
                           "NDG-aligned target", "Proportional reduction"),
                    levels = fam_levels),
    label  = scen_labels[as.character(scenario)]
  ) %>%
  transmute(
    scenario, family, label,
    deaths_est = deaths_averted,
    deaths_lo  = deaths_averted_lo,
    deaths_hi  = deaths_averted_hi,
    yll_est    = yll_averted,
    yll_lo     = yll_averted_lo,
    yll_hi     = yll_averted_hi,
    cost_est   = pvle_averted_billion_intl_ppp,
    cost_lo    = pvle_averted_billion_intl_ppp_lo,
    cost_hi    = pvle_averted_billion_intl_ppp_hi
  )

.compact <- label_number(scale_cut = cut_short_scale(), accuracy = 0.1)
scen_panels <- bind_rows(
  scen_long %>% transmute(scenario, family, label,
                          metric = "Deaths averted (per year)",
                          est = deaths_est, lo = deaths_lo, hi = deaths_hi,
                          fmt = .compact(est)),
  scen_long %>% transmute(scenario, family, label,
                          metric = "Years of life lost averted",
                          est = yll_est, lo = yll_lo, hi = yll_hi,
                          fmt = .compact(est)),
  scen_long %>% transmute(scenario, family, label,
                          metric = "Indirect cost averted (billion intl$ PPP)",
                          est = cost_est, lo = cost_lo, hi = cost_hi,
                          fmt = sprintf("%.2f", est))
) %>%
  group_by(metric) %>%
  mutate(
    metric_max = max(hi, na.rm = TRUE),
    rel_bg     = est / metric_max,
    yidx       = as.numeric(scenario)
  ) %>%
  ungroup() %>%
  mutate(metric = factor(metric, levels = c(
    "Deaths averted (per year)",
    "Years of life lost averted",
    "Indirect cost averted (billion intl$ PPP)"
  )))

family_bands <- scen_panels %>%
  group_by(metric, family) %>%
  summarise(ymin = min(yidx) - 0.5, ymax = max(yidx) + 0.5,
            xmax = max(metric_max), .groups = "drop")

fig_r4 <- ggplot(scen_panels, aes(y = yidx)) +
  geom_rect(data = family_bands,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
                fill = family),
            inherit.aes = FALSE, alpha = 0.08) +
  geom_segment(aes(x = 0, xend = metric_max, y = yidx, yend = yidx),
               colour = col_grid, linewidth = 4.4, lineend = "round",
               alpha = 0.55) +
  geom_segment(aes(x = 0, xend = est, y = yidx, yend = yidx,
                   colour = family),
               linewidth = 4.4, lineend = "round", alpha = 0.30) +
  geom_segment(aes(x = lo, xend = hi, y = yidx, yend = yidx,
                   colour = family),
               linewidth = 1.2, lineend = "round") +
  geom_point(aes(x = est, fill = family),
             shape = 21, colour = col_ink, stroke = 0.45, size = 3.4) +
  geom_line(aes(x = est, group = 1),
            colour = alpha(col_ink, 0.35), linewidth = 0.45,
            linetype = "22") +
  geom_text(aes(x = hi, label = fmt),
            hjust = -0.15, size = 2.55, colour = col_ink) +
  facet_wrap(~ metric, scales = "free_x", nrow = 1,
             labeller = label_wrap_gen(width = 22)) +
  scale_y_continuous(
    breaks = seq_along(scen_levels),
    labels = unname(scen_labels[scen_levels]),
    expand = expansion(add = c(0.6, 0.6))
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.22)),
                     labels = label_number(scale_cut = cut_short_scale()),
                     n.breaks = 4) +
  scale_colour_manual(values = fam_pal, name = NULL) +
  scale_fill_manual(values = fam_pal, name = NULL) +
  labs(x = NULL, y = NULL) +
  theme_nf() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.25),
    strip.text         = element_text(face = "bold", size = rel(0.9)),
    axis.text.y        = element_text(face = "bold"),
    legend.position    = "top",
    legend.justification = "left"
  )

# ---------------------------------------------------------------------------
# Composite Figure 1: panel A = heatmap, panel B = pyramid + burden, panel C = ladder
# ---------------------------------------------------------------------------
tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 13, colour = col_ink),
  plot.tag.position = c(0.005, 0.985)
)

pA <- fig_r1 + labs(tag = "a") +
  theme(legend.position  = "bottom",
        axis.text.y      = element_text(face = "bold", hjust = 1, margin = margin(r = 1)),
        plot.margin      = margin(4, 6, 4, -6)) + tag_theme
pB_left  <- fig_r2 + labs(tag = "b") +
  theme(plot.margin = margin(6, -6, 4, -110)) + tag_theme
pB_right <- fig_r3 +
  theme(plot.margin = margin(6, 8, 4, 0))
pC <- fig_r4 + labs(tag = "c") +
  theme(legend.position = "top",
        plot.margin     = margin(4, 22, 4, 0)) + tag_theme

pB <- (pB_left | plot_spacer() | pB_right) +
  plot_layout(widths = c(1.08, 0.03, 1))

composite <- pA / pB / pC +
  plot_layout(heights = c(0.78, 1.35, 1.15))

save_pair(composite, "Figure_1", w_mm = 220, h_mm = 285)
message("Saved Figure_1 to ", FIG_DIR)
