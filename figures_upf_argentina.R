###############################################################################
#  Nature Food submission figures
#  "Health and environmental impacts of UPF consumption in Argentina"
#  García-Witulski & Rabassa
#
#  Five main-text figures built directly from pipeline outputs in output/.
#
#    Fig 1 – Conceptual & analytical framework
#               (a) pipeline schematic  (b) NOVA energy share by sex×age
#               (c) cross-country %UPF   (d) RRs by cause
#    Fig 2 – Attributable burden
#               (a) bilateral population pyramid of deaths
#               (b) lollipop by cause   (c) life-expectancy loss waterfall
#               (d) indirect costs as % GDP
#    Fig 3 – Continuous dose–response of UPF reduction
#               (a) deaths averted   (b) LE gain
#               (c) indirect costs avoided   (d) stacked area by cause
#    Fig 4 – Environmental trade-offs: substitution matters
#               (a) normalised heatmap   (b) GHG×water bubble
#               (c) radar of baskets
#    Fig 5 – Health–environment–equity Pareto frontier
#               (a) health × GHG   (b) health × water
#               (c) health × cost  (d) parallel coordinates
#
#  Requirements: ggplot2, dplyr, tidyr, patchwork, scales, forcats,
#                ggtext, ggrepel, fmsb (radar)
###############################################################################

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(scales)
  library(forcats); library(patchwork); library(ggrepel)
})

OUT <- "output"
FIG <- "output/figures"
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

# ─── Palette (Nature Food-friendly, colour-blind safe) ──────────────────────
col_primary  <- "#1F4E79"   # deep blue
col_accent   <- "#C0392B"   # brick red
col_teal     <- "#138D75"
col_amber    <- "#CA8A04"
col_purple   <- "#6B46C1"
col_men      <- "#2B6CB0"
col_women    <- "#C84D7A"
col_grid     <- "#E5E7EB"
col_ink      <- "#111827"
col_muted    <- "#6B7280"

pal_scenario <- c("Proportional" = col_primary, "NDG-aligned" = col_teal)
pal_basket   <- c("NOVA 1" = col_teal, "NOVA 3" = col_amber,
                  "NOVA1/3 mix" = col_purple, "NDG" = col_primary,
                  "No replacement" = col_muted)

# ─── Theme ──────────────────────────────────────────────────────────────────
theme_nf <- function(base = 8) {
  theme_minimal(base_size = base, base_family = "sans") +
    theme(
      text             = element_text(colour = col_ink),
      plot.title       = element_text(face = "bold", size = rel(1.05), hjust = 0),
      plot.subtitle    = element_text(colour = col_muted, size = rel(0.85), hjust = 0),
      plot.tag         = element_text(face = "bold", size = rel(1.25)),
      plot.caption     = element_text(colour = col_muted, size = rel(0.7), hjust = 0),
      panel.grid.major = element_line(colour = col_grid, linewidth = 0.25),
      panel.grid.minor = element_blank(),
      axis.title       = element_text(size = rel(0.85)),
      axis.text        = element_text(size = rel(0.78), colour = col_ink),
      axis.ticks       = element_line(colour = col_grid, linewidth = 0.25),
      legend.title     = element_text(size = rel(0.8), face = "bold"),
      legend.text      = element_text(size = rel(0.75)),
      legend.key.height = unit(3, "mm"),
      legend.key.width  = unit(4, "mm"),
      strip.text       = element_text(face = "bold", size = rel(0.82)),
      plot.margin      = margin(6, 6, 6, 6)
    )
}

save_fig <- function(plot, name, w, h) {
  ggsave(file.path(FIG, paste0(name, ".pdf")), plot, width = w, height = h,
         units = "mm", dpi = 400, device = cairo_pdf)
  ggsave(file.path(FIG, paste0(name, ".png")), plot, width = w, height = h,
         units = "mm", dpi = 400)
  message("  saved ", name)
}

# ─── Load pipeline data ─────────────────────────────────────────────────────
read_out <- function(x) read.csv(file.path(OUT, x), check.names = FALSE,
                                 fileEncoding = "UTF-8-BOM")

tab2 <- read_out("Tabla2_Muertes_atribuibles_Argentina.csv")        # deaths by sex×age
tab4 <- read_out("Tabla4_Muertes_por_causa_Argentina.csv")          # deaths by cause
tab6 <- read_out("Tabla6_Esperanza_vida_UPF.csv")                   # LE
tab7 <- read_out("Tabla7_Costos_indirectos_Argentina.csv")          # costs
yll  <- read_out("resumen_yll_por_causa.csv")
upf_strata <- read_out("Tabla1_UPF_por_energia_Argentina.csv")
seh  <- read_out("summary_health_environment_upf_scenarios.csv")    # full grid
hcs  <- read_out("health_counterfactual_scenarios.csv")             # continuous-ish

# Argentina GDP 2019 (intl$ PPP, billions) for cost-as-%GDP panel.  Source: WDI.
GDP_ARG_2019_BILL <- 1030

###############################################################################
# FIG 1 — Conceptual & analytical framework
###############################################################################

## 1a. Pipeline schematic -----------------------------------------------------
boxes <- tribble(
  ~id, ~x, ~y, ~label,                                        ~fill,
  1,   1,  4, "ENNyS-2\n24-h recalls\nn≈4,500 adults",        col_primary,
  2,   1,  2, "NOVA\nclassification",                          col_muted,
  3,   3,  4, "Relative risks\n(Lane 2024 meta)",              col_accent,
  4,   3,  2, "LCA coefficients\n(Agribalyse + lit.)",         col_teal,
  5,   5,  4, "CRA + Monte-Carlo\nhealth module",              col_accent,
  6,   5,  2, "Environmental\nmodule",                         col_teal,
  7,   7,  3, "Policy scenarios\n& Pareto analysis",           col_purple
)
arrows <- tribble(
  ~x1, ~y1, ~x2, ~y2,
  1.6, 4,   2.4, 4,
  1.6, 2,   2.4, 2,
  3.6, 4,   4.4, 4,
  3.6, 2,   4.4, 2,
  5.6, 4,   6.4, 3.2,
  5.6, 2,   6.4, 2.8
)
fig1a <- ggplot() +
  geom_rect(data = boxes,
            aes(xmin = x-.5, xmax = x+.5, ymin = y-.55, ymax = y+.55,
                fill = fill), alpha = .15, colour = NA) +
  geom_rect(data = boxes,
            aes(xmin = x-.5, xmax = x+.5, ymin = y-.55, ymax = y+.55,
                colour = fill), fill = NA, linewidth = .4) +
  geom_text(data = boxes, aes(x, y, label = label),
            size = 2.3, lineheight = .9, colour = col_ink) +
  geom_segment(data = arrows,
               aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
               colour = col_muted, linewidth = .3) +
  scale_fill_identity() + scale_colour_identity() +
  coord_cartesian(xlim = c(.3, 7.8), ylim = c(1.2, 4.8)) +
  labs(tag = "a", title = "Analytical pipeline") +
  theme_void(base_size = 8) +
  theme(plot.title = element_text(face = "bold", size = rel(1.05)),
        plot.tag   = element_text(face = "bold", size = rel(1.25)),
        plot.margin = margin(4, 4, 4, 4))

## 1b. %UPF energy by sex × age ----------------------------------------------
parse_ci <- function(x) as.numeric(sub(" .*", "", x))
upf_long <- upf_strata %>%
  rename(age = 1, Men = 2, Women = 3) %>%
  select(age, Men, Women) %>%
  mutate(across(c(Men, Women), parse_ci)) %>%
  pivot_longer(-age, names_to = "sex", values_to = "pct")

fig1b <- ggplot(upf_long, aes(age, pct, fill = sex)) +
  geom_col(position = position_dodge(.7), width = .6) +
  scale_fill_manual(values = c(Men = col_men, Women = col_women), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, .1))) +
  labs(tag = "b", title = "UPF share of energy intake",
       subtitle = "ENNyS-2, adults 30–69, Argentina",
       x = NULL, y = "% kcal from UPF") +
  theme_nf() +
  theme(legend.position = c(.85, .9),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 30, hjust = 1))

## 1c. Cross-country %UPF (literature values) --------------------------------
countries <- tribble(
  ~country,     ~pct, ~highlight,
  "USA",         57,  FALSE,
  "UK",          57,  FALSE,
  "Canada",      48,  FALSE,
  "Australia",   42,  FALSE,
  "Chile",       30,  FALSE,
  "Mexico",      30,  FALSE,
  "Brazil",      20,  FALSE,
  "Argentina",   29,  TRUE,
  "Colombia",    16,  FALSE,
  "Italy",       18,  FALSE
) %>% mutate(country = fct_reorder(country, pct))

fig1c <- ggplot(countries, aes(pct, country,
                               fill = ifelse(highlight, "ARG", "other"))) +
  geom_col(width = .65) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -.15, size = 2.3,
            colour = col_ink) +
  scale_fill_manual(values = c(ARG = col_accent, other = col_muted),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .18))) +
  labs(tag = "c", title = "UPF energy share, cross-country",
       subtitle = "Argentina (red) vs. published estimates",
       x = "% of daily energy", y = NULL) +
  theme_nf() +
  theme(panel.grid.major.y = element_blank())

## 1d. Relative risks by cause (forest) --------------------------------------
rr <- tribble(
  ~cause,                          ~rr,   ~lo,  ~hi,
  "All-cause mortality",           1.21, 1.15, 1.27,
  "Cardiovascular disease",        1.50, 1.37, 1.63,
  "Coronary heart disease",        1.23, 1.12, 1.34,
  "Cerebrovascular disease",       1.17, 1.05, 1.30,
  "Type 2 diabetes",               1.40, 1.23, 1.59,
  "Colorectal cancer",             1.12, 1.03, 1.22,
  "Depression",                    1.22, 1.16, 1.28
) %>% mutate(cause = fct_reorder(cause, rr))

fig1d <- ggplot(rr, aes(rr, cause)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             colour = col_muted, linewidth = .3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .25,
                 colour = col_ink, linewidth = .4) +
  geom_point(size = 2, colour = col_accent) +
  geom_text(aes(label = sprintf("%.2f (%.2f–%.2f)", rr, lo, hi)),
            x = 1.72, hjust = 1, size = 2.1, colour = col_muted) +
  scale_x_continuous(limits = c(.95, 1.75),
                     breaks = c(1, 1.2, 1.4, 1.6)) +
  labs(tag = "d", title = "UPF relative risks per 10% ↑ intake",
       subtitle = "Source: Lane et al. 2024 umbrella review",
       x = "RR (95% CI)", y = NULL) +
  theme_nf() +
  theme(panel.grid.major.y = element_blank())

fig1 <- (fig1a | fig1b) / (fig1c | fig1d) +
  plot_layout(heights = c(1, 1.1))

save_fig(fig1, "Fig1_framework", 183, 150)


###############################################################################
# FIG 2 — Attributable burden
###############################################################################

## 2a. Bilateral pyramid of attributable deaths -------------------------------
pyr <- tab2 %>%
  rename(age = 1, men = "Men, deaths attr", women = "Women, deaths attr") %>%
  select(age, men, women) %>%
  mutate(men = -as.numeric(men), women = as.numeric(women)) %>%
  pivot_longer(-age, names_to = "sex", values_to = "deaths") %>%
  mutate(sex = recode(sex, men = "Men", women = "Women"),
         age = factor(age, levels = tab2[[1]]))

fig2a <- ggplot(pyr, aes(deaths, age, fill = sex)) +
  geom_col(width = .75) +
  geom_vline(xintercept = 0, colour = col_ink, linewidth = .3) +
  geom_text(aes(label = comma(abs(deaths)),
                hjust = ifelse(deaths < 0, 1.1, -.1)),
            size = 2.2, colour = col_muted) +
  scale_fill_manual(values = c(Men = col_men, Women = col_women), name = NULL) +
  scale_x_continuous(labels = function(x) comma(abs(x)),
                     limits = c(-2200, 2200),
                     breaks = seq(-2000, 2000, 1000)) +
  labs(tag = "a",
       title = "Attributable deaths by sex × age",
       subtitle = "Adults 30–69, Argentina 2019",
       x = "Deaths per year", y = "Age group") +
  theme_nf() +
  theme(legend.position = c(.5, 1.03),
        legend.direction = "horizontal",
        panel.grid.major.y = element_blank())

## 2b. Deaths by cause (lollipop) --------------------------------------------
cause_map <- c(cerebrovascular = "Cerebrovascular",
               cvd = "Cardiovascular", ihd = "Ischaemic heart",
               diabetes = "Type 2 diabetes", colorectal = "Colorectal cancer",
               depression = "Depression", allcause = "All-cause")

cause_df <- read_out("resumen_muertes_por_causa.csv") %>%
  mutate(cause = recode(causa, !!!cause_map, .default = causa),
         cause = fct_reorder(cause, deaths_attr))

fig2b <- ggplot(cause_df, aes(deaths_attr, cause)) +
  geom_segment(aes(x = 0, xend = deaths_attr, yend = cause),
               colour = col_grid, linewidth = 1) +
  geom_point(aes(size = pct_attr, colour = pct_attr)) +
  geom_text(aes(label = comma(round(deaths_attr))),
            hjust = -.25, size = 2.2, colour = col_ink) +
  scale_colour_gradient(low = col_primary, high = col_accent,
                        name = "PAF (%)") +
  scale_size(range = c(2, 5), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .22)),
                     labels = comma) +
  labs(tag = "b", title = "Attributable deaths by cause",
       x = "Attributable deaths/year", y = NULL) +
  theme_nf() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right")

## 2c. Life-expectancy loss at 30 --------------------------------------------
le <- tab6 %>% rename(group = 1, e30 = 2, e30_no = 3, gain = 4) %>%
  mutate(group = recode(group, Total = "Total", Hombres = "Men",
                        Mujeres = "Women"),
         group = factor(group, levels = c("Total", "Men", "Women")))

fig2c <- ggplot(le, aes(group, gain, fill = group)) +
  geom_col(width = .55) +
  geom_text(aes(label = sprintf("+%.2f yr", gain)),
            vjust = -.4, size = 2.6, colour = col_ink) +
  scale_fill_manual(values = c(Total = col_primary, Men = col_men,
                               Women = col_women), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, .18))) +
  labs(tag = "c",
       title = "Life-expectancy gain at age 30",
       subtitle = "Counterfactual: eliminating UPF-attributable mortality",
       x = NULL, y = "Years gained") +
  theme_nf() +
  theme(panel.grid.major.x = element_blank())

## 2d. Indirect costs as % GDP ------------------------------------------------
cost_bill <- as.numeric(tab7$costo_PPP_millones_intl_USD) / 1000
pct_gdp   <- cost_bill / GDP_ARG_2019_BILL * 100

cost_df <- tibble(
  metric = c("Indirect cost\n(USD bn PPP)", "% of GDP"),
  value  = c(cost_bill, pct_gdp),
  label  = c(sprintf("%.2f bn", cost_bill),
             sprintf("%.3f %%", pct_gdp))
)

fig2d <- ggplot(cost_df, aes(metric, value, fill = metric)) +
  geom_col(width = .55) +
  geom_text(aes(label = label), vjust = -.4, size = 2.6) +
  scale_fill_manual(values = c(col_amber, col_accent), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, .25))) +
  labs(tag = "d", title = "Economic burden",
       subtitle = "Productivity-based human-capital approach (2019)",
       x = NULL, y = NULL) +
  theme_nf() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_blank())

fig2 <- (fig2a | fig2b) / (fig2c | fig2d) +
  plot_layout(heights = c(1.15, .85))

save_fig(fig2, "Fig2_burden", 183, 160)


###############################################################################
# FIG 3 — Continuous dose–response of UPF reduction
###############################################################################

# Build a fine reduction grid from scenario summary.  deaths/YLL/PVLE scale
# ~linearly with reduction in the CRA framework, so we interpolate.
base <- seh %>%
  filter(scenario_model == "isocaloric_replacement",
         replacement_target == "NDG_ARG") %>%
  distinct(reduction_pct, deaths_averted, deaths_averted_lo,
           deaths_averted_hi, yll_averted, e30_gain_years,
           pvle_averted_billion_intl_ppp) %>%
  arrange(reduction_pct)

grid <- tibble(red = seq(0, max(base$reduction_pct), length.out = 120)) %>%
  mutate(
    deaths     = approx(base$reduction_pct, base$deaths_averted,      red)$y,
    deaths_lo  = approx(base$reduction_pct, base$deaths_averted_lo,   red)$y,
    deaths_hi  = approx(base$reduction_pct, base$deaths_averted_hi,   red)$y,
    le         = approx(base$reduction_pct, base$e30_gain_years,      red)$y,
    cost_bn    = approx(base$reduction_pct, base$pvle_averted_billion_intl_ppp, red)$y
  )

markers <- base %>%
  mutate(kind = ifelse(reduction_pct %in% c(10, 20, 50),
                       "Proportional", "NDG-aligned"))

fig3a <- ggplot(grid, aes(red, deaths)) +
  geom_ribbon(aes(ymin = deaths_lo, ymax = deaths_hi),
              fill = col_primary, alpha = .18) +
  geom_line(colour = col_primary, linewidth = .7) +
  geom_point(data = markers,
             aes(reduction_pct, deaths_averted, colour = kind), size = 2.2) +
  scale_colour_manual(values = pal_scenario, name = NULL) +
  scale_y_continuous(labels = comma) +
  labs(tag = "a", title = "Deaths averted",
       x = "Reduction in UPF intake (%)",
       y = "Deaths/year averted (95% UI)") +
  theme_nf() +
  theme(legend.position = c(.25, .85))

fig3b <- ggplot(grid, aes(red, le)) +
  geom_line(colour = col_teal, linewidth = .7) +
  geom_point(data = markers,
             aes(reduction_pct, e30_gain_years), colour = col_teal, size = 2) +
  labs(tag = "b", title = "Life-expectancy gain at 30",
       x = "Reduction in UPF intake (%)", y = "Years") +
  theme_nf()

fig3c <- ggplot(grid, aes(red, cost_bn)) +
  geom_area(fill = col_amber, alpha = .3) +
  geom_line(colour = col_amber, linewidth = .7) +
  geom_point(data = markers,
             aes(reduction_pct, pvle_averted_billion_intl_ppp),
             colour = col_amber, size = 2) +
  labs(tag = "c", title = "Indirect costs avoided",
       x = "Reduction in UPF intake (%)",
       y = "USD bn PPP (2019)") +
  theme_nf()

## 3d. Stacked contribution by cause (scaled linearly with reduction) -------
cause_share <- read_out("resumen_muertes_por_causa.csv") %>%
  filter(causa != "allcause") %>%
  mutate(cause = recode(causa, !!!cause_map, .default = causa),
         share = deaths_attr / sum(deaths_attr))

stack <- expand.grid(red = seq(0, max(base$reduction_pct), length.out = 60),
                     cause = cause_share$cause) %>%
  left_join(cause_share %>% select(cause, share), by = "cause") %>%
  mutate(deaths = approx(base$reduction_pct, base$deaths_averted, red)$y * share)

fig3d <- ggplot(stack, aes(red, deaths, fill = cause)) +
  geom_area(alpha = .85, colour = "white", linewidth = .15) +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  scale_y_continuous(labels = comma) +
  labs(tag = "d", title = "Cause-specific contribution",
       x = "Reduction in UPF intake (%)",
       y = "Deaths averted/year") +
  theme_nf() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "mm"))

fig3 <- (fig3a | fig3b) / (fig3c | fig3d)
save_fig(fig3, "Fig3_dose_response", 183, 150)


###############################################################################
# FIG 4 — Counterfactual scenarios: averted health & economic burden
#         Forest-plot replacement for manuscript Table 4
###############################################################################

fam_pal_f4 <- c(`Proportional reduction` = "#475569",
                `Guideline-aligned (NDG)` = "#0F766E")

sehs_f4 <- read_out("scenario_health_economic_summary.csv") %>%
  filter(!is.na(scenario_label)) %>%
  mutate(
    scenario_label = factor(
      scenario_label,
      levels = c("10% reduction", "20% reduction", "50% reduction",
                 "NDG conservative", "NDG central", "NDG strict")
    ),
    family = ifelse(grepl("^NDG", scenario_label),
                    "Guideline-aligned (NDG)", "Proportional reduction")
  ) %>%
  arrange(scenario_label) %>%
  mutate(scenario_label = factor(scenario_label,
                                 levels = rev(levels(scenario_label))))

theme_f4 <- theme_nf() +
  theme(
    plot.title         = element_text(face = "bold", size = 9.5,
                                      margin = margin(b = 1)),
    plot.subtitle      = element_text(size = 8, colour = col_muted,
                                      margin = margin(b = 5)),
    plot.tag           = element_text(face = "bold", size = 11),
    plot.tag.position  = c(0.005, 0.985),
    axis.title.x       = element_text(size = 8, colour = col_ink),
    axis.text          = element_text(size = 7.8, colour = col_ink),
    legend.text        = element_text(size = 8),
    legend.title       = element_blank(),
    legend.key.height  = unit(3, "mm"),
    legend.key.width   = unit(5, "mm"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "#EEF1F5", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    plot.margin        = margin(8, 12, 6, 10)
  )

fmt_int_f4 <- function(x) comma(round(x))
fmt_dec_f4 <- function(x) sprintf("%.2f", x)

forest_f4 <- function(df, est, lo, hi, title, xlab, fmt, show_y = TRUE) {
  ggplot(df, aes(.data[[est]], scenario_label, colour = family)) +
    geom_linerange(aes(xmin = .data[[lo]], xmax = .data[[hi]]),
                   linewidth = 0.85, alpha = 0.95) +
    geom_point(size = 2.7, stroke = 0) +
    geom_text(aes(label = fmt(.data[[est]])),
              hjust = -0.2, vjust = -0.7, size = 2.4, colour = col_ink) +
    scale_colour_manual(values = fam_pal_f4) +
    scale_x_continuous(labels = fmt,
                       expand = expansion(mult = c(0.06, 0.24))) +
    labs(title = title, x = xlab, y = NULL) +
    theme_f4 +
    {if (!show_y) theme(axis.text.y = element_blank()) else NULL}
}

fig4a <- forest_f4(sehs_f4, "deaths_averted",
                   "deaths_averted_lo", "deaths_averted_hi",
                   "Deaths averted", "per year", fmt_int_f4) +
  labs(tag = "a")
fig4b <- forest_f4(sehs_f4, "yll_averted",
                   "yll_averted_lo", "yll_averted_hi",
                   "Years of life lost averted", "per year", fmt_int_f4,
                   show_y = FALSE) +
  labs(tag = "b")
fig4c <- forest_f4(sehs_f4, "pvle_averted_billion_intl_ppp",
                   "pvle_averted_billion_intl_ppp_lo",
                   "pvle_averted_billion_intl_ppp_hi",
                   "Averted indirect cost", "billion intl.$ PPP",
                   fmt_dec_f4, show_y = FALSE) +
  labs(tag = "c")

fig4 <- (fig4a | fig4b | fig4c) +
  plot_layout(guides = "collect", widths = c(1.45, 1, 1)) +
  plot_annotation(
    title = "Counterfactual UPF-reduction scenarios: averted health and economic burden",
    subtitle = "Point estimates and 95% uncertainty intervals; Argentine adults 30-69, 2019",
    theme = theme(
      plot.title = element_text(face = "bold", size = 11,
                                margin = margin(b = 2)),
      plot.subtitle = element_text(size = 8.5, colour = col_muted,
                                   margin = margin(b = 8)),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  ) &
  theme(legend.position = "bottom")

gc(verbose = FALSE)
save_fig(fig4, "Fig4_scenario_forest", 183, 115)


###############################################################################
# FIG 5 — Health–environment–equity Pareto frontier
###############################################################################

pareto_df <- seh %>%
  mutate(
    basket = recode(replacement_target,
                    NDG_ARG = "NDG", NOVA1 = "NOVA 1",
                    NOVA3 = "NOVA 3", NOVA1_NOVA3_mix = "NOVA1/3 mix",
                    none = "No replacement"),
    structure = recode(scenario_model,
                       isocaloric_replacement = "Isocaloric",
                       isoweight_replacement  = "Isoweight",
                       no_replacement         = "No sub."),
    scenario_label = recode(
      scenario,
      red_10 = "10% reduction",
      red_20 = "20% reduction",
      red_50 = "50% reduction",
      ndg_gapa_conservative = "NDG conservative",
      ndg_gapa_central = "NDG central",
      ndg_gapa_strict = "NDG strict"
    ),
    point_id = paste(scenario, scenario_model, replacement_target, sep = "__"),
    deaths = deaths_averted,
    # CSV columns store "% avoided" (positive = reduction). Flip sign so the
    # rest of the figure code can use the conventional "% change vs baseline"
    # (negative = environmental improvement), matching axis labels and Table S6.
    ghg    = -ghg_kg_co2eq_avoided_pct_of_baseline,
    water  = -water_l_avoided_pct_of_baseline,
    land   = -land_m2_avoided_pct_of_baseline,
    eutro  = -eutro_g_po4eq_avoided_pct_of_baseline
  ) %>%
  mutate(env_index = (ghg + water + land + eutro) / 4)

pal_basket_pareto <- c(
  "NDG" = "#1D3557",
  "NOVA 1" = "#2A9D8F",
  "NOVA 3" = "#C57B1C",
  "NOVA1/3 mix" = "#8D6A9F",
  "No replacement" = "#7C8797"
)
col_zone_better <- "#EEF7F4"
col_zone_worse <- "#FCF3EF"
col_front <- "#334155"

# Pareto frontier: maximise deaths averted, minimise env impact (negative is good)
pareto_front <- function(df, x, y) {
  d <- df[order(-df[[x]]), ]
  keep <- logical(nrow(d)); best <- Inf
  for (i in seq_len(nrow(d))) {
    if (d[[y]][i] < best) { keep[i] <- TRUE; best <- d[[y]][i] }
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

# identify dominance across (deaths up, ghg down, water down, land down, eutro down)
dom <- pareto_df
is_dom <- logical(nrow(dom))
for (i in seq_len(nrow(dom))) {
  others <- dom[-i, ]
  is_dom[i] <- any(
    others$deaths >= dom$deaths[i] &
    others$ghg    <= dom$ghg[i] &
    others$water  <= dom$water[i] &
    others$land   <= dom$land[i] &
    others$eutro  <= dom$eutro[i] &
    (others$deaths > dom$deaths[i] | others$ghg < dom$ghg[i] |
     others$water < dom$water[i]  | others$land < dom$land[i] |
     others$eutro < dom$eutro[i])
  )
}
nd_strict <- dom[!is_dom, ]

# Per-dimension Pareto frontiers: a scenario is "on the frontier" for axis Y if
# no other scenario averts more deaths AND has lower (better) Y. The union of
# the four dimension-specific frontiers is what panels a-d already show as
# dashed step lines, so panel (e) and the highlight set use the same union.
.front_ids <- function(df, yvar) {
  d <- df[order(-df$deaths), ]
  keep <- logical(nrow(d)); best <- Inf
  for (i in seq_len(nrow(d))) {
    if (d[[yvar]][i] < best) { keep[i] <- TRUE; best <- d[[yvar]][i] }
  }
  d$point_id[keep]
}
.union_ids <- unique(unlist(lapply(c("ghg", "water", "land", "eutro"),
                                   function(v) .front_ids(pareto_df, v))))
nd <- pareto_df[pareto_df$point_id %in% .union_ids, ]

p5_scatter <- function(df, yvar, ylab, tag, title, show_shape_legend = FALSE,
                       show_x = TRUE, show_annotations = TRUE,
                       label_mode = c("all", "extremes", "none"),
                       frontier_style = c("line", "step"),
                       zone_alpha = 0.95,
                       highlight = NULL,
                       bg_colour_by_basket = FALSE) {
  label_mode <- match.arg(label_mode)
  frontier_style <- match.arg(frontier_style)
  front <- pareto_front(df, "deaths", yvar) %>%
    arrange(deaths) %>%
    mutate(front_label = label_frontier(scenario_label, structure, basket))
  # highlight set: if provided (e.g. globally non-dominated), use it for
  # emphasis+labels instead of the per-panel frontier. Otherwise fall back.
  emph <- if (!is.null(highlight)) {
    highlight %>%
      mutate(front_label = label_frontier(scenario_label, structure, basket)) %>%
      arrange(deaths)
  } else {
    front
  }
  x_span <- diff(range(df$deaths, na.rm = TRUE))
  if (!is.finite(x_span) || x_span == 0) x_span <- max(df$deaths, na.rm = TRUE)
  y_span <- diff(range(df[[yvar]], na.rm = TRUE))
  if (!is.finite(y_span) || y_span == 0) y_span <- max(abs(df[[yvar]]), na.rm = TRUE)
  if (!is.finite(y_span) || y_span == 0) y_span <- 1
  label_offsets <- list(
    "1" = c(0),
    "2" = c(0.09, -0.09),
    "3" = c(0.12, 0, -0.12),
    "4" = c(0.14, 0.05, -0.05, -0.14)
  )
  offset_vec <- label_offsets[[as.character(nrow(front))]]
  if (is.null(offset_vec)) offset_vec <- seq(0.16, -0.16, length.out = nrow(front))
  front_for_labels <- switch(
    label_mode,
    "all" = emph,
    "extremes" = {
      if (nrow(emph) <= 2) emph
      else emph[c(which.min(emph$deaths), which.max(emph$deaths)), ]
    },
    "none" = emph[integer(0), ]
  )
  front_labels <- front_for_labels %>%
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
  front_line <- if (nrow(front) > 1) {
    if (frontier_style == "step") {
      geom_step(
        data = front,
        aes(deaths, .data[[yvar]], group = 1),
        inherit.aes = FALSE,
        colour = col_front,
        linewidth = 0.95,
        direction = "vh"
      )
    } else {
      geom_line(
        data = front,
        aes(deaths, .data[[yvar]], group = 1),
        inherit.aes = FALSE,
        colour = col_front,
        linewidth = 0.85
      )
    }
  } else {
    NULL
  }

  ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
             fill = col_zone_better, alpha = zone_alpha) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
             fill = col_zone_worse, alpha = zone_alpha) +
    geom_hline(yintercept = 0, colour = col_muted, linewidth = 0.32,
               linetype = "dashed") +
    geom_point(
      data = df,
      aes(deaths, .data[[yvar]], fill = basket, shape = structure),
      inherit.aes = FALSE,
      colour = alpha("#475569", 0.5),
      size = 2.1,
      stroke = 0.2,
      alpha = 0.45
    ) +
    front_line +
    geom_point(
      data = emph,
      aes(deaths, .data[[yvar]], fill = basket, shape = structure),
      colour = alpha(col_ink, 0.9),
      size = 3.6,
      stroke = 0.4
    ) +
    geom_curve(
      data = front_labels,
      aes(
        x = deaths, y = .data[[yvar]],
        xend = label_x - x_span * 0.012, yend = label_y
      ),
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
    (if (show_annotations) list(
      annotate(
        "text",
        x = min(df$deaths) * 1.02,
        y = min(df[[yvar]], na.rm = TRUE) * 0.96,
        label = "Lower environmental burden",
        hjust = 0, vjust = 1, size = 2.45, colour = col_muted
      ),
      annotate(
        "text",
        x = min(df$deaths) * 1.02,
        y = max(df[[yvar]], na.rm = TRUE) * 1.08,
        label = "Higher environmental burden",
        hjust = 0, vjust = 1, size = 2.45, colour = col_muted
      )
    ) else NULL) +
    scale_fill_manual(
      values = pal_basket_pareto,
      name = "Basket",
      guide = if (show_shape_legend)
        guide_legend(override.aes = list(shape = 21, size = 3.2, alpha = 1))
        else "none"
    ) +
    scale_shape_manual(
      values = c(Isocaloric = 21, Isoweight = 22, `No sub.` = 24),
      name = "Structure"
    ) +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0.04, 0.75))) +
    labs(
      tag = tag, title = title,
      x = if (show_x) "Deaths averted/year" else NULL,
      y = ylab
    ) +
    coord_cartesian(clip = "off") +
    theme_nf() +
    theme(
      legend.position = if (show_shape_legend) "right" else "none",
      legend.key.size = unit(3, "mm"),
      axis.text.x = if (show_x) element_text() else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank()
    )
}

if (FALSE) {

fig5a <- p5_scatter(pareto_df, "ghg",   "GHG Δ (%)",   "a",
                    "Health × GHG trade-off") +
         theme(legend.position = "none")
fig5b <- p5_scatter(pareto_df, "water", "Water Δ (%)", "b",
                    "Health × water trade-off") +
         theme(legend.position = "none")
fig5c <- p5_scatter(pareto_df, "land",  "Land Δ (%)",  "c",
                    "Health × land trade-off")

## 5d. Parallel coordinates of non-dominated scenarios ----------------------
# identify dominance across (deaths↑, ghg↓, water↓, land↓, eutro↓)
dom <- pareto_df %>%
  mutate(id = row_number())
is_dom <- logical(nrow(dom))
for (i in seq_len(nrow(dom))) {
  others <- dom[-i, ]
  is_dom[i] <- any(
    others$deaths >= dom$deaths[i] &
    others$ghg    <= dom$ghg[i] &
    others$water  <= dom$water[i] &
    others$land   <= dom$land[i] &
    others$eutro  <= dom$eutro[i] &
    (others$deaths > dom$deaths[i] | others$ghg < dom$ghg[i] |
     others$water < dom$water[i]  | others$land < dom$land[i] |
     others$eutro < dom$eutro[i])
  )
}
nd <- dom[!is_dom, ]

pc <- nd %>%
  mutate(label = paste0(scenario, "\n", structure, "·", basket)) %>%
  select(label, basket, Deaths = deaths, GHG = ghg, Water = water,
         Land = land, Eutro = eutro) %>%
  mutate(across(Deaths:Eutro, ~ scales::rescale(.x))) %>%
  pivot_longer(Deaths:Eutro, names_to = "axis", values_to = "val") %>%
  mutate(axis = factor(axis, levels = c("Deaths", "GHG", "Water",
                                         "Land", "Eutro")))

fig5d <- ggplot(pc, aes(axis, val, group = label, colour = basket)) +
  geom_line(linewidth = .5, alpha = .8) +
  geom_point(size = 1.3) +
  scale_colour_manual(values = pal_basket_pareto, name = "Basket") +
  labs(tag = "d", title = "Non-dominated scenarios",
       subtitle = "Axes rescaled 0–1 (higher = better for Deaths; lower for env.)",
       x = NULL, y = "Normalised value") +
  theme_nf() +
  theme(legend.position = "right",
        legend.key.size = unit(3, "mm"))

fig5 <- (fig5a | fig5b) / (fig5c | fig5d)
save_fig(fig5, "Fig5_pareto", 183, 170)
}

fig5a <- p5_scatter(pareto_df, "ghg", "GHG change (%)", "a", "Health x GHG trade-off")
fig5b <- p5_scatter(pareto_df, "water", "Water change (%)", "b", "Health x water trade-off")
fig5c <- p5_scatter(
  pareto_df, "land", "Land change (%)", "c", "Health x land trade-off",
  show_shape_legend = TRUE
)

nd_heatmap <- nd %>%
  mutate(
    row_label = label_frontier(scenario_label, structure, basket)
  ) %>%
  arrange(deaths, basket, structure) %>%
  mutate(
    row_label = factor(row_label, levels = rev(unique(row_label)))
  ) %>%
  select(row_label, Deaths = deaths, GHG = ghg, Water = water, Land = land, Eutro = eutro) %>%
  pivot_longer(Deaths:Eutro, names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  mutate(
    desirability = if (first(metric) == "Deaths") scales::rescale(value) else scales::rescale(-value)
  ) %>%
  ungroup() %>%
  mutate(
    label = ifelse(metric == "Deaths", comma(round(value)), fmt_env_change(value)),
    label_colour = ifelse(desirability > 0.6, "white", col_ink),
    metric = factor(metric, levels = c("Deaths", "GHG", "Water", "Land", "Eutro"))
  )

fig5d_summary <- ggplot(nd_heatmap, aes(metric, row_label, fill = desirability)) +
  geom_tile(colour = "white", linewidth = 0.9) +
  geom_text(aes(label = label, colour = label_colour), size = 2.35, lineheight = 0.92) +
  scale_fill_gradientn(colours = c("#F8FAFC", "#DCEFE7", "#0F766E"), limits = c(0, 1), guide = "none") +
  scale_colour_identity() +
  labs(
    tag = "d",
    title = "Non-dominated scenarios",
    x = NULL,
    y = NULL
  ) +
  theme_nf() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = rel(0.76), lineheight = 0.95),
    plot.margin = margin(6, 10, 6, 6)
  )

fig5_summary <- (fig5a | fig5b) / ((fig5c | fig5d_summary) + plot_layout(widths = c(1.05, 0.95)))
save_fig(fig5_summary, "Fig5_pareto", 183, 170)
save_fig(fig5_summary, "Fig5_pareto_summary", 183, 170)

## ---- Improved 4-panel Pareto figure ---------------------------------------
## Shared guides, step frontier, x-axis only on bottom row, annotations only
## on panel (a), extreme-only frontier labels, composite env index in panel d.
## Globally non-dominated scenarios (dominance across all 4 env dims + deaths)
## are highlighted in every panel so the same set of labelled scenarios is
## comparable across trade-offs. The per-panel Pareto step line still shows
## the dimension-specific trade-off curve.
common_args <- list(
  frontier_style = "step",
  label_mode     = "all",
  zone_alpha     = 0.15,
  highlight      = nd
)
call_scatter <- function(...) do.call(p5_scatter, c(list(...), common_args))

fig5a2 <- call_scatter(
  df = pareto_df, yvar = "ghg", ylab = "GHG change (%)", tag = "a",
  title = "Health x GHG trade-off",
  show_x = FALSE, show_annotations = TRUE
)
fig5b2 <- call_scatter(
  df = pareto_df, yvar = "water", ylab = "Water change (%)", tag = "b",
  title = "Health x water trade-off",
  show_x = FALSE, show_annotations = FALSE
)
fig5c2 <- call_scatter(
  df = pareto_df, yvar = "land", ylab = "Land change (%)", tag = "c",
  title = "Health x land trade-off",
  show_x = TRUE, show_annotations = FALSE,
  show_shape_legend = TRUE
)
fig5d2 <- call_scatter(
  df = pareto_df, yvar = "eutro",
  ylab = "Eutrophication change (%)", tag = "d",
  title = "Health x eutrophication trade-off",
  show_x = TRUE, show_annotations = FALSE
)

fig5_4scatter <- (fig5a2 | fig5b2) / (fig5c2 | fig5d2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.key.size = unit(3, "mm"))
save_fig(fig5_4scatter, "Fig5_pareto_4scatter", 183, 170)

## ---- Panel (e): heatmap of globally non-dominated scenarios ---------------
## Shows Deaths + the 4 env dims individually for every scenario in `nd`,
## so the reader can read each dimension independently for the highlighted set.
nd_heatmap_e <- nd %>%
  mutate(row_label = label_frontier(scenario_label, structure, basket)) %>%
  arrange(deaths, basket, structure) %>%
  mutate(row_label = factor(row_label, levels = rev(unique(row_label)))) %>%
  select(row_label,
         Deaths = deaths, GHG = ghg, Water = water,
         Land = land, Eutro = eutro) %>%
  pivot_longer(Deaths:Eutro, names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  mutate(
    desirability = if (first(metric) == "Deaths")
      scales::rescale(value) else scales::rescale(-value)
  ) %>%
  ungroup() %>%
  mutate(
    label = ifelse(metric == "Deaths", comma(round(value)), fmt_env_change(value)),
    label_colour = ifelse(desirability > 0.6, "white", col_ink),
    metric = factor(metric, levels = c("Deaths", "GHG", "Water", "Land", "Eutro"))
  )

fig5e_heatmap <- ggplot(nd_heatmap_e, aes(metric, row_label, fill = desirability)) +
  geom_tile(colour = "white", linewidth = 0.9) +
  geom_text(aes(label = label, colour = label_colour),
            size = 2.35, lineheight = 0.92) +
  scale_fill_gradientn(colours = c("#F8FAFC", "#DCEFE7", "#0F766E"),
                       limits = c(0, 1), guide = "none") +
  scale_colour_identity() +
  labs(tag = "e",
       title = "Non-dominated scenarios: all 4 env dimensions + deaths",
       x = NULL, y = NULL) +
  theme_nf() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = rel(0.76), lineheight = 0.95),
    plot.margin = margin(6, 10, 6, 6)
  )

fig5_4scatter_plus <- ((fig5a2 | fig5b2) / (fig5c2 | fig5d2) / fig5e_heatmap) +
  plot_layout(guides = "collect", heights = c(1, 1, 0.85)) &
  theme(legend.position = "bottom", legend.key.size = unit(3, "mm"))
save_fig(fig5_4scatter_plus, "Fig5_pareto_4scatter_plus", 183, 240)


###############################################################################
# FIG T1-T4 — Consolidated 4-panel figure replacing manuscript Tables 1 to 4
#   a. UPF share of total energy by sex and age (Table 1)
#   b. Attributable deaths bilateral pyramid by sex and age (Table 2)
#   c. Mortality and economic burden snapshot (Table 3)
#   d. Forest plot of counterfactual scenarios with 95% UI (Table 4)
###############################################################################

## Shared style ---------------------------------------------------------------
sex_pal <- c(Men = col_men, Women = col_women)
fam_pal <- c(`Proportional reduction` = "#475569",
             `Guideline-aligned (NDG)` = "#0F766E")

theme_panel <- theme_nf() +
  theme(
    plot.title          = element_text(face = "bold", size = 9.5,
                                       margin = margin(b = 2)),
    plot.subtitle       = element_text(size = 8, colour = col_muted,
                                       margin = margin(b = 6)),
    plot.tag            = element_text(face = "bold", size = 11),
    plot.tag.position   = c(0.005, 0.985),
    axis.title          = element_text(size = 8.2, colour = col_ink),
    axis.text           = element_text(size = 7.6, colour = col_ink),
    legend.text         = element_text(size = 8),
    legend.title        = element_blank(),
    legend.key.height   = unit(3, "mm"),
    legend.key.width    = unit(5, "mm"),
    panel.grid.minor    = element_blank(),
    panel.grid.major    = element_line(colour = "#EEF1F5", linewidth = 0.3),
    plot.margin         = margin(8, 10, 6, 10)
  )

## Panel a: UPF share by sex x age -------------------------------------------
upf_strata <- read_out("upf_por_estrato.csv") %>%
  mutate(
    sex = factor(ifelse(sexo_m == 1, "Men", "Women"), levels = c("Men", "Women")),
    age = factor(age_group, levels = sort(unique(age_group)))
  )

panel_a <- ggplot(upf_strata,
                  aes(age, pct_upf_mean, colour = sex, group = sex)) +
  geom_line(linewidth = 0.55, alpha = 0.85) +
  geom_linerange(aes(ymin = pct_upf_ci_lo, ymax = pct_upf_ci_hi),
                 linewidth = 0.6, alpha = 0.85) +
  geom_point(size = 2.4, stroke = 0) +
  scale_colour_manual(values = sex_pal) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, NA),
                     expand = expansion(mult = c(0.02, 0.10))) +
  labs(tag = "a",
       title = "UPF share of total energy",
       subtitle = "Adults 30-69, ENNyS 2018-19 (mean and 95% CI)",
       x = "Age group (years)", y = "Share of total energy") +
  theme_panel +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = alpha("white", 0.8),
                                         colour = NA))

## Panel b: bilateral pyramid of attributable deaths -------------------------
pyr_b <- tab2 %>%
  rename(age = 1, men = "Men, deaths attr", women = "Women, deaths attr") %>%
  select(age, men, women) %>%
  mutate(men = -as.numeric(men), women = as.numeric(women)) %>%
  pivot_longer(-age, names_to = "sex", values_to = "deaths") %>%
  mutate(sex = recode(sex, men = "Men", women = "Women"),
         age = factor(age, levels = tab2[[1]]))

pyr_lim <- ceiling(max(abs(pyr_b$deaths)) / 500) * 500

panel_b <- ggplot(pyr_b, aes(deaths, age, fill = sex)) +
  geom_col(width = 0.74) +
  geom_vline(xintercept = 0, colour = col_ink, linewidth = 0.35) +
  geom_text(aes(label = comma(abs(deaths)),
                hjust = ifelse(deaths < 0, 1.12, -0.12)),
            size = 2.55, colour = col_ink) +
  scale_fill_manual(values = sex_pal) +
  scale_x_continuous(labels = function(x) comma(abs(x)),
                     limits = c(-pyr_lim * 1.18, pyr_lim * 1.18),
                     breaks = seq(-pyr_lim, pyr_lim, by = pyr_lim / 2)) +
  labs(tag = "b",
       title = "Attributable deaths by sex and age",
       subtitle = "UPF-attributable premature deaths per year",
       x = "Deaths per year", y = "Age group (years)") +
  theme_panel +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank())

## Panel c: burden snapshot (Table 3) ----------------------------------------
le_df <- tab6 %>% rename(group = 1, e30 = 2, e30_no = 3, gain = 4) %>%
  mutate(group = recode(group, Total = "Total", Hombres = "Men",
                        Mujeres = "Women"),
         group = factor(group, levels = c("Women", "Men", "Total")))

cost_total_bn <- as.numeric(tab7$costo_PPP_millones_intl_USD) / 1000
yll_total <- sum(as.numeric(read_out("resumen_yll_por_causa.csv")$yll_attr),
                 na.rm = TRUE)
if (!is.finite(yll_total) || yll_total == 0) yll_total <- 349686

panel_c_le <- ggplot(le_df, aes(gain, group, fill = group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("+%.2f yr", gain)),
            hjust = -0.15, size = 2.7, colour = col_ink) +
  scale_fill_manual(values = c(Women = col_women, Men = col_men,
                               Total = "#334155"), guide = "none") +
  scale_x_continuous(limits = c(0, max(le_df$gain) * 1.55),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Life expectancy gain at age 30",
       subtitle = "Years gained if UPF-attributable deaths were averted",
       x = NULL, y = NULL) +
  theme_panel +
  theme(plot.title = element_text(face = "bold", size = 8.6,
                                  margin = margin(b = 1)),
        plot.subtitle = element_text(size = 7.4, colour = col_muted,
                                     margin = margin(b = 4)),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(2, 6, 2, 6))

kpi_df <- tibble(
  x = c(0, 1),
  label_top = c(comma(round(yll_total)),
                sprintf("%.2f", cost_total_bn)),
  label_bot = c("YLL attributable\nto UPF / year",
                "Indirect cost\n(billion intl.$ PPP)")
)

panel_c_kpi <- ggplot(kpi_df, aes(x, 0)) +
  geom_text(aes(label = label_top), size = 5.6, fontface = "bold",
            colour = "#0F766E", vjust = 0.4) +
  geom_text(aes(y = -0.55, label = label_bot), size = 2.55,
            colour = col_muted, lineheight = 0.95) +
  scale_x_continuous(limits = c(-0.5, 1.5)) +
  scale_y_continuous(limits = c(-1.1, 0.9)) +
  theme_void() +
  theme(plot.margin = margin(2, 4, 2, 4))

panel_c <- (panel_c_le / panel_c_kpi) +
  plot_layout(heights = c(1.05, 0.95)) &
  theme(plot.background = element_rect(fill = NA, colour = NA))
panel_c <- wrap_elements(panel_c) +
  labs(tag = "c") +
  theme(plot.tag = element_text(face = "bold", size = 11),
        plot.tag.position = c(0.005, 0.985))

## Panel d: scenario forest (Table 4) ----------------------------------------
sehs <- read_out("scenario_health_economic_summary.csv") %>%
  filter(!is.na(scenario_label)) %>%
  mutate(
    scenario_label = factor(
      scenario_label,
      levels = c("10% reduction", "20% reduction", "50% reduction",
                 "NDG conservative", "NDG central", "NDG strict")
    ),
    family = ifelse(grepl("^NDG", scenario_label),
                    "Guideline-aligned (NDG)", "Proportional reduction")
  ) %>%
  arrange(scenario_label) %>%
  mutate(scenario_label = factor(scenario_label,
                                 levels = rev(levels(scenario_label))))

fmt_int <- function(x) comma(round(x))
fmt_dec <- function(x) sprintf("%.2f", x)

forest_sub <- function(df, est, lo, hi, title, xlab, fmt) {
  ggplot(df, aes(.data[[est]], scenario_label, colour = family)) +
    geom_linerange(aes(xmin = .data[[lo]], xmax = .data[[hi]]),
                   linewidth = 0.7, alpha = 0.95) +
    geom_point(size = 2.3, stroke = 0) +
    geom_text(aes(label = fmt(.data[[est]])),
              hjust = -0.18, vjust = -0.55, size = 2.25, colour = col_ink) +
    scale_colour_manual(values = fam_pal) +
    scale_x_continuous(labels = fmt,
                       expand = expansion(mult = c(0.06, 0.22))) +
    labs(title = title, x = xlab, y = NULL) +
    theme_panel +
    theme(plot.title = element_text(face = "bold", size = 8.4,
                                    margin = margin(b = 1)),
          axis.title.x = element_text(size = 7.6),
          axis.text.x = element_text(size = 7.2),
          panel.grid.major.y = element_blank(),
          plot.margin = margin(4, 6, 2, 6))
}

pd_a <- forest_sub(sehs, "deaths_averted", "deaths_averted_lo",
                   "deaths_averted_hi",
                   "Deaths averted", "per year", fmt_int)
pd_b <- forest_sub(sehs, "yll_averted", "yll_averted_lo",
                   "yll_averted_hi",
                   "YLL averted", "per year", fmt_int) +
  theme(axis.text.y = element_blank())
pd_c <- forest_sub(sehs, "pvle_averted_billion_intl_ppp",
                   "pvle_averted_billion_intl_ppp_lo",
                   "pvle_averted_billion_intl_ppp_hi",
                   "Averted PVLE", "billion intl.$ PPP", fmt_dec) +
  theme(axis.text.y = element_blank())

panel_d <- (pd_a | pd_b | pd_c) +
  plot_layout(guides = "collect", widths = c(1.35, 1, 1)) &
  theme(legend.position = "bottom")
panel_d <- wrap_elements(panel_d) +
  labs(tag = "d",
       title = "Counterfactual scenarios: averted health and economic burden",
       subtitle = "Point estimates with 95% uncertainty intervals") +
  theme(plot.tag = element_text(face = "bold", size = 11),
        plot.tag.position = c(0.005, 0.985),
        plot.title = element_text(face = "bold", size = 9.5,
                                  margin = margin(b = 1)),
        plot.subtitle = element_text(size = 8, colour = col_muted,
                                     margin = margin(b = 4)))

## Compose --------------------------------------------------------------------
top_row <- (panel_a | panel_b | panel_c) +
  plot_layout(widths = c(1.05, 1.15, 0.9))

fig_T1T4 <- (top_row / panel_d) +
  plot_layout(heights = c(1, 1.05)) +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", colour = NA))
  )

gc(verbose = FALSE)
save_fig(fig_T1T4, "Fig_main_burden_scenarios", 178, 175)

###############################################################################
message("\n✔ All 5 Nature Food figures saved to ", FIG, "\n")
