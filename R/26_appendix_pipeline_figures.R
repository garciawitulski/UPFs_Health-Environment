# 26_appendix_pipeline_figures.R
# ---------------------------------------------------------------------------
# Redesigned appendix Figures S1 and S2 as publication-style pipeline diagrams.
# Three alternatives are produced so the authors can choose:
#
#   Fig_S1_health_pipeline   - Option 3: split input -> core -> output graph
#                              for the health CRA pipeline.
#   Fig_S2_env_pipeline      - Option 3: split input -> core -> output graph
#                              for the environmental impact pipeline.
#   Fig_S1S2_unified         - Option 4: hybrid single diagram showing how the
#                              two pipelines share ENNyS inputs and diverge
#                              into health and environmental branches.
#
# Palette and typography are aligned with the main manuscript figures
# (slate #1F3A4D / slate-teal #6FA8B5).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(patchwork)
})

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
OUT  <- file.path(root, "output")
DEST <- file.path(OUT, "figures", "appendix_pipelines")
dir.create(DEST, showWarnings = FALSE, recursive = TRUE)

# --- Palette -----------------------------------------------------------------
col_ink    <- "#111827"
col_muted  <- "#6B7280"
col_rule   <- "#1F2937"
col_input  <- "#6FA8B5"   # slate-teal  -> inputs / data
col_core   <- "#1F3A4D"   # deep slate  -> core computations
col_output <- "#B45309"   # amber       -> outputs / headline results
col_note   <- "#9CA3AF"   # grey        -> annotations / scenarios
col_bg     <- "#F8FAFC"

node_fill <- function(kind) {
  switch(kind,
         input  = "#E1ECF0",
         core   = "#E2E8EE",
         output = "#FDF1DD",
         note   = "#F1F3F5",
         "#FFFFFF")
}
node_stroke <- function(kind) {
  switch(kind,
         input  = col_input,
         core   = col_core,
         output = col_output,
         note   = col_note,
         col_ink)
}

# --- Node / edge helpers -----------------------------------------------------
# A node is defined by its centre (x, y) and half-width / half-height.
make_node <- function(id, x, y, w, h, kind, title, body = "") {
  tibble(id = id, x = x, y = y, w = w, h = h, kind = kind,
         title = title, body = body,
         xmin = x - w, xmax = x + w,
         ymin = y - h, ymax = y + h,
         fill   = node_fill(kind),
         stroke = node_stroke(kind))
}

make_edge <- function(nodes, from, to, curvature = 0) {
  a <- nodes[nodes$id == from, ]
  b <- nodes[nodes$id == to, ]
  # Anchor on the nearest border to create clean entry / exit points.
  if (abs(b$x - a$x) >= abs(b$y - a$y)) {
    x1 <- ifelse(b$x >= a$x, a$xmax, a$xmin)
    x2 <- ifelse(b$x >= a$x, b$xmin, b$xmax)
    y1 <- a$y; y2 <- b$y
  } else {
    y1 <- ifelse(b$y >= a$y, a$ymax, a$ymin)
    y2 <- ifelse(b$y >= a$y, b$ymin, b$ymax)
    x1 <- a$x; x2 <- b$x
  }
  tibble(x = x1, y = y1, xend = x2, yend = y2, curvature = curvature)
}

draw_pipeline <- function(nodes, edges, title_text = NULL, subtitle_text = NULL,
                          xlim, ylim) {
  p <- ggplot() +
    # Node rectangles with a thin drop shadow for depth.
    geom_rect(data = nodes %>% mutate(xmin = xmin + 0.04, xmax = xmax + 0.04,
                                      ymin = ymin - 0.04, ymax = ymax - 0.04),
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = alpha("#000000", 0.06), colour = NA) +
    geom_rect(data = nodes,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = I(fill), colour = I(stroke)),
              linewidth = 0.55)
  # Edges: straight for 0 curvature, curved otherwise.
  straight <- edges %>% filter(curvature == 0)
  curved   <- edges %>% filter(curvature != 0)
  if (nrow(straight) > 0) {
    p <- p + geom_segment(
      data = straight,
      aes(x = x, y = y, xend = xend, yend = yend),
      colour = col_rule, linewidth = 0.5,
      arrow = arrow(length = unit(2.2, "mm"), type = "closed")
    )
  }
  if (nrow(curved) > 0) {
    for (cv in unique(curved$curvature)) {
      p <- p + geom_curve(
        data = curved %>% filter(curvature == cv),
        aes(x = x, y = y, xend = xend, yend = yend),
        colour = col_rule, linewidth = 0.5, curvature = cv,
        arrow = arrow(length = unit(2.2, "mm"), type = "closed")
      )
    }
  }
  # Node text: bold title + optional body.
  p <- p +
    geom_text(data = nodes,
              aes(x = x, y = y + h * 0.45, label = title),
              fontface = "bold", size = 2.9, colour = col_ink,
              lineheight = 0.95) +
    geom_text(data = nodes %>% filter(nzchar(body)),
              aes(x = x, y = y - h * 0.12, label = body),
              size = 2.25, colour = col_muted, lineheight = 1.0) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    labs(title = title_text, subtitle = subtitle_text) +
    theme_void(base_size = 10) +
    theme(
      plot.title    = element_text(face = "bold", size = 11, colour = col_ink,
                                   hjust = 0, margin = margin(b = 2)),
      plot.subtitle = element_text(size = 9, colour = col_muted,
                                   hjust = 0, margin = margin(b = 6)),
      plot.background = element_rect(fill = col_bg, colour = NA),
      plot.margin = margin(10, 12, 10, 12)
    )
  p
}

save_pair <- function(plot_obj, base_name, w_mm, h_mm) {
  for (ext in c(".pdf", ".png")) {
    f <- file.path(DEST, paste0(base_name, ext))
    if (ext == ".pdf") {
      ggsave(f, plot_obj, width = w_mm, height = h_mm, units = "mm",
             dpi = 400, device = cairo_pdf)
    } else {
      ggsave(f, plot_obj, width = w_mm, height = h_mm, units = "mm", dpi = 400)
    }
    message("  saved ", basename(f))
  }
}

# ===========================================================================
# FIGURE S1 (Option 3): Health impact pipeline
# Left  = data inputs; Centre = CRA computation core; Right = results.
# ===========================================================================
# Layout: 3 columns (x = 1, 5, 9), nodes stacked vertically in each column.
w_in <- 1.50; h_in <- 0.60
w_cr <- 1.65; h_cr <- 0.62
w_ou <- 1.55; h_ou <- 0.55

s1_nodes <- bind_rows(
  # Inputs (left column)
  make_node("in_ennys", 1.0, 5.40, w_in, h_in, "input",
            "ENNyS 2 (2018-19)",
            "24-h recalls  |  NOVA class\ng_kji, kcal_i, w_k"),
  make_node("in_deaths", 1.0, 3.60, w_in, h_in, "input",
            "DEIS mortality 2019",
            "All-cause deaths D_sa\nby sex x age"),
  make_node("in_rr",    1.0, 1.80, w_in, h_in, "input",
            "Dose-response",
            "Pagliai 2021 RR = 1.25\n(35.7 pp reference)"),

  # Core computations (middle column)
  make_node("co_exp",   5.0, 6.20, w_cr, h_cr, "core",
            "1. UPF energy share",
            "%UPF_kj  ->  person mean\nmu_sa, sigma_sa"),
  make_node("co_cra",   5.0, 4.60, w_cr, h_cr, "core",
            "2. Distributional CRA",
            "Log-normal truncated\nE[RR(X_sa)]"),
  make_node("co_paf",   5.0, 3.00, w_cr, h_cr, "core",
            "3. PAF & attributable deaths",
            "PAF_sa = (E[RR]-E[RR_cf])/E[RR]\nA_sa = PAF_sa * D_sa"),
  make_node("co_mc",    5.0, 1.40, w_cr, h_cr, "core",
            "4. Monte Carlo (10 000)",
            "Sample mu, beta, D\n95% UI from quantiles"),

  # Scenarios annotation
  make_node("co_scen",  5.0, 7.70, w_cr + 0.25, 0.55, "note",
            "Counterfactual scenarios",
            "TMRL  |  10/20/50% prop. reduction  |  NDG 5.45/3.63/1.81%"),

  # Outputs (right column)
  make_node("ou_deaths", 9.0, 5.80, w_ou, h_ou, "output",
            "Deaths averted",
            "sum A_sa"),
  make_node("ou_yll",    9.0, 4.30, w_ou, h_ou, "output",
            "YLL averted",
            "YLL = A_sa * L_a"),
  make_node("ou_le",     9.0, 2.80, w_ou, h_ou, "output",
            "Life expectancy",
            "Delta e30 (cause-deleted)"),
  make_node("ou_pvle",   9.0, 1.30, w_ou, h_ou, "output",
            "Economic PVLE",
            "Indirect cost averted"),
)

s1_edges <- bind_rows(
  make_edge(s1_nodes, "in_ennys",  "co_exp"),
  make_edge(s1_nodes, "in_deaths", "co_paf"),
  make_edge(s1_nodes, "in_rr",     "co_cra"),
  make_edge(s1_nodes, "co_exp",    "co_cra"),
  make_edge(s1_nodes, "co_cra",    "co_paf"),
  make_edge(s1_nodes, "co_paf",    "co_mc"),
  make_edge(s1_nodes, "co_scen",   "co_cra", curvature = -0.25),
  make_edge(s1_nodes, "co_mc",     "ou_deaths", curvature = -0.20),
  make_edge(s1_nodes, "co_mc",     "ou_yll",    curvature = -0.10),
  make_edge(s1_nodes, "co_mc",     "ou_le",     curvature =  0.10),
  make_edge(s1_nodes, "co_mc",     "ou_pvle",   curvature =  0.25)
)

# Column headers
s1_headers <- tibble(
  x = c(1.0, 5.0, 9.0),
  y = 8.55,
  label = c("INPUTS", "CORE COMPUTATIONS", "OUTPUTS"),
  colour = c(col_input, col_core, col_output)
)

fig_s1 <- draw_pipeline(
  s1_nodes, s1_edges,
  title_text   = "Figure S1. Health impact estimation pipeline",
  subtitle_text = "Dietary exposure, dose-response and mortality data flow through a comparative risk assessment core into health and economic outputs.",
  xlim = c(-0.4, 10.4), ylim = c(0.4, 9.0)
) +
  geom_text(data = s1_headers,
            aes(x = x, y = y, label = label, colour = I(colour)),
            fontface = "bold", size = 3.2) +
  annotate("segment", x = 2.7, xend = 2.7, y = 0.6, yend = 8.2,
           colour = col_note, linewidth = 0.3, linetype = "22") +
  annotate("segment", x = 7.3, xend = 7.3, y = 0.6, yend = 8.2,
           colour = col_note, linewidth = 0.3, linetype = "22")

save_pair(fig_s1, "Fig_S1_health_pipeline", 220, 180)

# ===========================================================================
# FIGURE S2 (Option 3): Environmental impact pipeline
# ===========================================================================
s2_nodes <- bind_rows(
  # Inputs
  make_node("en_intake", 1.0, 5.80, w_in, h_in, "input",
            "ENNyS intake",
            "g per food code\n(with survey weights)"),
  make_node("en_lca",    1.0, 4.00, w_in, h_in, "input",
            "LCA coefficients",
            "Arrieta 2022 + OWID\nc_i,m per kg"),
  make_node("en_phi",    1.0, 2.20, w_in, h_in, "input",
            "Yield ratios phi_i",
            "Retail -> farm-gate"),

  # Core
  make_node("co_mass",   5.0, 5.80, w_cr, h_cr, "core",
            "1. Intake -> mass",
            "q_ret = g/1000\nq_ref = q_ret / phi_i"),
  make_node("co_item",   5.0, 4.20, w_cr, h_cr, "core",
            "2. Item footprint",
            "I_kji,m = q_ref * c_i,m"),
  make_node("co_agg",    5.0, 2.60, w_cr, h_cr, "core",
            "3. Population totals",
            "I_base_m  &  I_UPF_m"),
  make_node("co_subs",   5.0, 1.00, w_cr, h_cr, "core",
            "4. Counterfactuals",
            "No sub / isocaloric / isoweight"),

  # Note: baskets
  make_node("no_bask",   5.0, 7.15, w_cr + 0.25, 0.55, "note",
            "Replacement baskets",
            "NOVA1  |  NOVA3  |  NOVA1+3 blend  |  NDG-aligned"),

  # Outputs
  make_node("ou_share",  9.0, 5.60, w_ou, h_ou, "output",
            "Baseline UPF share",
            "% of I_base per indicator"),
  make_node("ou_ghg",    9.0, 4.20, w_ou, h_ou, "output",
            "GHG / Land",
            "Net % change"),
  make_node("ou_water",  9.0, 2.80, w_ou, h_ou, "output",
            "Water / Eutroph.",
            "Net % change"),
  make_node("ou_trade",  9.0, 1.30, w_ou, h_ou, "output",
            "Trade-off frontier",
            "Figure 4 (Pareto)"),
)

s2_edges <- bind_rows(
  make_edge(s2_nodes, "en_intake", "co_mass"),
  make_edge(s2_nodes, "en_phi",    "co_mass"),
  make_edge(s2_nodes, "en_lca",    "co_item"),
  make_edge(s2_nodes, "co_mass",   "co_item"),
  make_edge(s2_nodes, "co_item",   "co_agg"),
  make_edge(s2_nodes, "co_agg",    "co_subs"),
  make_edge(s2_nodes, "no_bask",   "co_subs", curvature = -0.30),
  make_edge(s2_nodes, "co_agg",    "ou_share", curvature = -0.15),
  make_edge(s2_nodes, "co_subs",   "ou_ghg",   curvature = -0.20),
  make_edge(s2_nodes, "co_subs",   "ou_water", curvature = -0.05),
  make_edge(s2_nodes, "co_subs",   "ou_trade", curvature =  0.20)
)

s2_headers <- tibble(
  x = c(1.0, 5.0, 9.0),
  y = 8.10,
  label = c("INPUTS", "CORE COMPUTATIONS", "OUTPUTS"),
  colour = c(col_input, col_core, col_output)
)

fig_s2 <- draw_pipeline(
  s2_nodes, s2_edges,
  title_text   = "Figure S2. Environmental impact estimation pipeline",
  subtitle_text = "Dietary intake is combined with life-cycle coefficients and converted into counterfactual scenarios under four replacement baskets.",
  xlim = c(-0.4, 10.4), ylim = c(0.3, 8.6)
) +
  geom_text(data = s2_headers,
            aes(x = x, y = y, label = label, colour = I(colour)),
            fontface = "bold", size = 3.2) +
  annotate("segment", x = 2.7, xend = 2.7, y = 0.6, yend = 7.75,
           colour = col_note, linewidth = 0.3, linetype = "22") +
  annotate("segment", x = 7.3, xend = 7.3, y = 0.6, yend = 7.75,
           colour = col_note, linewidth = 0.3, linetype = "22")

save_pair(fig_s2, "Fig_S2_env_pipeline", 220, 180)

# ===========================================================================
# HYBRID (Option 4): Unified diagram with shared ENNyS root that diverges
# into a health branch (top) and an environmental branch (bottom).
# ===========================================================================
w_n <- 1.55; h_n <- 0.55

u_nodes <- bind_rows(
  # Shared root (left)
  make_node("root", 1.2, 4.50, 1.70, 0.80, "input",
            "ENNyS 2 dietary recalls",
            "g_kji  |  NOVA 1-4  |  kcal_i\nsurvey weights w_k"),

  # Health branch (top row)
  make_node("h_in",   4.0, 7.20, w_n, h_n, "input",
            "DEIS deaths + Pagliai RR",
            "D_sa  |  RR = 1.25"),
  make_node("h_exp",  4.0, 5.90, w_n, h_n, "core",
            "%UPF, mu_sa, sigma_sa",
            "person-level means"),
  make_node("h_cra",  6.5, 6.55, w_n, h_n, "core",
            "Distributional CRA",
            "E[RR(X_sa)]  |  PAF"),
  make_node("h_mc",   9.0, 6.55, w_n, h_n, "core",
            "Monte Carlo UI",
            "10 000 iterations"),
  make_node("h_out",  11.6, 6.55, 1.70, h_n, "output",
            "Health outputs",
            "Deaths | YLL | Delta e30 | PVLE"),

  # Environmental branch (bottom row)
  make_node("e_in",   4.0, 1.80, w_n, h_n, "input",
            "LCA coefficients",
            "Arrieta 2022 + OWID\nphi_i, c_i,m"),
  make_node("e_mass", 4.0, 3.10, w_n, h_n, "core",
            "Intake -> mass",
            "q_ret, q_ref"),
  make_node("e_item", 6.5, 2.45, w_n, h_n, "core",
            "Item footprints",
            "I_kji,m = q_ref * c_i,m"),
  make_node("e_agg",  9.0, 2.45, w_n, h_n, "core",
            "Population totals",
            "I_base  |  I_UPF"),
  make_node("e_out",  11.6, 2.45, 1.70, h_n, "output",
            "Environmental outputs",
            "GHG | Land | Water | Eutroph."),

  # Shared scenario engine (centre bridge between branches)
  make_node("scen", 9.0, 4.50, 2.00, 0.65, "note",
            "Shared scenario engine",
            "TMRL | 10/20/50% | NDG targets\nNOVA1 / NOVA3 / blend / NDG baskets"),

  # Final integrated output
  make_node("final", 11.6, 4.50, 1.70, 0.65, "output",
            "Integrated trade-off",
            "Figure 4: health vs environment"),
)

u_edges <- bind_rows(
  # Root to both branches
  make_edge(u_nodes, "root", "h_exp",  curvature = -0.15),
  make_edge(u_nodes, "root", "e_mass", curvature =  0.15),

  # Health branch
  make_edge(u_nodes, "h_in",  "h_cra", curvature = -0.10),
  make_edge(u_nodes, "h_exp", "h_cra"),
  make_edge(u_nodes, "h_cra", "h_mc"),
  make_edge(u_nodes, "h_mc",  "h_out"),

  # Environmental branch
  make_edge(u_nodes, "e_in",   "e_item", curvature =  0.10),
  make_edge(u_nodes, "e_mass", "e_item"),
  make_edge(u_nodes, "e_item", "e_agg"),
  make_edge(u_nodes, "e_agg",  "e_out"),

  # Scenario engine connects both branches
  make_edge(u_nodes, "scen", "h_cra",  curvature =  0.20),
  make_edge(u_nodes, "scen", "e_agg",  curvature = -0.20),

  # Convergence to integrated output
  make_edge(u_nodes, "h_out", "final", curvature =  0.20),
  make_edge(u_nodes, "e_out", "final", curvature = -0.20),
  make_edge(u_nodes, "scen",  "final")
)

# Branch labels
u_labels <- tibble(
  x = c(2.6, 2.6),
  y = c(8.10, 0.90),
  label = c("HEALTH BRANCH", "ENVIRONMENTAL BRANCH"),
  colour = c(col_core, col_core)
)

fig_u <- draw_pipeline(
  u_nodes, u_edges,
  title_text = "Figure S1+S2 (unified). Shared dietary exposure feeds parallel health and environmental pipelines that converge on a joint trade-off analysis.",
  subtitle_text = "ENNyS recalls anchor both pipelines; a shared counterfactual scenario engine links exposure changes to health and environmental outcomes.",
  xlim = c(-0.4, 13.0), ylim = c(0.2, 8.6)
) +
  geom_text(data = u_labels,
            aes(x = x, y = y, label = label, colour = I(colour)),
            fontface = "bold", size = 3.1, hjust = 0) +
  annotate("segment", x = 2.9, xend = 12.6, y = 8.10, yend = 8.10,
           colour = col_input, linewidth = 0.25, linetype = "22") +
  annotate("segment", x = 2.9, xend = 12.6, y = 0.90, yend = 0.90,
           colour = col_input, linewidth = 0.25, linetype = "22")

save_pair(fig_u, "Fig_S1S2_unified", 260, 170)

message("Done. Files in: ", DEST)
