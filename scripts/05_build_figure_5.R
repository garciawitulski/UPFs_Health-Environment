suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
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

# ---------------------------------------------------------------------------
# Figure 5
# Direct R/ggplot implementation adapted from R/26_appendix_pipeline_figures.R
# ---------------------------------------------------------------------------

col_input <- "#6FA8B5"
col_core <- "#1F3A4D"
col_output <- "#B45309"
col_note <- "#9CA3AF"
col_bg <- "#F8FAFC"

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

make_node <- function(id, x, y, w, h, kind, title, body = "") {
  tibble(
    id = id, x = x, y = y, w = w, h = h, kind = kind,
    title = title, body = body,
    xmin = x - w, xmax = x + w,
    ymin = y - h, ymax = y + h,
    fill = node_fill(kind),
    stroke = node_stroke(kind)
  )
}

make_edge <- function(nodes, from, to, curvature = 0) {
  a <- nodes[nodes$id == from, ]
  b <- nodes[nodes$id == to, ]
  if (abs(b$x - a$x) >= abs(b$y - a$y)) {
    x1 <- ifelse(b$x >= a$x, a$xmax, a$xmin)
    x2 <- ifelse(b$x >= a$x, b$xmin, b$xmax)
    y1 <- a$y
    y2 <- b$y
  } else {
    y1 <- ifelse(b$y >= a$y, a$ymax, a$ymin)
    y2 <- ifelse(b$y >= a$y, b$ymin, b$ymax)
    x1 <- a$x
    x2 <- b$x
  }
  tibble(x = x1, y = y1, xend = x2, yend = y2, curvature = curvature)
}

draw_pipeline <- function(nodes, edges, xlim, ylim) {
  p <- ggplot() +
    geom_rect(
      data = nodes %>% mutate(
        xmin = xmin + 0.04, xmax = xmax + 0.04,
        ymin = ymin - 0.04, ymax = ymax - 0.04
      ),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = alpha("#000000", 0.06), colour = NA
    ) +
    geom_rect(
      data = nodes,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
          fill = I(fill), colour = I(stroke)),
      linewidth = 0.55
    )

  straight <- edges %>% filter(curvature == 0)
  curved <- edges %>% filter(curvature != 0)

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

  p +
    geom_text(
      data = nodes,
      aes(x = x, y = y + h * 0.45, label = title),
      fontface = "bold", size = 2.9, colour = col_ink, lineheight = 0.95
    ) +
    geom_text(
      data = nodes %>% filter(nzchar(body)),
      aes(x = x, y = y - h * 0.12, label = body),
      size = 2.25, colour = col_muted, lineheight = 1.0
    ) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    theme_void(base_size = 10) +
    theme(
      plot.background = element_rect(fill = col_bg, colour = NA),
      plot.margin = margin(10, 12, 10, 12)
    )
}

w_n <- 1.55
h_n <- 0.55

u_nodes <- bind_rows(
  make_node("root", 1.2, 4.50, 1.70, 0.80, "input",
            "ENNyS 2 dietary recalls",
            "g_kji  |  NOVA 1-4  |  kcal_i\nsurvey weights w_k"),

  make_node("h_in", 4.0, 7.20, w_n, h_n, "input",
            "DEIS mortality 2019\n+\nDose-response (B.2)",
            "D_sa  |  RR = 1.25"),
  make_node("h_exp", 4.0, 5.90, w_n, h_n, "core",
            "1. %UPF exposure (B.1)",
            "person mean pctUPF_k\nmu_sa, sigma_sa"),
  make_node("h_cra", 6.5, 6.55, w_n, h_n, "core",
            "2. Distributional CRA (B.3)",
            "E[RR(X_sa)]  |  PAF"),
  make_node("h_mc", 9.0, 6.55, w_n, h_n, "core",
            "3. Monte Carlo (B.4)",
            "10,000 iterations"),
  make_node("h_out", 11.6, 6.55, 1.70, h_n, "output",
            "Health outputs",
            "Deaths | YLL | Delta e30 | PVLE"),

  make_node("e_in", 4.0, 1.80, w_n, h_n, "input",
            "LCA coefficients and yield ratios",
            "Arrieta 2022 + OWID\nphi_i, c_i,m"),
  make_node("e_mass", 4.0, 3.10, w_n, h_n, "core",
            "1. Intake to mass (B.8)",
            "q_ret  |  q_ref"),
  make_node("e_item", 6.5, 2.45, w_n, h_n, "core",
            "2. Item footprints",
            "I_kji,m = q_ref * c_i,m"),
  make_node("e_agg", 9.0, 2.45, w_n, h_n, "core",
            "3. Population totals",
            "I_base  |  I_UPF"),
  make_node("e_out", 11.6, 2.45, 1.70, h_n, "output",
            "Environmental outputs",
            "GHG | Land | Water | Eutroph."),

  make_node("scen", 9.0, 4.50, 2.00, 0.65, "note",
            "Shared scenario engine",
            "TMRL | 10/20/50% | NDG targets\nNOVA 1 / NOVA 3 / blend / NDG"),

  make_node("final", 11.6, 4.50, 1.70, 0.65, "output",
            "Integrated output",
            "Figure 4: health-environment trade-offs")
)

u_edges <- bind_rows(
  make_edge(u_nodes, "root", "h_exp", curvature = -0.15),
  make_edge(u_nodes, "root", "e_mass", curvature = 0.15),
  make_edge(u_nodes, "h_in", "h_cra", curvature = -0.10),
  make_edge(u_nodes, "h_exp", "h_cra"),
  make_edge(u_nodes, "h_cra", "h_mc"),
  make_edge(u_nodes, "h_mc", "h_out"),
  make_edge(u_nodes, "e_in", "e_item", curvature = 0.10),
  make_edge(u_nodes, "e_mass", "e_item"),
  make_edge(u_nodes, "e_item", "e_agg"),
  make_edge(u_nodes, "e_agg", "e_out"),
  make_edge(u_nodes, "scen", "h_cra", curvature = 0.20),
  make_edge(u_nodes, "scen", "e_agg", curvature = -0.20),
  make_edge(u_nodes, "h_out", "final", curvature = 0.20),
  make_edge(u_nodes, "e_out", "final", curvature = -0.20),
  make_edge(u_nodes, "scen", "final")
)

u_labels <- tibble(
  x = c(2.6, 2.6),
  y = c(8.10, 0.90),
  label = c("HEALTH BRANCH", "ENVIRONMENTAL BRANCH"),
  colour = c(col_core, col_core)
)

fig_u <- draw_pipeline(
  u_nodes, u_edges,
  xlim = c(-0.4, 13.0),
  ylim = c(0.2, 8.6)
) +
  geom_text(
    data = u_labels,
    aes(x = x, y = y, label = label, colour = I(colour)),
    fontface = "bold", size = 3.1, hjust = 0
  ) +
  annotate("segment", x = 2.9, xend = 12.6, y = 8.10, yend = 8.10,
           colour = col_input, linewidth = 0.25, linetype = "22") +
  annotate("segment", x = 2.9, xend = 12.6, y = 0.90, yend = 0.90,
           colour = col_input, linewidth = 0.25, linetype = "22")

save_pair(fig_u, "Figure_5", width_mm = 260, height_mm = 170)
message("Saved Figure_5 to ", FIG_DIR)
