suppressPackageStartupMessages({
  library(grid)
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

col_input_border <- "#6FA8B5"
col_input_fill <- "#E1ECF0"
col_core_border <- "#1F3A4D"
col_core_fill <- "#E2E8EE"
col_output_border <- "#B45309"
col_output_fill <- "#FDF1DD"
col_note_border <- "#9CA3AF"
col_note_fill <- "#F3F4F6"
col_bg <- "#F8FAFC"
col_health <- "#A63A50"
col_env <- "#1F7A6F"

draw_box <- function(x, y, w, h, title, body = character(), border, fill,
                     dashed = FALSE, title_col = col_ink, body_col = col_ink,
                     title_cex = 0.78, body_cex = 0.70, body_y = 0.43) {
  grid.roundrect(
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    width = unit(w, "npc"),
    height = unit(h, "npc"),
    r = unit(2.6, "mm"),
    gp = gpar(
      fill = fill,
      col = border,
      lwd = if (dashed) 1.0 else 1.25,
      lty = if (dashed) 2 else 1
    )
  )
  grid.text(
    title,
    x = unit(x, "npc"),
    y = unit(y + h * 0.24, "npc"),
    gp = gpar(col = title_col, fontsize = 10.5 * title_cex, fontface = "bold")
  )
  if (length(body)) {
    grid.text(
      paste(body, collapse = "\n"),
      x = unit(x, "npc"),
      y = unit(y - h * body_y / 2.6, "npc"),
      gp = gpar(col = body_col, fontsize = 10.2 * body_cex, lineheight = 1.08)
    )
  }
}

draw_arrow <- function(x0, y0, x1, y1, col = col_rule, lwd = 1.1, dashed = FALSE) {
  grid.lines(
    x = unit(c(x0, x1), "npc"),
    y = unit(c(y0, y1), "npc"),
    gp = gpar(col = col, lwd = lwd, lty = if (dashed) 2 else 1),
    arrow = arrow(type = "closed", length = unit(2.3, "mm"))
  )
}

draw_poly_arrow <- function(xs, ys, col = col_rule, lwd = 1.0, dashed = FALSE) {
  grid.lines(
    x = unit(xs, "npc"),
    y = unit(ys, "npc"),
    gp = gpar(col = col, lwd = lwd, lty = if (dashed) 2 else 1),
    arrow = arrow(type = "closed", length = unit(2.0, "mm"))
  )
}

draw_stage_guides <- function(x_pos, labels) {
  for (i in seq_along(x_pos)) {
    grid.text(
      labels[i],
      x = unit(x_pos[i], "npc"),
      y = unit(0.95, "npc"),
      gp = gpar(col = col_muted, fontsize = 7.2, fontface = "bold")
    )
    grid.lines(
      x = unit(c(x_pos[i], x_pos[i]), "npc"),
      y = unit(c(0.16, 0.93), "npc"),
      gp = gpar(col = col_grid, lwd = 0.8, lty = 3)
    )
  }
}

draw_legend <- function() {
  lx <- 0.17
  ly <- 0.075
  lw <- 0.27
  lh <- 0.085

  grid.roundrect(
    x = unit(lx, "npc"),
    y = unit(ly, "npc"),
    width = unit(lw, "npc"),
    height = unit(lh, "npc"),
    r = unit(2, "mm"),
    gp = gpar(fill = "white", col = col_note_border, lwd = 0.9)
  )

  swatch <- function(x, y, fill, border, label, dashed = FALSE) {
    grid.roundrect(
      x = unit(x, "npc"),
      y = unit(y, "npc"),
      width = unit(0.015, "npc"),
      height = unit(0.018, "npc"),
      r = unit(1.4, "mm"),
      gp = gpar(fill = fill, col = border, lwd = 0.9, lty = if (dashed) 2 else 1)
    )
    grid.text(label, x = unit(x + 0.022, "npc"), y = unit(y, "npc"),
              just = "left", gp = gpar(col = col_ink, fontsize = 8))
  }

  swatch(0.065, 0.092, col_input_fill, col_input_border, "Input data / exposure source")
  swatch(0.065, 0.064, col_output_fill, col_output_border, "Manuscript output (Figures 1-4)")
  swatch(0.20, 0.092, col_core_fill, col_core_border, "Core computation (Appendix B)")
  swatch(0.20, 0.064, col_note_fill, col_note_border, "Shared scenario / counterfactuals", dashed = TRUE)

  grid.lines(
    x = unit(c(0.065, 0.086), "npc"),
    y = unit(c(0.042, 0.042), "npc"),
    gp = gpar(col = col_rule, lwd = 1.0),
    arrow = arrow(type = "closed", length = unit(1.8, "mm"))
  )
  grid.text("solid arrow: data or computation flow", x = unit(0.091, "npc"), y = unit(0.042, "npc"),
            just = "left", gp = gpar(col = col_ink, fontsize = 7.8))

  grid.lines(
    x = unit(c(0.215, 0.236), "npc"),
    y = unit(c(0.042, 0.042), "npc"),
    gp = gpar(col = col_muted, lwd = 0.9, lty = 2),
    arrow = arrow(type = "closed", length = unit(1.8, "mm"))
  )
  grid.text("dashed: scenario linkage", x = unit(0.241, "npc"), y = unit(0.042, "npc"),
            just = "left", gp = gpar(col = col_ink, fontsize = 7.8))
}

draw_figure_5 <- function() {
  grid.newpage()
  pushViewport(viewport())

  grid.rect(gp = gpar(fill = col_bg, col = NA))

  stage_x <- c(0.22, 0.36, 0.50, 0.64, 0.78, 0.92)
  draw_stage_guides(stage_x, c("Exposure", "Risk model", "Attribution", "Uncertainty", "Post-processing", "Results"))

  grid.text("HEALTH BRANCH", x = unit(0.11, "npc"), y = unit(0.90, "npc"),
            gp = gpar(col = col_health, fontsize = 14, fontface = "bold"))
  grid.text("ENVIRONMENTAL BRANCH", x = unit(0.14, "npc"), y = unit(0.19, "npc"),
            gp = gpar(col = col_env, fontsize = 14, fontface = "bold"))

  grid.lines(x = unit(c(0.18, 0.95), "npc"), y = unit(c(0.68, 0.68), "npc"),
             gp = gpar(col = col_health, lwd = 0.9, lty = 3, alpha = 0.45))
  grid.lines(x = unit(c(0.18, 0.95), "npc"), y = unit(c(0.42, 0.42), "npc"),
             gp = gpar(col = col_env, lwd = 0.9, lty = 3, alpha = 0.45))

  draw_box(0.08, 0.55, 0.11, 0.10, "ENNyS 2 dietary recalls",
           c("(2018-2019)", "g_kji * NOVA 1-4 * kcal_i", "survey weights w_k"),
           border = col_input_border, fill = col_input_fill, title_cex = 0.82)

  draw_box(0.22, 0.80, 0.10, 0.08, "DEIS mortality 2019",
           c("all-cause deaths D_sa", "by sex x age 30-69"),
           border = col_input_border, fill = col_input_fill)
  draw_box(0.36, 0.80, 0.10, 0.08, "Dose-response (B.2)",
           c("Pagliai 2021 RR = 1.25", "beta = ln(RR) / 35.7"),
           border = col_input_border, fill = col_input_fill)

  draw_box(0.22, 0.67, 0.10, 0.08, "1. %UPF exposure (B.1)",
           c("person mean pctUPF_k", "stratum mu_sa, sigma_sa"),
           border = col_core_border, fill = col_core_fill)
  draw_box(0.36, 0.67, 0.10, 0.08, "2. Distributional CRA (B.3)",
           c("log-normal [0,100]", "E[RR(X_sa)]"),
           border = col_core_border, fill = col_core_fill)
  draw_box(0.50, 0.67, 0.10, 0.08, "3. PAF and attributable",
           c("PAF_sa = Delta E[RR] / E[RR]", "A_sa = PAF_sa * D_sa"),
           border = col_core_border, fill = col_core_fill)
  draw_box(0.64, 0.67, 0.10, 0.08, "4. Monte Carlo (B.4)",
           c("10,000 iterations", "sample mu, beta, D ~ Poi", "95% UI"),
           border = col_core_border, fill = col_core_fill)
  draw_box(0.78, 0.67, 0.10, 0.08, "5. YLL / Delta e30 / PVLE",
           c("WHO L_a (B.5)", "cause-deleted life table (B.6)", "PVLE, r = 3% (B.7)"),
           border = col_core_border, fill = col_core_fill, body_cex = 0.66)
  draw_box(0.92, 0.67, 0.10, 0.08, "Health outputs",
           c("Deaths averted * YLL", "Delta e30 * indirect cost"),
           border = col_output_border, fill = col_output_fill)

  draw_box(0.22, 0.42, 0.10, 0.08, "1. Intake to mass (B.8)",
           c("q_ret = g / 1000", "q_ref = q_ret / phi_i"),
           border = col_core_border, fill = col_core_fill)
  draw_box(0.36, 0.42, 0.10, 0.08, "2. Item footprints",
           c("I_kji,m = q_ref * c_i,m", "GHG * Land * Water * Eutro"),
           border = col_core_border, fill = col_core_fill)
  draw_box(0.50, 0.42, 0.10, 0.08, "3. Population totals",
           c("I_m^base, I_m^UPF", "restricted to NOVA 4"),
           border = col_core_border, fill = col_core_fill)
  draw_box(0.64, 0.42, 0.10, 0.08, "4. Counterfactuals (B.8)",
           c("no-sub * isocaloric", "isoweight"),
           border = col_core_border, fill = col_core_fill)
  draw_box(0.78, 0.42, 0.10, 0.08, "5. Net scenario impacts",
           c("%Delta I_m = 100 *", "((I_cf / I_base) - 1)", "reported vs baseline"),
           border = col_core_border, fill = col_core_fill, body_cex = 0.60)
  draw_box(0.92, 0.42, 0.10, 0.08, "Environmental outputs",
           c("GHG * Land", "Water * Eutrophication"),
           border = col_output_border, fill = col_output_fill)

  draw_box(0.22, 0.29, 0.10, 0.07, "LCA coefficients",
           c("Arrieta 2022 + OWID", "c_i,m per kg"),
           border = col_input_border, fill = col_input_fill)
  draw_box(0.36, 0.29, 0.10, 0.07, "Yield ratios phi_i",
           c("retail -> farm-gate", "mapping coverage (S5)"),
           border = col_input_border, fill = col_input_fill)

  draw_box(0.50, 0.55, 0.13, 0.075, "Shared scenario engine",
           c("TMRL * 10/20/50% proportional",
             "NDG targets: 5.45 / 3.63 / 1.81%",
             "Baskets: NOVA 1 * NOVA 3 * blend * NDG"),
           border = col_note_border, fill = col_note_fill, dashed = TRUE, body_cex = 0.68)

  draw_box(0.92, 0.55, 0.11, 0.10, "Integrated output",
           c("Figure 4", "health-environment", "trade-offs"),
           border = col_output_border, fill = col_output_fill)

  draw_poly_arrow(c(0.135, 0.15, 0.17), c(0.59, 0.67, 0.67))
  draw_poly_arrow(c(0.135, 0.15, 0.17), c(0.51, 0.42, 0.42))

  draw_arrow(0.22, 0.76, 0.22, 0.71)
  draw_arrow(0.36, 0.76, 0.36, 0.71)
  draw_arrow(0.22, 0.33, 0.22, 0.38)
  draw_arrow(0.36, 0.33, 0.36, 0.38)

  draw_arrow(0.27, 0.67, 0.31, 0.67)
  draw_arrow(0.41, 0.67, 0.45, 0.67)
  draw_arrow(0.55, 0.67, 0.59, 0.67)
  draw_arrow(0.69, 0.67, 0.73, 0.67)
  draw_arrow(0.83, 0.67, 0.87, 0.67)

  draw_arrow(0.27, 0.42, 0.31, 0.42)
  draw_arrow(0.41, 0.42, 0.45, 0.42)
  draw_arrow(0.55, 0.42, 0.59, 0.42)
  draw_arrow(0.69, 0.42, 0.73, 0.42)
  draw_arrow(0.83, 0.42, 0.87, 0.42)

  draw_poly_arrow(c(0.50, 0.50, 0.36), c(0.585, 0.61, 0.635), col = col_muted, lwd = 0.9, dashed = TRUE)
  draw_poly_arrow(c(0.50, 0.50, 0.64), c(0.515, 0.49, 0.455), col = col_muted, lwd = 0.9, dashed = TRUE)

  draw_arrow(0.92, 0.63, 0.92, 0.59)
  draw_arrow(0.92, 0.46, 0.92, 0.51)

  draw_legend()
}

save_grid_pair(draw_figure_5, "Figure_5", width_mm = 330, height_mm = 170)
message("Saved Figure_5 to ", FIG_DIR)
