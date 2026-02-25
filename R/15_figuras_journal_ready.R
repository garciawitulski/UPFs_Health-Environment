# 15_figuras_journal_ready.R
# Build journal-ready versions of 2 results figures.
# Outputs: PNG (300 dpi), TIFF (LZW, 300 dpi), PDF (vector).

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
out_dir <- file.path(root, "output", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fig_w_in <- 7.2
fig_h_in <- 5.0
dpi <- 300

parse_ci <- function(x) {
  x <- trimws(as.character(x))
  out <- matrix(NA_real_, nrow = length(x), ncol = 3)
  pattern <- "^([0-9]+\\.?[0-9]*)\\s*\\(([0-9]+\\.?[0-9]*),\\s*([0-9]+\\.?[0-9]*)\\)$"
  m <- regexec(pattern, x)
  mm <- regmatches(x, m)
  for (i in seq_along(mm)) {
    if (length(mm[[i]]) == 4) out[i, ] <- as.numeric(mm[[i]][2:4])
  }
  colnames(out) <- c("mean", "lo", "hi")
  out
}

draw_errorbars <- function(x, lo, hi, width = 0.08, col = "black", lwd = 1) {
  segments(x, lo, x, hi, col = col, lwd = lwd)
  segments(x - width, lo, x + width, lo, col = col, lwd = lwd)
  segments(x - width, hi, x + width, hi, col = col, lwd = lwd)
}

open_device <- function(path_base, kind = c("png", "tiff", "pdf"), width_in = fig_w_in, height_in = fig_h_in) {
  kind <- match.arg(kind)
  if (kind == "png") {
    png(
      filename = paste0(path_base, ".png"),
      width = width_in * dpi,
      height = height_in * dpi,
      res = dpi
    )
  } else if (kind == "tiff") {
    tiff(
      filename = paste0(path_base, ".tiff"),
      width = width_in * dpi,
      height = height_in * dpi,
      res = dpi,
      compression = "lzw"
    )
  } else {
    pdf(
      file = paste0(path_base, ".pdf"),
      width = width_in,
      height = height_in,
      useDingbats = FALSE
    )
  }
}

fig1_plot <- function() {
  tab1_path <- file.path(root, "output", "Tabla1_UPF_por_energia_Argentina.csv")
  tab1 <- read.csv(tab1_path, check.names = FALSE, stringsAsFactors = FALSE)
  tab1 <- tab1[tab1[["Age groups, years"]] != "Total", , drop = FALSE]

  ages <- tab1[["Age groups, years"]]
  men_vals <- parse_ci(tab1[["Men, % (95% CI)"]])
  women_vals <- parse_ci(tab1[["Women, % (95% CI)"]])

  x <- seq_along(ages)
  x_m <- x - 0.07
  x_w <- x + 0.07

  y_min <- floor(min(c(men_vals[, "lo"], women_vals[, "lo"]), na.rm = TRUE) - 0.5)
  y_max <- ceiling(max(c(men_vals[, "hi"], women_vals[, "hi"]), na.rm = TRUE) + 0.5)

  par(
    mar = c(4.8, 4.8, 1.3, 0.8),
    mgp = c(2.7, 0.75, 0),
    tcl = -0.25,
    bty = "l",
    family = "sans"
  )

  plot(
    x, men_vals[, "mean"],
    type = "n",
    xaxt = "n",
    xlab = "Age group (years)",
    ylab = "UPF share of total energy (%)",
    ylim = c(y_min, y_max),
    yaxs = "i"
  )
  axis(1, at = x, labels = ages)
  axis(2, at = pretty(c(y_min, y_max), n = 6))
  abline(h = pretty(c(y_min, y_max), n = 6), col = "#E6E6E6", lty = "dotted")

  draw_errorbars(x_m, men_vals[, "lo"], men_vals[, "hi"], col = "#1F77B4", lwd = 1.1)
  draw_errorbars(x_w, women_vals[, "lo"], women_vals[, "hi"], col = "#D62728", lwd = 1.1)
  lines(x_m, men_vals[, "mean"], type = "b", pch = 16, col = "#1F77B4", lwd = 1.8, cex = 0.9)
  lines(x_w, women_vals[, "mean"], type = "b", pch = 17, col = "#D62728", lwd = 1.8, cex = 0.95)

  legend(
    "topright",
    legend = c("Men", "Women"),
    col = c("#1F77B4", "#D62728"),
    pch = c(16, 17),
    lty = 1,
    lwd = 1.8,
    pt.cex = c(0.9, 0.95),
    bty = "n",
    inset = c(0.01, 0.01),
    cex = 0.95
  )
}

fig2_plot <- function() {
  sc_path <- file.path(root, "output", "health_counterfactual_scenarios.csv")
  sc <- read.csv(sc_path, stringsAsFactors = FALSE)

  label_map <- c(
    red_10 = "10% reduction",
    red_20 = "20% reduction",
    red_50 = "50% reduction",
    ndg_gapa_conservative = "NDG-GAPA conservative",
    ndg_gapa_central = "NDG-GAPA central",
    ndg_gapa_strict = "NDG-GAPA strict"
  )

  sc <- sc[sc$scenario %in% names(label_map), , drop = FALSE]
  sc$label <- unname(label_map[sc$scenario])
  sc <- sc[order(sc$deaths_averted), , drop = FALSE]

  y <- seq_len(nrow(sc))
  x_min <- 0
  x_max <- max(sc$deaths_averted_hi, na.rm = TRUE) * 1.12

  par(
    mar = c(4.8, 8.4, 1.3, 1.1),
    mgp = c(2.7, 0.75, 0),
    tcl = -0.25,
    bty = "l",
    family = "sans"
  )

  plot(
    sc$deaths_averted, y,
    type = "n",
    xaxt = "n",
    yaxt = "n",
    xlab = "Deaths averted (median and 95% UI)",
    ylab = "",
    xlim = c(x_min, x_max),
    xaxs = "i"
  )
  axis(2, at = y, labels = sc$label, las = 1)
  xt <- pretty(c(x_min, x_max), n = 7)
  axis(1, at = xt, labels = format(xt, big.mark = ",", scientific = FALSE))
  abline(v = xt, col = "#E6E6E6", lty = "dotted")

  segments(sc$deaths_averted_lo, y, sc$deaths_averted_hi, y, lwd = 2.0, col = "#6B7280")
  points(sc$deaths_averted, y, pch = 19, cex = 1.05, col = "#111827")

  text(
    x = pmin(sc$deaths_averted_hi + 0.025 * (x_max - x_min), x_max * 0.985),
    y = y,
    labels = format(round(sc$deaths_averted, 0), big.mark = ","),
    cex = 0.86,
    pos = 4
  )
}

fig3_plot <- function() {
  path <- file.path(root, "output", "summary_health_environment_upf_scenarios.csv")
  d <- read.csv(path, stringsAsFactors = FALSE)

  keep_scen <- c("red_10", "red_20", "red_50", "ndg_gapa_conservative", "ndg_gapa_central", "ndg_gapa_strict")
  d <- d[d$scenario %in% keep_scen, , drop = FALSE]

  d_no <- d[d$scenario_model == "no_replacement" & d$replacement_target == "none", , drop = FALSE]
  d_iso <- d[d$scenario_model == "isocaloric_replacement" & d$replacement_target == "GAPA_ARG", , drop = FALSE]

  d_no <- d_no[order(d_no$deaths_averted), , drop = FALSE]
  d_iso <- d_iso[order(d_iso$deaths_averted), , drop = FALSE]

  ind_cols <- c(
    ghg_kg_co2eq_avoided_pct_of_baseline = "GHG",
    land_m2_avoided_pct_of_baseline = "Land",
    water_l_avoided_pct_of_baseline = "Water",
    eutro_g_po4eq_avoided_pct_of_baseline = "Eutro"
  )
  ind_cols_names <- names(ind_cols)
  ind_labels <- unname(ind_cols)
  ind_colors <- c("#1F77B4", "#2CA02C", "#D62728", "#9467BD")
  ind_pch <- c(16, 15, 17, 18)

  yr <- range(
    c(
      unlist(d_no[ind_cols_names]),
      unlist(d_iso[ind_cols_names])
    ),
    na.rm = TRUE
  )
  ypad <- 0.07 * diff(yr)
  ylim <- c(yr[1] - ypad, yr[2] + ypad)

  x_max <- max(c(d_no$deaths_averted_hi, d_iso$deaths_averted_hi), na.rm = TRUE) * 1.04
  xt <- pretty(c(0, x_max), n = 6)
  yt <- pretty(ylim, n = 7)

  panel_draw <- function(df, panel_title, show_legend = FALSE) {
    par(
      mar = c(4.8, 4.6, 2.1, if (show_legend) 5.8 else 0.8),
      mgp = c(2.7, 0.75, 0),
      tcl = -0.25,
      bty = "l",
      family = "sans"
    )
    plot(
      df$deaths_averted, df[[ind_cols_names[1]]],
      type = "n",
      xlim = c(0, x_max),
      ylim = ylim,
      xaxs = "i",
      yaxs = "i",
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      main = panel_title
    )
    axis(1, at = xt, labels = format(xt, big.mark = ",", scientific = FALSE))
    axis(2, at = yt)
    abline(v = xt, col = "#EEEEEE", lty = "dotted")
    abline(h = yt, col = "#F0F0F0", lty = "dotted")
    abline(h = 0, col = "#4B5563", lwd = 1.2, lty = 2)

    for (k in seq_along(ind_cols_names)) {
      col_k <- ind_cols_names[k]
      x <- df$deaths_averted
      y <- df[[col_k]]
      lines(x, y, col = ind_colors[k], lwd = 1.8)
      points(x, y, col = ind_colors[k], pch = ind_pch[k], cex = 0.95)
    }

    if (show_legend) {
      legend(
        "topright",
        inset = c(-0.36, 0),
        xpd = NA,
        legend = ind_labels,
        col = ind_colors,
        pch = ind_pch,
        lty = 1,
        lwd = 1.8,
        bty = "n",
        cex = 0.9
      )
    }
  }

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(oma = c(3.2, 4.2, 0.2, 0.2))
  layout(matrix(c(1, 2), nrow = 1), widths = c(1, 1))
  panel_draw(d_no, "No substitution (upper bound)", show_legend = FALSE)
  panel_draw(d_iso, "Isocaloric replacement (GAPA)", show_legend = TRUE)
  mtext("Deaths averted (median)", side = 1, line = 2.8, outer = TRUE, cex = 1.0)
  mtext("Environmental change (% vs baseline)", side = 2, line = 2.6, outer = TRUE, cex = 1.0)
}

render_all_formats <- function(path_base, draw_fun, width_in = fig_w_in, height_in = fig_h_in) {
  for (k in c("png", "tiff", "pdf")) {
    open_device(path_base, k, width_in = width_in, height_in = height_in)
    draw_fun()
    dev.off()
  }
}

render_all_formats(file.path(out_dir, "fig1_upf_exposure_age_sex_journal"), fig1_plot)
render_all_formats(file.path(out_dir, "fig2_deaths_averted_scenarios_journal"), fig2_plot)
render_all_formats(
  file.path(out_dir, "fig3_tradeoff_health_environment_journal"),
  fig3_plot,
  width_in = 10.2,
  height_in = 4.8
)

cat("Journal-ready figures written to:", out_dir, "\n")
cat("- fig1_upf_exposure_age_sex_journal.(png|tiff|pdf)\n")
cat("- fig2_deaths_averted_scenarios_journal.(png|tiff|pdf)\n")
cat("- fig3_tradeoff_health_environment_journal.(png|tiff|pdf)\n")
