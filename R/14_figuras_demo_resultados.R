# 14_figuras_demo_resultados.R
# Genera 2 figuras de ejemplo para la seccion de resultados:
# 1) % energia UPF por edad y sexo (con IC95%)
# 2) Muertes evitables por escenario (mediana + UI95%)

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
out_dir <- file.path(root, "output", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

parse_ci <- function(x) {
  x <- trimws(as.character(x))
  out <- matrix(NA_real_, nrow = length(x), ncol = 3)
  # Formato esperado: "19.5 (17.6, 21.4)"
  pattern <- "^([0-9]+\\.?[0-9]*)\\s*\\(([0-9]+\\.?[0-9]*),\\s*([0-9]+\\.?[0-9]*)\\)$"
  m <- regexec(pattern, x)
  mm <- regmatches(x, m)
  for (i in seq_along(mm)) {
    if (length(mm[[i]]) == 4) {
      out[i, ] <- as.numeric(mm[[i]][2:4])
    }
  }
  colnames(out) <- c("mean", "lo", "hi")
  out
}

draw_errorbars <- function(x, lo, hi, width = 0.1, col = "black", lwd = 1) {
  segments(x, lo, x, hi, col = col, lwd = lwd)
  segments(x - width, lo, x + width, lo, col = col, lwd = lwd)
  segments(x - width, hi, x + width, hi, col = col, lwd = lwd)
}

# -----------------------------------------------------------------------------
# Figura 1: %UPF por edad y sexo
# -----------------------------------------------------------------------------

tab1_path <- file.path(root, "output", "Tabla1_UPF_por_energia_Argentina.csv")
tab1 <- read.csv(tab1_path, check.names = FALSE, stringsAsFactors = FALSE)
tab1 <- tab1[tab1[["Age groups, years"]] != "Total", , drop = FALSE]

ages <- tab1[["Age groups, years"]]
men_vals <- parse_ci(tab1[["Men, % (95% CI)"]])
women_vals <- parse_ci(tab1[["Women, % (95% CI)"]])

x <- seq_along(ages)
y_min <- min(c(men_vals[, "lo"], women_vals[, "lo"]), na.rm = TRUE) - 1
y_max <- max(c(men_vals[, "hi"], women_vals[, "hi"]), na.rm = TRUE) + 1

png(
  filename = file.path(out_dir, "fig1_upf_exposure_age_sex.png"),
  width = 1800, height = 1200, res = 200
)
par(mar = c(5, 5, 3, 2))
plot(
  x, men_vals[, "mean"],
  type = "n",
  xaxt = "n",
  xlab = "Age group (years)",
  ylab = "UPF share of total energy (%)",
  ylim = c(y_min, y_max),
  main = "UPF contribution by age group and sex"
)
axis(1, at = x, labels = ages)

# Pequeño offset horizontal para que no se superpongan
x_m <- x - 0.08
x_w <- x + 0.08

draw_errorbars(x_m, men_vals[, "lo"], men_vals[, "hi"], col = "#1f77b4", lwd = 1.3)
draw_errorbars(x_w, women_vals[, "lo"], women_vals[, "hi"], col = "#d62728", lwd = 1.3)
lines(x_m, men_vals[, "mean"], type = "b", pch = 16, col = "#1f77b4", lwd = 2)
lines(x_w, women_vals[, "mean"], type = "b", pch = 17, col = "#d62728", lwd = 2)
legend(
  "topright",
  legend = c("Men", "Women"),
  col = c("#1f77b4", "#d62728"),
  pch = c(16, 17),
  lty = 1,
  lwd = 2,
  bty = "n"
)
grid(nx = NA, ny = NULL, col = "#DDDDDD", lty = "dotted")
dev.off()

# -----------------------------------------------------------------------------
# Figura 2: Muertes evitables por escenario (mediana + UI95%)
# -----------------------------------------------------------------------------

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
x_min <- min(sc$deaths_averted_lo, na.rm = TRUE) * 0.92
x_max <- max(sc$deaths_averted_hi, na.rm = TRUE) * 1.08

png(
  filename = file.path(out_dir, "fig2_deaths_averted_scenarios.png"),
  width = 1900, height = 1200, res = 200
)
par(mar = c(5, 10, 3, 3))
plot(
  sc$deaths_averted, y,
  type = "n",
  yaxt = "n",
  xlab = "Deaths averted (median and 95% UI)",
  ylab = "",
  xlim = c(x_min, x_max),
  main = "Avertable premature deaths under UPF reduction scenarios"
)
axis(2, at = y, labels = sc$label, las = 1)
grid(nx = NULL, ny = NA, col = "#DDDDDD", lty = "dotted")

segments(sc$deaths_averted_lo, y, sc$deaths_averted_hi, y, lwd = 2, col = "#6B7280")
points(sc$deaths_averted, y, pch = 19, cex = 1.2, col = "#111827")

text(
  x = sc$deaths_averted_hi + 0.015 * (x_max - x_min),
  y = y,
  labels = format(round(sc$deaths_averted, 0), big.mark = ","),
  cex = 0.9,
  pos = 4
)
dev.off()

cat("Figuras generadas en:", out_dir, "\n")
cat("- fig1_upf_exposure_age_sex.png\n")
cat("- fig2_deaths_averted_scenarios.png\n")
