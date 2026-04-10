suppressPackageStartupMessages({
  library(readr)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(forcats)
  library(patchwork)
})

get_script_path <- function() {
  for (i in rev(seq_along(sys.frames()))) {
    this_file <- sys.frames()[[i]]$ofile
    if (!is.null(this_file)) {
      return(normalizePath(this_file, winslash = "/", mustWork = FALSE))
    }
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_flag <- "--file="
  file_arg <- args[grepl(file_flag, args)]
  if (length(file_arg)) {
    return(normalizePath(sub(file_flag, "", file_arg[1]), winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(getwd(), "unknown"), winslash = "/", mustWork = FALSE)
}

SCRIPT_PATH <- get_script_path()
SCRIPT_DIR <- dirname(SCRIPT_PATH)
PKG_ROOT <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = TRUE)

IS_NESTED_SUBMISSION <- basename(PKG_ROOT) == "github_figures_only" &&
  basename(dirname(PKG_ROOT)) == "submission_nature_food"

if (IS_NESTED_SUBMISSION) {
  SUBMISSION_ROOT <- normalizePath(file.path(PKG_ROOT, ".."), winslash = "/", mustWork = TRUE)
  REPO_ROOT <- normalizePath(file.path(SUBMISSION_ROOT, ".."), winslash = "/", mustWork = TRUE)
} else {
  SUBMISSION_ROOT <- PKG_ROOT
  REPO_ROOT <- PKG_ROOT
}

DATA_DIR <- file.path(PKG_ROOT, "data")
CALC_DATA_DIR <- file.path(DATA_DIR, "analysis")
GEN_DIR <- file.path(PKG_ROOT, "generated")
FIG_DIR <- file.path(GEN_DIR, "figures")
REPORT_DIR <- file.path(GEN_DIR, "reports")
CALC_OUTPUT_DIR <- file.path(GEN_DIR, "calculation_outputs")
SUBMISSION_FIG_DIR <- if (IS_NESTED_SUBMISSION) file.path(SUBMISSION_ROOT, "figures") else file.path(GEN_DIR, "figures")

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(REPORT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CALC_OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(SUBMISSION_FIG_DIR, showWarnings = FALSE, recursive = TRUE)

col_ink   <- "#111827"
col_muted <- "#6B7280"
col_grid  <- "#E5E7EB"
col_rule  <- "#1F2937"
col_men   <- "#2A6A86"
col_women <- "#86C7D4"
col_primary <- "#1F4E79"
col_teal <- "#138D75"
col_amber <- "#CA8A04"
col_accent <- "#C0392B"
col_purple <- "#6B46C1"

theme_nf <- function(base = 9.3) {
  theme_minimal(base_size = base) +
    theme(
      text             = element_text(colour = col_ink),
      plot.title       = element_text(face = "bold", hjust = 0, size = rel(1.05)),
      plot.subtitle    = element_text(colour = col_muted, hjust = 0, size = rel(0.86)),
      panel.grid.major = element_line(colour = col_grid, linewidth = 0.25),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(colour = col_ink, size = rel(0.82)),
      strip.text       = element_text(face = "bold", size = rel(0.9)),
      legend.title     = element_text(face = "bold", size = rel(0.9)),
      legend.text      = element_text(size = rel(0.8)),
      plot.margin      = margin(8, 10, 8, 10)
    )
}

read_pkg_csv <- function(name) {
  read.csv(file.path(DATA_DIR, name), stringsAsFactors = FALSE, check.names = FALSE)
}

read_calc_csv <- function(name) {
  read.csv(file.path(CALC_DATA_DIR, name), stringsAsFactors = FALSE, check.names = FALSE)
}

write_calc_csv <- function(df, name) {
  utils::write.csv(df, file.path(CALC_OUTPUT_DIR, name), row.names = FALSE, na = "")
}

read_calc_output_csv <- function(name) {
  read.csv(file.path(CALC_OUTPUT_DIR, name), stringsAsFactors = FALSE, check.names = FALSE)
}

parse_ci <- function(x) {
  m <- regmatches(x, regexec("([0-9.]+)\\s*\\(([0-9.]+),\\s*([0-9.]+)\\)", x))
  out <- do.call(rbind, lapply(m, function(v) {
    if (length(v) < 4) c(NA, NA, NA) else as.numeric(v[2:4])
  }))
  colnames(out) <- c("est", "lo", "hi")
  as.data.frame(out)
}

save_pair <- function(plot_obj, base_name, width_mm = NULL, height_mm = NULL, w_mm = NULL, h_mm = NULL, sync_submission = IS_NESTED_SUBMISSION) {
  if (is.null(width_mm)) width_mm <- w_mm
  if (is.null(height_mm)) height_mm <- h_mm
  if (is.null(width_mm) || is.null(height_mm)) {
    stop("Both width_mm and height_mm are required.")
  }
  for (ext in c(".pdf", ".png")) {
    target <- file.path(FIG_DIR, paste0(base_name, ext))
    if (ext == ".pdf") {
      ggsave(target, plot_obj, width = width_mm, height = height_mm, units = "mm",
             dpi = 400, device = cairo_pdf, bg = "white")
    } else {
      ggsave(target, plot_obj, width = width_mm, height = height_mm, units = "mm",
             dpi = 400, bg = "white")
    }
    if (sync_submission) {
      file.copy(target, file.path(SUBMISSION_ROOT, paste0(base_name, ext)), overwrite = TRUE)
      file.copy(target, file.path(SUBMISSION_FIG_DIR, paste0(base_name, ext)), overwrite = TRUE)
    }
  }
}

save_grid_pair <- function(draw_fun, base_name, width_mm, height_mm, sync_submission = IS_NESTED_SUBMISSION) {
  pdf_path <- file.path(FIG_DIR, paste0(base_name, ".pdf"))
  png_path <- file.path(FIG_DIR, paste0(base_name, ".png"))

  grDevices::cairo_pdf(
    filename = pdf_path,
    width = width_mm / 25.4,
    height = height_mm / 25.4,
    onefile = TRUE,
    family = "sans",
    bg = "white"
  )
  draw_fun()
  grDevices::dev.off()

  grDevices::png(
    filename = png_path,
    width = width_mm / 25.4,
    height = height_mm / 25.4,
    units = "in",
    res = 400,
    type = "cairo",
    bg = "white"
  )
  draw_fun()
  grDevices::dev.off()

  if (sync_submission) {
    file.copy(pdf_path, file.path(SUBMISSION_ROOT, paste0(base_name, ".pdf")), overwrite = TRUE)
    file.copy(pdf_path, file.path(SUBMISSION_FIG_DIR, paste0(base_name, ".pdf")), overwrite = TRUE)
    file.copy(png_path, file.path(SUBMISSION_ROOT, paste0(base_name, ".png")), overwrite = TRUE)
    file.copy(png_path, file.path(SUBMISSION_FIG_DIR, paste0(base_name, ".png")), overwrite = TRUE)
  }
  invisible(NULL)
}

sync_binary <- function(src, dest_name = basename(src), sync_submission = IS_NESTED_SUBMISSION) {
  out <- file.path(FIG_DIR, dest_name)
  file.copy(src, out, overwrite = TRUE)
  if (sync_submission) {
    file.copy(src, file.path(SUBMISSION_ROOT, dest_name), overwrite = TRUE)
    file.copy(src, file.path(SUBMISSION_FIG_DIR, dest_name), overwrite = TRUE)
  }
  invisible(out)
}

convert_pdf_to_png <- function(pdf_path, png_path) {
  exe <- Sys.which("pdftoppm")
  if (!nzchar(exe)) return(FALSE)
  prefix <- tempfile(tmpdir = dirname(png_path), pattern = "pdf_to_png_")
  args <- c("-png", "-singlefile", normalizePath(pdf_path, winslash = "/", mustWork = TRUE), prefix)
  suppressWarnings(system2(exe, args = args, stdout = TRUE, stderr = TRUE))
  generated <- paste0(prefix, ".png")
  if (!file.exists(generated)) return(FALSE)
  ok <- file.copy(generated, png_path, overwrite = TRUE)
  unlink(generated)
  ok
}

canonical_scenario <- function(x) {
  x <- trimws(as.character(x))
  out <- x
  out[x %in% c("ndg_gapa_conservative", "ndg_NDG_conservative")] <- "ndg_conservative"
  out[x %in% c("ndg_gapa_central", "ndg_NDG_central")] <- "ndg_central"
  out[x %in% c("ndg_gapa_strict", "ndg_NDG_strict")] <- "ndg_strict"
  out
}

scenario_label <- function(x) {
  labels <- c(
    red_10 = "10% reduction",
    red_20 = "20% reduction",
    red_50 = "50% reduction",
    ndg_conservative = "NDG conservative",
    ndg_central = "NDG central",
    ndg_strict = "NDG strict"
  )
  unname(labels[x])
}
