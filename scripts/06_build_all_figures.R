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

script_dir <- dirname(get_script_path())
build_scripts <- c(
  "01_build_figure_1.R",
  "02_build_figure_2.R",
  "03_build_figure_3.R",
  "04_build_figure_4.R",
  "05_build_figure_5.R"
)

for (script_name in build_scripts) {
  message("==> ", script_name)
  source(file.path(script_dir, script_name), local = new.env(parent = globalenv()))
}

message("Done. Generated figures are in:")
message("  - ", FIG_DIR)
message("  - ", SUBMISSION_ROOT)
message("  - ", SUBMISSION_FIG_DIR)
