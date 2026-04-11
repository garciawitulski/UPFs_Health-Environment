bootstrap_script_dir <- function() {
  for (i in rev(seq_along(sys.frames()))) {
    if (!is.null(sys.frames()[[i]]$ofile)) {
      return(dirname(normalizePath(sys.frames()[[i]]$ofile, winslash = "/", mustWork = FALSE)))
    }
  }
  args <- commandArgs(trailingOnly = FALSE)
  hit <- args[grepl("^--file=", args)]
  if (length(hit)) return(dirname(normalizePath(sub("^--file=", "", hit[1]), winslash = "/", mustWork = FALSE)))
  getwd()
}

script_dir <- bootstrap_script_dir()
source(file.path(script_dir, "90_run_manuscript_calculations.R"), local = new.env(parent = globalenv()))
source(file.path(script_dir, "91_build_main_figures.R"), local = new.env(parent = globalenv()))
message("Processed-data manuscript replication pipeline completed.")
