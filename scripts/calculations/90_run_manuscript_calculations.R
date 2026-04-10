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
ordered_scripts <- c(
  "01_estimate_all_cause_paf.R",
  "02_estimate_cause_specific_deaths.R",
  "03_compute_years_of_life_lost.R",
  "04_estimate_life_expectancy_gains.R",
  "05_estimate_indirect_costs.R",
  "06_build_health_economic_scenario_summary.R",
  "07_build_environmental_scenarios.R",
  "08_merge_health_environment_summaries.R",
  "09_build_table_s7_scenario_composition.R",
  "10_build_table_s8_environmental_coefficients.R"
)

for (script_name in ordered_scripts) {
  message("==> ", script_name)
  source(file.path(script_dir, script_name), local = new.env(parent = globalenv()))
}

message("Core manuscript calculation outputs rebuilt.")
