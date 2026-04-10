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
  "04_paf_resultados.R",
  "05_muertes_por_causa.R",
  "06_yll.R",
  "07_esperanza_vida.R",
  "08_costos_indirectos.R",
  "09_scenario_health_economic_summary.R",
  "10_impacto_ambiental.R",
  "13_merge_health_env_summaries.R",
  "16_appendix_scenario_composition_table.R",
  "17_appendix_environmental_coefficients_table.R"
)

for (script_name in ordered_scripts) {
  message("==> ", script_name)
  source(file.path(script_dir, script_name), local = new.env(parent = globalenv()))
}

message("Core manuscript calculation outputs rebuilt.")
