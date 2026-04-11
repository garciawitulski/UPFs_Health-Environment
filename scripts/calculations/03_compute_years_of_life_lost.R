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

source(file.path(bootstrap_script_dir(), "..", "00_config.R"), local = TRUE)

WHO_YLL <- c(
  "30-34" = 60.34, "35-39" = 55.38, "40-44" = 50.43, "45-49" = 45.51,
  "50-54" = 40.61, "55-59" = 35.74, "60-64" = 30.92, "65-69" = 26.21
)

res <- read_calc_output_csv("resultados_paper_argentina.csv")
res$yll_per_death <- unname(WHO_YLL[res$age_group])
res$YLL <- res$deaths_attr * res$yll_per_death
write_calc_csv(res[c("sexo_m", "age_group", "deaths_attr", "YLL", "yll_per_death")], "yll_allcause_por_estrato.csv")
summary_allcause <- data.frame(
  deaths_attr = sum(res$deaths_attr, na.rm = TRUE),
  YLL = sum(res$YLL, na.rm = TRUE)
)
write_calc_csv(summary_allcause, "yll_allcause_summary.csv")

message("Saved calculation outputs: yll_allcause_por_estrato.csv, yll_allcause_summary.csv")
