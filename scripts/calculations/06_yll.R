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

res_causa <- read_calc_output_csv("resultados_por_causa.csv")
res_causa$yll_per_death <- unname(WHO_YLL[res_causa$age_group])
res_causa$YLL <- res_causa$deaths_attr * res_causa$yll_per_death

resumen <- res_causa %>%
  group_by(causa) %>%
  summarise(deaths = sum(deaths), deaths_attr = sum(deaths_attr), YLL = sum(YLL), .groups = "drop")

resumen_out <- data.frame(
  causa = c(resumen$causa, "Total"),
  deaths_attr = c(resumen$deaths_attr, sum(resumen$deaths_attr)),
  YLL = c(resumen$YLL, sum(resumen$YLL))
)

write_calc_csv(resumen, "yll_por_causa.csv")
write_calc_csv(resumen_out, "resumen_yll_por_causa.csv")

message("Saved calculation outputs: yll_allcause_por_estrato.csv, yll_por_causa.csv, resumen_yll_por_causa.csv")
