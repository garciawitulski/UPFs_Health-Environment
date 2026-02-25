# 06_yll.R
# Años de vida perdidos (YLL). Tabla estándar WHO GHE 2021.

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
out_dir <- file.path(root, "output")

library(readr)
library(dplyr)

WHO_YLL <- c(
  "30-34" = 60.34, "35-39" = 55.38, "40-44" = 50.43, "45-49" = 45.51,
  "50-54" = 40.61, "55-59" = 35.74, "60-64" = 30.92, "65-69" = 26.21
)

res <- read_csv(file.path(out_dir, "resultados_paper_argentina.csv"), show_col_types = FALSE)
res$yll_per_death <- unname(WHO_YLL[res$age_group])
res$YLL <- res$deaths_attr * res$yll_per_death

write_csv(select(res, sexo_m, age_group, deaths_attr, YLL, yll_per_death),
  file.path(out_dir, "yll_allcause_por_estrato.csv"))
message("Total YLL (all-cause): ", round(sum(res$YLL)))

res_causa <- read_csv(file.path(out_dir, "resultados_por_causa.csv"), show_col_types = FALSE)
res_causa$yll_per_death <- unname(WHO_YLL[res_causa$age_group])
res_causa$YLL <- res_causa$deaths_attr * res_causa$yll_per_death

yll_por_causa <- res_causa %>% group_by(causa) %>% summarise(YLL = sum(YLL), .groups = "drop")
resumen <- res_causa %>%
  group_by(causa) %>%
  summarise(deaths = sum(deaths), deaths_attr = sum(deaths_attr), YLL = sum(YLL), .groups = "drop")
write_csv(resumen, file.path(out_dir, "yll_por_causa.csv"))

resumen_out <- data.frame(
  causa = c(yll_por_causa$causa, "Total"),
  deaths_attr = c(res_causa %>% group_by(causa) %>% summarise(s = sum(deaths_attr), .groups = "drop") %>% pull(s), sum(res_causa$deaths_attr)),
  YLL = c(yll_por_causa$YLL, sum(yll_por_causa$YLL))
)
write_csv(resumen_out, file.path(out_dir, "resumen_yll_por_causa.csv"))
message("Guardado: yll_allcause_por_estrato.csv, yll_por_causa.csv, resumen_yll_por_causa.csv")
