# 05_muertes_por_causa.R
# Muertes atribuibles por causa (CVD, cerebrovascular). PAF punto (RR-1)/RR.

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
out_dir <- file.path(root, "output")

library(readr)
library(dplyr)

RR_REF <- 0.357
RR_BY_CAUSE <- c(cvd = 1.29, cerebrovascular = 1.34)

rr_func <- function(x_pct, rr_point) {
  x <- pmax(pmin(x_pct / 100, 1), 0.001)
  exp(log(rr_point) / RR_REF * x)
}
paf_point <- function(mean_upf, rr_point) {
  rr <- rr_func(mean_upf, rr_point)
  paf <- (rr - 1) / rr
  paf[!is.finite(paf) | rr <= 0] <- 0
  pmax(0, pmin(1, paf))
}

upf <- read_csv(file.path(out_dir, "upf_por_estrato.csv"), show_col_types = FALSE)
def_causa <- read_csv(file.path(out_dir, "defunciones_2019_por_causa.csv"), show_col_types = FALSE)

res_list <- list()
for (causa_i in names(RR_BY_CAUSE)) {
  rr_i <- as.numeric(RR_BY_CAUSE[[causa_i]])
  df <- def_causa %>%
    filter(causa == causa_i) %>%
    left_join(select(upf, sexo_m, age_group, pct_upf_mean), by = c("sexo_m", "age_group")) %>%
    mutate(
      PAF = paf_point(pct_upf_mean, rr_i),
      deaths_attr = PAF * deaths,
      causa = causa_i
    )
  res_list[[causa_i]] <- select(df, sexo_m, age_group, causa, deaths, PAF, deaths_attr)
}
res <- bind_rows(res_list)

summ <- res %>%
  group_by(causa) %>%
  summarise(deaths = sum(deaths), deaths_attr = sum(deaths_attr), .groups = "drop") %>%
  mutate(pct_attr = 100 * deaths_attr / deaths)

write_csv(res, file.path(out_dir, "resultados_por_causa.csv"))
write_csv(summ, file.path(out_dir, "resumen_muertes_por_causa.csv"))
message("Guardado: resultados_por_causa.csv, resumen_muertes_por_causa.csv")
