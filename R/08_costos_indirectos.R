# 08_costos_indirectos.R
# Costos indirectos (PVLE). Capital humano, EPH 2019, edad retiro 65, descuento 3%.

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
data_dir <- file.path(root, "data")
out_dir <- file.path(root, "output")
PPP_2019 <- 20.38
RETIREMENT_AGE <- 65
R <- 0.03

age_mid <- c("30-34" = 32, "35-39" = 37, "40-44" = 42, "45-49" = 47, "50-54" = 52, "55-59" = 57, "60-64" = 62, "65-69" = 67)

pv_earnings <- function(annual_earnings, employment_rate, years_remaining) {
  if (years_remaining <= 0) return(0)
  eff <- annual_earnings * employment_rate
  if (R <= 0) return(eff * years_remaining)
  eff * (1 - (1 + R)^(-years_remaining)) / R
}

library(readr)
library(dplyr)

earnings_path <- file.path(data_dir, "EPH", "ingresos_por_estrato.csv")
if (!file.exists(earnings_path)) stop("No existe data/EPH/ingresos_por_estrato.csv. Ejecutar calcular_costos_indirectos.py o extraer EPH.")

earnings <- read_csv(earnings_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
res <- read_csv(file.path(out_dir, "resultados_paper_argentina.csv"), show_col_types = FALSE)
res <- res %>%
  left_join(earnings, by = c("sexo_m", "age_group")) %>%
  mutate(
    ingreso_promedio = coalesce(ingreso_promedio, 0),
    tasa_ocupacion = coalesce(tasa_ocupacion, 0)
  )
res$anos_productivos <- pmax(0, RETIREMENT_AGE - unname(age_mid[res$age_group]))
res$PV_por_muerte <- mapply(pv_earnings, res$ingreso_promedio, res$tasa_ocupacion, res$anos_productivos)
res$costo_indirecto <- res$deaths_attr * res$PV_por_muerte
costo_total <- sum(res$costo_indirecto)

write_csv(res, file.path(out_dir, "costos_indirectos_por_estrato.csv"))
write_csv(data.frame(
  costo_total_PVLE = costo_total,
  costo_PPP_millones_intl_USD = costo_total / 1e6 / PPP_2019,
  deaths_attr_total = sum(res$deaths_attr),
  costo_por_muerte = costo_total / sum(res$deaths_attr)
), file.path(out_dir, "resumen_costos_indirectos.csv"))
message("PVLE total: ", round(costo_total / 1e9, 2), " mil millones ARS (~", round(costo_total / PPP_2019 / 1e9, 2), " mil millones USD PPP)")
message("Guardado: costos_indirectos_por_estrato.csv, resumen_costos_indirectos.csv")
