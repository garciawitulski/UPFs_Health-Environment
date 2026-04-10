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

earnings <- read_calc_csv("earnings_by_stratum.csv")
res <- read_calc_output_csv("resultados_paper_argentina.csv")

res <- res %>%
  left_join(earnings, by = c("sexo_m", "age_group")) %>%
  mutate(
    ingreso_promedio = coalesce(ingreso_promedio, 0),
    tasa_ocupacion = coalesce(tasa_ocupacion, 0),
    anos_productivos = pmax(0, RETIREMENT_AGE - unname(age_mid[age_group]))
  )

res$PV_por_muerte <- mapply(pv_earnings, res$ingreso_promedio, res$tasa_ocupacion, res$anos_productivos)
res$costo_indirecto <- res$deaths_attr * res$PV_por_muerte

resumen_por_sexo <- res %>%
  mutate(sexo = case_when(sexo_m == 1 ~ "Men", sexo_m == 0 ~ "Women", TRUE ~ as.character(sexo_m))) %>%
  group_by(sexo) %>%
  summarise(
    costo_total_PVLE = sum(costo_indirecto, na.rm = TRUE),
    costo_PPP_millones_intl_USD = costo_total_PVLE / 1e6 / PPP_2019,
    deaths_attr_total = sum(deaths_attr, na.rm = TRUE),
    costo_por_muerte = ifelse(deaths_attr_total > 0, costo_total_PVLE / deaths_attr_total, NA_real_),
    .groups = "drop"
  )

costo_total <- sum(res$costo_indirecto, na.rm = TRUE)

write_calc_csv(res, "costos_indirectos_por_estrato.csv")
write_calc_csv(resumen_por_sexo, "resumen_costos_indirectos_por_sexo.csv")
write_calc_csv(
  data.frame(
    costo_total_PVLE = costo_total,
    costo_PPP_millones_intl_USD = costo_total / 1e6 / PPP_2019,
    deaths_attr_total = sum(res$deaths_attr, na.rm = TRUE),
    costo_por_muerte = costo_total / sum(res$deaths_attr, na.rm = TRUE)
  ),
  "resumen_costos_indirectos.csv"
)

message("Saved calculation outputs: costos_indirectos_por_estrato.csv, resumen_costos_indirectos_por_sexo.csv, resumen_costos_indirectos.csv")
