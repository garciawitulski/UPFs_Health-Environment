# 01_agregar_defunciones.R
# Agrega defunciones 2019 por SEXO y GRUPEDAD (30-69 años). Fuente: DEIS defunciones_2019.csv

# Ejecutar desde la raíz del proyecto (Ultra-processed_Mortality) o definir PROJECT_ROOT
root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
data_dir <- file.path(root, "data")
def_path <- file.path(data_dir, "defunciones_2019.csv")
out_path <- file.path(data_dir, "defunciones_2019_por_estrato.csv")

if (!file.exists(def_path)) {
  stop("No existe defunciones_2019.csv. Ejecutar data/descargar_defunciones_2019.py o descargar desde DEIS.")
}

library(readr)
library(dplyr)

df <- read_csv(def_path, locale = locale(encoding = "latin1"), show_col_types = FALSE)
grupos_30_69 <- paste0(sprintf("%02d", 7:14), "_")
df <- df %>%
  filter(grepl(paste(grupos_30_69, collapse = "|"), as.character(GRUPEDAD))) %>%
  filter(!grepl("99", as.character(GRUPEDAD))) %>%
  filter(SEXO %in% c(1, 2))

map_grupo <- c(
  "07_30 a 34" = "30-34", "08_35 a 39" = "35-39", "09_40 a 44" = "40-44",
  "10_45 a 49" = "45-49", "11_50 a 54" = "50-54", "12_55 a 59" = "55-59",
  "13_60 a 64" = "60-64", "14_65 a 69" = "65-69"
)

out <- df %>%
  group_by(SEXO, GRUPEDAD) %>%
  summarise(deaths = sum(CUENTA), .groups = "drop") %>%
  mutate(
    age_group = unname(map_grupo[as.character(GRUPEDAD)]),
    sexo_m = if_else(SEXO == 1, 1L, 0L)
  ) %>%
  filter(!is.na(age_group)) %>%
  select(sexo_m, age_group, deaths)

write_csv(out, out_path)
message("Total muertes 30-69 (2019): ", sum(out$deaths))
message("Guardado: ", out_path)
