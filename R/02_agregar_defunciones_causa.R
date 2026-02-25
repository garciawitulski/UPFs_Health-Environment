# 02_agregar_defunciones_causa.R
# Agrega defunciones 2019 por SEXO, GRUPEDAD y CAUSA (ICD-10).
# Causas incluidas: CVD (sin cerebrovascular) y cerebrovascular.
# Fuente: DEIS defunciones_2019.csv

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
data_dir <- file.path(root, "data")
out_dir <- file.path(root, "output")
def_path <- file.path(data_dir, "defunciones_2019.csv")
out_path <- file.path(out_dir, "defunciones_2019_por_causa.csv")

if (!file.exists(def_path)) stop("No existe data/defunciones_2019.csv")

library(readr)
library(dplyr)

clasificar_causa <- function(cod) {
  cod <- toupper(trimws(as.character(cod)))
  if (nchar(cod) < 3) return(NA_character_)
  if (substr(cod, 1, 1) == "I") {
    n <- as.integer(substr(cod, 2, 3))
    if (!is.na(n)) {
      if (n >= 60 && n <= 69) return("cerebrovascular")
      if ((n >= 0 && n <= 59) || (n >= 70 && n <= 99)) return("cvd")
    }
  }
  NA_character_
}

map_grupo <- c(
  "07_30 a 34" = "30-34", "08_35 a 39" = "35-39", "09_40 a 44" = "40-44",
  "10_45 a 49" = "45-49", "11_50 a 54" = "50-54", "12_55 a 59" = "55-59",
  "13_60 a 64" = "60-64", "14_65 a 69" = "65-69"
)
grupos_30_69 <- paste0(sprintf("%02d", 7:14), "_")

df <- read_csv(def_path, locale = locale(encoding = "latin1"), show_col_types = FALSE) %>%
  filter(SEXO %in% c(1, 2)) %>%
  filter(!grepl("99", as.character(GRUPEDAD))) %>%
  filter(grepl(paste(grupos_30_69, collapse = "|"), as.character(GRUPEDAD)))

df$causa <- vapply(df$CAUSA, clasificar_causa, character(1))
df <- df %>% filter(!is.na(causa))

out_df <- df %>%
  group_by(SEXO, GRUPEDAD, causa) %>%
  summarise(deaths = sum(CUENTA), .groups = "drop") %>%
  mutate(
    age_group = unname(map_grupo[as.character(GRUPEDAD)]),
    sexo_m = if_else(SEXO == 1, 1L, 0L)
  ) %>%
  select(sexo_m, age_group, causa, deaths)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(out_df, out_path)
message("Guardado: ", out_path)
print(out_df %>% group_by(causa) %>% summarise(deaths = sum(deaths)))
