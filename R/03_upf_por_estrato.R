# 03_upf_por_estrato.R
# Calcula %UPF por recall, promedia por persona, luego media ponderada por estrato sexo x edad.
# Usa: alimentos_clasificados_NOVA.csv, composicion_quimica.csv, Base_Alimentos, Base_Nutrientes, ENNyS2_encuesta.

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
ennys_dir <- file.path(root, "ENNyS")
out_dir <- file.path(root, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

library(readr)
library(dplyr)
library(tidyr)

AGE_GROUPS <- list(
  "30-34" = 30:34, "35-39" = 35:39, "40-44" = 40:44, "45-49" = 45:49,
  "50-54" = 50:54, "55-59" = 55:59, "60-64" = 60:64, "65-69" = 65:69
)

# 1) NOVA
nova_path <- file.path(ennys_dir, "alimentos_clasificados_NOVA.csv")
if (!file.exists(nova_path)) nova_path <- file.path(ennys_dir, "alimentos_clasificados_sin_suplementos.csv")
alimentos <- read_csv(nova_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
alimentos$codigo <- trimws(as.character(alimentos$codigo))
alimentos <- alimentos[!grepl("^S", alimentos$codigo), ]

# 2) Composición
comp_path <- file.path(ennys_dir, "composicion_quimica.csv")
if (!file.exists(comp_path)) comp_path <- file.path(ennys_dir, "composicion_SARA2.csv")
comp <- read_csv(comp_path, show_col_types = FALSE)
comp$codigo <- trimws(as.character(comp$codigo))
comp <- comp[!is.na(comp$codigo) & !is.na(comp$kcal_100g), ]
kcal_map <- setNames(comp$kcal_100g, comp$codigo)
codigos_validos <- intersect(alimentos$codigo, names(kcal_map))
nova_map <- setNames(alimentos$NOVA, alimentos$codigo)

# 3) Columnas de alimentos en Base_Alimentos
meta_cols <- c("informe_id", "miembro_id", "clave", "fecha_realizacion", "nro_R24H", "dia_anterior", "UPM", "EUPM", "F_STG_calib")
header <- names(read_csv(file.path(ennys_dir, "Base_Alimentos_Bebidas_Suplementos.csv"), n_max = 0))
food_cols <- setdiff(header, meta_cols)
food_cols <- food_cols[nchar(food_cols) >= 4 & nchar(food_cols) <= 6 & substr(food_cols, 1, 1) %in% c("V","F","A","B","C","D","G","H","I","L","O","P","Q","R","S","Y","Z")]
food_cols <- intersect(food_cols, codigos_validos)
food_cols <- food_cols[!grepl("^S", food_cols)]
usecols <- c("informe_id", "miembro_id", "clave", "F_STG_calib", food_cols)

# 4) Leer Base_Alimentos por fragmentos (evita fallo cuando una fila posterior tiene formato distinto).
path_ali <- file.path(ennys_dir, "Base_Alimentos_Bebidas_Suplementos.csv")
header <- names(read_csv(path_ali, n_max = 0, show_col_types = FALSE))
ct <- cols(.default = col_character())
chunk_size <- 50000
chunk1 <- read_csv(path_ali, col_types = ct, n_max = chunk_size, show_col_types = FALSE)
if (nrow(chunk1) == 0) stop("Base_Alimentos: no se leyó ninguna fila. Revisar encoding/ruta del CSV.")
ali_list <- list(chunk1)
skip <- nrow(chunk1) + 1
while (nrow(chunk1) == chunk_size) {
  chunk <- read_csv(path_ali, skip = skip, n_max = chunk_size, col_names = header, col_types = ct, show_col_types = FALSE)
  if (nrow(chunk) == 0) break
  ali_list[[length(ali_list) + 1]] <- chunk
  skip <- skip + nrow(chunk)
  chunk1 <- chunk
  if (nrow(chunk) < chunk_size) break
}
ali <- bind_rows(ali_list)
usecols_present <- intersect(usecols, names(ali))
ali <- ali %>% select(all_of(usecols_present))
food_cols <- setdiff(usecols_present, c("informe_id", "miembro_id", "clave", "F_STG_calib"))
ali$informe_id <- as.numeric(ali$informe_id)
ali$miembro_id <- as.numeric(ali$miembro_id)
ali$F_STG_calib <- as.numeric(ali$F_STG_calib)
ali <- ali %>% filter(if_any(all_of(food_cols), ~ !is.na(.) & . != "" & . != "0"))

# 5) Long format y energía UPF por fila
ali_long <- ali %>%
  pivot_longer(all_of(food_cols), names_to = "codigo", values_to = "gramos") %>%
  mutate(gramos = as.numeric(gramos)) %>%
  filter(gramos > 0)

ali_long$kcal_100g <- unname(kcal_map[ali_long$codigo])
ali_long$NOVA <- unname(nova_map[ali_long$codigo])
ali_long <- ali_long %>% filter(!is.na(kcal_100g))
ali_long <- ali_long %>%
  mutate(
    energia = gramos * kcal_100g / 100,
    energia_upf = if_else(NOVA == 4, energia, 0)
  )

agg_ali <- ali_long %>%
  group_by(informe_id, miembro_id, clave, F_STG_calib) %>%
  summarise(energia_upf = sum(energia_upf), energia_total_calc = sum(energia), .groups = "drop")

# 6) Base_Nutrientes
nut <- read_csv(file.path(ennys_dir, "Base_Nutrientes.csv"),
  col_select = c("informe_id", "miembro_id", "clave", "F_STG_calib", "tot_energia_kcal"),
  show_col_types = FALSE)
nut$tot_energia_kcal <- as.numeric(nut$tot_energia_kcal)
nut <- nut %>% filter(!is.na(tot_energia_kcal), tot_energia_kcal > 0)
nut$w <- as.numeric(nut$F_STG_calib)
nut$w[is.na(nut$w)] <- 1

nut <- nut %>%
  left_join(agg_ali, by = c("informe_id", "miembro_id", "clave")) %>%
  mutate(pct_upf = if_else(tot_energia_kcal > 0, 100 * replace(energia_upf, is.na(energia_upf), 0) / tot_energia_kcal, NA_real_)) %>%
  filter(!is.na(pct_upf), pct_upf >= 0, pct_upf <= 100)

# 7) Encuesta edad/sexo
enc_cols <- c("SD_MIEMBRO_SORTEADO_clave", "SD_MIEMBRO_SORTEADO_SD_4", "SD_MIEMBRO_SORTEADO_SD_3")
enc <- read_csv(file.path(ennys_dir, "ENNyS2_encuesta.csv"), col_select = enc_cols, show_col_types = FALSE)
enc <- enc %>% rename(clave = SD_MIEMBRO_SORTEADO_clave, edad = SD_MIEMBRO_SORTEADO_SD_4, sexo = SD_MIEMBRO_SORTEADO_SD_3) %>% distinct(clave, .keep_all = TRUE)
enc$edad <- as.numeric(enc$edad)
enc <- enc %>% filter(!is.na(edad))

nut <- nut %>% left_join(enc, by = "clave")
nut$sexo_m <- ifelse(grepl("1|Masculino|masc|varon", as.character(nut$sexo), ignore.case = TRUE), 1L, 0L)
nut$edad[is.na(nut$edad)] <- 50
nut$edad[nut$edad >= 1920 & nut$edad <= 2000] <- 2019 - nut$edad[nut$edad >= 1920 & nut$edad <= 2000]
nut <- nut %>% filter(edad >= 30, edad <= 69)

age_grp <- function(e) {
  for (ag in names(AGE_GROUPS)) {
    r <- AGE_GROUPS[[ag]]
    if (e >= min(r) && e <= max(r)) return(ag)
  }
  NA_character_
}
nut$age_group <- vapply(nut$edad, age_grp, character(1))
nut <- nut %>% filter(!is.na(age_group))

# 8) Promedio por persona (w ya definido arriba)
person <- nut %>%
  group_by(miembro_id, sexo_m, age_group) %>%
  summarise(pct_upf = mean(pct_upf), w = first(w), n_recalls = n(), .groups = "drop")

# 9) Media ponderada por estrato
upf_strata <- person %>%
  group_by(sexo_m, age_group) %>%
  summarise(
    pct_upf_mean = weighted.mean(pct_upf, w),
    pct_upf_sd = sqrt(weighted.mean((pct_upf - weighted.mean(pct_upf, w))^2, w)),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pct_upf_se = if_else(n > 1, pct_upf_sd / sqrt(n), 0),
    pct_upf_ci_lo = pct_upf_mean - 1.96 * pct_upf_se,
    pct_upf_ci_hi = pct_upf_mean + 1.96 * pct_upf_se
  )

write_csv(upf_strata, file.path(out_dir, "upf_por_estrato.csv"))
message("Guardado: ", file.path(out_dir, "upf_por_estrato.csv"))
