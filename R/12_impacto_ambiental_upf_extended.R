# 12_impacto_ambiental_upf_extended.R
# Environmental impact from ENNyS consumption using the extended comparable setup:
# - code mapping: data/environmental_footprints/ennys_codigo_to_env_group_extended.csv
# - coefficients: data/environmental_footprints/env_coefficients_comparable_4ind.csv
#
# Indicators (comparable units):
# - ghg_kg_co2eq
# - land_m2
# - water_l
# - eutro_g_po4eq
#
# Note:
# - For Arrieta groups, applies coefficient_factor_fw_rw_pct conversion (retail -> farm-gate).
# - For OWID-added groups (no explicit conversion factor), uses ratio = 1 (no conversion).
#
# Outputs:
# - output/env_impact_extended_comparable_coverage.csv
# - output/env_impact_extended_comparable_by_nova.csv
# - output/env_impact_extended_comparable_baseline_total.csv
# - output/env_impact_extended_comparable_upf_component.csv
# - output/env_impact_extended_comparable_upf_by_code.csv
# - output/env_impact_extended_comparable_scenarios_upf.csv

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
ennys_dir <- file.path(root, "ENNyS")
env_dir <- file.path(root, "data", "environmental_footprints")
out_dir <- file.path(root, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

library(readr)
library(dplyr)
library(tidyr)

mapping_path <- file.path(env_dir, "ennys_codigo_to_env_group_extended.csv")
coeff_path <- file.path(env_dir, "env_coefficients_comparable_4ind.csv")
arrieta_path <- file.path(env_dir, "arrieta_2022_tableS4_partI_coefficients.csv")
if (!file.exists(mapping_path)) stop("Falta mapping extendido: ", mapping_path, ". Ejecutar R/11_build_extended_coeffs_and_upf_coverage.R")
if (!file.exists(coeff_path)) stop("Falta base comparable: ", coeff_path, ". Ejecutar R/11_build_extended_coeffs_and_upf_coverage.R")
if (!file.exists(arrieta_path)) stop("Falta coeficientes Arrieta: ", arrieta_path)

mapping <- read_csv(mapping_path, show_col_types = FALSE) %>%
  transmute(
    codigo = trimws(as.character(codigo)),
    env_group = as.character(env_group_comparable)
  )

coeff <- read_csv(coeff_path, show_col_types = FALSE) %>%
  transmute(
    env_group = as.character(env_group),
    ghg_kg_co2eq_per_kg = as.numeric(ghg_kg_co2eq_per_kg),
    land_m2_per_kg = as.numeric(land_m2_per_kg),
    water_l_per_kg = as.numeric(water_l_per_kg),
    eutro_g_po4eq_per_kg = as.numeric(eutro_g_po4eq_per_kg),
    source = as.character(source)
  )

arrieta_factor <- read_csv(arrieta_path, show_col_types = FALSE) %>%
  transmute(
    env_group = as.character(food_items),
    coefficient_factor_fw_rw_pct = as.numeric(coefficient_factor_fw_rw_pct)
  )

coeff <- coeff %>%
  left_join(arrieta_factor, by = "env_group") %>%
  mutate(
    coef_ratio = if_else(!is.na(coefficient_factor_fw_rw_pct) & coefficient_factor_fw_rw_pct > 0, coefficient_factor_fw_rw_pct / 100, 1),
    factor_origin = if_else(!is.na(coefficient_factor_fw_rw_pct) & coefficient_factor_fw_rw_pct > 0, "arrieta_factor", "assumed_ratio_1")
  )

nova_path <- file.path(ennys_dir, "alimentos_clasificados_NOVA.csv")
if (!file.exists(nova_path)) nova_path <- file.path(ennys_dir, "alimentos_clasificados_sin_suplementos.csv")
foods <- read_csv(nova_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  transmute(
    codigo = trimws(as.character(codigo)),
    descripcion = as.character(descripcion),
    NOVA = as.integer(NOVA)
  ) %>%
  filter(!is.na(codigo), !grepl("^S", codigo))

comp_path <- file.path(ennys_dir, "composicion_quimica.csv")
if (!file.exists(comp_path)) comp_path <- file.path(ennys_dir, "composicion_SARA2.csv")
comp <- read_csv(comp_path, show_col_types = FALSE) %>%
  transmute(codigo = trimws(as.character(codigo)), kcal_100g = as.numeric(kcal_100g)) %>%
  filter(!is.na(codigo), !is.na(kcal_100g))
kcal_map <- setNames(comp$kcal_100g, comp$codigo)

meta_cols <- c("informe_id", "miembro_id", "clave", "fecha_realizacion", "nro_R24H", "dia_anterior", "UPM", "EUPM", "F_STG_calib")
path_ali <- file.path(ennys_dir, "Base_Alimentos_Bebidas_Suplementos.csv")
header <- names(read_csv(path_ali, n_max = 0, show_col_types = FALSE))
food_cols <- setdiff(header, meta_cols)
food_cols <- food_cols[nchar(food_cols) >= 4 & nchar(food_cols) <= 6 & substr(food_cols, 1, 1) %in% c("V", "F", "A", "B", "C", "D", "G", "H", "I", "L", "O", "P", "Q", "R", "S", "Y", "Z")]
food_cols <- food_cols[!grepl("^S", food_cols)]
food_cols <- intersect(food_cols, foods$codigo)
usecols <- c("informe_id", "miembro_id", "clave", "F_STG_calib", food_cols)

ct <- cols(.default = col_character())
chunk_size <- 50000
chunk1 <- read_csv(path_ali, col_types = ct, n_max = chunk_size, show_col_types = FALSE)
if (nrow(chunk1) == 0) stop("Base_Alimentos vacia o no legible.")
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

ali <- bind_rows(ali_list) %>%
  select(all_of(intersect(usecols, names(.))))

food_cols <- setdiff(names(ali), c("informe_id", "miembro_id", "clave", "F_STG_calib"))
ali <- ali %>%
  filter(if_any(all_of(food_cols), ~ !is.na(.) & . != "" & . != "0"))

ali_long <- ali %>%
  pivot_longer(all_of(food_cols), names_to = "codigo", values_to = "gramos") %>%
  mutate(gramos = as.numeric(gramos)) %>%
  filter(!is.na(gramos), gramos > 0) %>%
  left_join(foods %>% select(codigo, descripcion, NOVA), by = "codigo") %>%
  left_join(mapping, by = "codigo") %>%
  left_join(coeff, by = "env_group") %>%
  mutate(
    kcal_100g = unname(kcal_map[codigo]),
    energia_kcal = gramos * kcal_100g / 100,
    mapped = !is.na(env_group) & !is.na(ghg_kg_co2eq_per_kg),
    kg_retail = gramos / 1000,
    kg_ref = if_else(mapped & coef_ratio > 0, kg_retail / coef_ratio, 0),
    ghg_kg_co2eq = if_else(mapped, kg_ref * ghg_kg_co2eq_per_kg, 0),
    land_m2 = if_else(mapped, kg_ref * land_m2_per_kg, 0),
    water_l = if_else(mapped, kg_ref * water_l_per_kg, 0),
    eutro_g_po4eq = if_else(mapped, kg_ref * eutro_g_po4eq_per_kg, 0)
  )

impact_cols <- c("ghg_kg_co2eq", "land_m2", "water_l", "eutro_g_po4eq")

coverage_fn <- function(df, scope_label) {
  tibble(
    scope = scope_label,
    n_rows = nrow(df),
    n_rows_mapped = sum(df$mapped, na.rm = TRUE),
    mapped_rows_pct = 100 * n_rows_mapped / n_rows,
    grams_total = sum(df$gramos, na.rm = TRUE),
    grams_mapped = sum(df$gramos[df$mapped], na.rm = TRUE),
    mapped_grams_pct = 100 * grams_mapped / grams_total,
    energy_total = sum(df$energia_kcal, na.rm = TRUE),
    energy_mapped = sum(df$energia_kcal[df$mapped], na.rm = TRUE),
    mapped_energy_pct = 100 * energy_mapped / energy_total,
    n_unique_codes = dplyr::n_distinct(df$codigo),
    n_unique_codes_mapped = dplyr::n_distinct(df$codigo[df$mapped])
  )
}

cov_all <- coverage_fn(ali_long, "all_codes")
cov_upf <- coverage_fn(filter(ali_long, NOVA == 4), "upf_only")
coverage <- bind_rows(cov_all, cov_upf)
write_csv(coverage, file.path(out_dir, "env_impact_extended_comparable_coverage.csv"))

baseline_total <- ali_long %>%
  summarise(across(all_of(impact_cols), ~ sum(.x, na.rm = TRUE)))
write_csv(baseline_total, file.path(out_dir, "env_impact_extended_comparable_baseline_total.csv"))

by_nova <- ali_long %>%
  mutate(NOVA = if_else(is.na(NOVA), -1L, NOVA)) %>%
  group_by(NOVA) %>%
  summarise(
    n_rows = n(),
    n_rows_mapped = sum(mapped, na.rm = TRUE),
    mapped_rows_pct = 100 * n_rows_mapped / n_rows,
    grams_total = sum(gramos, na.rm = TRUE),
    grams_mapped = sum(gramos[mapped], na.rm = TRUE),
    mapped_grams_pct = 100 * grams_mapped / grams_total,
    energy_total = sum(energia_kcal, na.rm = TRUE),
    energy_mapped = sum(energia_kcal[mapped], na.rm = TRUE),
    mapped_energy_pct = 100 * energy_mapped / energy_total,
    across(all_of(impact_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(NOVA)
write_csv(by_nova, file.path(out_dir, "env_impact_extended_comparable_by_nova.csv"))

upf_component <- ali_long %>%
  filter(NOVA == 4) %>%
  summarise(across(all_of(impact_cols), ~ sum(.x, na.rm = TRUE)))
write_csv(upf_component, file.path(out_dir, "env_impact_extended_comparable_upf_component.csv"))

upf_by_code <- ali_long %>%
  filter(NOVA == 4) %>%
  group_by(codigo, descripcion, env_group) %>%
  summarise(
    n_rows = n(),
    grams_total = sum(gramos, na.rm = TRUE),
    grams_mapped = sum(gramos[mapped], na.rm = TRUE),
    mapped_grams_pct = 100 * grams_mapped / grams_total,
    energy_total = sum(energia_kcal, na.rm = TRUE),
    energy_mapped = sum(energia_kcal[mapped], na.rm = TRUE),
    mapped_energy_pct = 100 * energy_mapped / energy_total,
    across(all_of(impact_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(ghg_kg_co2eq))
write_csv(upf_by_code, file.path(out_dir, "env_impact_extended_comparable_upf_by_code.csv"))

scenarios <- tibble(
  scenario = c("red_10", "red_20", "red_50"),
  reduction = c(0.10, 0.20, 0.50)
)

scenario_tbl <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
  r <- scenarios$reduction[i]
  sc <- scenarios$scenario[i]
  bind_rows(lapply(impact_cols, function(v) {
    base_v <- baseline_total[[v]][1]
    upf_v <- upf_component[[v]][1]
    tibble(
      scenario = sc,
      reduction = r,
      indicator = v,
      baseline_value = base_v,
      upf_component = upf_v,
      avoided_value = r * upf_v,
      post_value_no_replacement = base_v - r * upf_v,
      avoided_pct_of_baseline = ifelse(base_v > 0, 100 * (r * upf_v) / base_v, NA_real_)
    )
  }))
}))
write_csv(scenario_tbl, file.path(out_dir, "env_impact_extended_comparable_scenarios_upf.csv"))

assumed_ratio_groups <- coeff %>%
  filter(factor_origin == "assumed_ratio_1") %>%
  distinct(env_group) %>%
  pull(env_group)

message("Guardado: output/env_impact_extended_comparable_coverage.csv")
message("Guardado: output/env_impact_extended_comparable_baseline_total.csv")
message("Guardado: output/env_impact_extended_comparable_by_nova.csv")
message("Guardado: output/env_impact_extended_comparable_upf_component.csv")
message("Guardado: output/env_impact_extended_comparable_upf_by_code.csv")
message("Guardado: output/env_impact_extended_comparable_scenarios_upf.csv")
message("Cobertura UPF (energia): ", round(cov_upf$mapped_energy_pct, 1), "%")
message("Cobertura UPF (gramos): ", round(cov_upf$mapped_grams_pct, 1), "%")
message("Grupos con ratio asumido=1 (sin factor explicito): ", paste(assumed_ratio_groups, collapse = ", "))
