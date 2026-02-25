# 10_impacto_ambiental.R
# Environmental impact of ENNyS consumption using:
# - comparable coefficients (main 4-indicator results),
# - weighted totals (F_STG_calib, adjusted by number of recalls per person),
# - a separate "true extended" GHG run using env_group_extended.

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
ennys_dir <- file.path(root, "ENNyS")
env_dir <- file.path(root, "data", "environmental_footprints")
out_dir <- file.path(root, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

library(readr)
library(dplyr)
library(tidyr)

AGE_GROUPS <- list(
  "30-34" = 30:34, "35-39" = 35:39, "40-44" = 40:44, "45-49" = 45:49,
  "50-54" = 50:54, "55-59" = 55:59, "60-64" = 60:64, "65-69" = 65:69
)

# GAPA-based replacement basket (Argentina, 2016):
# - "Grafica de la alimentacion diaria" group proportions
# - quantitative messages: fruits/vegetables daily, dairy daily, oils/nuts, fish
#   frequency, and moderate red-meat intake.
GAPA_GROUP_WEIGHTS <- c(
  "Fruits" = 0.18,
  "Vegetables (outdoor)" = 0.25,
  "Legumes and pulses" = 0.05,
  "Cereals (without rice)" = 0.06,
  "Rice" = 0.03,
  "Pasta" = 0.03,
  "Baked products" = 0.03,
  "Starchy vegetables" = 0.03,
  "Dairy products" = 0.13,
  "Beef" = 0.04,
  "Lamb and mutton" = 0.005,
  "Other meats" = 0.005,
  "Poultry" = 0.025,
  "Pork" = 0.01,
  "Fish and seafood" = 0.03,
  "Eggs" = 0.015,
  "Oil crops" = 0.06,
  "Nuts and seeds" = 0.02
)

compute_weighted_group_intensity <- function(df, env_group_col, energy_col, indicator_map, weights = GAPA_GROUP_WEIGHTS) {
  ind_cols <- unique(unname(indicator_map))
  by_group <- df %>%
    filter(!is.na(.data[[env_group_col]]), !is.na(.data[[energy_col]]), .data[[energy_col]] > 0) %>%
    group_by(env_group = .data[[env_group_col]]) %>%
    summarise(
      energy_kcal = sum(.data[[energy_col]], na.rm = TRUE),
      across(all_of(ind_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  w_tbl <- tibble(
    env_group = names(weights),
    weight_target = as.numeric(weights)
  )
  if (sum(w_tbl$weight_target) <= 0) stop("Pesos del perfil de reemplazo invalidos (suma <= 0).")

  available_groups <- intersect(w_tbl$env_group, by_group$env_group)
  missing_groups <- setdiff(w_tbl$env_group, available_groups)
  w_tbl <- w_tbl %>%
    mutate(
      available = env_group %in% available_groups,
      weight_used = if_else(available, weight_target / sum(weight_target[available]), 0)
    )

  intensity <- setNames(rep(NA_real_, length(indicator_map)), names(indicator_map))
  for (nm in names(indicator_map)) {
    col_nm <- indicator_map[[nm]]
    avail <- w_tbl %>%
      filter(weight_used > 0) %>%
      left_join(
        by_group %>%
          transmute(
            env_group,
            intensity = .data[[col_nm]] / energy_kcal
          ),
        by = "env_group"
      ) %>%
      filter(!is.na(intensity), is.finite(intensity))
    if (nrow(avail) == 0) stop("No se pudo calcular intensidad ponderada para indicador: ", nm)
    intensity[[nm]] <- sum(avail$weight_used * avail$intensity, na.rm = TRUE)
  }

  list(
    intensity = intensity,
    weight_coverage_pct = 100 * sum(w_tbl$weight_target[w_tbl$available]) / sum(w_tbl$weight_target),
    missing_groups = missing_groups,
    weights_used = w_tbl
  )
}

compute_policy_reduction_from_health_profile <- function(out_dir) {
  ndg_path <- file.path(out_dir, "upf_ndg_gapa_profile.csv")
  upf_path <- file.path(out_dir, "upf_por_estrato.csv")
  if (!file.exists(ndg_path) || !file.exists(upf_path)) return(tibble())

  ndg <- tryCatch(read_csv(ndg_path, show_col_types = FALSE), error = function(e) NULL)
  upf <- tryCatch(read_csv(upf_path, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(ndg) || is.null(upf) || nrow(ndg) == 0 || nrow(upf) == 0) return(tibble())

  if (!("scenario" %in% names(ndg))) {
    ndg <- ndg %>% mutate(scenario = "ndg_gapa")
  }
  if (!("profile" %in% names(ndg))) {
    ndg <- ndg %>% mutate(profile = "unspecified")
  }
  target_upf_pct <- suppressWarnings(as.numeric(ndg$target_upf_pct))
  baseline_upf_pct <- suppressWarnings(weighted.mean(upf$pct_upf_mean, w = upf$n, na.rm = TRUE))
  if (!is.finite(baseline_upf_pct) || baseline_upf_pct <= 0) return(tibble())

  ndg %>%
    transmute(
      scenario = as.character(scenario),
      profile = as.character(profile),
      baseline_upf_pct = baseline_upf_pct,
      target_upf_pct = target_upf_pct,
      reduction = pmax(0, pmin(1, (baseline_upf_pct - target_upf_pct) / baseline_upf_pct))
    ) %>%
    filter(is.finite(target_upf_pct), is.finite(reduction), reduction > 0) %>%
    distinct(scenario, .keep_all = TRUE)
}

format_nova_label <- function(x) {
  dplyr::case_when(
    x == 1L ~ "NOVA1",
    x == 2L ~ "NOVA2",
    x == 3L ~ "NOVA3",
    x == 4L ~ "NOVA4_UPF",
    TRUE ~ paste0("NOVA", x)
  )
}

safe_pct <- function(num, den) {
  den_vec <- as.numeric(den)
  num_vec <- as.numeric(num)
  if (length(den_vec) == 1L) den_vec <- rep(den_vec, length(num_vec))
  if (length(den_vec) != length(num_vec)) stop("safe_pct: longitudes incompatibles")
  out <- rep(NA_real_, length(num_vec))
  ok <- is.finite(den_vec) & den_vec > 0 & is.finite(num_vec)
  out[ok] <- 100 * num_vec[ok] / den_vec[ok]
  out
}

safe_per_1000_kcal <- function(num, den_kcal) {
  den_vec <- as.numeric(den_kcal)
  num_vec <- as.numeric(num)
  if (length(den_vec) == 1L) den_vec <- rep(den_vec, length(num_vec))
  if (length(den_vec) != length(num_vec)) stop("safe_per_1000_kcal: longitudes incompatibles")
  out <- rep(NA_real_, length(num_vec))
  ok <- is.finite(den_vec) & den_vec > 0 & is.finite(num_vec)
  out[ok] <- 1000 * num_vec[ok] / den_vec[ok]
  out
}

mapping_path <- file.path(env_dir, "ennys_codigo_to_env_group_extended.csv")
coeff_comp_path <- file.path(env_dir, "env_coefficients_comparable_4ind.csv")
coeff_agb_path <- file.path(env_dir, "env_coefficients_agribalyse_subgroups.csv")
coeff_expanded_path <- file.path(env_dir, "env_coefficients_expanded_from_xlsx.csv")
bridge_expanded_path <- file.path(env_dir, "env_group_extended_to_expanded_bridge.csv")
arrieta_path <- file.path(env_dir, "arrieta_2022_tableS4_partI_coefficients.csv")

if (!file.exists(mapping_path)) stop("Falta mapping extendido: ", mapping_path, ". Ejecutar R/11_build_extended_coeffs_and_upf_coverage.R")
if (!file.exists(coeff_comp_path)) stop("Falta coeficientes comparables: ", coeff_comp_path, ". Ejecutar R/11_build_extended_coeffs_and_upf_coverage.R")
if (!file.exists(arrieta_path)) stop("Falta tabla Arrieta: ", arrieta_path)

mapping <- read_csv(mapping_path, show_col_types = FALSE) %>%
  transmute(
    codigo = trimws(as.character(codigo)),
    env_group_comp = as.character(env_group_comparable),
    env_group_extended = as.character(env_group_extended),
    mapping_level = as.character(mapping_level)
  )

coeff_comp <- read_csv(coeff_comp_path, show_col_types = FALSE) %>%
  transmute(
    env_group = as.character(env_group),
    ghg_kg_co2eq_per_kg = as.numeric(ghg_kg_co2eq_per_kg),
    land_m2_per_kg = as.numeric(land_m2_per_kg),
    water_l_per_kg = as.numeric(water_l_per_kg),
    eutro_g_po4eq_per_kg = as.numeric(eutro_g_po4eq_per_kg),
    source = as.character(source)
  )

# Apply retail->farm-gate factor where explicitly available (Arrieta groups).
arrieta_factor <- read_csv(arrieta_path, show_col_types = FALSE) %>%
  transmute(
    env_group = as.character(food_items),
    coefficient_factor_fw_rw_pct = as.numeric(coefficient_factor_fw_rw_pct)
  )

coeff_comp <- coeff_comp %>%
  left_join(arrieta_factor, by = "env_group") %>%
  mutate(
    coef_ratio_comp = if_else(!is.na(coefficient_factor_fw_rw_pct) & coefficient_factor_fw_rw_pct > 0, coefficient_factor_fw_rw_pct / 100, 1),
    factor_origin = if_else(!is.na(coefficient_factor_fw_rw_pct) & coefficient_factor_fw_rw_pct > 0, "arrieta_factor", "assumed_ratio_1")
  )

# Build extended GHG coefficient set (separate run):
# - comparable groups (Arrieta + OWID proxies),
# - Agribalyse extended subgroups (GHG only).
coeff_ghg_ext <- coeff_comp %>%
  transmute(
    env_group = env_group,
    ghg_kg_co2eq_per_kg_ext = ghg_kg_co2eq_per_kg,
    coef_ratio_ext = coef_ratio_comp,
    source_ext = source
  )

if (file.exists(coeff_agb_path)) {
  coeff_agb <- read_csv(coeff_agb_path, show_col_types = FALSE) %>%
    transmute(
      env_group = as.character(env_group),
      ghg_kg_co2eq_per_kg_ext = as.numeric(ghg_kg_co2eq_per_kg),
      coef_ratio_ext = 1,
      source_ext = as.character(source)
    )
  coeff_ghg_ext <- bind_rows(coeff_ghg_ext, coeff_agb) %>%
    distinct(env_group, .keep_all = TRUE)
}

nova_path <- file.path(ennys_dir, "alimentos_clasificados_NOVA.csv")
if (!file.exists(nova_path)) nova_path <- file.path(ennys_dir, "alimentos_clasificados_sin_suplementos.csv")
foods <- read_csv(nova_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  transmute(
    codigo = trimws(as.character(codigo)),
    descripcion = as.character(descripcion),
    NOVA = as.integer(NOVA)
  ) %>%
  filter(!is.na(codigo), !grepl("^S", codigo))
nova_map <- setNames(foods$NOVA, foods$codigo)
desc_map <- setNames(foods$descripcion, foods$codigo)

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
  select(all_of(intersect(usecols, names(.)))) %>%
  mutate(F_STG_calib = suppressWarnings(as.numeric(F_STG_calib)))

food_cols <- setdiff(names(ali), c("informe_id", "miembro_id", "clave", "F_STG_calib"))
ali <- ali %>%
  filter(if_any(all_of(food_cols), ~ !is.na(.) & . != "" & . != "0"))

# Adjust survey weight when person has >1 recall to avoid double counting.
recall_n <- ali %>%
  group_by(clave) %>%
  summarise(n_recalls = dplyr::n(), .groups = "drop")

enc_cols <- c("SD_MIEMBRO_SORTEADO_clave", "SD_MIEMBRO_SORTEADO_SD_4", "SD_MIEMBRO_SORTEADO_SD_3")
enc <- read_csv(file.path(ennys_dir, "ENNyS2_encuesta.csv"), col_select = all_of(enc_cols), show_col_types = FALSE) %>%
  rename(clave = SD_MIEMBRO_SORTEADO_clave, edad = SD_MIEMBRO_SORTEADO_SD_4, sexo = SD_MIEMBRO_SORTEADO_SD_3) %>%
  distinct(clave, .keep_all = TRUE) %>%
  mutate(edad = as.numeric(edad))

age_grp <- function(e) {
  for (ag in names(AGE_GROUPS)) {
    r <- AGE_GROUPS[[ag]]
    if (e >= min(r) && e <= max(r)) return(ag)
  }
  NA_character_
}

ali_long <- ali %>%
  pivot_longer(all_of(food_cols), names_to = "codigo", values_to = "gramos_raw") %>%
  mutate(gramos_raw = as.numeric(gramos_raw)) %>%
  filter(!is.na(gramos_raw), gramos_raw > 0) %>%
  left_join(recall_n, by = "clave") %>%
  mutate(
    n_recalls = if_else(is.na(n_recalls) | n_recalls <= 0, 1, n_recalls),
    weight_person = if_else(is.na(F_STG_calib) | F_STG_calib < 0, 0, F_STG_calib),
    weight_recall = weight_person / n_recalls,
    NOVA = unname(nova_map[codigo]),
    descripcion = unname(desc_map[codigo]),
    kcal_100g = unname(kcal_map[codigo]),
    energia_kcal_raw = gramos_raw * kcal_100g / 100,
    gramos = gramos_raw * weight_recall,
    energia_kcal = energia_kcal_raw * weight_recall
  ) %>%
  left_join(mapping, by = "codigo") %>%
  left_join(coeff_comp, by = c("env_group_comp" = "env_group")) %>%
  left_join(coeff_ghg_ext, by = c("env_group_extended" = "env_group")) %>%
  left_join(enc, by = "clave") %>%
  mutate(
    sexo_m = ifelse(grepl("1|Masculino|masc|varon", as.character(sexo), ignore.case = TRUE), 1L, 0L),
    edad = if_else(!is.na(edad) & edad >= 1920 & edad <= 2000, 2019 - edad, edad),
    age_group = vapply(edad, age_grp, character(1)),
    mapped = !is.na(env_group_comp) & !is.na(ghg_kg_co2eq_per_kg),
    has_extended_group = !is.na(env_group_extended),
    mapped_ghg_ext = !is.na(env_group_extended) & !is.na(ghg_kg_co2eq_per_kg_ext),
    kg_retail = gramos / 1000,
    kg_ref = if_else(mapped & coef_ratio_comp > 0, kg_retail / coef_ratio_comp, 0),
    ghg_kg_co2eq = if_else(mapped, kg_ref * ghg_kg_co2eq_per_kg, 0),
    land_m2 = if_else(mapped, kg_ref * land_m2_per_kg, 0),
    water_l = if_else(mapped, kg_ref * water_l_per_kg, 0),
    eutro_g_po4eq = if_else(mapped, kg_ref * eutro_g_po4eq_per_kg, 0),
    kg_ref_ghg_ext = if_else(mapped_ghg_ext & coef_ratio_ext > 0, kg_retail / coef_ratio_ext, 0),
    ghg_kg_co2eq_ext = if_else(mapped_ghg_ext, kg_ref_ghg_ext * ghg_kg_co2eq_per_kg_ext, 0)
  ) %>%
  filter(!is.na(edad), edad >= 30, edad <= 69, !is.na(age_group))

impact_cols <- c("ghg_kg_co2eq", "land_m2", "water_l", "eutro_g_po4eq")

# ------------------------------------------------------------------------------
# Comparable 4-indicator outputs (weighted)
# ------------------------------------------------------------------------------

coverage <- tibble(
  n_rows_positive = nrow(ali_long),
  n_rows_mapped = sum(ali_long$mapped, na.rm = TRUE),
  mapped_rows_pct = 100 * n_rows_mapped / n_rows_positive,
  n_rows_with_extended_group = sum(ali_long$has_extended_group, na.rm = TRUE),
  rows_with_extended_group_pct = 100 * n_rows_with_extended_group / n_rows_positive,
  grams_positive = sum(ali_long$gramos, na.rm = TRUE),
  grams_positive_raw = sum(ali_long$gramos_raw, na.rm = TRUE),
  grams_mapped = sum(ali_long$gramos[ali_long$mapped], na.rm = TRUE),
  mapped_grams_pct = 100 * grams_mapped / grams_positive,
  grams_with_extended_group = sum(ali_long$gramos[ali_long$has_extended_group], na.rm = TRUE),
  grams_with_extended_group_pct = 100 * grams_with_extended_group / grams_positive,
  energia_kcal_total = sum(ali_long$energia_kcal, na.rm = TRUE),
  energia_kcal_total_raw = sum(ali_long$energia_kcal_raw, na.rm = TRUE),
  energia_kcal_mapped = sum(ali_long$energia_kcal[ali_long$mapped], na.rm = TRUE),
  mapped_energy_pct = 100 * energia_kcal_mapped / energia_kcal_total,
  energia_kcal_with_extended_group = sum(ali_long$energia_kcal[ali_long$has_extended_group], na.rm = TRUE),
  energy_with_extended_group_pct = 100 * energia_kcal_with_extended_group / energia_kcal_total,
  n_unique_codes_consumed = dplyr::n_distinct(ali_long$codigo),
  n_unique_codes_mapped = dplyr::n_distinct(ali_long$codigo[ali_long$mapped]),
  n_unique_codes_with_extended_group = dplyr::n_distinct(ali_long$codigo[ali_long$has_extended_group])
)
write_csv(coverage, file.path(out_dir, "env_impact_mapping_coverage.csv"))
write_csv(coverage, file.path(out_dir, "env_impact_comparable_coverage.csv"))

coverage_by_nova <- ali_long %>%
  mutate(NOVA = if_else(is.na(NOVA), -1L, NOVA)) %>%
  group_by(NOVA) %>%
  summarise(
    n_rows = n(),
    n_rows_mapped = sum(mapped, na.rm = TRUE),
    mapped_rows_pct = 100 * n_rows_mapped / n_rows,
    n_rows_with_extended_group = sum(has_extended_group, na.rm = TRUE),
    rows_with_extended_group_pct = 100 * n_rows_with_extended_group / n_rows,
    grams_total = sum(gramos, na.rm = TRUE),
    grams_mapped = sum(gramos[mapped], na.rm = TRUE),
    mapped_grams_pct = 100 * grams_mapped / grams_total,
    grams_with_extended_group = sum(gramos[has_extended_group], na.rm = TRUE),
    grams_with_extended_group_pct = 100 * grams_with_extended_group / grams_total,
    energia_kcal_total = sum(energia_kcal, na.rm = TRUE),
    energia_kcal_mapped = sum(energia_kcal[mapped], na.rm = TRUE),
    mapped_energy_pct = 100 * energia_kcal_mapped / energia_kcal_total,
    energia_kcal_with_extended_group = sum(energia_kcal[has_extended_group], na.rm = TRUE),
    energy_with_extended_group_pct = 100 * energia_kcal_with_extended_group / energia_kcal_total,
    .groups = "drop"
  ) %>%
  arrange(NOVA)
write_csv(coverage_by_nova, file.path(out_dir, "env_impact_mapping_coverage_by_nova.csv"))
write_csv(coverage_by_nova, file.path(out_dir, "env_impact_comparable_coverage_by_nova.csv"))

baseline_total <- ali_long %>%
  summarise(across(all_of(impact_cols), ~ sum(.x, na.rm = TRUE)))
write_csv(baseline_total, file.path(out_dir, "env_impact_baseline_total.csv"))
write_csv(baseline_total, file.path(out_dir, "env_impact_comparable_baseline_total.csv"))

by_nova <- ali_long %>%
  group_by(NOVA) %>%
  summarise(across(all_of(impact_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
write_csv(by_nova, file.path(out_dir, "env_impact_by_nova.csv"))
write_csv(by_nova, file.path(out_dir, "env_impact_comparable_by_nova.csv"))

by_env_group <- ali_long %>%
  filter(mapped) %>%
  group_by(env_group = env_group_comp) %>%
  summarise(across(all_of(impact_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(desc(ghg_kg_co2eq))
write_csv(by_env_group, file.path(out_dir, "env_impact_by_env_group.csv"))
write_csv(by_env_group, file.path(out_dir, "env_impact_comparable_by_env_group.csv"))

by_strata <- ali_long %>%
  group_by(sexo_m, age_group) %>%
  summarise(
    grams_total = sum(gramos, na.rm = TRUE),
    grams_mapped = sum(gramos[mapped], na.rm = TRUE),
    mapped_grams_pct = 100 * grams_mapped / grams_total,
    grams_with_extended_group = sum(gramos[has_extended_group], na.rm = TRUE),
    extended_group_grams_pct = 100 * grams_with_extended_group / grams_total,
    energia_kcal_total = sum(energia_kcal, na.rm = TRUE),
    energia_kcal_mapped = sum(energia_kcal[mapped], na.rm = TRUE),
    mapped_energy_pct = 100 * energia_kcal_mapped / energia_kcal_total,
    energia_kcal_with_extended_group = sum(energia_kcal[has_extended_group], na.rm = TRUE),
    extended_group_energy_pct = 100 * energia_kcal_with_extended_group / energia_kcal_total,
    across(all_of(impact_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(sexo_m, age_group)
write_csv(by_strata, file.path(out_dir, "env_impact_by_strata.csv"))
write_csv(by_strata, file.path(out_dir, "env_impact_comparable_by_strata.csv"))

upf_component <- ali_long %>%
  filter(NOVA == 4) %>%
  summarise(
    upf_energy_mapped = sum(energia_kcal[mapped], na.rm = TRUE),
    upf_mass_kg_mapped = sum(gramos[mapped], na.rm = TRUE) / 1000,
    across(all_of(impact_cols), ~ sum(.x, na.rm = TRUE))
  )

scenarios <- tibble(
  scenario = c("red_10", "red_20", "red_50"),
  reduction = c(0.10, 0.20, 0.50)
)

policy_scen <- compute_policy_reduction_from_health_profile(out_dir)
if (nrow(policy_scen) > 0) {
  scenarios <- bind_rows(scenarios, policy_scen %>% transmute(scenario, reduction))
  write_csv(
    policy_scen %>%
      transmute(
        scenario,
        profile,
        baseline_upf_pct,
        target_upf_pct,
        implied_reduction_pct = 100 * reduction
      ),
    file.path(out_dir, "env_ndg_gapa_scenario_profile.csv")
  )
  msg_vec <- policy_scen %>%
    mutate(
      txt = paste0(
        scenario, ": ",
        round(100 * reduction, 1), "% (baseline=",
        round(baseline_upf_pct, 2), "%; target=",
        round(target_upf_pct, 2), "%)"
      )
    ) %>%
    pull(txt)
  message("Escenarios policy-feasible agregados: ", paste(msg_vec, collapse = " | "))
} else {
  message("Escenarios policy-feasible no agregados (falta perfil health o no hay reduccion positiva).")
}

replacement_intensity <- ali_long %>%
  filter(mapped, NOVA %in% c(1L, 3L), energia_kcal > 0) %>%
  group_by(NOVA) %>%
  summarise(
    energy_kcal = sum(energia_kcal, na.rm = TRUE),
    mass_kg = sum(gramos, na.rm = TRUE) / 1000,
    across(all_of(impact_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

if (!all(c(1L, 3L) %in% replacement_intensity$NOVA)) {
  stop("No se pudieron calcular intensidades de reemplazo para NOVA1 y NOVA3.")
}

repl_1 <- filter(replacement_intensity, NOVA == 1L)
repl_3 <- filter(replacement_intensity, NOVA == 3L)
mix_w1 <- repl_1$energy_kcal / (repl_1$energy_kcal + repl_3$energy_kcal)
mix_w3 <- 1 - mix_w1

gapa_comp_profile <- compute_weighted_group_intensity(
  df = ali_long %>% filter(mapped, NOVA != 4L),
  env_group_col = "env_group_comp",
  energy_col = "energia_kcal",
  indicator_map = setNames(impact_cols, impact_cols)
)
gapa_comp_intensity <- gapa_comp_profile$intensity
gapa_comp_profile_mass <- compute_weighted_group_intensity(
  df = ali_long %>% filter(mapped, NOVA != 4L) %>% mutate(mass_kg = gramos / 1000),
  env_group_col = "env_group_comp",
  energy_col = "mass_kg",
  indicator_map = setNames(impact_cols, impact_cols)
)
gapa_comp_intensity_kg <- gapa_comp_profile_mass$intensity
write_csv(
  mutate(gapa_comp_profile$weights_used, profile = "GAPA_ARG", context = "comparable"),
  file.path(out_dir, "env_replacement_gapa_weights_comparable.csv")
)
write_csv(
  mutate(gapa_comp_profile_mass$weights_used, profile = "GAPA_ARG", context = "comparable_mass"),
  file.path(out_dir, "env_replacement_gapa_weights_comparable_mass.csv")
)
message(
  "Perfil GAPA (comparable) cobertura de pesos: ",
  round(gapa_comp_profile$weight_coverage_pct, 1),
  "%"
)
if (length(gapa_comp_profile$missing_groups) > 0) {
  message("Perfil GAPA (comparable) grupos sin energia observada: ", paste(gapa_comp_profile$missing_groups, collapse = ", "))
}

build_rows <- function(sc, r, model, target) {
  bind_rows(lapply(impact_cols, function(v) {
    base_v <- baseline_total[[v]][1]
    upf_v <- upf_component[[v]][1]
    gross_removed <- r * upf_v
    reduced_upf_energy <- r * upf_component$upf_energy_mapped[1]
    reduced_upf_mass <- r * upf_component$upf_mass_kg_mapped[1]

    replacement_added <- 0
    if (model == "isocaloric_replacement") {
      i1 <- repl_1[[v]][1] / repl_1$energy_kcal[1]
      i3 <- repl_3[[v]][1] / repl_3$energy_kcal[1]
      intensity <- case_when(
        target == "NOVA1" ~ i1,
        target == "NOVA3" ~ i3,
        target == "NOVA1_NOVA3_mix" ~ mix_w1 * i1 + mix_w3 * i3,
        target == "GAPA_ARG" ~ gapa_comp_intensity[[v]],
        TRUE ~ NA_real_
      )
      replacement_added <- reduced_upf_energy * intensity
    } else if (model == "isoweight_replacement") {
      i1 <- repl_1[[v]][1] / repl_1$mass_kg[1]
      i3 <- repl_3[[v]][1] / repl_3$mass_kg[1]
      intensity <- case_when(
        target == "NOVA1" ~ i1,
        target == "NOVA3" ~ i3,
        target == "NOVA1_NOVA3_mix" ~ mix_w1 * i1 + mix_w3 * i3,
        target == "GAPA_ARG" ~ gapa_comp_intensity_kg[[v]],
        TRUE ~ NA_real_
      )
      replacement_added <- reduced_upf_mass * intensity
    }

    net_avoided <- gross_removed - replacement_added
    tibble(
      scenario = sc,
      reduction = r,
      scenario_model = model,
      replacement_target = target,
      indicator = v,
      baseline_value = base_v,
      upf_mapped_component = upf_v,
      gross_removed_value = gross_removed,
      replacement_added_value = replacement_added,
      avoided_value = net_avoided,
      post_value = base_v - net_avoided,
      avoided_pct_of_baseline = ifelse(base_v > 0, 100 * net_avoided / base_v, NA_real_)
    )
  }))
}

rows_no_replacement <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
  build_rows(scenarios$scenario[i], scenarios$reduction[i], "no_replacement", "none")
}))

rows_isocaloric <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
  sc <- scenarios$scenario[i]
  r <- scenarios$reduction[i]
  bind_rows(
    build_rows(sc, r, "isocaloric_replacement", "NOVA1"),
    build_rows(sc, r, "isocaloric_replacement", "NOVA3"),
    build_rows(sc, r, "isocaloric_replacement", "NOVA1_NOVA3_mix"),
    build_rows(sc, r, "isocaloric_replacement", "GAPA_ARG")
  )
}))

rows_isoweight <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
  sc <- scenarios$scenario[i]
  r <- scenarios$reduction[i]
  bind_rows(
    build_rows(sc, r, "isoweight_replacement", "NOVA1"),
    build_rows(sc, r, "isoweight_replacement", "NOVA3"),
    build_rows(sc, r, "isoweight_replacement", "NOVA1_NOVA3_mix"),
    build_rows(sc, r, "isoweight_replacement", "GAPA_ARG")
  )
}))

scenario_tbl <- bind_rows(rows_no_replacement, rows_isocaloric, rows_isoweight)
write_csv(scenario_tbl, file.path(out_dir, "env_impact_scenarios_upf.csv"))
write_csv(scenario_tbl, file.path(out_dir, "env_impact_comparable_scenarios_upf.csv"))

scenario_summary <- scenario_tbl %>%
  filter(indicator %in% c("ghg_kg_co2eq", "land_m2", "water_l", "eutro_g_po4eq")) %>%
  select(scenario, reduction, scenario_model, replacement_target, indicator, avoided_value, avoided_pct_of_baseline) %>%
  pivot_wider(
    names_from = indicator,
    values_from = c(avoided_value, avoided_pct_of_baseline),
    names_glue = "{indicator}_{.value}"
  ) %>%
  arrange(reduction, scenario_model, replacement_target)
write_csv(scenario_summary, file.path(out_dir, "env_impact_scenarios_upf_summary.csv"))
write_csv(scenario_summary, file.path(out_dir, "env_impact_comparable_scenarios_upf_summary.csv"))

# ------------------------------------------------------------------------------
# Expanded beverages sensitivity (from user-provided expanded coefficient mapping)
# ------------------------------------------------------------------------------

if (file.exists(coeff_expanded_path) && file.exists(bridge_expanded_path)) {
  coeff_expanded <- read_csv(coeff_expanded_path, show_col_types = FALSE) %>%
    transmute(
      env_group_expanded = as.character(env_group),
      ghg_kg_co2eq_per_kg_exp = as.numeric(ghg_kg_co2eq_per_kg),
      land_m2_per_kg_exp = as.numeric(land_m2_per_kg),
      water_l_per_kg_exp = as.numeric(water_l_per_kg),
      eutro_g_po4eq_per_kg_exp = as.numeric(eutro_g_po4eq_per_kg),
      coefficient_factor_fw_rw_pct_exp = as.numeric(coefficient_factor_fw_rw_pct),
      coef_ratio_exp = if_else(
        !is.na(coefficient_factor_fw_rw_pct_exp) & coefficient_factor_fw_rw_pct_exp > 0,
        coefficient_factor_fw_rw_pct_exp / 100,
        1
      ),
      source_expanded = as.character(source),
      status_expanded = as.character(status)
    ) %>%
    distinct(env_group_expanded, .keep_all = TRUE)

  bridge_expanded <- read_csv(bridge_expanded_path, show_col_types = FALSE) %>%
    transmute(
      env_group_extended = as.character(env_group_extended),
      env_group_expanded_bridge = as.character(env_group_expanded)
    )

  ali_exp <- ali_long %>%
    left_join(bridge_expanded, by = "env_group_extended") %>%
    mutate(env_group_expanded = dplyr::coalesce(env_group_comp, env_group_expanded_bridge)) %>%
    left_join(coeff_expanded, by = c("env_group_expanded" = "env_group_expanded")) %>%
    mutate(
      mapped_expanded = !is.na(env_group_expanded) & !is.na(ghg_kg_co2eq_per_kg_exp),
      kg_ref_exp = if_else(mapped_expanded & coef_ratio_exp > 0, kg_retail / coef_ratio_exp, 0),
      ghg_kg_co2eq_exp = if_else(mapped_expanded, kg_ref_exp * ghg_kg_co2eq_per_kg_exp, 0),
      land_m2_exp = if_else(mapped_expanded, kg_ref_exp * land_m2_per_kg_exp, 0),
      water_l_exp = if_else(mapped_expanded, kg_ref_exp * water_l_per_kg_exp, 0),
      eutro_g_po4eq_exp = if_else(mapped_expanded, kg_ref_exp * eutro_g_po4eq_per_kg_exp, 0)
    )

  coverage_exp <- tibble(
    n_rows_positive = nrow(ali_exp),
    n_rows_mapped = sum(ali_exp$mapped_expanded, na.rm = TRUE),
    mapped_rows_pct = 100 * n_rows_mapped / n_rows_positive,
    grams_positive = sum(ali_exp$gramos, na.rm = TRUE),
    grams_mapped = sum(ali_exp$gramos[ali_exp$mapped_expanded], na.rm = TRUE),
    mapped_grams_pct = 100 * grams_mapped / grams_positive,
    energia_kcal_total = sum(ali_exp$energia_kcal, na.rm = TRUE),
    energia_kcal_mapped = sum(ali_exp$energia_kcal[ali_exp$mapped_expanded], na.rm = TRUE),
    mapped_energy_pct = 100 * energia_kcal_mapped / energia_kcal_total,
    n_unique_codes_consumed = dplyr::n_distinct(ali_exp$codigo),
    n_unique_codes_mapped = dplyr::n_distinct(ali_exp$codigo[ali_exp$mapped_expanded])
  )
  write_csv(coverage_exp, file.path(out_dir, "env_impact_expanded_coverage.csv"))

  coverage_exp_by_nova <- ali_exp %>%
    mutate(NOVA = if_else(is.na(NOVA), -1L, NOVA)) %>%
    group_by(NOVA) %>%
    summarise(
      n_rows = n(),
      n_rows_mapped = sum(mapped_expanded, na.rm = TRUE),
      mapped_rows_pct = 100 * n_rows_mapped / n_rows,
      grams_total = sum(gramos, na.rm = TRUE),
      grams_mapped = sum(gramos[mapped_expanded], na.rm = TRUE),
      mapped_grams_pct = 100 * grams_mapped / grams_total,
      energia_kcal_total = sum(energia_kcal, na.rm = TRUE),
      energia_kcal_mapped = sum(energia_kcal[mapped_expanded], na.rm = TRUE),
      mapped_energy_pct = 100 * energia_kcal_mapped / energia_kcal_total,
      .groups = "drop"
    ) %>%
    arrange(NOVA)
  write_csv(coverage_exp_by_nova, file.path(out_dir, "env_impact_expanded_coverage_by_nova.csv"))

  impact_cols_exp <- c("ghg_kg_co2eq_exp", "land_m2_exp", "water_l_exp", "eutro_g_po4eq_exp")
  indicator_map_exp <- tibble(
    indicator = c("ghg_kg_co2eq", "land_m2", "water_l", "eutro_g_po4eq"),
    col = impact_cols_exp
  )

  baseline_total_exp <- ali_exp %>%
    summarise(
      ghg_kg_co2eq = sum(ghg_kg_co2eq_exp, na.rm = TRUE),
      land_m2 = sum(land_m2_exp, na.rm = TRUE),
      water_l = sum(water_l_exp, na.rm = TRUE),
      eutro_g_po4eq = sum(eutro_g_po4eq_exp, na.rm = TRUE)
    )
  write_csv(baseline_total_exp, file.path(out_dir, "env_impact_expanded_baseline_total.csv"))

  by_nova_exp <- ali_exp %>%
    group_by(NOVA) %>%
    summarise(
      ghg_kg_co2eq = sum(ghg_kg_co2eq_exp, na.rm = TRUE),
      land_m2 = sum(land_m2_exp, na.rm = TRUE),
      water_l = sum(water_l_exp, na.rm = TRUE),
      eutro_g_po4eq = sum(eutro_g_po4eq_exp, na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(by_nova_exp, file.path(out_dir, "env_impact_expanded_by_nova.csv"))

  upf_component_exp <- ali_exp %>%
    filter(NOVA == 4) %>%
    summarise(
      upf_energy_mapped = sum(energia_kcal[mapped_expanded], na.rm = TRUE),
      upf_mass_kg_mapped = sum(gramos[mapped_expanded], na.rm = TRUE) / 1000,
      ghg_kg_co2eq = sum(ghg_kg_co2eq_exp, na.rm = TRUE),
      land_m2 = sum(land_m2_exp, na.rm = TRUE),
      water_l = sum(water_l_exp, na.rm = TRUE),
      eutro_g_po4eq = sum(eutro_g_po4eq_exp, na.rm = TRUE)
    )

  replacement_intensity_exp <- ali_exp %>%
    filter(mapped_expanded, NOVA %in% c(1L, 3L), energia_kcal > 0) %>%
    group_by(NOVA) %>%
    summarise(
      energy_kcal = sum(energia_kcal, na.rm = TRUE),
      mass_kg = sum(gramos, na.rm = TRUE) / 1000,
      ghg_kg_co2eq = sum(ghg_kg_co2eq_exp, na.rm = TRUE),
      land_m2 = sum(land_m2_exp, na.rm = TRUE),
      water_l = sum(water_l_exp, na.rm = TRUE),
      eutro_g_po4eq = sum(eutro_g_po4eq_exp, na.rm = TRUE),
      .groups = "drop"
    )

  if (all(c(1L, 3L) %in% replacement_intensity_exp$NOVA)) {
    repl_1_exp <- filter(replacement_intensity_exp, NOVA == 1L)
    repl_3_exp <- filter(replacement_intensity_exp, NOVA == 3L)
    mix_w1_exp <- repl_1_exp$energy_kcal / (repl_1_exp$energy_kcal + repl_3_exp$energy_kcal)
    mix_w3_exp <- 1 - mix_w1_exp

    build_rows_exp <- function(sc, r, model, target) {
      bind_rows(lapply(seq_len(nrow(indicator_map_exp)), function(i) {
        ind <- indicator_map_exp$indicator[i]
        col_nm <- indicator_map_exp$col[i]
        base_v <- baseline_total_exp[[ind]][1]
        upf_v <- upf_component_exp[[ind]][1]
        gross_removed <- r * upf_v
        reduced_upf_energy <- r * upf_component_exp$upf_energy_mapped[1]
        reduced_upf_mass <- r * upf_component_exp$upf_mass_kg_mapped[1]

        replacement_added <- 0
        if (model == "isocaloric_replacement") {
          i1 <- repl_1_exp[[ind]][1] / repl_1_exp$energy_kcal[1]
          i3 <- repl_3_exp[[ind]][1] / repl_3_exp$energy_kcal[1]
          intensity <- case_when(
            target == "NOVA1" ~ i1,
            target == "NOVA3" ~ i3,
            target == "NOVA1_NOVA3_mix" ~ mix_w1_exp * i1 + mix_w3_exp * i3,
            TRUE ~ NA_real_
          )
          replacement_added <- reduced_upf_energy * intensity
        } else if (model == "isoweight_replacement") {
          i1 <- repl_1_exp[[ind]][1] / repl_1_exp$mass_kg[1]
          i3 <- repl_3_exp[[ind]][1] / repl_3_exp$mass_kg[1]
          intensity <- case_when(
            target == "NOVA1" ~ i1,
            target == "NOVA3" ~ i3,
            target == "NOVA1_NOVA3_mix" ~ mix_w1_exp * i1 + mix_w3_exp * i3,
            TRUE ~ NA_real_
          )
          replacement_added <- reduced_upf_mass * intensity
        }

        net_avoided <- gross_removed - replacement_added
        tibble(
          scenario = sc,
          reduction = r,
          scenario_model = model,
          replacement_target = target,
          indicator = ind,
          baseline_value = base_v,
          upf_mapped_component = upf_v,
          gross_removed_value = gross_removed,
          replacement_added_value = replacement_added,
          avoided_value = net_avoided,
          post_value = base_v - net_avoided,
          avoided_pct_of_baseline = ifelse(base_v > 0, 100 * net_avoided / base_v, NA_real_)
        )
      }))
    }

    rows_no_replacement_exp <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
      build_rows_exp(scenarios$scenario[i], scenarios$reduction[i], "no_replacement", "none")
    }))

    rows_isocaloric_exp <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
      bind_rows(
        build_rows_exp(scenarios$scenario[i], scenarios$reduction[i], "isocaloric_replacement", "NOVA1"),
        build_rows_exp(scenarios$scenario[i], scenarios$reduction[i], "isocaloric_replacement", "NOVA3"),
        build_rows_exp(scenarios$scenario[i], scenarios$reduction[i], "isocaloric_replacement", "NOVA1_NOVA3_mix")
      )
    }))

    rows_isoweight_exp <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
      bind_rows(
        build_rows_exp(scenarios$scenario[i], scenarios$reduction[i], "isoweight_replacement", "NOVA1"),
        build_rows_exp(scenarios$scenario[i], scenarios$reduction[i], "isoweight_replacement", "NOVA3"),
        build_rows_exp(scenarios$scenario[i], scenarios$reduction[i], "isoweight_replacement", "NOVA1_NOVA3_mix")
      )
    }))

    scenario_tbl_exp <- bind_rows(rows_no_replacement_exp, rows_isocaloric_exp, rows_isoweight_exp)
    write_csv(scenario_tbl_exp, file.path(out_dir, "env_impact_expanded_scenarios_upf.csv"))

    scenario_summary_exp <- scenario_tbl_exp %>%
      select(scenario, reduction, scenario_model, replacement_target, indicator, avoided_value, avoided_pct_of_baseline) %>%
      pivot_wider(
        names_from = indicator,
        values_from = c(avoided_value, avoided_pct_of_baseline),
        names_glue = "{indicator}_{.value}"
      ) %>%
      arrange(reduction, scenario_model, replacement_target)
    write_csv(scenario_summary_exp, file.path(out_dir, "env_impact_expanded_scenarios_upf_summary.csv"))

    # --------------------------------------------------------------------------
    # Expanded-curated principal model:
    # - keeps expanded coefficients for better food/beverage coverage
    # - excludes plain drinking water / ice from impact scope
    # - applies beverage-specific fallback rules for AGB_* groups
    # --------------------------------------------------------------------------
    coeff_cur <- coeff_expanded %>%
      transmute(
        env_group_curated = env_group_expanded,
        ghg_kg_co2eq_per_kg_cur = ghg_kg_co2eq_per_kg_exp,
        land_m2_per_kg_cur = land_m2_per_kg_exp,
        water_l_per_kg_cur = water_l_per_kg_exp,
        eutro_g_po4eq_per_kg_cur = eutro_g_po4eq_per_kg_exp,
        coef_ratio_cur = coef_ratio_exp,
        source_cur = source_expanded
      )

    ali_cur <- ali_exp %>%
      mutate(
        desc_norm = iconv(as.character(descripcion), from = "", to = "ASCII//TRANSLIT"),
        desc_norm = if_else(is.na(desc_norm), "", desc_norm),
        desc_norm = tolower(desc_norm),
        desc_norm = gsub("[^a-z0-9 ]", " ", desc_norm),
        desc_norm = gsub("\\s+", " ", desc_norm),
        is_plain_water = env_group_extended == "AGB_eaux" &
          grepl("agua de red|agua corriente|agua embotellada|agua de pozo|agua de perforacion|agua origen desconocido|agua de lluvia|rio|canal|arroyo|acequia|agua con gas|soda|hielo", desc_norm) &
          !grepl("saborizada|gaseosa|tonica|aquarius|levite|h2o|jugo|energizante|powerade|gatorade|fernet|cerveza|vino", desc_norm),
        env_group_curated = case_when(
          !is.na(env_group_comp) ~ env_group_comp,
          env_group_extended == "AGB_eaux" & is_plain_water ~ NA_character_,
          env_group_extended == "AGB_eaux" ~ "Flavoured_water_drink",
          env_group_extended == "AGB_boissons_sans_alcool" & grepl("yerba|mate", desc_norm) ~ "Mate_yerba",
          env_group_extended == "AGB_boissons_sans_alcool" & grepl("te|infusion|manzanilla|boldo", desc_norm) ~ "Tea_infusions",
          env_group_extended == "AGB_boissons_sans_alcool" & grepl("cafe", desc_norm) & grepl("polvo|granulado|capsula|saquito|hebras|para preparar", desc_norm) ~ "Coffee_OWID",
          env_group_extended == "AGB_boissons_sans_alcool" & grepl("cafe", desc_norm) ~ "Tea_infusions",
          env_group_extended == "AGB_boissons_sans_alcool" & grepl("jugo|zumo", desc_norm) ~ "Fruit_juice_industrial",
          env_group_extended == "AGB_boissons_sans_alcool" ~ "Soft_drinks_carbonated",
          env_group_extended == "AGB_boisson_alcoolisees" & grepl("vino|sidra|champagne|vermouth", desc_norm) ~ "Wine_OWID",
          env_group_extended == "AGB_boisson_alcoolisees" & grepl("cerveza|beer", desc_norm) ~ "Beer",
          env_group_extended == "AGB_boisson_alcoolisees" ~ "Spirits_distilled",
          env_group_extended == "AGB_confiseries" ~ "Confectionery_candy",
          env_group_extended == "AGB_glaces" ~ "Ice_cream",
          TRUE ~ env_group_expanded
        ),
        in_scope_curated = !is_plain_water
      ) %>%
      select(-ghg_kg_co2eq_per_kg_exp, -land_m2_per_kg_exp, -water_l_per_kg_exp, -eutro_g_po4eq_per_kg_exp, -coef_ratio_exp, -source_expanded, -status_expanded) %>%
      left_join(coeff_cur, by = "env_group_curated") %>%
      mutate(
        mapped_curated = in_scope_curated & !is.na(env_group_curated) & !is.na(ghg_kg_co2eq_per_kg_cur),
        kg_ref_cur = if_else(mapped_curated & coef_ratio_cur > 0, kg_retail / coef_ratio_cur, 0),
        ghg_kg_co2eq_cur = if_else(mapped_curated, kg_ref_cur * ghg_kg_co2eq_per_kg_cur, 0),
        land_m2_cur = if_else(mapped_curated, kg_ref_cur * land_m2_per_kg_cur, 0),
        water_l_cur = if_else(mapped_curated, kg_ref_cur * water_l_per_kg_cur, 0),
        eutro_g_po4eq_cur = if_else(mapped_curated, kg_ref_cur * eutro_g_po4eq_per_kg_cur, 0)
      )

    coverage_cur <- tibble(
      n_rows_positive = nrow(ali_cur),
      n_rows_in_scope = sum(ali_cur$in_scope_curated, na.rm = TRUE),
      n_rows_mapped_in_scope = sum(ali_cur$mapped_curated, na.rm = TRUE),
      mapped_rows_pct_in_scope = 100 * n_rows_mapped_in_scope / n_rows_in_scope,
      grams_positive_all = sum(ali_cur$gramos, na.rm = TRUE),
      grams_in_scope = sum(ali_cur$gramos[ali_cur$in_scope_curated], na.rm = TRUE),
      grams_mapped_in_scope = sum(ali_cur$gramos[ali_cur$mapped_curated], na.rm = TRUE),
      mapped_grams_pct_in_scope = 100 * grams_mapped_in_scope / grams_in_scope,
      mapped_grams_pct_all = 100 * grams_mapped_in_scope / grams_positive_all,
      energia_kcal_total_all = sum(ali_cur$energia_kcal, na.rm = TRUE),
      energia_kcal_in_scope = sum(ali_cur$energia_kcal[ali_cur$in_scope_curated], na.rm = TRUE),
      energia_kcal_mapped_in_scope = sum(ali_cur$energia_kcal[ali_cur$mapped_curated], na.rm = TRUE),
      mapped_energy_pct_in_scope = 100 * energia_kcal_mapped_in_scope / energia_kcal_in_scope,
      mapped_energy_pct_all = 100 * energia_kcal_mapped_in_scope / energia_kcal_total_all,
      excluded_plain_water_grams = sum(ali_cur$gramos[ali_cur$is_plain_water], na.rm = TRUE),
      excluded_plain_water_grams_pct_all = 100 * excluded_plain_water_grams / grams_positive_all,
      excluded_plain_water_energy_kcal = sum(ali_cur$energia_kcal[ali_cur$is_plain_water], na.rm = TRUE),
      excluded_plain_water_energy_pct_all = 100 * excluded_plain_water_energy_kcal / energia_kcal_total_all
    )
    write_csv(coverage_cur, file.path(out_dir, "env_impact_curated_coverage.csv"))
    write_csv(coverage_cur, file.path(out_dir, "env_impact_mapping_coverage.csv"))

    coverage_cur_by_nova <- ali_cur %>%
      mutate(NOVA = if_else(is.na(NOVA), -1L, NOVA)) %>%
      group_by(NOVA) %>%
      summarise(
        n_rows = n(),
        n_rows_in_scope = sum(in_scope_curated, na.rm = TRUE),
        n_rows_mapped_in_scope = sum(mapped_curated, na.rm = TRUE),
        mapped_rows_pct_in_scope = 100 * n_rows_mapped_in_scope / n_rows_in_scope,
        grams_total_all = sum(gramos, na.rm = TRUE),
        grams_in_scope = sum(gramos[in_scope_curated], na.rm = TRUE),
        grams_mapped_in_scope = sum(gramos[mapped_curated], na.rm = TRUE),
        mapped_grams_pct_in_scope = 100 * grams_mapped_in_scope / grams_in_scope,
        mapped_grams_pct_all = 100 * grams_mapped_in_scope / grams_total_all,
        energia_kcal_total_all = sum(energia_kcal, na.rm = TRUE),
        energia_kcal_in_scope = sum(energia_kcal[in_scope_curated], na.rm = TRUE),
        energia_kcal_mapped_in_scope = sum(energia_kcal[mapped_curated], na.rm = TRUE),
        mapped_energy_pct_in_scope = 100 * energia_kcal_mapped_in_scope / energia_kcal_in_scope,
        mapped_energy_pct_all = 100 * energia_kcal_mapped_in_scope / energia_kcal_total_all,
        excluded_plain_water_grams = sum(gramos[is_plain_water], na.rm = TRUE),
        excluded_plain_water_grams_pct_all = 100 * excluded_plain_water_grams / grams_total_all,
        .groups = "drop"
      ) %>%
      arrange(NOVA)
    write_csv(coverage_cur_by_nova, file.path(out_dir, "env_impact_curated_coverage_by_nova.csv"))
    write_csv(coverage_cur_by_nova, file.path(out_dir, "env_impact_mapping_coverage_by_nova.csv"))

    baseline_total_cur <- ali_cur %>%
      summarise(
        ghg_kg_co2eq = sum(ghg_kg_co2eq_cur, na.rm = TRUE),
        land_m2 = sum(land_m2_cur, na.rm = TRUE),
        water_l = sum(water_l_cur, na.rm = TRUE),
        eutro_g_po4eq = sum(eutro_g_po4eq_cur, na.rm = TRUE)
      )
    write_csv(baseline_total_cur, file.path(out_dir, "env_impact_curated_baseline_total.csv"))
    write_csv(baseline_total_cur, file.path(out_dir, "env_impact_baseline_total.csv"))

    by_nova_cur <- ali_cur %>%
      group_by(NOVA) %>%
      summarise(
        ghg_kg_co2eq = sum(ghg_kg_co2eq_cur, na.rm = TRUE),
        land_m2 = sum(land_m2_cur, na.rm = TRUE),
        water_l = sum(water_l_cur, na.rm = TRUE),
        eutro_g_po4eq = sum(eutro_g_po4eq_cur, na.rm = TRUE),
        .groups = "drop"
      )
    write_csv(by_nova_cur, file.path(out_dir, "env_impact_curated_by_nova.csv"))
    write_csv(by_nova_cur, file.path(out_dir, "env_impact_by_nova.csv"))

    # Main table for manuscript/reporting: totals + shares.
    totals_shares_by_nova_cur <- by_nova_cur %>%
      mutate(
        component = format_nova_label(NOVA),
        ghg_share_pct = safe_pct(ghg_kg_co2eq, baseline_total_cur$ghg_kg_co2eq[1]),
        land_share_pct = safe_pct(land_m2, baseline_total_cur$land_m2[1]),
        water_share_pct = safe_pct(water_l, baseline_total_cur$water_l[1]),
        eutro_share_pct = safe_pct(eutro_g_po4eq, baseline_total_cur$eutro_g_po4eq[1])
      ) %>%
      left_join(
        coverage_cur_by_nova %>%
          transmute(
            NOVA,
            energy_kcal_mapped_denominator = energia_kcal_mapped_in_scope,
            mapped_energy_pct_in_scope
          ),
        by = "NOVA"
      ) %>%
      select(
        component, NOVA, energy_kcal_mapped_denominator, mapped_energy_pct_in_scope,
        ghg_kg_co2eq, ghg_share_pct,
        land_m2, land_share_pct,
        water_l, water_share_pct,
        eutro_g_po4eq, eutro_share_pct
      ) %>%
      arrange(NOVA)

    totals_shares_total_cur <- tibble(
      component = "TOTAL_DIET_MAPPED_SCOPE",
      NOVA = NA_integer_,
      energy_kcal_mapped_denominator = coverage_cur$energia_kcal_mapped_in_scope[1],
      mapped_energy_pct_in_scope = coverage_cur$mapped_energy_pct_in_scope[1],
      ghg_kg_co2eq = baseline_total_cur$ghg_kg_co2eq[1],
      ghg_share_pct = 100,
      land_m2 = baseline_total_cur$land_m2[1],
      land_share_pct = 100,
      water_l = baseline_total_cur$water_l[1],
      water_share_pct = 100,
      eutro_g_po4eq = baseline_total_cur$eutro_g_po4eq[1],
      eutro_share_pct = 100
    )

    env_table_main_totals_shares_cur <- bind_rows(totals_shares_total_cur, totals_shares_by_nova_cur)
    write_csv(env_table_main_totals_shares_cur, file.path(out_dir, "env_impact_curated_table_main_totals_shares.csv"))
    write_csv(env_table_main_totals_shares_cur, file.path(out_dir, "env_impact_table_main_totals_shares.csv"))

    # Secondary table/figure: intensity per 1000 kcal.
    # Denominator uses mapped kcal in scope (same coverage as impact numerator).
    env_table_secondary_intensity_cur <- env_table_main_totals_shares_cur %>%
      mutate(
        ghg_kg_co2eq_per_1000kcal_mapped = safe_per_1000_kcal(ghg_kg_co2eq, energy_kcal_mapped_denominator),
        land_m2_per_1000kcal_mapped = safe_per_1000_kcal(land_m2, energy_kcal_mapped_denominator),
        water_l_per_1000kcal_mapped = safe_per_1000_kcal(water_l, energy_kcal_mapped_denominator),
        eutro_g_po4eq_per_1000kcal_mapped = safe_per_1000_kcal(eutro_g_po4eq, energy_kcal_mapped_denominator),
        denominator_definition = "mapped_energy_kcal_in_scope"
      ) %>%
      select(
        component, NOVA, denominator_definition, energy_kcal_mapped_denominator, mapped_energy_pct_in_scope,
        ghg_kg_co2eq_per_1000kcal_mapped,
        land_m2_per_1000kcal_mapped,
        water_l_per_1000kcal_mapped,
        eutro_g_po4eq_per_1000kcal_mapped
      )
    write_csv(env_table_secondary_intensity_cur, file.path(out_dir, "env_impact_curated_table_secondary_intensity_1000kcal.csv"))
    write_csv(env_table_secondary_intensity_cur, file.path(out_dir, "env_impact_table_secondary_intensity_1000kcal.csv"))

    env_table_secondary_intensity_cur_long <- env_table_secondary_intensity_cur %>%
      pivot_longer(
        cols = c(
          ghg_kg_co2eq_per_1000kcal_mapped,
          land_m2_per_1000kcal_mapped,
          water_l_per_1000kcal_mapped,
          eutro_g_po4eq_per_1000kcal_mapped
        ),
        names_to = "indicator",
        values_to = "intensity_per_1000kcal_mapped"
      )
    write_csv(env_table_secondary_intensity_cur_long, file.path(out_dir, "env_impact_table_secondary_intensity_1000kcal_long.csv"))

    by_env_group_cur <- ali_cur %>%
      filter(mapped_curated) %>%
      group_by(env_group = env_group_curated) %>%
      summarise(
        ghg_kg_co2eq = sum(ghg_kg_co2eq_cur, na.rm = TRUE),
        land_m2 = sum(land_m2_cur, na.rm = TRUE),
        water_l = sum(water_l_cur, na.rm = TRUE),
        eutro_g_po4eq = sum(eutro_g_po4eq_cur, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(ghg_kg_co2eq))
    write_csv(by_env_group_cur, file.path(out_dir, "env_impact_curated_by_env_group.csv"))
    write_csv(by_env_group_cur, file.path(out_dir, "env_impact_by_env_group.csv"))

    by_strata_cur <- ali_cur %>%
      group_by(sexo_m, age_group) %>%
      summarise(
        grams_total = sum(gramos[in_scope_curated], na.rm = TRUE),
        grams_mapped = sum(gramos[mapped_curated], na.rm = TRUE),
        mapped_grams_pct = 100 * grams_mapped / grams_total,
        energia_kcal_total = sum(energia_kcal[in_scope_curated], na.rm = TRUE),
        energia_kcal_mapped = sum(energia_kcal[mapped_curated], na.rm = TRUE),
        mapped_energy_pct = 100 * energia_kcal_mapped / energia_kcal_total,
        ghg_kg_co2eq = sum(ghg_kg_co2eq_cur, na.rm = TRUE),
        land_m2 = sum(land_m2_cur, na.rm = TRUE),
        water_l = sum(water_l_cur, na.rm = TRUE),
        eutro_g_po4eq = sum(eutro_g_po4eq_cur, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(sexo_m, age_group)
    write_csv(by_strata_cur, file.path(out_dir, "env_impact_curated_by_strata.csv"))
    write_csv(by_strata_cur, file.path(out_dir, "env_impact_by_strata.csv"))

    upf_component_cur <- ali_cur %>%
      filter(NOVA == 4) %>%
      summarise(
        upf_energy_mapped = sum(energia_kcal[mapped_curated], na.rm = TRUE),
        upf_mass_kg_mapped = sum(gramos[mapped_curated], na.rm = TRUE) / 1000,
        ghg_kg_co2eq = sum(ghg_kg_co2eq_cur, na.rm = TRUE),
        land_m2 = sum(land_m2_cur, na.rm = TRUE),
        water_l = sum(water_l_cur, na.rm = TRUE),
        eutro_g_po4eq = sum(eutro_g_po4eq_cur, na.rm = TRUE)
      )

    replacement_intensity_cur <- ali_cur %>%
      filter(mapped_curated, NOVA %in% c(1L, 3L), energia_kcal > 0) %>%
      group_by(NOVA) %>%
      summarise(
        energy_kcal = sum(energia_kcal, na.rm = TRUE),
        mass_kg = sum(gramos, na.rm = TRUE) / 1000,
        ghg_kg_co2eq = sum(ghg_kg_co2eq_cur, na.rm = TRUE),
        land_m2 = sum(land_m2_cur, na.rm = TRUE),
        water_l = sum(water_l_cur, na.rm = TRUE),
        eutro_g_po4eq = sum(eutro_g_po4eq_cur, na.rm = TRUE),
        .groups = "drop"
      )

    if (all(c(1L, 3L) %in% replacement_intensity_cur$NOVA)) {
      repl_1_cur <- filter(replacement_intensity_cur, NOVA == 1L)
      repl_3_cur <- filter(replacement_intensity_cur, NOVA == 3L)
      mix_w1_cur <- repl_1_cur$energy_kcal / (repl_1_cur$energy_kcal + repl_3_cur$energy_kcal)
      mix_w3_cur <- 1 - mix_w1_cur

      gapa_cur_profile <- compute_weighted_group_intensity(
        df = ali_cur %>% filter(mapped_curated, NOVA != 4L),
        env_group_col = "env_group_curated",
        energy_col = "energia_kcal",
        indicator_map = c(
          ghg_kg_co2eq = "ghg_kg_co2eq_cur",
          land_m2 = "land_m2_cur",
          water_l = "water_l_cur",
          eutro_g_po4eq = "eutro_g_po4eq_cur"
        )
      )
      gapa_cur_intensity <- gapa_cur_profile$intensity
      gapa_cur_profile_mass <- compute_weighted_group_intensity(
        df = ali_cur %>% filter(mapped_curated, NOVA != 4L) %>% mutate(mass_kg = gramos / 1000),
        env_group_col = "env_group_curated",
        energy_col = "mass_kg",
        indicator_map = c(
          ghg_kg_co2eq = "ghg_kg_co2eq_cur",
          land_m2 = "land_m2_cur",
          water_l = "water_l_cur",
          eutro_g_po4eq = "eutro_g_po4eq_cur"
        )
      )
      gapa_cur_intensity_kg <- gapa_cur_profile_mass$intensity
      write_csv(
        mutate(gapa_cur_profile$weights_used, profile = "GAPA_ARG", context = "curated_principal"),
        file.path(out_dir, "env_replacement_gapa_weights_curated.csv")
      )
      write_csv(
        mutate(gapa_cur_profile_mass$weights_used, profile = "GAPA_ARG", context = "curated_principal_mass"),
        file.path(out_dir, "env_replacement_gapa_weights_curated_mass.csv")
      )
      message(
        "Perfil GAPA (curated) cobertura de pesos: ",
        round(gapa_cur_profile$weight_coverage_pct, 1),
        "%"
      )
      if (length(gapa_cur_profile$missing_groups) > 0) {
        message("Perfil GAPA (curated) grupos sin energia observada: ", paste(gapa_cur_profile$missing_groups, collapse = ", "))
      }

      indicators <- c("ghg_kg_co2eq", "land_m2", "water_l", "eutro_g_po4eq")
      build_rows_cur <- function(sc, r, model, target) {
        bind_rows(lapply(indicators, function(ind) {
          base_v <- baseline_total_cur[[ind]][1]
          upf_v <- upf_component_cur[[ind]][1]
          gross_removed <- r * upf_v
          reduced_upf_energy <- r * upf_component_cur$upf_energy_mapped[1]
          reduced_upf_mass <- r * upf_component_cur$upf_mass_kg_mapped[1]

          replacement_added <- 0
          if (model == "isocaloric_replacement") {
            i1 <- repl_1_cur[[ind]][1] / repl_1_cur$energy_kcal[1]
            i3 <- repl_3_cur[[ind]][1] / repl_3_cur$energy_kcal[1]
            intensity <- case_when(
              target == "NOVA1" ~ i1,
              target == "NOVA3" ~ i3,
              target == "NOVA1_NOVA3_mix" ~ mix_w1_cur * i1 + mix_w3_cur * i3,
              target == "GAPA_ARG" ~ gapa_cur_intensity[[ind]],
              TRUE ~ NA_real_
            )
            replacement_added <- reduced_upf_energy * intensity
          } else if (model == "isoweight_replacement") {
            i1 <- repl_1_cur[[ind]][1] / repl_1_cur$mass_kg[1]
            i3 <- repl_3_cur[[ind]][1] / repl_3_cur$mass_kg[1]
            intensity <- case_when(
              target == "NOVA1" ~ i1,
              target == "NOVA3" ~ i3,
              target == "NOVA1_NOVA3_mix" ~ mix_w1_cur * i1 + mix_w3_cur * i3,
              target == "GAPA_ARG" ~ gapa_cur_intensity_kg[[ind]],
              TRUE ~ NA_real_
            )
            replacement_added <- reduced_upf_mass * intensity
          }

          net_avoided <- gross_removed - replacement_added
          tibble(
            scenario = sc,
            reduction = r,
            scenario_model = model,
            replacement_target = target,
            indicator = ind,
            baseline_value = base_v,
            upf_mapped_component = upf_v,
            gross_removed_value = gross_removed,
            replacement_added_value = replacement_added,
            avoided_value = net_avoided,
            post_value = base_v - net_avoided,
            avoided_pct_of_baseline = ifelse(base_v > 0, 100 * net_avoided / base_v, NA_real_)
          )
        }))
      }

      rows_no_replacement_cur <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
        build_rows_cur(scenarios$scenario[i], scenarios$reduction[i], "no_replacement", "none")
      }))

      rows_isocaloric_cur <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
        sc <- scenarios$scenario[i]
        r <- scenarios$reduction[i]
        bind_rows(
          build_rows_cur(sc, r, "isocaloric_replacement", "NOVA1"),
          build_rows_cur(sc, r, "isocaloric_replacement", "NOVA3"),
          build_rows_cur(sc, r, "isocaloric_replacement", "NOVA1_NOVA3_mix"),
          build_rows_cur(sc, r, "isocaloric_replacement", "GAPA_ARG")
        )
      }))

      rows_isoweight_cur <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
        sc <- scenarios$scenario[i]
        r <- scenarios$reduction[i]
        bind_rows(
          build_rows_cur(sc, r, "isoweight_replacement", "NOVA1"),
          build_rows_cur(sc, r, "isoweight_replacement", "NOVA3"),
          build_rows_cur(sc, r, "isoweight_replacement", "NOVA1_NOVA3_mix"),
          build_rows_cur(sc, r, "isoweight_replacement", "GAPA_ARG")
        )
      }))

      scenario_tbl_cur <- bind_rows(rows_no_replacement_cur, rows_isocaloric_cur, rows_isoweight_cur)
      write_csv(scenario_tbl_cur, file.path(out_dir, "env_impact_curated_scenarios_upf.csv"))
      write_csv(scenario_tbl_cur, file.path(out_dir, "env_impact_scenarios_upf.csv"))

      scenario_summary_cur <- scenario_tbl_cur %>%
        select(scenario, reduction, scenario_model, replacement_target, indicator, avoided_value, avoided_pct_of_baseline) %>%
        pivot_wider(
          names_from = indicator,
          values_from = c(avoided_value, avoided_pct_of_baseline),
          names_glue = "{indicator}_{.value}"
        ) %>%
        arrange(reduction, scenario_model, replacement_target)
      write_csv(scenario_summary_cur, file.path(out_dir, "env_impact_curated_scenarios_upf_summary.csv"))
      write_csv(scenario_summary_cur, file.path(out_dir, "env_impact_scenarios_upf_summary.csv"))
    } else {
      message("Expanded-curated principal: no fue posible calcular escenarios isocaloricos (faltan NOVA1/NOVA3 mapeados).")
    }

    message("Guardado (expanded-curated principal): env_impact_curated_coverage.csv, env_impact_curated_coverage_by_nova.csv")
    message("Guardado (expanded-curated principal): env_impact_curated_baseline_total.csv, env_impact_curated_by_nova.csv")
    message("Guardado (expanded-curated principal): env_impact_curated_scenarios_upf.csv, env_impact_curated_scenarios_upf_summary.csv")
    message("Cobertura curated en alcance (gramos): ", round(coverage_cur$mapped_grams_pct_in_scope, 1), "%")
    message("Cobertura curated en alcance (energia): ", round(coverage_cur$mapped_energy_pct_in_scope, 1), "%")
    message("Masa excluida por agua simple (% gramos all): ", round(coverage_cur$excluded_plain_water_grams_pct_all, 1), "%")
  } else {
    message("Expanded sensitivity: no fue posible calcular escenarios isocaloricos (faltan NOVA1/NOVA3 mapeados).")
  }

  message("Guardado (expanded beverages): env_impact_expanded_coverage.csv, env_impact_expanded_coverage_by_nova.csv")
  message("Guardado (expanded beverages): env_impact_expanded_baseline_total.csv, env_impact_expanded_by_nova.csv")
  message("Guardado (expanded beverages): env_impact_expanded_scenarios_upf.csv, env_impact_expanded_scenarios_upf_summary.csv")
  message("Cobertura expanded (gramos, ponderada): ", round(coverage_exp$mapped_grams_pct, 1), "%")
  message("Cobertura expanded (energia, ponderada): ", round(coverage_exp$mapped_energy_pct, 1), "%")
} else {
  message("No se detectaron insumos de sensibilidad expanded (", coeff_expanded_path, " y/o ", bridge_expanded_path, "). Se omite corrida expanded.")
}

# ------------------------------------------------------------------------------
# True extended GHG-only outputs (weighted, separate)
# ------------------------------------------------------------------------------

coverage_ghg_ext <- tibble(
  n_rows_positive = nrow(ali_long),
  n_rows_mapped_ghg_ext = sum(ali_long$mapped_ghg_ext, na.rm = TRUE),
  mapped_rows_pct = 100 * n_rows_mapped_ghg_ext / n_rows_positive,
  grams_positive = sum(ali_long$gramos, na.rm = TRUE),
  grams_mapped_ghg_ext = sum(ali_long$gramos[ali_long$mapped_ghg_ext], na.rm = TRUE),
  mapped_grams_pct = 100 * grams_mapped_ghg_ext / grams_positive,
  energia_kcal_total = sum(ali_long$energia_kcal, na.rm = TRUE),
  energia_kcal_mapped_ghg_ext = sum(ali_long$energia_kcal[ali_long$mapped_ghg_ext], na.rm = TRUE),
  mapped_energy_pct = 100 * energia_kcal_mapped_ghg_ext / energia_kcal_total,
  n_unique_codes_consumed = dplyr::n_distinct(ali_long$codigo),
  n_unique_codes_mapped_ghg_ext = dplyr::n_distinct(ali_long$codigo[ali_long$mapped_ghg_ext])
)
write_csv(coverage_ghg_ext, file.path(out_dir, "env_impact_ghg_extended_coverage.csv"))

baseline_ghg_ext <- ali_long %>%
  summarise(ghg_kg_co2eq = sum(ghg_kg_co2eq_ext, na.rm = TRUE))
write_csv(baseline_ghg_ext, file.path(out_dir, "env_impact_ghg_extended_baseline_total.csv"))

by_nova_ghg_ext <- ali_long %>%
  group_by(NOVA) %>%
  summarise(ghg_kg_co2eq = sum(ghg_kg_co2eq_ext, na.rm = TRUE), .groups = "drop")
write_csv(by_nova_ghg_ext, file.path(out_dir, "env_impact_ghg_extended_by_nova.csv"))

upf_component_ghg_ext <- ali_long %>%
  filter(NOVA == 4) %>%
  summarise(
    upf_energy_mapped = sum(energia_kcal[mapped_ghg_ext], na.rm = TRUE),
    upf_mass_kg_mapped = sum(gramos[mapped_ghg_ext], na.rm = TRUE) / 1000,
    ghg_kg_co2eq = sum(ghg_kg_co2eq_ext, na.rm = TRUE)
  )

replacement_intensity_ghg_ext <- ali_long %>%
  filter(mapped_ghg_ext, NOVA %in% c(1L, 3L), energia_kcal > 0) %>%
  group_by(NOVA) %>%
  summarise(
    energy_kcal = sum(energia_kcal, na.rm = TRUE),
    mass_kg = sum(gramos, na.rm = TRUE) / 1000,
    ghg_kg_co2eq = sum(ghg_kg_co2eq_ext, na.rm = TRUE),
    .groups = "drop"
  )

if (!all(c(1L, 3L) %in% replacement_intensity_ghg_ext$NOVA)) {
  stop("No se pudieron calcular intensidades GHG extendidas para NOVA1 y NOVA3.")
}

repl_1_ext <- filter(replacement_intensity_ghg_ext, NOVA == 1L)
repl_3_ext <- filter(replacement_intensity_ghg_ext, NOVA == 3L)
mix_w1_ext <- repl_1_ext$energy_kcal / (repl_1_ext$energy_kcal + repl_3_ext$energy_kcal)
mix_w3_ext <- 1 - mix_w1_ext

build_rows_ghg_ext <- function(sc, r, model, target) {
  base_v <- baseline_ghg_ext$ghg_kg_co2eq[1]
  upf_v <- upf_component_ghg_ext$ghg_kg_co2eq[1]
  gross_removed <- r * upf_v
  reduced_upf_energy <- r * upf_component_ghg_ext$upf_energy_mapped[1]
  reduced_upf_mass <- r * upf_component_ghg_ext$upf_mass_kg_mapped[1]

  replacement_added <- 0
  if (model == "isocaloric_replacement") {
    i1 <- repl_1_ext$ghg_kg_co2eq[1] / repl_1_ext$energy_kcal[1]
    i3 <- repl_3_ext$ghg_kg_co2eq[1] / repl_3_ext$energy_kcal[1]
    intensity <- case_when(
      target == "NOVA1" ~ i1,
      target == "NOVA3" ~ i3,
      target == "NOVA1_NOVA3_mix" ~ mix_w1_ext * i1 + mix_w3_ext * i3,
      TRUE ~ NA_real_
    )
    replacement_added <- reduced_upf_energy * intensity
  } else if (model == "isoweight_replacement") {
    i1 <- repl_1_ext$ghg_kg_co2eq[1] / repl_1_ext$mass_kg[1]
    i3 <- repl_3_ext$ghg_kg_co2eq[1] / repl_3_ext$mass_kg[1]
    intensity <- case_when(
      target == "NOVA1" ~ i1,
      target == "NOVA3" ~ i3,
      target == "NOVA1_NOVA3_mix" ~ mix_w1_ext * i1 + mix_w3_ext * i3,
      TRUE ~ NA_real_
    )
    replacement_added <- reduced_upf_mass * intensity
  }

  net_avoided <- gross_removed - replacement_added
  tibble(
    scenario = sc,
    reduction = r,
    scenario_model = model,
    replacement_target = target,
    ghg_kg_co2eq_avoided_value = net_avoided,
    ghg_kg_co2eq_avoided_pct_of_baseline = ifelse(base_v > 0, 100 * net_avoided / base_v, NA_real_)
  )
}

ghg_ext_no_repl <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
  build_rows_ghg_ext(scenarios$scenario[i], scenarios$reduction[i], "no_replacement", "none")
}))
ghg_ext_iso <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
  bind_rows(
    build_rows_ghg_ext(scenarios$scenario[i], scenarios$reduction[i], "isocaloric_replacement", "NOVA1"),
    build_rows_ghg_ext(scenarios$scenario[i], scenarios$reduction[i], "isocaloric_replacement", "NOVA3"),
    build_rows_ghg_ext(scenarios$scenario[i], scenarios$reduction[i], "isocaloric_replacement", "NOVA1_NOVA3_mix")
  )
}))
ghg_ext_isoweight <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
  bind_rows(
    build_rows_ghg_ext(scenarios$scenario[i], scenarios$reduction[i], "isoweight_replacement", "NOVA1"),
    build_rows_ghg_ext(scenarios$scenario[i], scenarios$reduction[i], "isoweight_replacement", "NOVA3"),
    build_rows_ghg_ext(scenarios$scenario[i], scenarios$reduction[i], "isoweight_replacement", "NOVA1_NOVA3_mix")
  )
}))
ghg_ext_scenarios <- bind_rows(ghg_ext_no_repl, ghg_ext_iso, ghg_ext_isoweight) %>%
  arrange(reduction, scenario_model, replacement_target)
write_csv(ghg_ext_scenarios, file.path(out_dir, "env_impact_ghg_extended_scenarios_upf.csv"))

assumed_ratio_groups <- coeff_comp %>%
  filter(factor_origin == "assumed_ratio_1") %>%
  distinct(env_group) %>%
  pull(env_group)

message("Guardado (principal, expanded-curated): env_impact_mapping_coverage.csv, env_impact_mapping_coverage_by_nova.csv")
message("Guardado (principal, expanded-curated): env_impact_baseline_total.csv, env_impact_by_nova.csv, env_impact_by_env_group.csv, env_impact_by_strata.csv")
message("Guardado (principal, expanded-curated): env_impact_scenarios_upf.csv, env_impact_scenarios_upf_summary.csv")
message("Guardado (principal, nuevas tablas): env_impact_table_main_totals_shares.csv, env_impact_table_secondary_intensity_1000kcal.csv, env_impact_table_secondary_intensity_1000kcal_long.csv")
message("Guardado (sensibilidad comparable): env_impact_comparable_coverage.csv, env_impact_comparable_coverage_by_nova.csv")
message("Guardado (sensibilidad comparable): env_impact_comparable_baseline_total.csv, env_impact_comparable_by_nova.csv, env_impact_comparable_by_env_group.csv, env_impact_comparable_by_strata.csv")
message("Guardado (sensibilidad comparable): env_impact_comparable_scenarios_upf.csv, env_impact_comparable_scenarios_upf_summary.csv")
message("Guardado (true extended GHG, ponderado): env_impact_ghg_extended_coverage.csv, env_impact_ghg_extended_baseline_total.csv, env_impact_ghg_extended_by_nova.csv, env_impact_ghg_extended_scenarios_upf.csv")
if (exists("coverage_cur_by_nova")) {
  message("Cobertura UPF principal curated (energia en alcance): ", round(filter(coverage_cur_by_nova, NOVA == 4)$mapped_energy_pct_in_scope, 1), "%")
  message("Cobertura UPF principal curated (gramos en alcance): ", round(filter(coverage_cur_by_nova, NOVA == 4)$mapped_grams_pct_in_scope, 1), "%")
}
message("Cobertura UPF comparable (energia, ponderada): ", round(filter(coverage_by_nova, NOVA == 4)$mapped_energy_pct, 1), "%")
message("Cobertura UPF comparable (gramos, ponderada): ", round(filter(coverage_by_nova, NOVA == 4)$mapped_grams_pct, 1), "%")
message("Cobertura UPF con grupo extendido (energia, ponderada): ", round(filter(coverage_by_nova, NOVA == 4)$energy_with_extended_group_pct, 1), "%")
message("Cobertura UPF con grupo extendido (gramos, ponderada): ", round(filter(coverage_by_nova, NOVA == 4)$grams_with_extended_group_pct, 1), "%")
message("Cobertura GHG true-extended (energia, ponderada): ", round(coverage_ghg_ext$mapped_energy_pct, 1), "%")
message("Grupos con ratio asumido=1 (sin factor explicito): ", paste(assumed_ratio_groups, collapse = ", "))
