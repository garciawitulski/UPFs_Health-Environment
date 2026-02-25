# 11_build_extended_coeffs_and_upf_coverage.R
# Build an extended coefficient base from explicit sources:
# - Arrieta et al. 2022 (Table S4 Part I)
# - OWID/Poore (selected products: sugar, wine, chocolate, coffee)
# - Agribalyse 3.1 (beverages/sweets subgroup medians)
# Then estimate UPF coverage reached by:
# - comparable mapping (Arrieta + OWID-compatible units)
# - extended mapping (adds Agribalyse subgroup coefficients with their own units)

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
ennys_dir <- file.path(root, "ENNyS")
env_dir <- file.path(root, "data", "environmental_footprints")
out_dir <- file.path(root, "output")
dir.create(env_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

library(readr)
library(dplyr)
library(tidyr)

norm_txt <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# ------------------------------------------------------------------------------
# 1) Build comparable coefficients (same core units used in current pipeline)
# ------------------------------------------------------------------------------

arrieta_path <- file.path(env_dir, "arrieta_2022_tableS4_partI_coefficients.csv")
if (!file.exists(arrieta_path)) {
  stop("Falta: data/environmental_footprints/arrieta_2022_tableS4_partI_coefficients.csv")
}

arrieta <- read_csv(arrieta_path, show_col_types = FALSE) %>%
  transmute(
    env_group = as.character(food_items),
    source = "Arrieta2022_TableS4",
    source_detail = "10.1007/s11625-021-01087-7 (MOESM1 Table S4 Part I)",
    ghg_kg_co2eq_per_kg = as.numeric(ghg_kg_co2eq_per_kg),
    land_m2_per_kg = as.numeric(land_occupation_m2_per_kg),
    water_l_per_kg = as.numeric(freshwater_consumption_l_per_kg),
    eutro_g_po4eq_per_kg = as.numeric(eutrophication_g_po4eq_per_kg)
  )

owid_files <- list(
  ghg = file.path(env_dir, "owid_poore_ghg_per_kg.csv"),
  land = file.path(env_dir, "owid_poore_land_use_per_kg.csv"),
  water = file.path(env_dir, "owid_poore_freshwater_withdrawals_per_kg.csv"),
  eutro = file.path(env_dir, "owid_poore_eutrophying_emissions_per_kg.csv")
)
if (!all(file.exists(unlist(owid_files)))) {
  stop("Faltan archivos OWID en data/environmental_footprints/")
}

owid_ghg <- read_csv(owid_files$ghg, show_col_types = FALSE)
owid_land <- read_csv(owid_files$land, show_col_types = FALSE)
owid_water <- read_csv(owid_files$water, show_col_types = FALSE)
owid_eutro <- read_csv(owid_files$eutro, show_col_types = FALSE)

owid <- owid_ghg %>%
  select(Entity, Year, ghg_kg_co2eq_per_kg = 3) %>%
  left_join(owid_land %>% select(Entity, Year, land_m2_per_kg = 3), by = c("Entity", "Year")) %>%
  left_join(owid_water %>% select(Entity, Year, water_l_per_kg = 3), by = c("Entity", "Year")) %>%
  left_join(owid_eutro %>% select(Entity, Year, eutro_g_po4eq_per_kg = 3), by = c("Entity", "Year"))

owid_pick <- function(entity) {
  x <- owid %>% filter(Entity == entity)
  if (nrow(x) == 0) return(NULL)
  x[1, c("ghg_kg_co2eq_per_kg", "land_m2_per_kg", "water_l_per_kg", "eutro_g_po4eq_per_kg")]
}

owid_sugar <- bind_rows(owid_pick("Cane Sugar"), owid_pick("Beet Sugar")) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

owid_rows <- bind_rows(
  tibble(
    env_group = "Sugars_OWID",
    source = "OWID_Poore2018",
    source_detail = "Mean of Cane Sugar + Beet Sugar",
    ghg_kg_co2eq_per_kg = owid_sugar$ghg_kg_co2eq_per_kg,
    land_m2_per_kg = owid_sugar$land_m2_per_kg,
    water_l_per_kg = owid_sugar$water_l_per_kg,
    eutro_g_po4eq_per_kg = owid_sugar$eutro_g_po4eq_per_kg
  ),
  tibble(
    env_group = "Wine_OWID",
    source = "OWID_Poore2018",
    source_detail = "Entity: Wine",
    ghg_kg_co2eq_per_kg = owid_pick("Wine")$ghg_kg_co2eq_per_kg,
    land_m2_per_kg = owid_pick("Wine")$land_m2_per_kg,
    water_l_per_kg = owid_pick("Wine")$water_l_per_kg,
    eutro_g_po4eq_per_kg = owid_pick("Wine")$eutro_g_po4eq_per_kg
  ),
  tibble(
    env_group = "Chocolate_OWID",
    source = "OWID_Poore2018",
    source_detail = "Entity: Dark Chocolate",
    ghg_kg_co2eq_per_kg = owid_pick("Dark Chocolate")$ghg_kg_co2eq_per_kg,
    land_m2_per_kg = owid_pick("Dark Chocolate")$land_m2_per_kg,
    water_l_per_kg = owid_pick("Dark Chocolate")$water_l_per_kg,
    eutro_g_po4eq_per_kg = owid_pick("Dark Chocolate")$eutro_g_po4eq_per_kg
  ),
  tibble(
    env_group = "Coffee_OWID",
    source = "OWID_Poore2018",
    source_detail = "Entity: Coffee",
    ghg_kg_co2eq_per_kg = owid_pick("Coffee")$ghg_kg_co2eq_per_kg,
    land_m2_per_kg = owid_pick("Coffee")$land_m2_per_kg,
    water_l_per_kg = owid_pick("Coffee")$water_l_per_kg,
    eutro_g_po4eq_per_kg = owid_pick("Coffee")$eutro_g_po4eq_per_kg
  )
)

coeff_comparable <- bind_rows(arrieta, owid_rows) %>%
  mutate(
    unit_ghg = "kg CO2-eq / kg product",
    unit_land = "m2 / kg product",
    unit_water = "L / kg product",
    unit_eutro = "g PO4-eq / kg product"
  )

write_csv(coeff_comparable, file.path(env_dir, "env_coefficients_comparable_4ind.csv"))

# ------------------------------------------------------------------------------
# 2) Build Agribalyse subgroup coefficients for beverages/sweets
# ------------------------------------------------------------------------------

agb_path <- file.path(env_dir, "agribalyse_31_synthese_raw.csv")
if (!file.exists(agb_path)) {
  stop("Falta: data/environmental_footprints/agribalyse_31_synthese_raw.csv")
}
agb <- read_csv(agb_path, show_col_types = FALSE)

name_col <- names(agb)[grepl("^Nom du Produit", names(agb))][1]
group_col <- "Groupe d'aliment"
sub_col <- "Sous-groupe d'aliment"
ghg_col <- "Changement climatique"
land_col <- "Utilisation du sol"
water_col <- names(agb)[grepl("ressources eau", names(agb), ignore.case = TRUE)][1]
eutro_col <- "Eutrophisation eaux douces"
energy_col <- names(agb)[grepl("ressources energ", norm_txt(names(agb)))] [1]

agb_sel <- agb %>%
  filter(grepl("boisson|sucr|glaces", norm_txt(.data[[group_col]])))

agb_sub <- agb_sel %>%
  mutate(
    group_norm = norm_txt(.data[[group_col]]),
    sub_norm = norm_txt(.data[[sub_col]])
  ) %>%
  group_by(group_norm, sub_norm) %>%
  summarise(
    ghg_kg_co2eq_per_kg = median(.data[[ghg_col]], na.rm = TRUE),
    land_pt_per_kg = median(.data[[land_col]], na.rm = TRUE),
    water_m3depriv_per_kg = median(.data[[water_col]], na.rm = TRUE),
    eutro_kg_p_eq_per_kg = median(.data[[eutro_col]], na.rm = TRUE),
    energy_mj_per_kg = if (!is.na(energy_col)) median(.data[[energy_col]], na.rm = TRUE) else NA_real_,
    n_products = n(),
    .groups = "drop"
  )

map_agb_subgroup <- function(sub_norm) {
  dplyr::case_when(
    grepl("boisson alcool", sub_norm) ~ "AGB_boisson_alcoolisees",
    grepl("^boissons? sans alcool", sub_norm) ~ "AGB_boissons_sans_alcool",
    grepl("^eaux?$", sub_norm) ~ "AGB_eaux",
    grepl("confiser", sub_norm) ~ "AGB_confiseries",
    grepl("chocolat", sub_norm) ~ "AGB_chocolats",
    grepl("confitur", sub_norm) ~ "AGB_confitures",
    grepl("sucres? miels?", sub_norm) ~ "AGB_sucres_miels",
    grepl("desserts? glac", sub_norm) ~ "AGB_desserts_glaces",
    grepl("^glaces?$", sub_norm) ~ "AGB_glaces",
    grepl("sorbets?", sub_norm) ~ "AGB_sorbets",
    TRUE ~ NA_character_
  )
}

coeff_agb <- agb_sub %>%
  mutate(env_group = map_agb_subgroup(sub_norm)) %>%
  filter(!is.na(env_group)) %>%
  transmute(
    env_group,
    source = "Agribalyse3.1_synthese_median",
    source_detail = paste0("group=", group_norm, "; subgroup=", sub_norm),
    ghg_kg_co2eq_per_kg,
    land_pt_per_kg,
    water_m3depriv_per_kg,
    eutro_kg_p_eq_per_kg,
    energy_mj_per_kg,
    n_products,
    unit_ghg = "kg CO2-eq / kg product",
    unit_land = "Pt / kg product",
    unit_water = "m3 depriv. / kg product",
    unit_eutro = "kg P-eq / kg product",
    unit_energy = "MJ / kg product"
  )

write_csv(coeff_agb, file.path(env_dir, "env_coefficients_agribalyse_subgroups.csv"))

coef_groups_comp <- unique(coeff_comparable$env_group)
coef_groups_ext <- unique(c(coef_groups_comp, coeff_agb$env_group))

# ------------------------------------------------------------------------------
# 3) Extended mapping ENNyS code -> coefficient groups
# ------------------------------------------------------------------------------

nova_path <- file.path(ennys_dir, "alimentos_clasificados_NOVA.csv")
if (!file.exists(nova_path)) nova_path <- file.path(ennys_dir, "alimentos_clasificados_sin_suplementos.csv")

foods <- read_csv(nova_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  transmute(
    codigo = trimws(as.character(codigo)),
    descripcion = as.character(descripcion),
    NOVA = as.integer(NOVA)
  ) %>%
  filter(!is.na(codigo), !grepl("^S", codigo))

base_map_path <- file.path(env_dir, "ennys_codigo_to_env_group.csv")
if (file.exists(base_map_path)) {
  base_map <- read_csv(base_map_path, show_col_types = FALSE) %>%
    transmute(codigo, env_group_arrieta = env_group)
} else {
  base_map <- foods %>% transmute(codigo, env_group_arrieta = NA_character_)
}

map_ext <- foods %>%
  left_join(base_map, by = "codigo") %>%
  mutate(desc_norm = norm_txt(descripcion))

map_extra <- function(codigo, desc_norm) {
  p <- substr(codigo, 1, 1)
  comp <- NA_character_
  ext <- NA_character_
  rule <- "none"

  if (p == "B") {
    if (grepl("agua", desc_norm)) {
      ext <- "AGB_eaux"; rule <- "B_water_to_agb_eaux"
    } else if (grepl("vino|sidra|champagne|vermouth", desc_norm)) {
      comp <- "Wine_OWID"; ext <- "AGB_boisson_alcoolisees"; rule <- "B_wine_to_owid_wine"
    } else if (grepl("cerveza|licor|fernet|gin|vodka|whisk|ron|tequila|pastis|alcohol|aperitif|aperitivo", desc_norm)) {
      ext <- "AGB_boisson_alcoolisees"; rule <- "B_alcohol_to_agb_alcohol"
    } else if (grepl("cafe", desc_norm)) {
      comp <- "Coffee_OWID"; ext <- "AGB_boissons_sans_alcool"; rule <- "B_coffee_to_owid_coffee"
    } else if (grepl("jugo|zumo|exprim", desc_norm)) {
      comp <- "Fruits"; ext <- "AGB_boissons_sans_alcool"; rule <- "B_juice_to_fruits"
    } else {
      ext <- "AGB_boissons_sans_alcool"; rule <- "B_other_to_agb_non_alcohol"
    }
  } else if (p == "D") {
    if (grepl("azucar|miel|jarabe|sirope|caramelo", desc_norm)) {
      comp <- "Sugars_OWID"; ext <- "AGB_sucres_miels"; rule <- "D_sugar_to_owid_sugars"
    } else if (grepl("chocolate|cacao", desc_norm)) {
      comp <- "Chocolate_OWID"; ext <- "AGB_chocolats"; rule <- "D_chocolate_to_owid_choc"
    } else if (grepl("mermelada|jalea|confit|membrillo|dulce de fruta", desc_norm)) {
      comp <- "Fruits"; ext <- "AGB_confitures"; rule <- "D_jam_to_fruits"
    } else if (grepl("helado|sorbete|gelatina|postre", desc_norm)) {
      ext <- "AGB_glaces"; rule <- "D_icecream_to_agb_glaces"
    } else if (grepl("alfajor|gallet|torta|bizcocho|barra", desc_norm)) {
      comp <- "Baked products"; ext <- "AGB_confiseries"; rule <- "D_bakedsweet_to_baked"
    } else {
      ext <- "AGB_confiseries"; rule <- "D_other_to_agb_confiseries"
    }
  } else if (p == "O") {
    if (grepl("sal$", desc_norm) || grepl("^sal ", desc_norm)) {
      rule <- "O_salt_unmapped"
    } else if (grepl("salsa de tomate|pure de tomate|tomate triturado|ketchup|pomarola", desc_norm)) {
      comp <- "Vegetables (outdoor)"; ext <- "AGB_confitures"; rule <- "O_tomato_sauce_to_veg"
    } else if (grepl("mayonesa|aderezo|vinagreta|mostaza", desc_norm)) {
      comp <- "Oil crops"; ext <- "AGB_boissons_sans_alcool"; rule <- "O_dressing_to_oil"
    } else if (grepl("snack|copetin|palito|chips|nacho|papa frit", desc_norm)) {
      comp <- "Baked products"; ext <- "AGB_confiseries"; rule <- "O_snack_to_baked"
    } else if (grepl("mani|cacahu", desc_norm)) {
      comp <- "Nuts and seeds"; ext <- "AGB_confiseries"; rule <- "O_peanut_to_nuts"
    } else {
      rule <- "O_other_unmapped"
    }
  } else if (p == "R") {
    if (grepl("pollo", desc_norm)) {
      comp <- "Poultry"; rule <- "R_poultry"
    } else if (grepl("cerdo", desc_norm)) {
      comp <- "Pork"; rule <- "R_pork"
    } else if (grepl("hamburguesa|carne|jamon|salch|choriz", desc_norm)) {
      comp <- "Beef"; rule <- "R_beef_preparation"
    } else if (grepl("pizza|empan|tarta|sandwich|pan|masa|prepizza|fajita|rapidita", desc_norm)) {
      comp <- "Baked products"; rule <- "R_baked_to_baked_products"
    } else {
      rule <- "R_other_unmapped"
    }
  }

  c(comp, ext, rule)
}

extra <- lapply(seq_len(nrow(map_ext)), function(i) map_extra(map_ext$codigo[i], map_ext$desc_norm[i]))
extra <- do.call(rbind, extra)
colnames(extra) <- c("env_group_comp_extra", "env_group_ext_extra", "extra_rule")
map_ext <- bind_cols(map_ext, as.data.frame(extra, stringsAsFactors = FALSE)) %>%
  mutate(
    env_group_comparable = coalesce(env_group_arrieta, env_group_comp_extra),
    env_group_extended = coalesce(env_group_comparable, env_group_ext_extra),
    mapping_level = case_when(
      !is.na(env_group_arrieta) ~ "base_arrieta",
      !is.na(env_group_comp_extra) ~ "extra_comparable",
      !is.na(env_group_ext_extra) ~ "extra_agribalyse",
      TRUE ~ "unmapped"
    )
  ) %>%
  select(codigo, descripcion, NOVA, env_group_arrieta, env_group_comparable, env_group_extended, mapping_level, extra_rule)

write_csv(map_ext, file.path(env_dir, "ennys_codigo_to_env_group_extended.csv"))

# ------------------------------------------------------------------------------
# 4) Coverage for UPFs (rows/grams/energy)
# ------------------------------------------------------------------------------

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
food_cols <- intersect(food_cols, map_ext$codigo)
usecols <- c("informe_id", "miembro_id", "clave", "F_STG_calib", food_cols)

ct <- cols(.default = col_character())
chunk_size <- 50000
chunk1 <- read_csv(path_ali, col_types = ct, n_max = chunk_size, show_col_types = FALSE)
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
  filter(if_any(all_of(setdiff(names(.), c("informe_id", "miembro_id", "clave", "F_STG_calib"))), ~ !is.na(.) & . != "" & . != "0"))

ali_long <- ali %>%
  pivot_longer(all_of(setdiff(names(ali), c("informe_id", "miembro_id", "clave", "F_STG_calib"))), names_to = "codigo", values_to = "gramos") %>%
  mutate(gramos = as.numeric(gramos)) %>%
  filter(!is.na(gramos), gramos > 0) %>%
  left_join(foods %>% select(codigo, NOVA), by = "codigo") %>%
  left_join(map_ext %>% select(codigo, env_group_comparable, env_group_extended), by = "codigo") %>%
  mutate(
    kcal_100g = unname(kcal_map[codigo]),
    energia_kcal = gramos * kcal_100g / 100,
    has_comp = !is.na(env_group_comparable) & env_group_comparable %in% coef_groups_comp,
    has_ext = !is.na(env_group_extended) & env_group_extended %in% coef_groups_ext
  )

coverage_fn <- function(df, label) {
  tibble(
    scope = label,
    n_rows = nrow(df),
    rows_comp = sum(df$has_comp, na.rm = TRUE),
    rows_comp_pct = 100 * rows_comp / n_rows,
    rows_ext = sum(df$has_ext, na.rm = TRUE),
    rows_ext_pct = 100 * rows_ext / n_rows,
    grams_total = sum(df$gramos, na.rm = TRUE),
    grams_comp = sum(df$gramos[df$has_comp], na.rm = TRUE),
    grams_comp_pct = 100 * grams_comp / grams_total,
    grams_ext = sum(df$gramos[df$has_ext], na.rm = TRUE),
    grams_ext_pct = 100 * grams_ext / grams_total,
    energy_total = sum(df$energia_kcal, na.rm = TRUE),
    energy_comp = sum(df$energia_kcal[df$has_comp], na.rm = TRUE),
    energy_comp_pct = 100 * energy_comp / energy_total,
    energy_ext = sum(df$energia_kcal[df$has_ext], na.rm = TRUE),
    energy_ext_pct = 100 * energy_ext / energy_total
  )
}

cov_all <- coverage_fn(ali_long, "all_codes")
cov_upf <- coverage_fn(filter(ali_long, NOVA == 4), "upf_only")
coverage <- bind_rows(cov_all, cov_upf)
write_csv(coverage, file.path(out_dir, "env_upf_coverage_extended.csv"))

coverage_upf_prefix <- ali_long %>%
  filter(NOVA == 4) %>%
  mutate(prefix = substr(codigo, 1, 1)) %>%
  group_by(prefix) %>%
  summarise(
    n_rows = n(),
    rows_comp_pct = 100 * mean(has_comp),
    rows_ext_pct = 100 * mean(has_ext),
    grams_total = sum(gramos, na.rm = TRUE),
    grams_comp_pct = 100 * sum(gramos[has_comp], na.rm = TRUE) / grams_total,
    grams_ext_pct = 100 * sum(gramos[has_ext], na.rm = TRUE) / grams_total,
    energy_total = sum(energia_kcal, na.rm = TRUE),
    energy_comp_pct = 100 * sum(energia_kcal[has_comp], na.rm = TRUE) / energy_total,
    energy_ext_pct = 100 * sum(energia_kcal[has_ext], na.rm = TRUE) / energy_total,
    .groups = "drop"
  ) %>%
  arrange(desc(grams_total))
write_csv(coverage_upf_prefix, file.path(out_dir, "env_upf_coverage_extended_by_prefix.csv"))

upf_code_coverage <- ali_long %>%
  filter(NOVA == 4) %>%
  group_by(codigo) %>%
  summarise(
    env_group_comparable = first(na.omit(env_group_comparable), default = NA_character_),
    env_group_extended = first(na.omit(env_group_extended), default = NA_character_),
    rows_n = n(),
    grams_total = sum(gramos, na.rm = TRUE),
    energy_total = sum(energia_kcal, na.rm = TRUE),
    has_comp = any(has_comp),
    has_ext = any(has_ext),
    .groups = "drop"
  ) %>%
  left_join(
    foods %>% select(codigo, descripcion) %>% distinct(codigo, .keep_all = TRUE),
    by = "codigo"
  ) %>%
  arrange(desc(grams_total))
write_csv(upf_code_coverage, file.path(out_dir, "env_upf_code_coverage_extended.csv"))

write_csv(
  filter(upf_code_coverage, !has_ext),
  file.path(out_dir, "env_upf_codes_without_coeff_extended.csv")
)

message("Guardado: data/environmental_footprints/env_coefficients_comparable_4ind.csv")
message("Guardado: data/environmental_footprints/env_coefficients_agribalyse_subgroups.csv")
message("Guardado: data/environmental_footprints/ennys_codigo_to_env_group_extended.csv")
message("Guardado: output/env_upf_coverage_extended.csv")
message("Guardado: output/env_upf_coverage_extended_by_prefix.csv")
message("Guardado: output/env_upf_code_coverage_extended.csv")
message("Guardado: output/env_upf_codes_without_coeff_extended.csv")
