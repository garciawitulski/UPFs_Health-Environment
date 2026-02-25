# 09_build_env_mapping.R
# Build a reproducible mapping: ENNyS food code -> environmental group
# used by Arrieta et al. 2022 Table S4 Part I coefficients.
#
# This script does not impute environmental coefficients. It only maps codes
# to available groups with explicit rules and exports unmapped items for review.

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
ennys_dir <- file.path(root, "ENNyS")
env_dir <- file.path(root, "data", "environmental_footprints")
out_dir <- file.path(root, "output")
dir.create(env_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

library(readr)
library(dplyr)

nova_path <- file.path(ennys_dir, "alimentos_clasificados_NOVA.csv")
if (!file.exists(nova_path)) nova_path <- file.path(ennys_dir, "alimentos_clasificados_sin_suplementos.csv")
if (!file.exists(nova_path)) stop("No existe ENNyS/alimentos_clasificados_NOVA.csv")

coeff_path <- file.path(env_dir, "arrieta_2022_tableS4_partI_coefficients.csv")
if (!file.exists(coeff_path)) {
  stop("No existe data/environmental_footprints/arrieta_2022_tableS4_partI_coefficients.csv")
}

foods <- read_csv(nova_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  transmute(
    codigo = trimws(as.character(codigo)),
    descripcion = as.character(descripcion),
    NOVA = as.integer(NOVA)
  ) %>%
  filter(!is.na(codigo), !grepl("^S", codigo))

coeff <- read_csv(coeff_path, show_col_types = FALSE)
valid_groups <- unique(coeff$food_items)

norm_txt <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

pick <- function(group, rule) c(group, rule)

assign_group <- function(codigo, desc_norm) {
  p <- substr(codigo, 1, 1)
  if (is.na(p) || p == "") return(pick(NA_character_, "no_prefix"))

  # Direct prefix-group mappings with low ambiguity
  if (p == "H") return(pick("Eggs", "prefix_H"))
  if (p == "P") return(pick("Fish and seafood", "prefix_P"))
  if (p %in% c("L", "Q", "Y")) return(pick("Dairy products", paste0("prefix_", p)))
  if (p == "Z") return(pick("Oil crops", "prefix_Z"))

  # Fats and oils – G prefix contains both animal and vegetable fats
  if (p == "G") {
    if (grepl("crema|chantilly", desc_norm)) return(pick("Dairy products", "prefix_G_cream_dairy"))
    if (grepl("manteca", desc_norm)) return(pick("Dairy products", "prefix_G_butter_dairy"))
    if (grepl("grasa.*vacu", desc_norm)) return(pick("Beef", "prefix_G_beef_fat"))
    if (grepl("grasa.*cerdo", desc_norm)) return(pick("Pork", "prefix_G_lard"))
    if (grepl("grasa.*pollo", desc_norm)) return(pick("Poultry", "prefix_G_chicken_fat"))
    if (grepl("chicharron", desc_norm)) return(pick("Pork", "prefix_G_chicharron_pork"))
    return(pick("Oil crops", "prefix_G_default_oil"))
  }

  # Vegetables and starchy vegetables
  if (p == "V") {
    if (grepl("papa|batata|mandioca|yuca|boniato|camote", desc_norm)) {
      return(pick("Starchy vegetables", "prefix_V_starchy_keyword"))
    }
    return(pick("Vegetables (outdoor)", "prefix_V"))
  }

  # Fruits
  if (p == "F") return(pick("Fruits", "prefix_F"))

  # Meat block
  if (p == "C") {
    if (grepl("pollo|gallina|pavo|ave|suprema", desc_norm)) {
      return(pick("Poultry", "prefix_C_poultry_keyword"))
    }
    if (grepl("cerdo|pork|lechon|chancho", desc_norm)) {
      return(pick("Pork", "prefix_C_pork_keyword"))
    }
    if (grepl("cordero|ovino|caprino|chivo|chivito|cabrito|cabra|llama", desc_norm)) {
      return(pick("Lamb and mutton", "prefix_C_lamb_keyword"))
    }
    if (grepl("conejo|carpincho|nutria|vizcacha|ciervo|jabali", desc_norm)) {
      return(pick("Other meats", "prefix_C_game_keyword"))
    }
    if (grepl("jamon|bondiola|panceta|morcilla|mortadela|salame|salamin|salchichon|cantimpalo|paleta|choriz", desc_norm)) {
      return(pick("Pork", "prefix_C_pork_processed"))
    }
    if (grepl("hamburguesa|milanesa|matambre|picadillo|salch|embut|pate|fiambre|procesad", desc_norm)) {
      return(pick("Beef", "prefix_C_beef_processed"))
    }
    return(pick("Beef", "prefix_C_default_beef"))
  }

  # Mixed preparations (fast-food style)
  if (p == "R") {
    if (grepl("pollo", desc_norm)) return(pick("Poultry", "prefix_R_poultry"))
    if (grepl("cerdo", desc_norm)) return(pick("Pork", "prefix_R_pork"))
    if (grepl("hamburguesa|carne|jamon|salch|choriz", desc_norm)) {
      return(pick("Beef", "prefix_R_beef_preparation"))
    }
    if (grepl("pizza|empan|tarta|sandwich|pan|masa|prepizza|fajita|rapidita", desc_norm)) {
      return(pick("Baked products", "prefix_R_baked_preparation"))
    }
    return(pick(NA_character_, "prefix_R_unmapped"))
  }

  # Drinks and beverages (many items have no direct group in Table S4; map only clear cases)
  if (p == "B") {
    if (grepl("agua", desc_norm)) return(pick(NA_character_, "prefix_B_water_unmapped"))
    if (grepl("jugo|zumo|naranja|limon|pomelo|fruta exprim", desc_norm)) {
      return(pick("Fruits", "prefix_B_juice_to_fruits"))
    }
    if (grepl("vino", desc_norm)) return(pick("Fruits", "prefix_B_wine_to_fruits"))
    if (grepl("soja|soy milk|leche de soja", desc_norm)) {
      return(pick("Legumes and pulses", "prefix_B_soy_to_legumes"))
    }
    if (grepl("cafe|mate|yerba|\\bte\\b|infusion", desc_norm)) {
      return(pick(NA_character_, "prefix_B_infusion_unmapped"))
    }
    return(pick(NA_character_, "prefix_B_other_unmapped"))
  }

  # Sugars/sweets/desserts (map only with explicit ingredient proxies)
  if (p == "D") {
    if (grepl("helado|crema|flan|postre lacteo", desc_norm)) {
      return(pick("Dairy products", "prefix_D_dairy_dessert"))
    }
    if (grepl("chocolate|cacao", desc_norm)) {
      return(pick("Nuts and seeds", "prefix_D_cocoa_to_nuts"))
    }
    if (grepl("alfajor|gallet|torta|bizcocho", desc_norm)) {
      return(pick("Baked products", "prefix_D_baked_sweet"))
    }
    if (grepl("mermelada|dulce de fruta|jalea|membrillo", desc_norm)) {
      return(pick("Fruits", "prefix_D_fruit_preserve"))
    }
    if (grepl("azucar|miel|caramelo|jarabe|sirope", desc_norm)) {
      return(pick(NA_character_, "prefix_D_sugar_unmapped"))
    }
    return(pick(NA_character_, "prefix_D_other_unmapped"))
  }

  # Dressings/snacks/condiments
  if (p == "O") {
    if (grepl("mayonesa|aderezo|mostaza|vinagreta", desc_norm)) {
      return(pick("Oil crops", "prefix_O_dressing_oil_proxy"))
    }
    if (grepl("salsa de tomate|tomate triturado|pure de tomate|ketchup|pomarola", desc_norm)) {
      return(pick("Vegetables (outdoor)", "prefix_O_tomato_sauce"))
    }
    if (grepl("papas frit|snack|palito|copetin|nacho|chizito|chips", desc_norm)) {
      return(pick("Baked products", "prefix_O_snack_baked_proxy"))
    }
    if (grepl("mani|cacahuate|groundnut", desc_norm)) {
      return(pick("Nuts and seeds", "prefix_O_peanut"))
    }
    if (grepl("sal$", desc_norm) || grepl("^sal ", desc_norm)) {
      return(pick(NA_character_, "prefix_O_salt_unmapped"))
    }
    return(pick(NA_character_, "prefix_O_other_unmapped"))
  }

  # Cereals/baked/pasta/legumes/nuts
  if (p == "A") {
    if (grepl("arroz", desc_norm)) return(pick("Rice", "prefix_A_rice_keyword"))
    if (grepl("fideo|pasta|raviol|lasagna|canelon|n[o|o]qui|gnocchi|sorrentino|agnolotti|panzzotti", desc_norm)) {
      return(pick("Pasta", "prefix_A_pasta_keyword"))
    }
    if (grepl("lenteja|garbanzo|poroto|arveja|legumbre|soja", desc_norm)) {
      return(pick("Legumes and pulses", "prefix_A_legumes_keyword"))
    }
    if (grepl("nuez|almendra|mani|cacahu|avellana|pistacho|castan|semilla", desc_norm)) {
      return(pick("Nuts and seeds", "prefix_A_nuts_keyword"))
    }
    if (grepl("pan|gallet|factura|churro|torta|pizza|prepizza|rapidita|fajita|empanada|masa|grisin|bizcocho|rosquita|doughnut|dona|donut", desc_norm)) {
      return(pick("Baked products", "prefix_A_baked_keyword"))
    }
    return(pick("Cereals (without rice)", "prefix_A_default_cereals"))
  }

  # Conservative choice for remaining prefixes with high ambiguity
  pick(NA_character_, paste0("prefix_", p, "_unmapped"))
}

assigned <- lapply(seq_len(nrow(foods)), function(i) {
  assign_group(foods$codigo[i], norm_txt(foods$descripcion[i]))
})
assigned <- do.call(rbind, assigned)
colnames(assigned) <- c("env_group", "mapping_rule")

mapping <- bind_cols(foods, as.data.frame(assigned, stringsAsFactors = FALSE)) %>%
  mutate(
    in_coeff_table = !is.na(env_group) & env_group %in% valid_groups
  )

map_out <- file.path(env_dir, "ennys_codigo_to_env_group.csv")
write_csv(mapping, map_out)

unmapped <- mapping %>%
  filter(is.na(env_group) | !in_coeff_table) %>%
  arrange(codigo)
write_csv(unmapped, file.path(out_dir, "env_unmapped_codes.csv"))

summary_tbl <- mapping %>%
  mutate(prefix = substr(codigo, 1, 1), mapped = !is.na(env_group) & in_coeff_table) %>%
  group_by(prefix) %>%
  summarise(
    n_codes = n(),
    n_mapped = sum(mapped),
    mapped_pct = 100 * n_mapped / n_codes,
    .groups = "drop"
  ) %>%
  arrange(prefix)
write_csv(summary_tbl, file.path(out_dir, "env_mapping_summary_by_prefix.csv"))

global_summary <- tibble(
  n_codes_total = nrow(mapping),
  n_codes_mapped = sum(mapping$in_coeff_table, na.rm = TRUE),
  mapped_pct = 100 * n_codes_mapped / n_codes_total
)
write_csv(global_summary, file.path(out_dir, "env_mapping_summary_global.csv"))

message("Guardado mapping: ", map_out)
message("Guardado unmapped: ", file.path(out_dir, "env_unmapped_codes.csv"))
message("Cobertura codigos: ", round(global_summary$mapped_pct, 1), "%")
