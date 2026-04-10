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

source_label <- function(src) {
  case_when(
    src == "Arrieta2022_TableS4" ~ "Arrieta et al. 2022 (Table S4, Part I)",
    src == "OWID_Poore2018" ~ "OWID / Poore and Nemecek 2018",
    src == "Poore2018_retail" ~ "Poore and Nemecek 2018 (retail proxy)",
    src == "Agribalyse3.1_proxy" ~ "Agribalyse 3.1 proxy",
    src == "Poore2018_proxy" ~ "Poore and Nemecek 2018 proxy",
    src == "Estimated_Argentina" ~ "Argentina-specific estimate",
    src == "Composite_estimate" ~ "Composite estimate",
    src == "Minimal_impact" ~ "Minimal-impact assumption",
    TRUE ~ as.character(src)
  )
}

food_group_label <- function(x) {
  case_when(
    x == "Coffee_OWID" ~ "Coffee",
    x == "Sugars_OWID" ~ "Sugars",
    x == "Wine_OWID" ~ "Wine",
    x == "Soft_drinks_carbonated" ~ "Soft drinks (carbonated)",
    x == "Flavoured_water_drink" ~ "Flavoured water drink",
    x == "Confectionery_candy" ~ "Confectionery/candy",
    x == "Ice_cream" ~ "Ice cream",
    x == "Mate_yerba" ~ "Mate/yerba",
    x == "Tea_infusions" ~ "Tea/infusions",
    x == "Spirits_distilled" ~ "Spirits (distilled)",
    TRUE ~ as.character(x)
  )
}

boundary_label <- function(src, notes) {
  notes_low <- tolower(ifelse(is.na(notes), "", notes))
  case_when(
    src == "Arrieta2022_TableS4" ~ "Farm gate",
    grepl("farm-gate|farm gate", notes_low) ~ "Farm gate",
    src %in% c("OWID_Poore2018", "Poore2018_retail", "Agribalyse3.1_proxy", "Poore2018_proxy", "Estimated_Argentina", "Composite_estimate", "Minimal_impact") ~ "Retail/product",
    TRUE ~ "Proxy/mixed"
  )
}

coeff <- read_calc_csv("environmental_coefficients_by_group.csv")
used_groups <- read_calc_output_csv("env_impact_curated_by_env_group.csv") %>%
  transmute(env_group = as.character(env_group)) %>%
  filter(!is.na(env_group), env_group != "") %>%
  distinct()

tbl <- used_groups %>%
  left_join(
    coeff %>%
      transmute(
        env_group = as.character(env_group),
        ghg_kg_co2eq_per_kg = as.numeric(ghg_kg_co2eq_per_kg),
        land_m2_per_kg = as.numeric(land_m2_per_kg),
        water_l_per_kg = as.numeric(water_l_per_kg),
        eutro_g_po4eq_per_kg = as.numeric(eutro_g_po4eq_per_kg),
        coefficient_factor_fw_rw_pct = as.numeric(coefficient_factor_fw_rw_pct),
        source = as.character(source),
        notes = as.character(notes)
      ),
    by = "env_group"
  ) %>%
  mutate(
    system_boundary = boundary_label(source, notes),
    fw_rw_factor_pct = if_else(is.na(coefficient_factor_fw_rw_pct) | coefficient_factor_fw_rw_pct <= 0, 100, coefficient_factor_fw_rw_pct),
    source_specific = source_label(source),
    food_group = food_group_label(env_group)
  ) %>%
  arrange(food_group) %>%
  select(food_group, ghg_kg_co2eq_per_kg, land_m2_per_kg, water_l_per_kg, eutro_g_po4eq_per_kg, system_boundary, fw_rw_factor_pct, source_specific)

write_calc_csv(tbl, "table_s8_environmental_coefficients.csv")
message("Saved calculation output: table_s8_environmental_coefficients.csv")
