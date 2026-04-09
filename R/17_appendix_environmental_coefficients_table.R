# 17_appendix_environmental_coefficients_table.R
# Builds an appendix-ready table with environmental coefficients by food group
# used in the manuscript environmental model specification (curated run).

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
out_dir <- file.path(root, "output")
env_dir <- file.path(root, "data", "environmental_footprints")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

library(readr)
library(dplyr)

coeff_path <- file.path(env_dir, "env_coefficients_expanded_from_xlsx.csv")
used_groups_path <- file.path(out_dir, "env_impact_curated_by_env_group.csv")

if (!file.exists(coeff_path)) {
  stop("Missing coefficients file: ", coeff_path, ". Run R/10_impacto_ambiental.R first.")
}
if (!file.exists(used_groups_path)) {
  stop("Missing curated impact-by-group file: ", used_groups_path, ". Run R/10_impacto_ambiental.R first.")
}

escape_latex <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("[\u200B\u200C\u200D\uFEFF]", "", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
  x
}

fmt_num <- function(x, digits = 2) {
  x <- as.numeric(x)
  ifelse(is.na(x), "--", sprintf(paste0("%.", digits, "f"), x))
}

source_label <- function(src) {
  dplyr::case_when(
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
  dplyr::case_when(
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
  dplyr::case_when(
    src == "Arrieta2022_TableS4" ~ "Farm gate",
    grepl("farm-gate|farm gate", notes_low) ~ "Farm gate",
    src %in% c(
      "OWID_Poore2018", "Poore2018_retail", "Agribalyse3.1_proxy",
      "Poore2018_proxy", "Estimated_Argentina", "Composite_estimate", "Minimal_impact"
    ) ~ "Retail/product",
    TRUE ~ "Proxy/mixed"
  )
}

coeff <- read_csv(coeff_path, show_col_types = FALSE) %>%
  transmute(
    env_group = as.character(env_group),
    ghg_kg_co2eq_per_kg = as.numeric(ghg_kg_co2eq_per_kg),
    land_m2_per_kg = as.numeric(land_m2_per_kg),
    water_l_per_kg = as.numeric(water_l_per_kg),
    eutro_g_po4eq_per_kg = as.numeric(eutro_g_po4eq_per_kg),
    coefficient_factor_fw_rw_pct = as.numeric(coefficient_factor_fw_rw_pct),
    source = as.character(source),
    notes = as.character(notes)
  )

used_groups <- read_csv(used_groups_path, show_col_types = FALSE) %>%
  transmute(env_group = as.character(env_group)) %>%
  filter(!is.na(env_group), env_group != "") %>%
  distinct()

tbl <- used_groups %>%
  left_join(coeff, by = "env_group") %>%
  mutate(
    boundary = boundary_label(source, notes),
    fw_rw_pct_used = if_else(
      is.na(coefficient_factor_fw_rw_pct) | coefficient_factor_fw_rw_pct <= 0,
      100,
      coefficient_factor_fw_rw_pct
    ),
    source_specific = source_label(source)
  ) %>%
  arrange(env_group)

if (any(is.na(tbl$ghg_kg_co2eq_per_kg))) {
  missing_groups <- tbl$env_group[is.na(tbl$ghg_kg_co2eq_per_kg)]
  stop(
    "Missing coefficients for groups used in curated run: ",
    paste(missing_groups, collapse = ", ")
  )
}

table_csv <- tbl %>%
  transmute(
    food_group = food_group_label(env_group),
    ghg_kg_co2eq_per_kg,
    land_m2_per_kg,
    water_l_per_kg,
    eutro_g_po4eq_per_kg,
    system_boundary = boundary,
    fw_rw_factor_pct = fw_rw_pct_used,
    source_specific
  )

write_csv(table_csv, file.path(out_dir, "table_s8_environmental_coefficients.csv"))

row_lines <- vapply(seq_len(nrow(table_csv)), function(i) {
  sprintf(
    "%s & %s & %s & %s & %s & %s & %s & %s\\\\",
    escape_latex(table_csv$food_group[i]),
    fmt_num(table_csv$ghg_kg_co2eq_per_kg[i], 2),
    fmt_num(table_csv$land_m2_per_kg[i], 2),
    fmt_num(table_csv$water_l_per_kg[i], 2),
    fmt_num(table_csv$eutro_g_po4eq_per_kg[i], 2),
    escape_latex(table_csv$system_boundary[i]),
    fmt_num(table_csv$fw_rw_factor_pct[i], 2),
    escape_latex(table_csv$source_specific[i])
  )
}, character(1))

tex_lines <- c(
  "\\setlength{\\LTleft}{0pt}",
  "\\setlength{\\LTright}{0pt}",
  "{\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{longtable}{p{2.5cm}rrrrp{1.5cm}rp{3.4cm}}",
  "\\multicolumn{8}{l}{\\textbf{Table S8. Environmental coefficients by food group}}\\\\",
  "\\toprule",
  "\\textbf{Food group} & \\textbf{\\shortstack{GHG\\\\(kg CO$_2$-eq/kg)}} & \\textbf{\\shortstack{Land\\\\(m$^2$/kg)}} & \\textbf{\\shortstack{Water\\\\(L/kg)}} & \\textbf{\\shortstack{Eutro\\\\(g PO$_4$-eq/kg)}} & \\textbf{Boundary} & \\textbf{\\shortstack{FW/RW\\\\(\\%)}} & \\textbf{Source}\\\\",
  "\\midrule",
  "\\endfirsthead",
  "\\multicolumn{8}{l}{\\textbf{Table S8. Environmental coefficients by food group (continued)}}\\\\",
  "\\toprule",
  "\\textbf{Food group} & \\textbf{\\shortstack{GHG\\\\(kg CO$_2$-eq/kg)}} & \\textbf{\\shortstack{Land\\\\(m$^2$/kg)}} & \\textbf{\\shortstack{Water\\\\(L/kg)}} & \\textbf{\\shortstack{Eutro\\\\(g PO$_4$-eq/kg)}} & \\textbf{Boundary} & \\textbf{\\shortstack{FW/RW\\\\(\\%)}} & \\textbf{Source}\\\\",
  "\\midrule",
  "\\endhead",
  "\\midrule",
  "\\multicolumn{8}{r}{\\emph{Continued on next page}}\\\\",
  "\\endfoot",
  "\\bottomrule",
  "\\endlastfoot",
  row_lines,
  "\\end{longtable}",
  paste0(
    "\\tabnote{This table reports the coefficient set used in the manuscript environmental specification ",
    "(curated run; main manuscript Table~5 and Online Appendix Tables~S5--S6), restricted to food groups ",
    "with mapped intake in adults aged 30--69 years. GHG denotes greenhouse gas emissions and Eutro denotes freshwater eutrophication. ",
    "FW/RW is the farm-weight to retail-weight factor used to transform observed retail intake to coefficient reference mass, ",
    "with conversion $kg_{ref}=kg_{retail}/(FW/RW/100)$. FW/RW values of 100 imply no mass-conversion adjustment. ",
    "Source labels refer to the exact provenance of each group-level value: Arrieta et al. 2022 (Argentina-focused LCA), ",
    "OWID/Poore and Nemecek 2018, and documented proxy or estimated extensions.}"
  ),
  "}"
)

writeLines(enc2utf8(tex_lines), file.path(out_dir, "table_s8_environmental_coefficients.tex"), useBytes = TRUE)

message("Saved: output/table_s8_environmental_coefficients.csv")
message("Saved: output/table_s8_environmental_coefficients.tex")
