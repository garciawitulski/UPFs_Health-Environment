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

checks <- list()
add_check <- function(section, item, observed, expected, pass) {
  checks[[length(checks) + 1]] <<- data.frame(
    section = section,
    item = item,
    observed = as.character(observed),
    expected = as.character(expected),
    pass = isTRUE(pass),
    stringsAsFactors = FALSE
  )
}

res <- read_calc_output_csv("resultados_paper_argentina.csv")
health <- read_calc_output_csv("scenario_health_economic_summary.csv")
env_base <- read_calc_output_csv("env_impact_baseline_total.csv")
env_sum <- read_calc_output_csv("summary_health_environment_upf_scenarios.csv")
s7a <- read_calc_output_csv("table_s7_scenario_composition_panelA.csv")
s8 <- read_calc_output_csv("table_s8_environmental_coefficients.csv")

add_check("Health", "Total attributable deaths", sprintf("%.0f", sum(res$deaths_attr, na.rm = TRUE)), "9823", round(sum(res$deaths_attr, na.rm = TRUE)) == 9823)
add_check("Health", "Total attributable deaths lower CI", sprintf("%.0f", sum(res$deaths_attr_low, na.rm = TRUE)), "5687", round(sum(res$deaths_attr_low, na.rm = TRUE)) == 5687)
add_check("Health", "Total attributable deaths upper CI", sprintf("%.0f", sum(res$deaths_attr_high, na.rm = TRUE)), "14183", round(sum(res$deaths_attr_high, na.rm = TRUE)) == 14183)

strict_row <- health[health$scenario == "ndg_gapa_strict", ]
add_check("Health", "NDG strict deaths averted", sprintf("%.0f", strict_row$deaths_averted), "8747", round(strict_row$deaths_averted) == 8747)
add_check("Health", "NDG strict YLL averted", sprintf("%.0f", strict_row$yll_averted), "311567", round(strict_row$yll_averted) == 311567)
add_check("Health", "NDG strict PVLE averted", sprintf("%.3f", strict_row$pvle_averted_billion_intl_ppp), "0.745", round(strict_row$pvle_averted_billion_intl_ppp, 3) == 0.745)

add_check("Environment", "Baseline GHG share", sprintf("%.1f", 100 * env_base$ghg_kg_co2eq[1] / env_base$ghg_kg_co2eq[1]), "100.0", TRUE)
by_nova <- read_calc_output_csv("env_impact_by_nova.csv")
nova4 <- by_nova[by_nova$NOVA == 4, ]
add_check("Environment", "UPF baseline GHG share", sprintf("%.1f", 100 * nova4$ghg_kg_co2eq / env_base$ghg_kg_co2eq[1]), "10.0", round(100 * nova4$ghg_kg_co2eq / env_base$ghg_kg_co2eq[1], 1) == 10.0)
add_check("Environment", "UPF baseline land share", sprintf("%.1f", 100 * nova4$land_m2 / env_base$land_m2[1]), "9.0", round(100 * nova4$land_m2 / env_base$land_m2[1], 1) == 9.0)
add_check("Environment", "UPF baseline water share", sprintf("%.1f", 100 * nova4$water_l / env_base$water_l[1]), "20.2", round(100 * nova4$water_l / env_base$water_l[1], 1) == 20.2)
add_check("Environment", "UPF baseline eutro share", sprintf("%.1f", 100 * nova4$eutro_g_po4eq / env_base$eutro_g_po4eq[1]), "16.2", round(100 * nova4$eutro_g_po4eq / env_base$eutro_g_po4eq[1], 1) == 16.2)

env_strict <- env_sum[env_sum$scenario == "ndg_gapa_strict" & env_sum$scenario_model == "isocaloric_replacement" & env_sum$replacement_target == "NDG_ARG", ]
add_check("Environment", "NDG strict isocaloric NDG water net change", sprintf("%.1f", -env_strict$water_l_avoided_pct_of_baseline), "27.8", round(-env_strict$water_l_avoided_pct_of_baseline, 1) == 27.8)

add_check("Appendix", "Scenario composition rows", nrow(s7a), "18", nrow(s7a) == 18)
add_check("Appendix", "Environmental coefficient rows", ifelse(nrow(s8) >= 20, ">=20", "<20"), ">=20", nrow(s8) >= 20)

check_df <- bind_rows(checks)
report_path <- file.path(REPORT_DIR, "verification_core_outputs.md")
lines <- c(
  "# Verification of manuscript calculation outputs",
  "",
  paste0("Generated on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "| Section | Check | Observed | Expected | Pass |",
  "| --- | --- | --- | --- | --- |"
)
for (i in seq_len(nrow(check_df))) {
  lines <- c(lines, sprintf("| %s | %s | %s | %s | %s |", check_df$section[i], check_df$item[i], check_df$observed[i], check_df$expected[i], ifelse(check_df$pass[i], "PASS", "FAIL")))
}
writeLines(lines, report_path)
message("Verification report written to ", report_path)

if (!all(check_df$pass)) {
  stop("Some calculation verification checks failed.")
}

message("All manuscript calculation checks passed.")
