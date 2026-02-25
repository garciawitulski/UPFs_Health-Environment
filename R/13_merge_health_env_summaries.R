# 13_merge_health_env_summaries.R
# Rebuild merged health + environment scenario summary files.

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
out_dir <- file.path(root, "output")

library(readr)
library(dplyr)

health <- read_csv(file.path(out_dir, "health_counterfactual_scenarios.csv"), show_col_types = FALSE)
env <- read_csv(file.path(out_dir, "env_impact_scenarios_upf_summary.csv"), show_col_types = FALSE)

summary_env <- env %>%
  left_join(
    health %>% select(scenario, deaths_averted, deaths_averted_lo, deaths_averted_hi),
    by = "scenario"
  ) %>%
  transmute(
    scenario,
    reduction_pct = 100 * reduction,
    scenario_model,
    replacement_target,
    deaths_averted,
    deaths_averted_lo,
    deaths_averted_hi,
    reduction,
    ghg_kg_co2eq_avoided_value,
    land_m2_avoided_value,
    water_l_avoided_value,
    eutro_g_po4eq_avoided_value,
    ghg_kg_co2eq_avoided_pct_of_baseline,
    land_m2_avoided_pct_of_baseline,
    water_l_avoided_pct_of_baseline,
    eutro_g_po4eq_avoided_pct_of_baseline
  ) %>%
  arrange(reduction, scenario_model, replacement_target)

write_csv(summary_env, file.path(out_dir, "summary_health_environment_upf_scenarios.csv"))

ghg_ext <- read_csv(file.path(out_dir, "env_impact_ghg_extended_scenarios_upf.csv"), show_col_types = FALSE)

summary_ghg_ext <- ghg_ext %>%
  left_join(
    health %>% select(scenario, deaths_averted, deaths_averted_lo, deaths_averted_hi),
    by = "scenario"
  ) %>%
  transmute(
    scenario,
    reduction_pct = 100 * reduction,
    reduction,
    scenario_model,
    replacement_target,
    deaths_averted,
    deaths_averted_lo,
    deaths_averted_hi,
    ghg_kg_co2eq_avoided_value,
    ghg_kg_co2eq_avoided_pct_of_baseline
  ) %>%
  arrange(reduction, scenario_model, replacement_target)

write_csv(summary_ghg_ext, file.path(out_dir, "summary_health_ghg_extended_upf_scenarios.csv"))

message("Guardado: ", file.path(out_dir, "summary_health_environment_upf_scenarios.csv"))
message("Guardado: ", file.path(out_dir, "summary_health_ghg_extended_upf_scenarios.csv"))
