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

ensure_numeric_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
  }
  df
}

health <- read_calc_output_csv("scenario_health_economic_summary.csv")
health <- ensure_numeric_cols(
  health,
  c(
    "yll_averted", "yll_averted_lo", "yll_averted_hi",
    "pvle_averted_billion_intl_ppp", "pvle_averted_billion_intl_ppp_lo", "pvle_averted_billion_intl_ppp_hi",
    "e30_gain_years"
  )
)
env <- read_calc_output_csv("env_impact_scenarios_upf_summary.csv")

summary_env <- env %>%
  left_join(
    health %>%
      select(
        any_of(c(
          "scenario",
          "deaths_averted", "deaths_averted_lo", "deaths_averted_hi",
          "yll_averted", "yll_averted_lo", "yll_averted_hi",
          "pvle_averted_billion_intl_ppp", "pvle_averted_billion_intl_ppp_lo", "pvle_averted_billion_intl_ppp_hi",
          "e30_gain_years"
        ))
      ),
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
    yll_averted,
    yll_averted_lo,
    yll_averted_hi,
    pvle_averted_billion_intl_ppp,
    pvle_averted_billion_intl_ppp_lo,
    pvle_averted_billion_intl_ppp_hi,
    e30_gain_years,
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

write_calc_csv(summary_env, "summary_health_environment_upf_scenarios.csv")
message("Saved calculation output: summary_health_environment_upf_scenarios.csv")
