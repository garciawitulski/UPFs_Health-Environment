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

impact_cols <- c("ghg_kg_co2eq", "land_m2", "water_l", "eutro_g_po4eq")

baseline_total <- read_calc_csv("environmental_baseline_total.csv")
by_nova <- read_calc_csv("environmental_baseline_by_nova.csv")
by_group <- read_calc_csv("environmental_baseline_by_group.csv")
by_strata <- read_calc_csv("environmental_baseline_by_stratum.csv")
upf_component <- read_calc_csv("environmental_upf_component.csv")
slopes <- read_calc_csv("environmental_replacement_slopes.csv")

profile_path <- file.path(CALC_OUTPUT_DIR, "upf_ndg_gapa_profile.csv")
if (!file.exists(profile_path)) {
  stop("Missing health-derived NDG profile. Run calculations/04_paf_resultados.R first.")
}
policy_scen <- read.csv(profile_path, stringsAsFactors = FALSE, check.names = FALSE) %>%
  transmute(
    scenario = as.character(scenario),
    profile = as.character(profile),
    baseline_upf_pct = as.numeric(baseline_upf_pct_from_strata),
    target_upf_pct = as.numeric(target_upf_pct),
    reduction = pmax(0, pmin(1, (baseline_upf_pct - target_upf_pct) / baseline_upf_pct))
  ) %>%
  filter(is.finite(reduction), reduction > 0)

baseline_total <- baseline_total %>% mutate(across(everything(), as.numeric))
upf_component <- tibble(
  indicator = impact_cols,
  upf_value = c(
    as.numeric(upf_component$ghg_kg_co2eq),
    as.numeric(upf_component$land_m2),
    as.numeric(upf_component$water_l),
    as.numeric(upf_component$eutro_g_po4eq)
  )
)

scenarios <- bind_rows(
  tibble(
    scenario = c("red_10", "red_20", "red_50"),
    reduction = c(0.10, 0.20, 0.50),
    profile = NA_character_,
    baseline_upf_pct = NA_real_,
    target_upf_pct = NA_real_
  ),
  policy_scen
)

policy_scen_out <- policy_scen %>%
  transmute(
    scenario,
    profile,
    baseline_upf_pct,
    target_upf_pct,
    implied_reduction_pct = 100 * reduction
  )
write_calc_csv(policy_scen_out, "env_ndg_NDG_scenario_profile.csv")
write_calc_csv(policy_scen_out, "env_ndg_gapa_scenario_profile.csv")

build_rows <- function(sc, r, model, target) {
  bind_rows(lapply(impact_cols, function(v) {
    base_v <- as.numeric(baseline_total[[v]][1])
    upf_v <- upf_component$upf_value[upf_component$indicator == v]
    gross_removed <- r * upf_v

    replacement_added <- 0
    if (model != "no_replacement" && target != "none") {
      replacement_added <- slopes %>%
        filter(scenario_model == model, replacement_target == target, indicator == v) %>%
        pull(replacement_added_per_unit_reduction)
      if (length(replacement_added) == 0) replacement_added <- 0
      replacement_added <- replacement_added[1] * r
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
    build_rows(sc, r, "isocaloric_replacement", "NDG_ARG")
  )
}))

rows_isoweight <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
  sc <- scenarios$scenario[i]
  r <- scenarios$reduction[i]
  bind_rows(
    build_rows(sc, r, "isoweight_replacement", "NOVA1"),
    build_rows(sc, r, "isoweight_replacement", "NOVA3"),
    build_rows(sc, r, "isoweight_replacement", "NOVA1_NOVA3_mix"),
    build_rows(sc, r, "isoweight_replacement", "NDG_ARG")
  )
}))

scenario_tbl <- bind_rows(rows_no_replacement, rows_isocaloric, rows_isoweight) %>%
  arrange(reduction, scenario_model, replacement_target, indicator)

scenario_summary <- scenario_tbl %>%
  filter(indicator %in% impact_cols) %>%
  select(scenario, reduction, scenario_model, replacement_target, indicator, avoided_value, avoided_pct_of_baseline) %>%
  pivot_wider(
    names_from = indicator,
    values_from = c(avoided_value, avoided_pct_of_baseline),
    names_glue = "{indicator}_{.value}"
  ) %>%
  arrange(reduction, scenario_model, replacement_target)

write_calc_csv(baseline_total, "env_impact_baseline_total.csv")
write_calc_csv(baseline_total, "env_impact_comparable_baseline_total.csv")
write_calc_csv(by_nova, "env_impact_by_nova.csv")
write_calc_csv(by_nova, "env_impact_comparable_by_nova.csv")
write_calc_csv(by_group, "env_impact_by_env_group.csv")
write_calc_csv(by_group, "env_impact_comparable_by_env_group.csv")
write_calc_csv(by_group, "env_impact_curated_by_env_group.csv")
write_calc_csv(by_strata, "env_impact_by_strata.csv")
write_calc_csv(by_strata, "env_impact_comparable_by_strata.csv")
write_calc_csv(scenario_tbl, "env_impact_scenarios_upf.csv")
write_calc_csv(scenario_tbl, "env_impact_comparable_scenarios_upf.csv")
write_calc_csv(scenario_tbl, "env_impact_curated_scenarios_upf.csv")
write_calc_csv(scenario_summary, "env_impact_scenarios_upf_summary.csv")
write_calc_csv(scenario_summary, "env_impact_comparable_scenarios_upf_summary.csv")
write_calc_csv(scenario_summary, "env_impact_curated_scenarios_upf_summary.csv")

message("Saved calculation outputs: environmental baseline tables and env_impact_scenarios_upf_summary.csv")
