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

PPP_2019 <- 20.38
E70 <- 18.5
age_order <- c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69")
WHO_YLL <- c(
  "30-34" = 60.34, "35-39" = 55.38, "40-44" = 50.43, "45-49" = 45.51,
  "50-54" = 40.61, "55-59" = 35.74, "60-64" = 30.92, "65-69" = 26.21
)
RETIREMENT_AGE <- 65
DISCOUNT_RATE <- 0.03
age_mid <- c(
  "30-34" = 32, "35-39" = 37, "40-44" = 42, "45-49" = 47,
  "50-54" = 52, "55-59" = 57, "60-64" = 62, "65-69" = 67
)

pv_earnings <- function(annual_earnings, employment_rate, years_remaining) {
  if (years_remaining <= 0) return(0)
  eff <- annual_earnings * employment_rate
  if (DISCOUNT_RATE <= 0) return(eff * years_remaining)
  eff * (1 - (1 + DISCOUNT_RATE)^(-years_remaining)) / DISCOUNT_RATE
}

build_lt <- function(deaths_vec, pop_vec) {
  n <- 5
  m_x <- deaths_vec / replace(pop_vec, pop_vec == 0, 1)
  q_x <- (n * m_x) / (1 + (n / 2) * m_x)
  q_x[m_x >= 2] <- 0.999
  l_x <- c(100000, 100000 * cumprod(1 - q_x[-length(q_x)]))
  l70 <- l_x[8] * (1 - q_x[8])
  L_x <- n * (l_x + c(l_x[-1], l70)) / 2
  T_x <- rev(cumsum(rev(L_x)))
  T_x[8] <- T_x[8] + l70 * E70
  e_x <- T_x / l_x
  data.frame(age_group = age_order, deaths = deaths_vec, pop = pop_vec, l_x = l_x, e_x = e_x)
}

res <- read_calc_output_csv("resultados_paper_argentina.csv")
health <- read_calc_output_csv("health_counterfactual_scenarios.csv")
earnings <- read_calc_csv("earnings_by_stratum.csv")
def_df <- read_calc_csv("all_cause_mortality_by_stratum.csv")
pop_df <- read_calc_csv("population_by_stratum.csv")

res <- res %>%
  left_join(earnings, by = c("sexo_m", "age_group")) %>%
  mutate(
    yll_per_death = unname(WHO_YLL[age_group]),
    ingreso_promedio = coalesce(ingreso_promedio, 0),
    tasa_ocupacion = coalesce(tasa_ocupacion, 0),
    anos_productivos = pmax(0, RETIREMENT_AGE - unname(age_mid[age_group])),
    pv_per_death = mapply(pv_earnings, ingreso_promedio, tasa_ocupacion, anos_productivos)
  )

pop_agg <- pop_df %>% group_by(age_group) %>% summarise(population = sum(population), .groups = "drop")
deaths_agg <- def_df %>% group_by(age_group) %>% summarise(deaths = sum(deaths), .groups = "drop")
deaths_dict <- setNames(deaths_agg$deaths, deaths_agg$age_group)
pop_dict <- setNames(pop_agg$population, pop_agg$age_group)
deaths_vec <- unname(replace(deaths_dict[age_order], is.na(deaths_dict[age_order]), 0))
pop_vec <- unname(replace(pop_dict[age_order], is.na(pop_dict[age_order]), 1))
e30_actual <- build_lt(deaths_vec, pop_vec)$e_x[1]

scenario_labels <- c(
  red_10 = "10% reduction",
  red_20 = "20% reduction",
  red_50 = "50% reduction",
  ndg_gapa_conservative = "NDG conservative (target UPF=5.45%)",
  ndg_gapa_central = "NDG central (target UPF=3.63%)",
  ndg_gapa_strict = "NDG strict (target UPF=1.81%)"
)

scenario_rows <- lapply(health$scenario, function(sc) {
  deaths_col <- paste0(sc, "_deaths")
  deaths_lo_col <- paste0(sc, "_deaths_lo")
  deaths_hi_col <- paste0(sc, "_deaths_hi")

  stratum_averted <- res[[deaths_col]]
  stratum_averted_lo <- res[[deaths_lo_col]]
  stratum_averted_hi <- res[[deaths_hi_col]]

  yll_averted <- sum(stratum_averted * res$yll_per_death, na.rm = TRUE)
  yll_averted_lo <- sum(stratum_averted_lo * res$yll_per_death, na.rm = TRUE)
  yll_averted_hi <- sum(stratum_averted_hi * res$yll_per_death, na.rm = TRUE)

  pvle_averted_ars <- sum(stratum_averted * res$pv_per_death, na.rm = TRUE)
  pvle_averted_lo_ars <- sum(stratum_averted_lo * res$pv_per_death, na.rm = TRUE)
  pvle_averted_hi_ars <- sum(stratum_averted_hi * res$pv_per_death, na.rm = TRUE)

  attr_cf <- res %>%
    transmute(age_group, deaths_averted = .data[[deaths_col]]) %>%
    group_by(age_group) %>%
    summarise(deaths_averted = sum(deaths_averted, na.rm = TRUE), .groups = "drop")

  attr_cf_dict <- setNames(attr_cf$deaths_averted, attr_cf$age_group)
  attr_cf_vec <- unname(replace(attr_cf_dict[age_order], is.na(attr_cf_dict[age_order]), 0))
  lt_cf <- build_lt(pmax(0, deaths_vec - attr_cf_vec), pop_vec)

  label <- unname(scenario_labels[sc])
  if (length(label) == 0 || is.na(label)) label <- sc

  data.frame(
    scenario = sc,
    scenario_label = label,
    deaths_averted = sum(stratum_averted, na.rm = TRUE),
    deaths_averted_lo = sum(stratum_averted_lo, na.rm = TRUE),
    deaths_averted_hi = sum(stratum_averted_hi, na.rm = TRUE),
    yll_averted = yll_averted,
    yll_averted_lo = yll_averted_lo,
    yll_averted_hi = yll_averted_hi,
    pvle_averted_billion_intl_ppp = pvle_averted_ars / PPP_2019 / 1e9,
    pvle_averted_billion_intl_ppp_lo = pvle_averted_lo_ars / PPP_2019 / 1e9,
    pvle_averted_billion_intl_ppp_hi = pvle_averted_hi_ars / PPP_2019 / 1e9,
    e30_actual = e30_actual,
    e30_without_scenario_deaths = lt_cf$e_x[1],
    e30_gain_years = lt_cf$e_x[1] - e30_actual,
    stringsAsFactors = FALSE
  )
})

scenario_summary <- bind_rows(scenario_rows) %>%
  left_join(select(health, scenario, scenario_type, scenario_value), by = "scenario") %>%
  select(
    scenario,
    scenario_label,
    scenario_type,
    scenario_value,
    deaths_averted,
    deaths_averted_lo,
    deaths_averted_hi,
    yll_averted,
    yll_averted_lo,
    yll_averted_hi,
    pvle_averted_billion_intl_ppp,
    pvle_averted_billion_intl_ppp_lo,
    pvle_averted_billion_intl_ppp_hi,
    e30_actual,
    e30_without_scenario_deaths,
    e30_gain_years
  )

write_calc_csv(scenario_summary, "scenario_health_economic_summary.csv")
message("Saved calculation output: scenario_health_economic_summary.csv")
