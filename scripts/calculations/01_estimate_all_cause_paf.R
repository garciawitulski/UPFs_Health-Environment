bootstrap_script_dir <- function() {
  for (i in rev(seq_along(sys.frames()))) {
    if (!is.null(sys.frames()[[i]]$ofile)) {
      return(dirname(normalizePath(sys.frames()[[i]]$ofile, winslash = "/", mustWork = FALSE)))
    }
  }
  args <- commandArgs(trailingOnly = FALSE)
  hit <- args[grepl("^--file=", args)]
  if (length(hit)) {
    return(dirname(normalizePath(sub("^--file=", "", hit[1]), winslash = "/", mustWork = FALSE)))
  }
  getwd()
}

source(file.path(bootstrap_script_dir(), "..", "00_config.R"), local = TRUE)

set.seed(42)
RR_POINT <- 1.25
RR_LOW <- 1.14
RR_HIGH <- 1.37
UPF_REF <- 35.7
N_SIM <- 10000
BETA_CENTRAL <- log(RR_POINT) / UPF_REF
BETA_LO <- log(RR_LOW) / UPF_REF
BETA_HI <- log(RR_HIGH) / UPF_REF
BETA_SE <- (BETA_HI - BETA_LO) / (2 * 1.96)

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

NDG_UPF_CAP_BY_GROUP <- c(
  "Fruits" = 0.00,
  "Vegetables (outdoor)" = 0.00,
  "Legumes and pulses" = 0.00,
  "Cereals (without rice)" = 0.10,
  "Rice" = 0.00,
  "Pasta" = 0.10,
  "Baked products" = 0.20,
  "Starchy vegetables" = 0.00,
  "Dairy products" = 0.10,
  "Beef" = 0.02,
  "Lamb and mutton" = 0.02,
  "Other meats" = 0.02,
  "Poultry" = 0.02,
  "Pork" = 0.02,
  "Fish and seafood" = 0.02,
  "Eggs" = 0.00,
  "Oil crops" = 0.10,
  "Nuts and seeds" = 0.00
)

build_ndg_cap_profile <- function(profile = c("conservative", "central", "strict")) {
  profile <- match.arg(profile)
  mult <- switch(profile, conservative = 1.50, central = 1.00, strict = 0.50)
  out <- pmin(0.95, NDG_UPF_CAP_BY_GROUP * mult)
  names(out) <- names(NDG_UPF_CAP_BY_GROUP)
  out
}

expected_rr <- function(mean_upf, sd_upf, beta, n_pts = 800, lower = 0, upper = 100) {
  if (mean_upf <= 0 || sd_upf <= 0 || beta <= 0) return(1)
  cv <- max(sd_upf / mean_upf, 0.01)
  sigma2 <- log(1 + cv^2)
  sigma <- sqrt(sigma2)
  mu <- log(mean_upf) - sigma2 / 2

  x_pts <- seq(lower, upper, length.out = n_pts)
  dx <- (upper - lower) / (n_pts - 1)
  pdf_vals <- dlnorm(x_pts, mu, sigma)
  rr_vals <- exp(beta * x_pts)
  mass_trunc <- sum(pdf_vals) * dx

  if (!is.finite(mass_trunc) || mass_trunc <= 0) {
    x0 <- max(lower, min(upper, mean_upf))
    return(exp(beta * x0))
  }
  sum(rr_vals * pdf_vals) * dx / mass_trunc
}

compute_paf_between <- function(mean_b, sd_b, mean_cf, sd_cf, beta) {
  if (mean_b <= 0 || sd_b <= 0 || beta <= 0) return(0)
  e_rr_baseline <- expected_rr(mean_b, sd_b, beta)
  e_rr_counter <- if (mean_cf <= 0.001 || sd_cf <= 0.001) 1 else expected_rr(mean_cf, sd_cf, beta)
  if (!is.finite(e_rr_baseline) || e_rr_baseline <= 0) return(0)
  paf <- (e_rr_baseline - e_rr_counter) / e_rr_baseline
  max(0, min(1, paf))
}

compute_paf_reduction <- function(mean_upf, sd_upf, beta, reduction_pct = 1) {
  mean_cf <- max(0.001, mean_upf * (1 - reduction_pct))
  sd_cf <- max(0.001, sd_upf * (1 - reduction_pct))
  compute_paf_between(mean_upf, sd_upf, mean_cf, sd_cf, beta)
}

compute_ndg_upf_target <- function(baseline_upf_pct, profile = c("conservative", "central", "strict")) {
  profile <- match.arg(profile)
  cap_map <- build_ndg_cap_profile(profile)
  w_tbl <- tibble(
    env_group = names(GAPA_GROUP_WEIGHTS),
    weight_target = as.numeric(GAPA_GROUP_WEIGHTS),
    upf_cap = as.numeric(cap_map[names(GAPA_GROUP_WEIGHTS)]),
    available = TRUE,
    weight_used = as.numeric(GAPA_GROUP_WEIGHTS) / sum(GAPA_GROUP_WEIGHTS),
    upf_share = upf_cap,
    upf_share_pct = 100 * upf_cap,
    weighted_upf_contribution_pct = 100 * weight_used * upf_cap,
    assumption = "ndg_group_upf_cap",
    ndg_profile = profile
  )

  list(
    profile = profile,
    target_upf_pct = 100 * sum(w_tbl$weight_used * w_tbl$upf_cap, na.rm = TRUE),
    baseline_upf_pct = baseline_upf_pct,
    weight_coverage_pct = 100,
    group_table = w_tbl
  )
}

upf <- read_calc_csv("upf_exposure_by_stratum.csv") %>%
  mutate(
    sexo_m = as.integer(sexo_m),
    pct_upf_mean = as.numeric(pct_upf_mean),
    pct_upf_sd = as.numeric(pct_upf_sd),
    pct_upf_se = as.numeric(pct_upf_se),
    n = as.numeric(n)
  )

baseline_upf_pct <- suppressWarnings(weighted.mean(upf$pct_upf_mean, w = upf$n, na.rm = TRUE))
ndg_profiles <- lapply(c("conservative", "central", "strict"), function(p) compute_ndg_upf_target(baseline_upf_pct, p))

ndg_group_tbl <- bind_rows(lapply(ndg_profiles, function(x) x$group_table))
ndg_profile_tbl <- bind_rows(lapply(ndg_profiles, function(x) {
  tibble(
    scenario = paste0("ndg_gapa_", x$profile),
    profile = x$profile,
    target_upf_pct = x$target_upf_pct,
    baseline_upf_pct_from_strata = x$baseline_upf_pct,
    weight_coverage_pct = x$weight_coverage_pct,
    target_definition = "ndg_group_upf_caps"
  )
}))

write_calc_csv(ndg_group_tbl, "upf_ndg_gapa_group_shares.csv")
write_calc_csv(ndg_profile_tbl, "upf_ndg_gapa_profile.csv")

def19 <- read_calc_csv("all_cause_mortality_by_stratum.csv") %>%
  mutate(
    sexo_m = as.integer(sexo_m),
    deaths = as.integer(deaths)
  )

deaths_df <- upf %>%
  left_join(select(def19, sexo_m, age_group, deaths), by = c("sexo_m", "age_group")) %>%
  mutate(deaths = if_else(is.na(deaths), 0L, as.integer(deaths)))

beta_samples <- rnorm(N_SIM, BETA_CENTRAL, BETA_SE)
scenarios <- list(
  red_10 = list(type = "relative_reduction", value = 0.10),
  red_20 = list(type = "relative_reduction", value = 0.20),
  red_50 = list(type = "relative_reduction", value = 0.50),
  ndg_gapa_conservative = list(type = "absolute_target_pct", value = ndg_profile_tbl$target_upf_pct[ndg_profile_tbl$profile == "conservative"]),
  ndg_gapa_central = list(type = "absolute_target_pct", value = ndg_profile_tbl$target_upf_pct[ndg_profile_tbl$profile == "central"]),
  ndg_gapa_strict = list(type = "absolute_target_pct", value = ndg_profile_tbl$target_upf_pct[ndg_profile_tbl$profile == "strict"])
)
scenario_names <- names(scenarios)

mc_results <- vector("list", nrow(deaths_df))
for (i in seq_len(nrow(deaths_df))) {
  row <- deaths_df[i, ]
  mean_upf <- row$pct_upf_mean
  sd_upf <- row$pct_upf_sd
  se_upf <- row$pct_upf_se
  n_deaths <- row$deaths

  paf_sims <- numeric(N_SIM)
  deaths_sims <- numeric(N_SIM)
  sc_deaths <- setNames(lapply(scenario_names, function(x) numeric(N_SIM)), scenario_names)

  for (j in seq_len(N_SIM)) {
    mean_s <- min(100, max(0.1, rnorm(1, mean_upf, se_upf)))
    beta_j <- beta_samples[j]
    deaths_j <- rpois(1, n_deaths)
    paf_sims[j] <- compute_paf_reduction(mean_s, sd_upf, beta_j, 1)
    deaths_sims[j] <- paf_sims[j] * deaths_j

    for (sc in scenario_names) {
      spec <- scenarios[[sc]]
      paf_sc <- 0
      if (spec$type == "relative_reduction") {
        paf_sc <- compute_paf_reduction(mean_s, sd_upf, beta_j, spec$value)
      } else if (spec$type == "absolute_target_pct") {
        mean_cf <- min(mean_s, max(0.001, spec$value))
        sd_cf <- max(0.001, sd_upf * (mean_cf / max(mean_s, 0.001)))
        paf_sc <- compute_paf_between(mean_s, sd_upf, mean_cf, sd_cf, beta_j)
      }
      sc_deaths[[sc]][j] <- paf_sc * deaths_j
    }
  }

  row_out <- list(
    sexo_m = row$sexo_m,
    age_group = row$age_group,
    pct_upf_mean = mean_upf,
    pct_upf_sd = sd_upf,
    n = row$n,
    deaths = n_deaths,
    PAF = as.numeric(stats::quantile(paf_sims, 0.5)),
    PAF_lo = as.numeric(stats::quantile(paf_sims, 0.025)),
    PAF_hi = as.numeric(stats::quantile(paf_sims, 0.975)),
    deaths_attr = as.numeric(stats::quantile(deaths_sims, 0.5)),
    deaths_attr_low = as.numeric(stats::quantile(deaths_sims, 0.025)),
    deaths_attr_high = as.numeric(stats::quantile(deaths_sims, 0.975))
  )

  for (sc in scenario_names) {
    row_out[[paste0(sc, "_deaths")]] <- as.numeric(stats::quantile(sc_deaths[[sc]], 0.5))
    row_out[[paste0(sc, "_deaths_lo")]] <- as.numeric(stats::quantile(sc_deaths[[sc]], 0.025))
    row_out[[paste0(sc, "_deaths_hi")]] <- as.numeric(stats::quantile(sc_deaths[[sc]], 0.975))
  }

  mc_results[[i]] <- as.data.frame(row_out, check.names = FALSE)
}

results <- bind_rows(mc_results)
write_calc_csv(results, "resultados_paper_argentina.csv")

deaths_total <- sum(results$deaths, na.rm = TRUE)
deaths_attr_total <- sum(results$deaths_attr, na.rm = TRUE)
deaths_attr_lo <- sum(results$deaths_attr_low, na.rm = TRUE)
deaths_attr_hi <- sum(results$deaths_attr_high, na.rm = TRUE)

scenario_summary <- tibble(
  scenario = scenario_names,
  scenario_type = vapply(scenarios, function(x) as.character(x$type), character(1)),
  scenario_value = vapply(scenarios, function(x) as.numeric(x$value), numeric(1)),
  deaths_averted = vapply(scenario_names, function(sc) sum(results[[paste0(sc, "_deaths")]], na.rm = TRUE), numeric(1)),
  deaths_averted_lo = vapply(scenario_names, function(sc) sum(results[[paste0(sc, "_deaths_lo")]], na.rm = TRUE), numeric(1)),
  deaths_averted_hi = vapply(scenario_names, function(sc) sum(results[[paste0(sc, "_deaths_hi")]], na.rm = TRUE), numeric(1))
) %>%
  mutate(
    deaths_total = deaths_total,
    pct_of_premature_deaths = if_else(deaths_total > 0, 100 * deaths_averted / deaths_total, NA_real_)
  )
write_calc_csv(scenario_summary, "health_counterfactual_scenarios.csv")

summary_health <- tibble(
  deaths_total = deaths_total,
  deaths_attr = deaths_attr_total,
  deaths_attr_lo = deaths_attr_lo,
  deaths_attr_hi = deaths_attr_hi,
  red_10 = sum(results$red_10_deaths, na.rm = TRUE),
  red_20 = sum(results$red_20_deaths, na.rm = TRUE),
  red_50 = sum(results$red_50_deaths, na.rm = TRUE),
  ndg_gapa_conservative = sum(results$ndg_gapa_conservative_deaths, na.rm = TRUE),
  ndg_gapa_central = sum(results$ndg_gapa_central_deaths, na.rm = TRUE),
  ndg_gapa_strict = sum(results$ndg_gapa_strict_deaths, na.rm = TRUE),
  ndg_gapa = sum(results$ndg_gapa_central_deaths, na.rm = TRUE)
)
write_calc_csv(summary_health, "summary_health_main.csv")

message("Saved calculation outputs: resultados_paper_argentina.csv, health_counterfactual_scenarios.csv, summary_health_main.csv")
