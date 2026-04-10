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

basket <- read_pkg_csv("replacement_basket_composition.csv")
ndg <- read_calc_output_csv("upf_ndg_gapa_profile.csv")

panel_a <- basket %>%
  transmute(
    food_group = food_group,
    observed_non_upf = observed_non_upf,
    nova1 = nova1,
    nova3 = nova3,
    nova1_nova3_mix = nova1_nova3_mix,
    NDG_target = NDG_target
  )

label_map <- c(
  conservative = "NDG conservative target",
  central = "NDG central target",
  strict = "NDG strict target"
)

panel_b <- ndg %>%
  transmute(
    scenario_label = unname(label_map[profile]),
    target_upf_pct = target_upf_pct,
    baseline_upf_pct = baseline_upf_pct_from_strata,
    implied_reduction_pct = 100 * pmax(0, pmin(1, (baseline_upf_pct - target_upf_pct) / baseline_upf_pct))
  ) %>%
  arrange(match(scenario_label, c("NDG conservative target", "NDG central target", "NDG strict target")))

write_calc_csv(panel_a, "table_s7_scenario_composition_panelA.csv")
write_calc_csv(panel_b, "table_s7_scenario_composition_panelB.csv")

message("Saved calculation outputs: table_s7_scenario_composition_panelA.csv, table_s7_scenario_composition_panelB.csv")
