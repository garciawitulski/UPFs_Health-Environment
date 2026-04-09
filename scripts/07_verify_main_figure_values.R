suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(scales)
})

bootstrap_script_dir <- function() {
  for (i in rev(seq_along(sys.frames()))) {
    if (!is.null(sys.frames()[[i]]$ofile)) return(dirname(normalizePath(sys.frames()[[i]]$ofile, winslash = "/", mustWork = FALSE)))
  }
  args <- commandArgs(trailingOnly = FALSE)
  hit <- args[grepl("^--file=", args)]
  if (length(hit)) return(dirname(normalizePath(sub("^--file=", "", hit[1]), winslash = "/", mustWork = FALSE)))
  getwd()
}
source(file.path(bootstrap_script_dir(), "00_config.R"), local = TRUE)

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

t1 <- read_pkg_csv("upf_exposure_by_age_and_sex.csv")
t2 <- read_pkg_csv("upf_attributable_deaths_by_age_and_sex.csv")
env_base <- read_pkg_csv("environmental_baseline_shares.csv")
env_sum <- read_pkg_csv("scenario_environment_results.csv")
s7a <- read_pkg_csv("replacement_basket_composition.csv")

t1_total <- t1[t1[[1]] == "Total", ]
t1_m <- parse_ci(t1_total$`Men, % (95% CI)`)$est
t1_w <- parse_ci(t1_total$`Women, % (95% CI)`)$est
t1_t <- parse_ci(t1_total$`Total, % (95% CI)`)$est
add_check("Figure 1", "Overall UPF share, men", sprintf("%.1f", t1_m), "15.9", abs(t1_m - 15.9) < 1e-9)
add_check("Figure 1", "Overall UPF share, women", sprintf("%.1f", t1_w), "17.2", abs(t1_w - 17.2) < 1e-9)
add_check("Figure 1", "Overall UPF share, total", sprintf("%.1f", t1_t), "16.7", abs(t1_t - 16.7) < 1e-9)

t2_total <- t2[t2[[1]] == "Total", ]
add_check("Figure 1", "Attributable deaths, total", as.character(t2_total$`Total, deaths attr`), "9823", as.numeric(t2_total$`Total, deaths attr`) == 9823)
add_check("Figure 1", "Total PAF, men", sprintf("%.1f", as.numeric(t2_total$`Men, PAF %`)), "9.0", as.numeric(t2_total$`Men, PAF %`) == 9.0)
add_check("Figure 1", "Total PAF, women", sprintf("%.1f", as.numeric(t2_total$`Women, PAF %`)), "10.4", as.numeric(t2_total$`Women, PAF %`) == 10.4)

base_upf <- env_base %>% filter(component == "NOVA4_UPF")
add_check("Figure 2", "Baseline GHG share", sprintf("%.1f", base_upf$ghg_share_pct), "10.0", round(base_upf$ghg_share_pct, 1) == 10.0)
add_check("Figure 2", "Baseline Land share", sprintf("%.1f", base_upf$land_share_pct), "9.0", round(base_upf$land_share_pct, 1) == 9.0)
add_check("Figure 2", "Baseline Water share", sprintf("%.1f", base_upf$water_share_pct), "20.2", round(base_upf$water_share_pct, 1) == 20.2)
add_check("Figure 2", "Baseline Eutro share", sprintf("%.1f", base_upf$eutro_share_pct), "16.2", round(base_upf$eutro_share_pct, 1) == 16.2)

row_no_sub_10 <- env_sum %>% filter(scenario == "red_10", scenario_model == "no_replacement", replacement_target == "none")
add_check("Figure 2", "10% no-substitution GHG net change", sprintf("%.1f", -row_no_sub_10$ghg_kg_co2eq_avoided_pct_of_baseline), "-1.0", round(-row_no_sub_10$ghg_kg_co2eq_avoided_pct_of_baseline, 1) == -1.0)
row_iso_ndg_strict <- env_sum %>% filter(scenario %in% c("ndg_gapa_strict", "ndg_NDG_strict"), scenario_model == "isocaloric_replacement", replacement_target == "NDG_ARG")
add_check("Figure 2", "Strict isocaloric NDG water net change", sprintf("%.1f", -row_iso_ndg_strict$water_l_avoided_pct_of_baseline), "27.8", round(-row_iso_ndg_strict$water_l_avoided_pct_of_baseline, 1) == 27.8)

sum_cols <- c("observed_non_upf", "nova1", "nova3", "nova1_nova3_mix", "NDG_target")
for (col in sum_cols) {
  add_check("Figure 3", paste("Basket sums to 100:", col), sprintf("%.1f", sum(s7a[[col]])), "100.0", abs(sum(s7a[[col]]) - 100) < 1e-8)
}
fg_baked <- s7a %>% filter(food_group == "Baked products")
add_check("Figure 3", "Observed non-UPF baked products share", sprintf("%.1f", fg_baked$observed_non_upf), "25.8", round(fg_baked$observed_non_upf, 1) == 25.8)
add_check("Figure 3", "NOVA 3 baked products share", sprintf("%.1f", fg_baked$nova3), "55.4", round(fg_baked$nova3, 1) == 55.4)
fg_veg <- s7a %>% filter(food_group == "Vegetables (outdoor)")
add_check("Figure 3", "NDG vegetables share", sprintf("%.1f", fg_veg$NDG_target), "25.0", round(fg_veg$NDG_target, 1) == 25.0)

pareto_df <- env_sum %>%
  mutate(
    basket = dplyr::recode(replacement_target, NDG_ARG = "NDG", NOVA1 = "NOVA 1", NOVA3 = "NOVA 3", NOVA1_NOVA3_mix = "NOVA1/3 mix", none = "No replacement"),
    structure = dplyr::recode(scenario_model, isocaloric_replacement = "Isocaloric", isoweight_replacement = "Isoweight", no_replacement = "No sub."),
    scenario_label = dplyr::recode(
      scenario,
      red_10 = "10% reduction",
      red_20 = "20% reduction",
      red_50 = "50% reduction",
      ndg_gapa_conservative = "NDG conservative",
      ndg_NDG_conservative = "NDG conservative",
      ndg_gapa_central = "NDG central",
      ndg_NDG_central = "NDG central",
      ndg_gapa_strict = "NDG strict",
      ndg_NDG_strict = "NDG strict"
    ),
    point_id = paste(scenario, scenario_model, replacement_target, sep = "__"),
    deaths = deaths_averted,
    ghg = -ghg_kg_co2eq_avoided_pct_of_baseline,
    water = -water_l_avoided_pct_of_baseline,
    land = -land_m2_avoided_pct_of_baseline,
    eutro = -eutro_g_po4eq_avoided_pct_of_baseline
  )

front_ids <- function(df, yvar) {
  d <- df[order(-df$deaths), ]
  keep <- logical(nrow(d))
  best <- Inf
  for (i in seq_len(nrow(d))) {
    if (d[[yvar]][i] < best) {
      keep[i] <- TRUE
      best <- d[[yvar]][i]
    }
  }
  d$point_id[keep]
}

union_ids <- unique(unlist(lapply(c("ghg", "water", "land", "eutro"), function(v) front_ids(pareto_df, v))))
highlight <- pareto_df[pareto_df$point_id %in% union_ids, ]
add_check("Figure 4", "Highlighted scenario count", nrow(highlight), "5", nrow(highlight) == 5)

strict_iso_nova1 <- pareto_df %>% filter(scenario_label == "NDG strict", structure == "Isocaloric", basket == "NOVA 1")
add_check("Figure 4", "Strict isocaloric NOVA 1 deaths", sprintf("%.0f", strict_iso_nova1$deaths), "8747", round(strict_iso_nova1$deaths) == 8747)
add_check("Figure 4", "Strict isocaloric NOVA 1 water change", sprintf("%.1f", strict_iso_nova1$water), "21.4", round(strict_iso_nova1$water, 1) == 21.4)

figure5_source <- readLines(file.path(SCRIPT_DIR, "05_build_figure_5.R"), warn = FALSE)
add_check("Figure 5", "Figure 5 PDF generated", ifelse(file.exists(file.path(FIG_DIR, "Figure_5.pdf")), "yes", "no"), "yes", file.exists(file.path(FIG_DIR, "Figure_5.pdf")))
appendix_path <- file.path(SUBMISSION_ROOT, "Online_Appendix_UPF_Argentina.tex")
if (file.exists(appendix_path)) {
  appendix_tex <- readLines(appendix_path, warn = FALSE)
  for (sec in paste0("B.", 1:8)) {
    add_check("Figure 5", paste("Appendix section exists:", sec), ifelse(any(grepl(sec, appendix_tex, fixed = TRUE)), "yes", "no"), "yes", any(grepl(sec, appendix_tex, fixed = TRUE)))
  }
} else {
  add_check("Figure 5", "Appendix source included", "no", "optional", TRUE)
}
add_check("Figure 5", "No stale label: Pareto", ifelse(any(grepl("Pareto|pareto", figure5_source)), "present", "absent"), "absent", !any(grepl("Pareto|pareto", figure5_source)))
add_check("Figure 5", "No stale label: manuscript", ifelse(any(grepl("\\(manuscript\\)", figure5_source)), "present", "absent"), "absent", !any(grepl("\\(manuscript\\)", figure5_source)))
add_check("Figure 5", "No stale label: sensitivity", ifelse(any(grepl("\\(sensitivity\\)", figure5_source)), "present", "absent"), "absent", !any(grepl("\\(sensitivity\\)", figure5_source)))
add_check("Figure 5", "No stale label: truncated", ifelse(any(grepl("truncated", figure5_source)), "present", "absent"), "absent", !any(grepl("truncated", figure5_source)))

check_df <- bind_rows(checks)
report_path <- file.path(REPORT_DIR, "verification_main_figures.md")

lines <- c(
  "# Verification of main manuscript figure inputs",
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
  failed <- paste(check_df$item[!check_df$pass], collapse = "; ")
  stop("Some verification checks failed: ", failed)
}

message("All verification checks passed.")
