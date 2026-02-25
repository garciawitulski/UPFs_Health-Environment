# run_all.R
# Runs the full UPF Argentina pipeline from R.
# Requires: readr, dplyr, tidyr (install.packages(c("readr","dplyr","tidyr"))).
#
# Usage:
#   From RStudio: set the working directory to the project root
#   (Ultra-processed_Mortality) and run: source("R/run_all.R")
#   From R console: setwd("path/to/Ultra-processed_Mortality"); source("R/run_all.R")

PROJECT_ROOT <- getwd()
if (!file.exists(file.path(PROJECT_ROOT, "ENNyS", "Base_Alimentos_Bebidas_Suplementos.csv"))) {
  stop("Set the working directory to the project root (Ultra-processed_Mortality).")
}

# Optional: set explicit project root for script execution
# PROJECT_ROOT <- "C:/Users/admin/Documents/Papers/Ultra-processed_Mortality"

run_script <- function(name) {
  e <- new.env()
  e$PROJECT_ROOT <- PROJECT_ROOT
  source(file.path(PROJECT_ROOT, "R", name), local = e)
}

message("=== UPF Argentina Pipeline (R) ===\n")
message("1. Aggregate deaths by stratum...")
run_script("01_agregar_defunciones.R")

message("\n2. Aggregate deaths by cause...")
run_script("02_agregar_defunciones_causa.R")

message("\n3. UPF exposure by stratum (diet data, can take time)...")
run_script("03_upf_por_estrato.R")

message("\n4. PAF and outcomes (10k Monte Carlo)...")
run_script("04_paf_resultados.R")

message("\n5. Deaths by cause...")
run_script("05_muertes_por_causa.R")

message("\n6. YLL...")
run_script("06_yll.R")

message("\n7. Life expectancy impact...")
run_script("07_esperanza_vida.R")

message("\n8. Indirect costs...")
run_script("08_costos_indirectos.R")

message("\n9. Environmental impact (using pre-mapped/pre-built coefficient tables)...")
run_script("10_impacto_ambiental.R")

message("\n10. Merge health and environmental summaries...")
run_script("13_merge_health_env_summaries.R")

message("\n=== Pipeline completed. Outputs are written to output/ ===")
