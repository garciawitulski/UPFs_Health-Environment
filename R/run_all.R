# run_all.R
# Ejecuta todo el pipeline UPF Argentina desde R.
# Requiere: readr, dplyr, tidyr (install.packages(c("readr","dplyr","tidyr"))).
#
# Uso:
#   Desde RStudio: establecer el directorio de trabajo en la raíz del proyecto
#   (Ultra-processed_Mortality) y ejecutar: source("R/run_all.R")
#   Desde R en consola: setwd("ruta/a/Ultra-processed_Mortality"); source("R/run_all.R")

PROJECT_ROOT <- getwd()
if (!file.exists(file.path(PROJECT_ROOT, "ENNyS", "Base_Alimentos_Bebidas_Suplementos.csv"))) {
  stop("Establecer directorio de trabajo en la raíz del proyecto (Ultra-processed_Mortality).")
}

# Opcional: establecer raíz explícita para los scripts
# PROJECT_ROOT <- "C:/Users/admin/Documents/Papers/Ultra-processed_Mortality"

run_script <- function(name) {
  e <- new.env()
  e$PROJECT_ROOT <- PROJECT_ROOT
  source(file.path(PROJECT_ROOT, "R", name), local = e)
}

message("=== Pipeline UPF Argentina (R) ===\n")
message("1. Agregar defunciones por estrato...")
run_script("01_agregar_defunciones.R")

message("\n2. Agregar defunciones por causa...")
run_script("02_agregar_defunciones_causa.R")

message("\n3. UPF por estrato (diet data, puede tardar)...")
run_script("03_upf_por_estrato.R")

message("\n4. PAF y resultados (Monte Carlo 10k)...")
run_script("04_paf_resultados.R")

message("\n5. Muertes por causa...")
run_script("05_muertes_por_causa.R")

message("\n6. YLL...")
run_script("06_yll.R")

message("\n7. Esperanza de vida...")
run_script("07_esperanza_vida.R")

message("\n8. Costos indirectos...")
run_script("08_costos_indirectos.R")

message("\n9. Mapping ambiental (codigo -> grupo)...")
run_script("09_build_env_mapping.R")

message("\n10. Base extendida de coeficientes y cobertura UPF...")
run_script("11_build_extended_coeffs_and_upf_coverage.R")

message("\n11. Impacto ambiental (extendido + escenarios isocaloricos)...")
run_script("10_impacto_ambiental.R")

message("\n=== Pipeline completado. Salidas en output/ ===")
