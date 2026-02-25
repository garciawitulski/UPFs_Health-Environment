# Pipeline UPF Argentina en R

RÃ©plica del cÃ¡lculo de muertes prematuras atribuibles a alimentos ultraprocesados (UPF) en Argentina, implementada en R.

## Requisitos

- R >= 4.0
- Paquetes: `readr`, `dplyr`, `tidyr`

```r
install.packages(c("readr", "dplyr", "tidyr"))
```

## Estructura de scripts

| Script | DescripciÃ³n | Entradas | Salidas |
|--------|-------------|----------|---------|
| `01_agregar_defunciones.R` | Agrega defunciones DEIS 2019 por sexo y edad 30-69 | `data/defunciones_2019.csv` | `data/defunciones_2019_por_estrato.csv` |
| `02_agregar_defunciones_causa.R` | Agrega defunciones por causa (diabetes, CVD, cerebrovascular, cÃ¡ncer) | `data/defunciones_2019.csv` | `output/defunciones_2019_por_causa.csv` |
| `03_upf_por_estrato.R` | %UPF por recall, persona y estrato (peso encuesta) | ENNyS: Base_Alimentos, Base_Nutrientes, encuesta, NOVA, composiciÃ³n | `output/upf_por_estrato.csv` |
| `04_paf_resultados.R` | PAF distribucional (log-normal) + Monte Carlo 10k; escenarios 10/20/50 + `NDG_GAPA` en 3 perfiles (conservative/central/strict) con target UPF derivado por caps de grupos | upf_por_estrato, defunciones_2019_por_estrato | `output/resultados_paper_argentina.csv`, `output/health_counterfactual_scenarios.csv`, `output/upf_ndg_gapa_profile.csv` |
| `05_muertes_por_causa.R` | Muertes atribuibles por causa (PAF punto) | upf_por_estrato, defunciones_2019_por_causa | `output/resultados_por_causa.csv`, `resumen_muertes_por_causa.csv` |
| `06_yll.R` | AÃ±os de vida perdidos (YLL) | resultados_paper_argentina, resultados_por_causa | `output/yll_*.csv` |
| `07_esperanza_vida.R` | Tabla de vida causa-eliminada, ganancia e30 | defunciones_2019_por_estrato, resultados_paper_argentina, poblaciÃ³n | `output/esperanza_vida_perdida_UPF.csv` |
| `08_costos_indirectos.R` | PVLE (capital humano, EPH 2019) | resultados_paper_argentina, data/EPH/ingresos_por_estrato.csv | `output/costos_indirectos_por_estrato.csv`, `resumen_costos_indirectos.csv` |
| `09_build_env_mapping.R` | Mapea cÃ³digos ENNyS a grupos ambientales (reglas explÃ­citas) | `ENNyS/alimentos_clasificados_NOVA.csv`, `data/environmental_footprints/arrieta_2022_tableS4_partI_coefficients.csv` | `data/environmental_footprints/ennys_codigo_to_env_group.csv`, `output/env_unmapped_codes.csv`, `output/env_mapping_summary_*.csv` |
| `10_impacto_ambiental.R` | Calcula huella ambiental de la dieta y escenarios de reduccion/reemplazo UPF (incluye `NDG_GAPA`) | Base_Alimentos ENNyS, mapping ambiental, coeficientes Arrieta Table S4 | `output/env_impact_*.csv`, `output/env_impact_mapping_coverage_by_nova.csv`, `output/env_ndg_gapa_scenario_profile.csv` |

## CÃ³mo ejecutar

1. Establecer el directorio de trabajo en la **raÃ­z del proyecto** (`Ultra-processed_Mortality`).

2. Ejecutar todo el pipeline:
   ```r
   source("R/run_all.R")
   ```

3. O ejecutar scripts individuales (en orden si dependen de salidas previas):
   ```r
   setwd("ruta/a/Ultra-processed_Mortality")
   source("R/01_agregar_defunciones.R")
   source("R/02_agregar_defunciones_causa.R")
   # ...
   ```

## Datos necesarios

- **data/defunciones_2019.csv**: DEIS (descargar con `python data/descargar_defunciones_2019.py` si no existe).
- **ENNyS/**: Base_Alimentos_Bebidas_Suplementos.csv, Base_Nutrientes.csv, ENNyS2_encuesta.csv, alimentos_clasificados_NOVA.csv, composicion_quimica.csv.
- **data/poblacion_argentina_2018_ennys.csv** (o poblacion_argentina_2018.csv) para esperanza de vida.
- **data/EPH/ingresos_por_estrato.csv** para costos (generable con el script Python `calcular_costos_indirectos.py` si se tienen microdatos EPH).
- **data/environmental_footprints/** para mÃ³dulo ambiental:
  - `arrieta_2022_tableS4_partI_coefficients.csv`
  - `ennys_codigo_to_env_group.csv` (generado por `R/09_build_env_mapping.R`)
  - opcional: CSV OWID Poore para anÃ¡lisis de sensibilidad/documentaciÃ³n.

## Nota

La clasificaciÃ³n NOVA se toma del archivo generado por Python (`clasificar_NOVA.py` â†’ `alimentos_clasificados_NOVA.csv`). No se incluye reclasificaciÃ³n NOVA en R; usar ese CSV como entrada.
