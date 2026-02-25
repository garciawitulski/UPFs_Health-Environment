# UPF Argentina Pipeline in R

This folder contains the R implementation of the UPF-attributable health and environmental analysis workflow.

## Requirements

- R >= 4.0
- Packages: `readr`, `dplyr`, `tidyr`

```r
install.packages(c("readr", "dplyr", "tidyr"))
```

## Script overview

| Script | Description | Main inputs | Main outputs |
|--------|-------------|-------------|--------------|
| `01_agregar_defunciones.R` | Aggregates 2019 DEIS deaths by sex and age (30-69) | `data/defunciones_2019.csv` | `data/defunciones_2019_por_estrato.csv` |
| `02_agregar_defunciones_causa.R` | Aggregates deaths by cause (diabetes, CVD, stroke, cancer) | `data/defunciones_2019.csv` | `output/defunciones_2019_por_causa.csv` |
| `03_upf_por_estrato.R` | Computes UPF exposure by recall/person/stratum with survey weights | ENNyS diet + NOVA + composition tables | `output/upf_por_estrato.csv` |
| `04_paf_resultados.R` | Distributional PAF (log-normal) + 10k Monte Carlo for policy scenarios | `output/upf_por_estrato.csv`, `data/defunciones_2019_por_estrato.csv` | `output/resultados_paper_argentina.csv`, `output/health_counterfactual_scenarios.csv` |
| `05_muertes_por_causa.R` | Attributable deaths by cause | `output/upf_por_estrato.csv`, `output/defunciones_2019_por_causa.csv` | `output/resultados_por_causa.csv`, `output/resumen_muertes_por_causa.csv` |
| `06_yll.R` | Years of Life Lost (YLL) | `output/resultados_paper_argentina.csv`, `output/resultados_por_causa.csv` | `output/yll_*.csv` |
| `07_esperanza_vida.R` | Cause-eliminated life table and life expectancy effects | deaths + population + main results | `output/esperanza_vida_perdida_UPF.csv` |
| `08_costos_indirectos.R` | Indirect costs (human capital/PVLE, EPH 2019) | main results + `data/EPH/ingresos_por_estrato.csv` | `output/costos_indirectos_por_estrato.csv`, `output/resumen_costos_indirectos.csv` |
| `10_impacto_ambiental.R` | Environmental footprint baseline and UPF replacement/reduction scenarios | ENNyS diet + pre-mapped/pre-built coefficients | `output/env_impact_*.csv` |
| `12_impacto_ambiental_upf_extended.R` | Environmental scenarios using extended coefficients | extended mapping/coefs + ENNyS | extended scenario outputs |
| `13_merge_health_env_summaries.R` | Merges health and environmental scenario summaries | health and environmental summary tables | merged summaries |
| `14_figuras_demo_resultados.R` | Generates exploratory figures | output tables | demo figures |
| `15_figuras_journal_ready.R` | Generates publication-ready figures | output tables | journal-ready figures |

## Run

From the project root:

```r
source("R/run_all.R")
```

Or run scripts individually in sequence:

```r
source("R/01_agregar_defunciones.R")
source("R/02_agregar_defunciones_causa.R")
# ...
```

## Data dependencies

The scripts use inputs under `ENNyS/` and `data/` (DEIS, EPH, population, ENNyS diet, and environmental footprint inputs).
This repository includes pre-classified UPF tables and pre-mapped/pre-built environmental tables, so mapping/classification builder scripts are not included.
