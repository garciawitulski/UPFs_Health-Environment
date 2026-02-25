# UPFs_Health-Environment

R repository for the UPF health and environment paper workflow.

## Scope

This repository contains:
- R scripts to run paper results
- pre-classified UPF and pre-mapped environmental input tables
- the minimum input datasets required by the R pipeline

The mapping/classification builder scripts were intentionally removed.

## Requirements

- R >= 4.0
- Packages: `readr`, `dplyr`, `tidyr`

```r
install.packages(c("readr", "dplyr", "tidyr"))
```

## Run

From the project root:

```r
source("R/run_all.R")
```

## Included data

- ENNyS inputs used by the analysis (`Base_Alimentos_Bebidas_Suplementos.csv`, `Base_Nutrientes.csv`, `ENNyS2_encuesta.csv`, `composicion_quimica.csv`)
- pre-classified UPF tables (`ENNyS/alimentos_clasificados_NOVA.csv`, `ENNyS/alimentos_clasificados_sin_suplementos.csv`)
- health inputs (`data/defunciones_2019.csv`, `data/poblacion_argentina_2018_ennys.csv`, `data/EPH/ingresos_por_estrato.csv`)
- pre-mapped and pre-built environmental tables under `data/environmental_footprints/`
