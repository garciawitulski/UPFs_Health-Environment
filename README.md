# UPFs_Health-Environment

R-only repository for the UPF health and environment analysis pipeline.

## Scope

This repository now contains only:
- `R/` scripts used in the paper workflow
- project documentation (`README.md` and `R/README.md`)

Python code and datasets were intentionally removed from version control.

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

## Data inputs

The R scripts expect external input data (for example ENNyS, DEIS, EPH, and environmental coefficients) available in local folders such as `ENNyS/` and `data/`.
These datasets are not versioned in this repository.
