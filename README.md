# Health and environmental co-benefits of reducing ultra-processed food consumption in Argentina: a modelling study

This repository is a processed-data manuscript replication package for the study:

> Health and environmental co-benefits of reducing ultra-processed food consumption in Argentina: a modelling study

It contains the code and processed analytical inputs needed to regenerate the main manuscript calculations and the five figures used in the submission package, without redistributing the full underlying ENNyS survey microdata.

## What this package reproduces

The public package reproduces:

- the core health calculations used in the manuscript
- the scenario-level health and economic summaries
- the environmental scenario calculations used in the main manuscript
- the appendix-ready scenario-composition and environmental-coefficient tables
- the five main manuscript figures

The package is intentionally built from processed analytical inputs. This keeps the repository compact and reproducible while avoiding oversized source files that are impractical for a standard public GitHub repository.

## Repository structure

```text
data/
  Processed analytical inputs shipped with the package
  analysis/      Inputs used by the manuscript calculation scripts

scripts/
  Figure-building scripts used for the main manuscript figures
  calculations/  Ordered manuscript-calculation pipeline adapted for this public package

generated/
  Regenerated calculation outputs, figures, and verification reports
  Created locally and ignored by git
```

## Quick start

Run the manuscript calculations:

```r
source("scripts/calculations/90_run_core_outputs.R")
source("scripts/calculations/95_verify_core_outputs.R")
```

Rebuild the manuscript figures:

```r
source("scripts/06_build_all_figures.R")
source("scripts/07_verify_main_figure_values.R")
```

Run the full public replication pipeline:

```r
source("scripts/calculations/99_full_submission_pipeline.R")
```

Generated outputs are written to:

- `generated/calculation_outputs/`
- `generated/figures/`
- `generated/reports/`

When this package is nested inside the authors' local `submission_nature_food` folder, the figure scripts also synchronize refreshed figure files into the manuscript submission directory.

## Included analytical inputs

The repository ships two processed input layers:

- figure-ready inputs in `data/`, used by the public figure scripts
- calculation-ready inputs in `data/analysis/`, used by the ordered manuscript calculation pipeline

These include:

- stratum-level UPF exposure
- stratum-level all-cause mortality
- stratum-level cause-specific mortality
- population and earnings by sex-age stratum
- processed environmental baseline totals and group summaries
- processed environmental scenario slopes
- food-group replacement basket composition
- environmental coefficients used by the public appendix-table script

See [data/README.md](data/README.md) and [data/analysis/README.md](data/analysis/README.md) for file-level descriptions.

## Script layout

### Main figures

- `scripts/01_build_figure_1.R`
- `scripts/02_build_figure_2.R`
- `scripts/03_build_figure_3.R`
- `scripts/04_build_figure_4.R`
- `scripts/05_build_figure_5.R`
- `scripts/06_build_all_figures.R`
- `scripts/07_verify_main_figure_values.R`

### Manuscript calculations

- `scripts/calculations/04_paf_resultados.R`
- `scripts/calculations/05_muertes_por_causa.R`
- `scripts/calculations/06_yll.R`
- `scripts/calculations/07_esperanza_vida.R`
- `scripts/calculations/08_costos_indirectos.R`
- `scripts/calculations/09_scenario_health_economic_summary.R`
- `scripts/calculations/10_impacto_ambiental.R`
- `scripts/calculations/13_merge_health_env_summaries.R`
- `scripts/calculations/16_appendix_scenario_composition_table.R`
- `scripts/calculations/17_appendix_environmental_coefficients_table.R`
- `scripts/calculations/90_run_core_outputs.R`
- `scripts/calculations/91_build_submission_figures.R`
- `scripts/calculations/95_verify_core_outputs.R`
- `scripts/calculations/99_full_submission_pipeline.R`

## Software requirements

- R 4.4 or later
- R packages:
  - `readr`
  - `ggplot2`
  - `dplyr`
  - `tidyr`
  - `scales`
  - `forcats`
  - `patchwork`
- `pdflatex` on the system path for `Figure_5`
- `pdftoppm` on the system path if you also want the PNG version of `Figure_5`

`Figure_5` is generated from R through an embedded TikZ/LaTeX source written by the script at runtime.

## Reproducibility notes

- This public package does not redistribute the full raw ENNyS microdata workflow.
- The manuscript calculations included here are rebuilt from processed analytical inputs derived from the full internal study pipeline.
- The verification scripts check the main numerical targets used in the manuscript package, but they do not perform pixel-level image comparison.
- The public package is focused on the main manuscript calculations and figures rather than every intermediate file produced during the original internal workflow.

## Citation

If you use this code or these processed analytical inputs, please cite the associated manuscript and acknowledge this public replication package.
