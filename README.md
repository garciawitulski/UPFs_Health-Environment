# Manuscript Replication Package

This repository provides a compact replication package for the manuscript:

**Health and environmental impacts of ultra-processed food consumption in Argentina**

It contains the code and processed analytical inputs needed to regenerate the main quantitative visual outputs used in the paper, without requiring the full internal workflow used to build every intermediate dataset, table, and appendix artifact.

## Scope of this package

The current public package reproduces the five main figures cited in the manuscript:

- `Figure_1`: UPF exposure and attributable mortality by sex and age
- `Figure_2`: Baseline environmental shares and scenario-specific environmental changes
- `Figure_3`: Composition of the replacement baskets used in the environmental scenarios
- `Figure_4`: Health-environment trade-offs across UPF-reduction scenarios
- `Figure_5`: Overview of the health and environmental analytical workflows

## Repository structure

```text
data/         Processed analytical inputs used by the manuscript replication package
scripts/      Reproducible figure-building scripts and verification checks
generated/    Regenerated outputs (created locally, ignored by git)
```

## Included data

The repository includes the processed data files required by the public manuscript replication package:

- `upf_exposure_by_age_and_sex.csv`
- `upf_attributable_deaths_by_age_and_sex.csv`
- `environmental_baseline_shares.csv`
- `scenario_health_economic_results.csv`
- `scenario_environment_results.csv`
- `replacement_basket_composition.csv`

See [data/README.md](data/README.md) for a short description of each file.

## Quick start

Rebuild the manuscript figures:

```r
source("scripts/06_build_all_figures.R")
```

Verify that the regenerated outputs remain numerically aligned with the manuscript:

```r
source("scripts/07_verify_main_figure_values.R")
```

Generated outputs are written to:

- `generated/figures/`
- `generated/reports/`

If this package is nested inside the local `submission_nature_food` folder used by the authors, the scripts also synchronize the refreshed figure files into the manuscript submission directory.

## Script order

Individual scripts can also be run independently:

1. `scripts/01_build_figure_1.R`
2. `scripts/02_build_figure_2.R`
3. `scripts/03_build_figure_3.R`
4. `scripts/04_build_figure_4.R`
5. `scripts/05_build_figure_5.R`
6. `scripts/07_verify_main_figure_values.R`

Or use the single entry point:

1. `scripts/06_build_all_figures.R`
2. `scripts/07_verify_main_figure_values.R`

## Software requirements

- R 4.4 or later
- R packages:
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

- The repository contains processed analytical inputs rather than raw survey microdata.
- The verification script checks key figure values and workflow labels, but it does not perform pixel-perfect image comparison.
- This public package is focused on the main manuscript outputs and does not include every table, appendix figure, or intermediate dataset from the full study workflow.

## Citation

If you use this code or these processed manuscript inputs, please cite the associated manuscript and acknowledge this replication package.
