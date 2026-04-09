# GitHub-ready main-figure replication package

Minimal replication package for the five main-text figures used in the manuscript:

`Health and environmental impacts of ultra-processed food consumption in Argentina`

This folder is the clean subset to share if the goal is to reproduce only those figures, with the minimal code and data needed to rebuild them.

## Scope

- Reproduces `Figure_1` to `Figure_5` used by the Nature Food submission manuscript
- Keeps only the minimal data needed for those figures
- Writes regenerated figures to:
  - `generated/figures`
  - and, when this package is nested inside `submission_nature_food/github_figures_only`, also syncs them to the manuscript submission folders

## Included minimal data

- `Tabla1_UPF_por_energia_Argentina.csv`
- `Tabla2_Muertes_atribuibles_Argentina.csv`
- `env_impact_curated_table_main_totals_shares.csv`
- `scenario_health_economic_summary.csv`
- `summary_health_environment_upf_scenarios.csv`
- `table_s7_scenario_composition_panelA.csv`

## Run order

1. `scripts/06_build_all_figures.R`
2. `scripts/07_verify_main_figure_values.R`

Or run figures one by one:

1. `scripts/01_build_figure_1.R`
2. `scripts/02_build_figure_2.R`
3. `scripts/03_build_figure_3.R`
4. `scripts/04_build_figure_4.R`
5. `scripts/05_build_figure_5.R`

## Dependencies

- R packages: `ggplot2`, `dplyr`, `tidyr`, `scales`, `forcats`, `patchwork`
- MiKTeX `latexmk` for `Figure_5`
- `pdftoppm` only if you also want the workflow PNG regenerated; the manuscript uses the PDF

## Notes

- `Figure_5` is compiled from `figure_sources/Figure_5.tex`
- The verification script checks key numeric values and Appendix-B references, not pixel-perfect image identity
- If you publish this package as a standalone repository, you only need `README.md`, `data/`, `scripts/`, and `figure_sources/`
