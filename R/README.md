# UPF Argentina Pipeline in R

This folder contains the R implementation of the UPF-attributable health and environmental analysis workflow.

## Requirements

- R >= 4.0
- Packages: `readr`, `dplyr`, `tidyr`, `ggplot2`, `scales`, `ggalluvial`, `patchwork`

```r
install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "scales", "ggalluvial", "patchwork"))
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
| `09_scenario_health_economic_summary.R` | Scenario-level deaths, YLL, life expectancy, and PVLE summaries reconstructed from the R pipeline | `output/resultados_paper_argentina.csv`, `output/health_counterfactual_scenarios.csv`, deaths/population/EPH inputs | `output/scenario_health_economic_summary.csv` |
| `10_impacto_ambiental.R` | Environmental footprint baseline and UPF replacement/reduction scenarios | ENNyS diet + pre-mapped/pre-built coefficients | `output/env_impact_*.csv` |
| `11_appendix_nova_classification_table.R` | Builds the appendix longtable with all ENNyS food codes classified by NOVA | `ENNyS/alimentos_clasificados_NOVA.csv` | `output/table_s2_nova_classification_all_items.csv`, `output/table_s2_nova_classification_all_items.tex` |
| `12_impacto_ambiental_upf_extended.R` | Environmental scenarios using extended coefficients | extended mapping/coefs + ENNyS | extended scenario outputs |
| `13_merge_health_env_summaries.R` | Merges health and environmental scenario summaries | health and environmental summary tables | merged summaries |
| `16_appendix_scenario_composition_table.R` | Builds the appendix table summarizing replacement baskets and UPF targets used in the scenarios | `output/env_replacement_basket_shares_curated.csv`, `output/env_ndg_NDG_scenario_profile.csv` | `output/table_s7_scenario_composition.tex`, `output/table_s7_scenario_composition_panelA.csv`, `output/table_s7_scenario_composition_panelB.csv` |
| `17_appendix_environmental_coefficients_table.R` | Builds the appendix table with group-level environmental coefficients used in the manuscript environmental model (intensities, boundary, FW/RW factor, source) | `data/environmental_footprints/env_coefficients_expanded_from_xlsx.csv`, `output/env_impact_curated_by_env_group.csv` | `output/table_s8_environmental_coefficients.tex`, `output/table_s8_environmental_coefficients.csv` |
| `14_figuras_demo_resultados.R` | Generates exploratory figures | output tables | demo figures |
| `15_figuras_journal_ready.R` | Generates reproducible journal-ready figures directly from the current pipeline tables and merged scenario summaries | `output/Tabla1_UPF_por_energia_Argentina.csv`, `output/Tabla2_Muertes_atribuibles_Argentina.csv`, `output/health_counterfactual_scenarios.csv`, `output/summary_health_environment_upf_scenarios.csv` | `output/figures/fig1_upf_exposure_age_sex_journal.*`, `output/figures/fig2_attributable_deaths_age_sex_journal.*`, `output/figures/fig3_deaths_averted_scenarios_journal.*`, `output/figures/fig4_tradeoff_panel_journal.*`, `output/figures/fig5_environment_heatmap_journal.*` |
| `18_figuras_submission_alternative.R` | Generates an alternative, submission-oriented figure set with denser visual summaries (butterfly burden chart, policy ladder, frontier plot, heatmap) directly from current output tables | `output/Tabla1_UPF_por_energia_Argentina.csv`, `output/Tabla2_Muertes_atribuibles_Argentina.csv`, `output/scenario_health_economic_summary.csv`, `output/summary_health_environment_upf_scenarios.csv` | `output/figures_alternative/alt_fig1_butterfly_burden_exposure.*`, `output/figures_alternative/alt_fig2_policy_ladder.*`, `output/figures_alternative/alt_fig3_frontier_small_multiples.*`, `output/figures_alternative/alt_fig4_environment_heatmap.*` |
| `19_figuras_submission_options.R` | Generates an additional set of submission-oriented options focused on substitution sensitivity and policy comparison (dumbbell trade-off plot, policy profile matrix, decomposition bars, replacement-basket alluvial) directly from current output tables | `output/scenario_health_economic_summary.csv`, `output/summary_health_environment_upf_scenarios.csv`, `output/env_replacement_basket_shares_curated.csv` | `output/figures_options/opt_fig1_dumbbell_substitution_tradeoffs.*`, `output/figures_options/opt_fig1_slopegraph_substitution_tradeoffs.*`, `output/figures_options/opt_fig2_policy_profile_matrix.*`, `output/figures_options/opt_fig3_decomposition_environmental_response.*`, `output/figures_options/opt_fig4_replacement_baskets_alluvial.*` |
| `20_bundle_table_replacement_figures.R` | Builds a curated bundle of figures selected to replace the main manuscript tables, and copies both the figures and a runnable script into a dedicated bundle folder | `output/Tabla6_Esperanza_vida_UPF.csv`, `output/yll_allcause_por_estrato.csv`, `output/resumen_costos_indirectos*.csv`, `output/scenario_health_economic_summary.csv`, `output/summary_health_environment_upf_scenarios.csv`, plus the pooled outputs from scripts 18, 19, and 22 | `output/table_replacement_bundle/fig01_tables1_2_butterfly_exposure_burden.*`, `output/table_replacement_bundle/fig02_table3_allcause_burden_matrix.*`, `output/table_replacement_bundle/fig03_table4_policy_gains_ladder.*`, `output/table_replacement_bundle/fig04_table5_environment_policy_heatmap.*`, `output/table_replacement_bundle/fig05_complement_replacement_baskets_alluvial.*`, `output/table_replacement_bundle/build_table_replacement_bundle.R` |
| `22_ed1_replacement_baskets.R` | Builds a cleaner 2-panel replacement-basket figure directly from Table S7 food groups, showing both absolute energy shares and deviations from the observed non-UPF basket | `output/table_s7_scenario_composition_panelA.csv` | `output/figures_final/extended_data/ED1_replacement_baskets.*` |
| `23_ed1_replacement_baskets_stacked_delta.R` | Builds an alternative composition figure from Table S7 food groups with stacked bars for absolute basket composition and a delta heatmap versus the observed non-UPF basket | `output/table_s7_scenario_composition_panelA.csv` | `output/figures_final/alternatives/ED1_replacement_baskets_stacked_delta.*` |
| `24_results_section_figures.R` | Builds the composite Results figure used as `Figure_1` in the Nature Food submission, plus its component panels | `output/Tabla1_UPF_por_energia_Argentina.csv`, `output/Tabla2_Muertes_atribuibles_Argentina.csv`, `output/scenario_health_economic_summary.csv`, `output/yll_allcause_por_estrato.csv`, `output/Tabla6_Esperanza_vida_UPF.csv`, `output/resumen_costos_indirectos.csv` | `output/figures/results_section/Fig_R1_upf_heatmap.*`, `Fig_R2_attributable_pyramid.*`, `Fig_R3_burden_lollipop.*`, `Fig_R4_scenario_ladder.*`, `Fig_1_composite.*` |
| `25_manuscript_figures_main.R` | Canonical entry point for the four figures used in `submission_nature_food/Paper_UPF_Argentina_manuscript.tex`; rebuilds upstream figure pools and syncs `Figure_1`-`Figure_4` to both `output/figures/manuscript/` and `submission_nature_food/` | Outputs from scripts `20`, `23`, `24`, plus `figures_upf_argentina.R` for the Pareto figure | `output/figures/manuscript/Figure_1.*` ... `Figure_4.*`, `submission_nature_food/Figure_1.*` ... `Figure_4.*`, `submission_nature_food/figures/Figure_1.*` ... `Figure_4.*` |
| `26_appendix_pipeline_figures.R` | Builds publication-style pipeline diagrams for the appendix health and environmental workflows | no tabular inputs; internal diagram specification | `output/figures/appendix_pipelines/Fig_S1_health_pipeline.*`, `Fig_S2_env_pipeline.*`, `Fig_S1S2_unified.*` |

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

To rebuild the journal-ready figure set after the main pipeline has finished:

```r
source("R/15_figuras_journal_ready.R")
```

To build the alternative submission-oriented figure set:

```r
source("R/18_figuras_submission_alternative.R")
```

To build the additional submission-option figures:

```r
source("R/19_figuras_submission_options.R")
```

To build the curated bundle that replaces the main manuscript tables:

```r
source("R/20_bundle_table_replacement_figures.R")
```

To rebuild the exact four figure files used by the Nature Food submission manuscript:

```r
source("R/25_manuscript_figures_main.R")
```

## Data dependencies

The scripts use inputs under `ENNyS/` and `data/` (DEIS, EPH, population, ENNyS diet, and environmental footprint inputs).
This repository includes pre-classified UPF tables and pre-mapped/pre-built environmental tables, so mapping/classification builder scripts are not included.

