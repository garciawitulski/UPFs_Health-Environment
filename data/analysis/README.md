# Processed analytical inputs for manuscript calculations

This folder contains the processed analytical inputs used by the public manuscript-calculation pipeline.

- `upf_exposure_by_stratum.csv`
  Sex- and age-specific mean UPF exposure and uncertainty inputs for the core comparative risk assessment.

- `all_cause_mortality_by_stratum.csv`
  All-cause deaths by sex-age stratum for the target adult population.

- `population_by_stratum.csv`
  Population counts by sex-age stratum used in life-table calculations.

- `earnings_by_stratum.csv`
  Average earnings and employment rates by sex-age stratum used in the indirect-cost calculations.

- `environmental_baseline_total.csv`
  Baseline total environmental impacts across the modeled diet.

- `environmental_baseline_by_nova.csv`
  Baseline environmental impacts by NOVA group.

- `environmental_baseline_by_group.csv`
  Baseline environmental impacts by food group in the curated environmental model.

- `environmental_baseline_by_stratum.csv`
  Baseline environmental impacts by sex-age stratum.

- `environmental_upf_component.csv`
  Baseline environmental component attributable to NOVA 4 / UPFs.

- `environmental_replacement_slopes.csv`
  Replacement-effect slopes used to reconstruct scenario-specific environmental trade-offs from the processed environmental module.

- `environmental_coefficients_by_group.csv`
  Food-group environmental coefficients used by the public appendix-table script.

These files are derived from the full internal workflow and are included so the public package can regenerate the manuscript calculations without redistributing oversized raw survey files.
