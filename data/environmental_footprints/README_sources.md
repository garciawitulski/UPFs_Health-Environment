# Environmental Footprint Data Sources

This folder contains only downloaded/extracted source data (no imputed values).

## Files

- `owid_poore_ghg_per_kg.csv`
  - Source: Our World in Data grapher endpoint
  - URL: `https://ourworldindata.org/grapher/ghg-per-kg-poore.csv`
  - Metric: Greenhouse gas emissions per kilogram of food product.

- `owid_poore_land_use_per_kg.csv`
  - Source: Our World in Data grapher endpoint
  - URL: `https://ourworldindata.org/grapher/land-use-per-kg-poore.csv`
  - Metric: Land use per kilogram of food product.

- `owid_poore_freshwater_withdrawals_per_kg.csv`
  - Source: Our World in Data grapher endpoint
  - URL: `https://ourworldindata.org/grapher/water-withdrawals-per-kg-poore.csv`
  - Metric: Freshwater withdrawals per kilogram of food product.

- `owid_poore_eutrophying_emissions_per_kg.csv`
  - Source: Our World in Data grapher endpoint
  - URL: `https://ourworldindata.org/grapher/eutrophying-emissions-per-kg-poore.csv`
  - Metric: Eutrophying emissions per kilogram of food product.

- `11625_2021_1087_MOESM1_ESM.pdf`
  - Source: Supplementary material for Arrieta et al. (Sustainability Science, 2022)
  - DOI landing page: `https://doi.org/10.1007/s11625-021-01087-7`
  - Direct supplementary URL:
    `https://static-content.springer.com/esm/art%3A10.1007%2Fs11625-021-01087-7/MediaObjects/11625_2021_1087_MOESM1_ESM.pdf`

- `11625_2021_1087_MOESM1_ESM.txt`
  - Derived from the PDF above using `pdftotext -layout`.
  - Used only as machine-readable intermediary for extracting Table S4.

- `arrieta_2022_tableS4_partI_coefficients.csv`
  - Extracted from `11625_2021_1087_MOESM1_ESM.pdf`, Table S4 Part I (per kg farm-gate coefficients).
  - Includes: FW-RW coefficient, GHG, fossil energy, land occupation, cropland demand, freshwater consumption, eutrophication potential.

- `agribalyse_31_synthese_raw.csv`
  - Source: ADEME Agribalyse 3.1 "Synthèse" dataset (open data, one record per product configuration).
  - Dataset page: `https://www.data.gouv.fr/datasets/agribalyse-synthese`
  - Direct CSV URL: `https://data.ademe.fr/data-fair/api/v1/datasets/agribalyse-31-synthese/raw`
  - Contains explicit coefficients for many beverages and sweet products, including climate change, land use, water resource depletion, and eutrophication indicators.

- `agribalyse_31_detail_par_etape_raw.csv`
  - Source: ADEME Agribalyse 3.1 "Détail par étape".
  - Dataset page: `https://www.data.gouv.fr/datasets/agribalyse-detail-par-etape`
  - Direct CSV URL: `https://data.ademe.fr/data-fair/api/v1/datasets/agribalyse-31-detail-par-etape/raw`
  - Contains stage-disaggregated impacts (agriculture, transformation, packaging, transport, distribution, consumption), useful for UPF chain-stage analysis.

## Retrieval date

- Downloaded/extracted on: 2026-02-18 (local system date).
