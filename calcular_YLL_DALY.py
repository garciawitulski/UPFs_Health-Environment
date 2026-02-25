# -*- coding: utf-8 -*-
"""
Calcula YLL (Years of Life Lost) atribuibles al consumo de UPF en Argentina.

YLL = muertes atribuibles × esperanza de vida restante estándar (años perdidos por muerte).

Usa la tabla estándar WHO GHE 2021 (life expectancy at birth ≈ 92.65 años,
basada en proyección ONS 2050). Valores para grupos de edad 30-69 años
(estandarizados, sin discriminación por sexo en la tabla estándar WHO "Persons").

Referencia: WHO. Methods and data sources for global burden of disease estimates 2000-2021.
WHO/DDI/DNA/GHE/2024.3. Table 2.1.

NOTA: Los DALYs completos = YLL + YLD. No calculamos YLD porque requiere
incidencia de enfermedades no fatales atribuibles a UPF y pesos de discapacidad,
que no están disponibles en este proyecto. Por lo tanto reportamos solo YLL.
"""

import pandas as pd
from pathlib import Path

OUT = Path(__file__).parent / "output"

# WHO GHE 2021 standard: expected years of life lost for death at age (midpoint of group)
# Same for both sexes in WHO standard "Persons" column
WHO_STANDARD_YLL = {
    '30-34': 60.34,  # years lost if death in this age group
    '35-39': 55.38,
    '40-44': 50.43,
    '45-49': 45.51,
    '50-54': 40.61,
    '55-59': 35.74,
    '60-64': 30.92,
    '65-69': 26.21,
}


def main():
    # 1) YLL all-cause
    res = pd.read_csv(OUT / "resultados_paper_argentina.csv")
    res['yll_per_death'] = res['age_group'].map(WHO_STANDARD_YLL)
    res['YLL'] = res['deaths_attr'] * res['yll_per_death']
    
    yll_total_allcause = res['YLL'].sum()
    
    print("="*70)
    print("YLL (Years of Life Lost) atribuibles a UPF - Todas las causas")
    print("Tabla estándar WHO GHE 2021")
    print("="*70)
    print(res[['sexo_m', 'age_group', 'deaths_attr', 'yll_per_death', 'YLL']].to_string(index=False))
    print(f"\nTotal YLL (all-cause): {yll_total_allcause:,.0f}")
    
    # Guardar por estrato
    res[['sexo_m', 'age_group', 'deaths_attr', 'YLL', 'yll_per_death']].to_csv(
        OUT / "yll_allcause_por_estrato.csv", index=False
    )
    
    # 2) YLL por causa
    res_causa = pd.read_csv(OUT / "resultados_por_causa.csv")
    res_causa['yll_per_death'] = res_causa['age_group'].map(WHO_STANDARD_YLL)
    res_causa['YLL'] = res_causa['deaths_attr'] * res_causa['yll_per_death']
    
    yll_por_causa = res_causa.groupby('causa')['YLL'].sum()
    print("\n" + "="*70)
    print("YLL atribuibles por causa")
    print("="*70)
    for c, y in yll_por_causa.items():
        print(f"  {c}: {y:,.0f}")
    print(f"  Total (4 causas): {yll_por_causa.sum():,.0f}")
    
    # Resumen
    resumen = res_causa.groupby('causa').agg({
        'deaths': 'sum',
        'deaths_attr': 'sum',
        'YLL': 'sum'
    }).reset_index()
    resumen['YLL_per_1000'] = resumen['YLL'] / (resumen['deaths'] / 1000)  # YLL por 1000 muertes de esa causa
    resumen.to_csv(OUT / "yll_por_causa.csv", index=False)
    
    # Guardar resumen
    resumen_out = pd.DataFrame({
        'causa': list(yll_por_causa.index) + ['Total'],
        'deaths_attr': list(res_causa.groupby('causa')['deaths_attr'].sum()) + [res_causa['deaths_attr'].sum()],
        'YLL': list(yll_por_causa.values) + [yll_por_causa.sum()],
    })
    resumen_out.to_csv(OUT / "resumen_yll_por_causa.csv", index=False)
    
    print(f"\nGuardado: {OUT / 'yll_allcause_por_estrato.csv'}")
    print(f"Guardado: {OUT / 'yll_por_causa.csv'}")
    print(f"Guardado: {OUT / 'resumen_yll_por_causa.csv'}")
    
    print("\n" + "="*70)
    print("NOTA: DALYs = YLL + YLD. No calculamos YLD (requiere incidencia de")
    print("enfermedades no fatales y pesos de discapacidad). Solo YLL es reportable.")
    print("="*70)


if __name__ == "__main__":
    main()
