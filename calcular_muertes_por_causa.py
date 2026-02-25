# -*- coding: utf-8 -*-
"""
Calcula muertes atribuibles a UPF por causa específica (diabetes, ECV, cerebrovascular, cáncer).
RR de Pagliai et al. Br J Nutr 2021 y estudios incluidos.
"""
import pandas as pd
import numpy as np
from pathlib import Path

OUT = Path(__file__).parent / "output"
RR_REF_UPF = 0.357  # 35.7% UPF como referencia

# RR por causa (Pagliai et al. 2021 - high vs low UPF)
RR_BY_CAUSE = {
    'diabetes': 1.25,      # proxy all-cause (no hay RR mortalidad diabetes)
    'cvd': 1.29,           # Pagliai: 3 estudios, RR 1.29 (1.12-1.48)
    'cerebrovascular': 1.34, # Pagliai: 2 estudios, RR 1.34 (1.07-1.68)
    'cancer': 1.23,        # Fiolet/NutriNet (Pagliai): cancer risk 1.23 (1.08-1.40)
}

def rr_func(x_pct, rr_point):
    """RR exponencial: RR(0)=1, RR(35.7)=rr_point"""
    x = np.clip(x_pct / 100, 0.001, 1)
    return np.exp(np.log(rr_point) / RR_REF_UPF * x)

def paf(mean_upf, rr_point):
    """PAF = (RR - 1) / RR para contrafactual 0%"""
    rr = rr_func(mean_upf, rr_point)
    return (rr - 1) / rr if rr > 0 else 0

def main():
    upf = pd.read_csv(OUT / "upf_por_estrato.csv")
    def_causa = pd.read_csv(OUT / "defunciones_2019_por_causa.csv")
    
    res_list = []
    for causa, rr_point in RR_BY_CAUSE.items():
        df = def_causa[def_causa['causa'] == causa].merge(
            upf[['sexo_m', 'age_group', 'pct_upf_mean']],
            on=['sexo_m', 'age_group'],
            how='left'
        )
        df['PAF'] = df['pct_upf_mean'].apply(lambda m: paf(m, rr_point))
        df['deaths_attr'] = df['PAF'] * df['deaths']
        df['causa'] = causa
        res_list.append(df[['sexo_m', 'age_group', 'causa', 'deaths', 'PAF', 'deaths_attr']])
    
    res = pd.concat(res_list, ignore_index=True)
    
    # Resumen por causa
    summ = res.groupby('causa').agg(
        deaths=('deaths', 'sum'),
        deaths_attr=('deaths_attr', 'sum')
    ).reset_index()
    summ['pct_attr'] = 100 * summ['deaths_attr'] / summ['deaths']
    
    print("="*60)
    print("Muertes atribuibles a UPF por causa (30-69 años, 2019)")
    print("="*60)
    print(summ.to_string(index=False))
    print("\nPor estrato (primeros registros):")
    print(res.head(16).to_string())
    
    res.to_csv(OUT / "resultados_por_causa.csv", index=False, encoding='utf-8-sig')
    summ.to_csv(OUT / "resumen_muertes_por_causa.csv", index=False, encoding='utf-8-sig')
    print(f"\nGuardado: {OUT / 'resultados_por_causa.csv'}")
    print(f"Guardado: {OUT / 'resumen_muertes_por_causa.csv'}")
    
    # Total all-cause para comparar
    allcause = pd.read_csv(OUT / "resultados_paper_argentina.csv")
    total_all = allcause['deaths_attr'].sum()
    total_causa = res['deaths_attr'].sum()
    print(f"\nComparación:")
    print(f"  All-cause attributable (modelo actual): {total_all:,.0f}")
    print(f"  Suma 4 causas específicas: {total_causa:,.0f}")

if __name__ == "__main__":
    main()
