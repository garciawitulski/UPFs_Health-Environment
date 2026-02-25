# -*- coding: utf-8 -*-
"""
Genera Tabla 1 y Tabla 2 en formato comparable al paper de Brasil (Nilson et al. 2023).
Uso: python generar_tablas_paper.py
"""

import pandas as pd
import numpy as np
from pathlib import Path

OUT = Path(__file__).parent / "output"
RR_POINT = 1.25
RR_REF_UPF = 0.357

def format_pct(val):
    return f"{val:.1f}"

def format_pct_ci(mean, low, high):
    return f"{mean:.1f} ({low:.1f}, {high:.1f})"

def main():
    # Cargar resultados Argentina
    upf = pd.read_csv(OUT / "upf_por_estrato.csv")
    res = pd.read_csv(OUT / "resultados_paper_argentina.csv")

    # Índice edad para orden
    age_order = ['30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69']

    # ========== TABLA 1: Contribución UPF a energía total (%)
    # Formato Brasil: Age | Men % (95% CI) | Women % (95% CI)
    # sexo_m: 1=Men, 0=Women
    # 95% CI aproximado: mean ± 1.96 * SE, con SE = sd/sqrt(n)
    upf['SE'] = upf['pct_upf_sd'] / np.sqrt(upf['n'])
    upf['ci_low'] = np.clip(upf['pct_upf_mean'] - 1.96 * upf['SE'], 0, 100)
    upf['ci_high'] = np.clip(upf['pct_upf_mean'] + 1.96 * upf['SE'], 0, 100)

    men = upf[upf['sexo_m']==1].set_index('age_group').reindex(age_order)
    women = upf[upf['sexo_m']==0].set_index('age_group').reindex(age_order)

    tabla1 = pd.DataFrame({
        'Age groups, years': age_order,
        'Men, % (95% CI)': [format_pct_ci(m['pct_upf_mean'], m['ci_low'], m['ci_high']) 
                            if pd.notna(m['pct_upf_mean']) else '' 
                            for _, m in men.iterrows()],
        'Women, % (95% CI)': [format_pct_ci(w['pct_upf_mean'], w['ci_low'], w['ci_high']) 
                              if pd.notna(w['pct_upf_mean']) else '' 
                              for _, w in women.iterrows()],
    })

    # Totales (promedio ponderado por n)
    n_men = men['n'].sum()
    n_women = women['n'].sum()
    mean_men = (men['pct_upf_mean'] * men['n']).sum() / n_men if n_men else 0
    mean_women = (women['pct_upf_mean'] * women['n']).sum() / n_women if n_women else 0
    sd_men = np.sqrt(((men['pct_upf_sd']**2 * men['n']).sum()) / n_men) if n_men else 0
    sd_women = np.sqrt(((women['pct_upf_sd']**2 * women['n']).sum()) / n_women) if n_women else 0
    se_men = sd_men / np.sqrt(n_men) if n_men else 0
    se_women = sd_women / np.sqrt(n_women) if n_women else 0

    tabla1.loc[len(tabla1)] = [
        'Total',
        format_pct_ci(mean_men, max(0, mean_men - 1.96*se_men), min(100, mean_men + 1.96*se_men)),
        format_pct_ci(mean_women, max(0, mean_women - 1.96*se_women), min(100, mean_women + 1.96*se_women)),
    ]

    tabla1.to_csv(OUT / "Tabla1_UPF_por_energia_Argentina.csv", index=False, encoding='utf-8-sig')
    print("="*70)
    print("TABLA 1. Contribution of Ultraprocessed Foods to Total Energy Intake")
    print("         in Argentine Adults Aged 30-69 Years by Sex and Age Group")
    print("         (ENNyS 2)")
    print("="*70)
    print(tabla1.to_string(index=False))
    print(f"\nGuardado: {OUT / 'Tabla1_UPF_por_energia_Argentina.csv'}")

    # ========== TABLA 2: Deaths, PAF, Deaths Attributable
    # Formato Brasil: Age | Men PAF | Women PAF | Men deaths | Women deaths | Total
    men_r = res[res['sexo_m']==1].set_index('age_group').reindex(age_order)
    women_r = res[res['sexo_m']==0].set_index('age_group').reindex(age_order)

    # PAF en % (×100), deaths como enteros
    def fmt_paf(p):
        return f"{p*100:.1f}" if pd.notna(p) else ""

    tabla2 = pd.DataFrame({
        'Age groups, years': age_order,
        'Men, PAF %': [fmt_paf(m['PAF']) for _, m in men_r.iterrows()],
        'Women, PAF %': [fmt_paf(w['PAF']) for _, w in women_r.iterrows()],
        'Men, deaths attr': [f"{int(round(m['deaths_attr']))}" if pd.notna(m['deaths_attr']) else '' for _, m in men_r.iterrows()],
        'Women, deaths attr': [f"{int(round(w['deaths_attr']))}" if pd.notna(w['deaths_attr']) else '' for _, w in women_r.iterrows()],
    })
    def total_attr(a):
        try:
            m = men_r.loc[a]['deaths_attr'] if pd.notna(men_r.loc[a]['deaths_attr']) else 0
        except (KeyError, TypeError):
            m = 0
        try:
            w = women_r.loc[a]['deaths_attr'] if pd.notna(women_r.loc[a]['deaths_attr']) else 0
        except (KeyError, TypeError):
            w = 0
        return int(round(m + w))
    tabla2['Total, deaths attr'] = [total_attr(a) for a in age_order]

    # Fila Total
    tot_men_deaths = men_r['deaths_attr'].sum()
    tot_women_deaths = women_r['deaths_attr'].sum()
    tot_deaths = men_r['deaths'].sum() + women_r['deaths'].sum()
    paf_men = tot_men_deaths / men_r['deaths'].sum() * 100 if men_r['deaths'].sum() > 0 else 0
    paf_women = tot_women_deaths / women_r['deaths'].sum() * 100 if women_r['deaths'].sum() > 0 else 0

    tabla2.loc[len(tabla2)] = [
        'Total',
        f"{paf_men:.1f}",
        f"{paf_women:.1f}",
        f"{int(round(tot_men_deaths))}",
        f"{int(round(tot_women_deaths))}",
        f"{int(round(tot_men_deaths + tot_women_deaths))}",
    ]

    tabla2.to_csv(OUT / "Tabla2_Muertes_atribuibles_Argentina.csv", index=False, encoding='utf-8-sig')
    print("\n" + "="*70)
    print("TABLA 2. Deaths From All Causes, PAF, and Estimated Deaths Attributable")

    # ========== TABLA 3: Escenarios de reducción (10%, 20%, 50%)
    def rr(x):
        x = np.clip(x/100, 0.001, 1)
        return np.exp(np.log(RR_POINT) / RR_REF_UPF * x)

    def paf_simple(mean_upf, reduction=0):
        m = np.clip(mean_upf/100, 0.001, 1)
        m_cf = m * (1 - reduction)
        rr_b = rr(m*100)
        rr_cf = rr(m_cf*100) if m_cf > 0 else 1.0
        return (rr_b - rr_cf) / rr_b if rr_b > 0 else 0

    deaths_averted = {}
    for red_pct, red in [(10, 0.1), (20, 0.2), (50, 0.5)]:
        tot = 0
        for _, r in res.iterrows():
            paf_red = paf_simple(r['pct_upf_mean'], reduction=red)
            tot += paf_red * r['deaths']
        deaths_averted[red_pct] = int(round(tot))

    tabla3 = pd.DataFrame({
        'Scenario': ['10% reduction', '20% reduction', '50% reduction'],
        'Deaths averted (approximate)': [f"~{deaths_averted[10]}", f"~{deaths_averted[20]}", f"~{deaths_averted[50]}"],
    })
    tabla3.to_csv(OUT / "Tabla3_Escenarios_reduccion_Argentina.csv", index=False, encoding='utf-8-sig')
    print("\n" + "="*70)
    print("TABLA 3. Estimated Premature Deaths Averted by Reduction Scenarios")
    print("="*70)
    print(tabla3.to_string(index=False))
    print(f"\nGuardado: {OUT / 'Tabla3_Escenarios_reduccion_Argentina.csv'}")
    print("         to the Consumption of Ultraprocessed Foods in Argentine Adults")
    print("         Aged 30-69 Years (defunciones DEIS 2019)")
    print("="*70)
    print(tabla2.to_string(index=False))
    print(f"\nGuardado: {OUT / 'Tabla2_Muertes_atribuibles_Argentina.csv'}")

    # ========== TABLA 4: Muertes atribuibles por causa
    res_causa = pd.read_csv(OUT / "resumen_muertes_por_causa.csv")
    causa_map = {
        'diabetes': 'Diabetes',
        'cvd': 'Cardiovascular disease',
        'cerebrovascular': 'Cerebrovascular disease',
        'cancer': 'Cancer',
    }
    tabla4 = pd.DataFrame({
        'Cause': [causa_map.get(c, c) for c in res_causa['causa']],
        'Deaths': res_causa['deaths'].astype(int),
        'Attributable deaths': (res_causa['deaths_attr'].round()).astype(int),
        '% of cause': (res_causa['pct_attr']).round(1).astype(str) + '%',
    })
    tabla4.loc[len(tabla4)] = [
        'Total (3 causes)',
        int(res_causa['deaths'].sum()),
        int(round(res_causa['deaths_attr'].sum())),
        f"{100 * res_causa['deaths_attr'].sum() / res_causa['deaths'].sum():.1f}%",
    ]
    tabla4.to_csv(OUT / "Tabla4_Muertes_por_causa_Argentina.csv", index=False, encoding='utf-8-sig')
    print("\n" + "="*70)
    print("TABLA 4. Deaths and Deaths Attributable to UPF Consumption by Cause")
    print("         in Argentine Adults Aged 30-69 Years (2019)")
    print("="*70)
    print(tabla4.to_string(index=False))
    print(f"\nGuardado: {OUT / 'Tabla4_Muertes_por_causa_Argentina.csv'}")

    # ========== TABLA 5: YLL por causa
    if (OUT / "resumen_yll_por_causa.csv").exists():
        yll = pd.read_csv(OUT / "resumen_yll_por_causa.csv")
        yll_total = pd.read_csv(OUT / "yll_allcause_por_estrato.csv")
        filas = []
        for _, r in yll[yll['causa'] != 'Total'].iterrows():
            filas.append({'Cause': causa_map.get(r['causa'], r['causa']), 'Attributable deaths': int(round(r['deaths_attr'])), 'YLL': f"~{int(r['YLL']):,}"})
        tot4 = yll[yll['causa'] == 'Total'].iloc[0]
        filas.append({'Cause': 'Total (3 causes)', 'Attributable deaths': int(round(tot4['deaths_attr'])), 'YLL': f"~{int(tot4['YLL']):,}"})
        filas.append({'Cause': 'All causes', 'Attributable deaths': int(round(tot_men_deaths + tot_women_deaths)), 'YLL': f"~{int(yll_total['YLL'].sum()):,}"})
        tabla5 = pd.DataFrame(filas)
        tabla5.to_csv(OUT / "Tabla5_YLL_por_causa_Argentina.csv", index=False, encoding='utf-8-sig')
        print("\n" + "="*70)
        print("TABLA 5. YLL Attributable to UPF Consumption by Cause")
        print("="*70)
        print(tabla5.to_string(index=False))

    # ========== TABLA 6 y 7: Esperanza de vida y costos (si existen)
    if (OUT / "esperanza_vida_perdida_UPF.csv").exists():
        ev = pd.read_csv(OUT / "esperanza_vida_perdida_UPF.csv")
        ev.to_csv(OUT / "Tabla6_Esperanza_vida_UPF.csv", index=False, encoding='utf-8-sig')
        print("\nTABLA 6. Life expectancy at 30: actual vs. without UPF deaths")
        print(ev.to_string(index=False))
    if (OUT / "resumen_costos_indirectos.csv").exists():
        cost = pd.read_csv(OUT / "resumen_costos_indirectos.csv")
        cost.to_csv(OUT / "Tabla7_Costos_indirectos_Argentina.csv", index=False, encoding='utf-8-sig')
        print("\nTABLA 7. Indirect economic costs (PVLE)")
        print(cost.to_string(index=False))

    # Tabla comparativa Brasil vs Argentina (solo totales)
    print("\n" + "="*70)
    print("COMPARACIÓN BRASIL vs ARGENTINA (totales)")
    print("="*70)
    comp = pd.DataFrame({
        'Métrica': [
            'Muertes totales 30-69 años',
            'Muertes atribuibles a UPF',
            '% muertes prematuras atribuibles',
            'Contribución UPF energía (hombres, aprox)',
            'Contribución UPF energía (mujeres, aprox)',
        ],
        'Brasil 2019': ['541,160', '~57,000', '10.5%', '13-18%', '16-21%'],
        'Argentina 2019': [
            f"{int(tot_deaths):,}",
            f"{int(round(tot_men_deaths + tot_women_deaths)):,}",
            f"{100*(tot_men_deaths+tot_women_deaths)/tot_deaths:.1f}%" if tot_deaths else '-',
            f"{mean_men:.1f}%",
            f"{mean_women:.1f}%",
        ],
    })
    print(comp.to_string(index=False))

if __name__ == "__main__":
    main()
