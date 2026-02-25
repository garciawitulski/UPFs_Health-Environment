# -*- coding: utf-8 -*-
"""
Calcula la esperanza de vida perdida (o ganada si se elimina el riesgo) atribuible
al consumo de alimentos ultraprocesados en Argentina.

Método: Tabla de vida con causa eliminada (cause-deleted life table).
- Se construye la tabla de vida actual con mortalidad observada (DEIS 2018).
- Se construye una tabla "sin UPF" eliminando las muertes atribuibles a UPF.
- Ganancia en esperanza de vida = e_x(sin UPF) - e_x(con UPF)

Datos de población: ENNyS 2 (pesos F_STG_calib, calibrados INDEC) o archivo manual.

Uso: python calcular_esperanza_vida.py
"""

import pandas as pd
import numpy as np
from pathlib import Path

OUT = Path(__file__).parent / "output"
DATA = Path(__file__).parent / "data"

# Esperanza de vida restante a los 70 años (estándar WHO GHE) para cerrar la tabla
E70_STANDARD = 18.5  # años (aproximado para Argentina, OMS)


def build_life_table(deaths, population, age_order):
    """Construye tabla de vida abrevada (5 años) desde edad 30.
    deaths, population: Series indexados por age_group.
    """
    # nM_x = tasa central de mortalidad = defunciones en n años / personas-año vividos
    # Con datos anuales: nM_x ≈ D/(n*P) = d/(5*P) para n=5, o aprox. d/P como tasa anual
    # Usamos 5M_x = d/P (tasa anual); 5q_x = 5*5M_x / (1 + 2.5*5M_x)
    n = 5
    m_list = []
    for ag in age_order:
        d = deaths.get(ag, 0)
        p = population.get(ag, 1)
        # Tasa central de mortalidad por persona-año: d/P (deaths per capita per year)
        nM_x = d / p if p > 0 else 0
        # Probabilidad de muerte en 5 años: 5q_x = 5*nM_x / (1 + 2.5*nM_x)
        qx5 = (n * nM_x) / (1 + (n/2) * nM_x) if nM_x < 2 else 0.999
        m_list.append({'age_group': ag, 'deaths': d, 'pop': p, '5M_x': nM_x, '5q_x': qx5})

    lt = pd.DataFrame(m_list)

    # l_x (supervivientes)
    l = [100_000]
    for i in range(1, len(lt)):
        l.append(l[-1] * (1 - lt.iloc[i-1]['5q_x']))
    lt['l_x'] = l

    # 5L_x (personas-año vividos)
    # l_70 para el último grupo
    l70 = lt.iloc[-1]['l_x'] * (1 - lt.iloc[-1]['5q_x'])
    lt['5L_x'] = n * (lt['l_x'] + lt['l_x'].shift(-1).fillna(l70)) / 2
    # T_65 incluye años vividos 70+ (l_70 * e_70)
    T_rev = lt['5L_x'].iloc[::-1].cumsum().iloc[::-1].values
    T_rev[-1] = T_rev[-1] + l70 * E70_STANDARD
    lt['T_x'] = T_rev

    # e_x (esperanza de vida restante)
    lt['e_x'] = lt['T_x'] / lt['l_x']

    return lt


def main():
    age_order = ['30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69']

    # Población: preferir ENNyS (INDEC), sino archivo manual
    pop_path_ennys = DATA / "poblacion_argentina_2018_ennys.csv"
    pop_path_manual = DATA / "poblacion_argentina_2018.csv"
    if pop_path_ennys.exists():
        pop_df = pd.read_csv(pop_path_ennys)
        pop_source = "ENNyS 2 (pesos F_STG_calib, INDEC)"
    elif pop_path_manual.exists():
        pop_df = pd.read_csv(pop_path_manual)
        pop_source = "data/poblacion_argentina_2018.csv"
    else:
        print("ERROR: No se encontró archivo de población.")
        print("Ejecutar: python data/extraer_poblacion_ennys.py")
        return

    # Defunciones y muertes atribuibles (DEIS 2019)
    def_path = DATA / "defunciones_2019_por_estrato.csv"
    if not def_path.exists():
        print("ERROR: No existe defunciones_2019_por_estrato.csv. Ejecutar: python data/descargar_defunciones_2019.py; python data/agregar_defunciones_2019.py")
        return
    def_df = pd.read_csv(def_path)
    res = pd.read_csv(OUT / "resultados_paper_argentina.csv")

    # Combinar por sexo (suma M+F para tabla de vida simplificada)
    # O bien: hacer tabla separada por sexo
    pop_agg = pop_df.groupby('age_group')['population'].sum()
    deaths_agg = def_df.groupby('age_group')['deaths'].sum()
    attr_agg = res.groupby('age_group')['deaths_attr'].sum()

    # Tabla actual
    deaths_dict = {ag: deaths_agg.get(ag, 0) for ag in age_order}
    pop_dict = {ag: pop_agg.get(ag, 1) for ag in age_order}
    attr_dict = {ag: attr_agg.get(ag, 0) for ag in age_order}

    lt_actual = build_life_table(deaths_dict, pop_dict, age_order)
    # Tabla sin UPF
    deaths_sin_upf = {ag: max(0, deaths_dict[ag] - attr_dict[ag]) for ag in age_order}
    lt_sin_upf = build_life_table(deaths_sin_upf, pop_dict, age_order)

    e30_actual = lt_actual[lt_actual['age_group'] == '30-34']['e_x'].values[0]
    e30_sin_upf = lt_sin_upf[lt_sin_upf['age_group'] == '30-34']['e_x'].values[0]
    ganancia_e30 = e30_sin_upf - e30_actual

    # Por sexo
    results_sex = []
    for sexo_m in [0, 1]:
        pop_s = pop_df[pop_df['sexo_m'] == sexo_m].set_index('age_group')['population']
        deaths_s = def_df[def_df['sexo_m'] == sexo_m].set_index('age_group')['deaths']
        attr_s = res[res['sexo_m'] == sexo_m].set_index('age_group')['deaths_attr']
        sex_label = "Mujeres" if sexo_m == 0 else "Hombres"
        deaths_d = {ag: deaths_s.get(ag, 0) for ag in age_order}
        pop_d = {ag: pop_s.get(ag, 1) for ag in age_order}
        attr_d = {ag: attr_s.get(ag, 0) for ag in age_order}
        lt_a = build_life_table(deaths_d, pop_d, age_order)
        lt_b = build_life_table({ag: max(0, deaths_d[ag] - attr_d[ag]) for ag in age_order}, pop_d, age_order)
        e30_a = lt_a[lt_a['age_group'] == '30-34']['e_x'].values[0]
        e30_b = lt_b[lt_b['age_group'] == '30-34']['e_x'].values[0]
        results_sex.append({
            'sexo': sex_label,
            'e30_actual': e30_a,
            'e30_sin_UPF': e30_b,
            'ganancia_años': e30_b - e30_a,
        })

    # Salida
    print("="*70)
    print("ESPERANZA DE VIDA PERDIDA POR CONSUMO DE UPF - ARGENTINA 2019")
    print("Método: Tabla de vida con causa eliminada")
    print("="*70)
    print(f"\nPoblación: {pop_source}")
    print(f"Población total 30-69 años: {pop_agg.sum():,.0f}")

    print("\n--- Resultados totales (ambos sexos) ---")
    print(f"  Esperanza de vida a los 30 años (actual):     {e30_actual:.2f} años")
    print(f"  Esperanza de vida a los 30 años (sin UPF):    {e30_sin_upf:.2f} años")
    print(f"  GANANCIA si se eliminaran muertes UPF:        +{ganancia_e30:.3f} años")

    print("\n--- Por sexo ---")
    for r in results_sex:
        print(f"  {r['sexo']}: e30 actual={r['e30_actual']:.2f}, sin UPF={r['e30_sin_UPF']:.2f}, ganancia={r['ganancia_años']:.3f} años")

    # Guardar
    out_list = [{'tipo': 'Total', 'e30_actual': e30_actual, 'e30_sin_UPF': e30_sin_upf, 'ganancia_años': ganancia_e30}]
    out_list.extend([{'tipo': r['sexo'], 'e30_actual': r['e30_actual'], 'e30_sin_UPF': r['e30_sin_UPF'], 'ganancia_años': r['ganancia_años']} for r in results_sex])
    out_df = pd.DataFrame(out_list)
    out_df.to_csv(OUT / "esperanza_vida_perdida_UPF.csv", index=False, encoding='utf-8-sig')
    print(f"\nGuardado: {OUT / 'esperanza_vida_perdida_UPF.csv'}")


if __name__ == "__main__":
    main()
