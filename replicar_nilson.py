# -*- coding: utf-8 -*-
"""
Replicación del paper de Nilson et al. (2023) para Argentina
"Premature Deaths Attributable to the Consumption of Ultraprocessed Foods"

Modelo de evaluación comparativa de riesgos (Comparative Risk Assessment)
Adaptado de Brasil a Argentina usando datos de ENNyS 2 (2018-2019)

Fuente original: Am J Prev Med 2023;64(1):129-136
GitHub original: https://github.com/eduardonilson/UPF-modeling
"""

import pandas as pd
import numpy as np
from scipy import stats
# from scipy.integrate import quad  # Replaced by analytical approximation
import json
import warnings
import os
import sys

warnings.filterwarnings('ignore')

# Forzar output sin buffer
sys.stdout.reconfigure(line_buffering=True)

BASE_DIR = r'c:\Users\admin\Documents\Papers\Ultra-processed_Mortality\ENNyS'

# ============================================================================
# PASO 1: Construir tabla de energía por código de alimento
# ============================================================================
print("=" * 70)
print("PASO 1: Construyendo tabla de energía por código de alimento")
print("=" * 70)

# 1a. Energía desde alimentos_sara.xls (match directo por código)
sara = pd.read_excel(os.path.join(BASE_DIR, 'alimentos_sara.xls'))
energy_lookup = dict(zip(sara['Código'], sara['Energía (kcal)']))
print(f"  Códigos con energía desde sara: {len(energy_lookup)}")

# 1b. Obtener todos los códigos de alimentos de Base_Alimentos
alim_header = pd.read_csv(os.path.join(BASE_DIR, 'Base_Alimentos_Bebidas_Suplementos.csv'), nrows=0)
META_COLS = ['informe_id', 'miembro_id', 'clave', 'fecha_realizacion',
             'nro_R24H', 'dia_anterior', 'UPM', 'EUPM', 'F_STG_calib']
food_cols = [c for c in alim_header.columns if c not in META_COLS]
print(f"  Códigos de alimentos en Base_Alimentos: {len(food_cols)}")

# 1c. Match por descripción con composicion_quimica
nova = pd.read_csv(os.path.join(BASE_DIR, 'alimentos_clasificados_NOVA.csv'))
comp = pd.read_excel(os.path.join(BASE_DIR, 'composicion_quimica_alimentos.xlsx'))

def normalize(s):
    return str(s).lower().strip().replace(',', '').replace('  ', ' ')

comp_energy = {}
for _, row in comp.iterrows():
    d = normalize(row['descripcion'])
    e = row['Valor energético']
    if pd.notna(e):
        comp_energy[d] = float(e)

nova_desc = dict(zip(nova['codigo'], nova['descripcion']))
matched_comp = 0
for code in food_cols:
    if code in energy_lookup:
        continue
    if code in nova_desc:
        desc = normalize(nova_desc[code])
        if desc in comp_energy:
            energy_lookup[code] = comp_energy[desc]
            matched_comp += 1
            continue
        for suffix in [' cruda', ' crudo', ' crudas', ' crudos', ' fresca',
                       ' fresco', ' frescas', ' hervida', ' hervido']:
            if desc + suffix in comp_energy:
                energy_lookup[code] = comp_energy[desc + suffix]
                matched_comp += 1
                break

print(f"  Códigos matcheados via composición: {matched_comp}")

# 1d. Defaults por categoría para los no matcheados
cat_defaults = {
    'A': 300, 'B': 30, 'C': 180, 'D': 300, 'E': 200, 'F': 50,
    'G': 700, 'H': 150, 'L': 60, 'O': 5, 'P': 120, 'Q': 300,
    'R': 150, 'S': 50, 'V': 25, 'Y': 80, 'Z': 500
}
for code in food_cols:
    if code not in energy_lookup:
        energy_lookup[code] = cat_defaults.get(code[0], 100)

print(f"  Total códigos con energía: {len(energy_lookup)}")

# 1e. Clasificación NOVA por código
nova_class = dict(zip(nova['codigo'], nova['NOVA']))

# ============================================================================
# PASO 2: Cargar datos demográficos y computar edad/sexo
# ============================================================================
print("\n" + "=" * 70)
print("PASO 2: Cargando datos demográficos y de consumo")
print("=" * 70)

# Cargar encuesta para sexo y fecha de nacimiento
enc = pd.read_csv(os.path.join(BASE_DIR, 'ENNyS2_encuesta.csv'),
                  usecols=['id', 'miembro_id', 'C4_SEXO', 'C4_FECHA', 'E_CUEST'],
                  low_memory=False)
enc_adults = enc[enc['E_CUEST'] == '18 o mas años'].copy()

# Parsear fecha de nacimiento y calcular edad (referencia: 2019)
def parse_date(d):
    if pd.isna(d) or str(d).strip() == '':
        return np.nan
    d = str(d).strip()
    for fmt in ['%m/%d/%Y', '%d/%m/%Y', '%Y-%m-%d']:
        try:
            return pd.to_datetime(d, format=fmt)
        except:
            continue
    try:
        return pd.to_datetime(d)
    except:
        return np.nan

enc_adults['fecha_nac'] = enc_adults['C4_FECHA'].apply(parse_date)
enc_adults['edad_2019'] = (pd.Timestamp('2019-07-01') - enc_adults['fecha_nac']).dt.days / 365.25
enc_adults = enc_adults.dropna(subset=['edad_2019', 'C4_SEXO'])
enc_adults['sexo'] = enc_adults['C4_SEXO'].map({'Mujer': 'F', 'Varón': 'M'})
enc_adults = enc_adults.dropna(subset=['sexo'])

# Filtrar 30-69 años
enc_30_69 = enc_adults[(enc_adults['edad_2019'] >= 30) & (enc_adults['edad_2019'] < 70)].copy()

# Crear grupos de edad de 5 años
bins = [30, 35, 40, 45, 50, 55, 60, 65, 70]
labels = ['30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69']
enc_30_69['age_group'] = pd.cut(enc_30_69['edad_2019'], bins=bins, labels=labels, right=False)
enc_30_69 = enc_30_69.dropna(subset=['age_group'])

print(f"  Adultos 30-69 en encuesta: {len(enc_30_69)}")
print(f"  Por sexo: {enc_30_69['sexo'].value_counts().to_dict()}")

# ============================================================================
# PASO 3: Calcular contribución energética de UPF por persona
# ============================================================================
print("\n" + "=" * 70)
print("PASO 3: Calculando contribución energética de UPF por persona")
print("=" * 70)

# IDs de adultos 30-69 para filtrar
adult_ids = set(enc_30_69['miembro_id'].unique())

# Cargar Base_Alimentos por chunks para eficiencia de memoria
print("  Cargando Base_Alimentos (esto puede tardar)...")
alim = pd.read_csv(os.path.join(BASE_DIR, 'Base_Alimentos_Bebidas_Suplementos.csv'),
                   low_memory=False)

# Filtrar solo adultos 30-69
alim_adults = alim[alim['miembro_id'].isin(adult_ids)].copy()
print(f"  Registros de alimentos para adultos 30-69: {len(alim_adults)}")

# Códigos de edulcorantes (son 0/1, no gramos - excluir del cálculo energético)
edulc_codes = set(['D083', 'D084', 'D085', 'D071', 'D086', 'D093', 'D094',
                   'D092', 'D091', 'D088', 'D089', 'D072', 'D090', 'D087'])
# Códigos de sal (representan cantidad en preparaciones)
sal_codes = set(['O026', 'O050', 'O033', 'O027', 'O031', 'O032'])

# Códigos válidos para cálculo (excluir edulcorantes y sal)
valid_food_cols = [c for c in food_cols if c not in edulc_codes and c not in sal_codes]

# Convertir columnas de alimentos a numérico (vectorizado)
print("  Convirtiendo datos a numérico...")
for col in valid_food_cols:
    alim_adults[col] = pd.to_numeric(alim_adults[col], errors='coerce').fillna(0)

# Crear vectores de energía por 100g y máscara UPF
energy_vec = np.array([energy_lookup.get(c, 0) for c in valid_food_cols])
upf_mask = np.array([1 if nova_class.get(c, 0) == 4 else 0 for c in valid_food_cols])

# Cálculo vectorizado: energía = grams * kcal_100g / 100
print("  Calculando energía por alimento (vectorizado)...")
food_matrix = alim_adults[valid_food_cols].values  # (n_records, n_foods)
energy_matrix = food_matrix * energy_vec[np.newaxis, :] / 100.0  # kcal por alimento

# Energía total y energía UPF
total_energy_calc = energy_matrix.sum(axis=1)
upf_energy = (energy_matrix * upf_mask[np.newaxis, :]).sum(axis=1)

# Crear DataFrame de resultados
df_upf = pd.DataFrame({
    'miembro_id': alim_adults['miembro_id'].values,
    'informe_id': alim_adults['informe_id'].values,
    'nro_R24H': alim_adults['nro_R24H'].values,
    'total_energy_calc': total_energy_calc,
    'upf_energy': upf_energy,
    'upf_pct': np.where(total_energy_calc > 0, upf_energy / total_energy_calc * 100, 0)
})

print(f"  Registros persona-día calculados: {len(df_upf)}")

# Validar contra Base_Nutrientes
nut = pd.read_csv(os.path.join(BASE_DIR, 'Base_Nutrientes.csv'),
                  usecols=['informe_id', 'miembro_id', 'tot_energia_kcal'])
df_upf = df_upf.merge(nut, on=['informe_id', 'miembro_id'], how='left')

# Usar total de energía de Base_Nutrientes (más preciso) para denominador
df_upf['upf_pct_adj'] = np.where(
    df_upf['tot_energia_kcal'] > 0,
    df_upf['upf_energy'] / df_upf['tot_energia_kcal'] * 100,
    df_upf['upf_pct']
)

# Promediar por persona (si tiene 2 R24H)
df_person = df_upf.groupby('miembro_id').agg(
    upf_pct=('upf_pct_adj', 'mean'),
    n_recalls=('informe_id', 'count')
).reset_index()

print(f"  Personas únicas con datos: {len(df_person)}")

# ============================================================================
# PASO 4: Calcular % UPF por sexo y grupo de edad (Tabla 1)
# ============================================================================
print("\n" + "=" * 70)
print("PASO 4: Contribución de UPF a la energía total (Tabla 1)")
print("=" * 70)

# Merge con demografía
df_analysis = df_person.merge(
    enc_30_69[['miembro_id', 'sexo', 'age_group']],
    on='miembro_id', how='inner'
)

print(f"  Personas en análisis final: {len(df_analysis)}")

# Tabla 1: % UPF por sexo y grupo de edad
table1_data = []
for ag in labels:
    row_data = {'age_group': ag}
    for sex, sex_label in [('M', 'Hombres'), ('F', 'Mujeres')]:
        subset = df_analysis[(df_analysis['age_group'] == ag) & (df_analysis['sexo'] == sex)]
        if len(subset) > 0:
            mean_val = subset['upf_pct'].mean()
            se = subset['upf_pct'].std() / np.sqrt(len(subset))
            ci_lo = mean_val - 1.96 * se
            ci_hi = mean_val + 1.96 * se
            sd = subset['upf_pct'].std()
            row_data[f'{sex_label}_mean'] = mean_val
            row_data[f'{sex_label}_sd'] = sd
            row_data[f'{sex_label}_ci_lo'] = ci_lo
            row_data[f'{sex_label}_ci_hi'] = ci_hi
            row_data[f'{sex_label}_n'] = len(subset)
    table1_data.append(row_data)

table1 = pd.DataFrame(table1_data)

print("\n  TABLA 1: Contribución de UPF a la energía total (%)")
print("  " + "-" * 80)
print(f"  {'Grupo edad':<12} {'Hombres % (95% CI)':<30} {'Mujeres % (95% CI)':<30}")
print("  " + "-" * 80)
for _, r in table1.iterrows():
    m_str = f"{r.get('Hombres_mean', 0):.1f} ({r.get('Hombres_ci_lo', 0):.1f}, {r.get('Hombres_ci_hi', 0):.1f})"
    f_str = f"{r.get('Mujeres_mean', 0):.1f} ({r.get('Mujeres_ci_lo', 0):.1f}, {r.get('Mujeres_ci_hi', 0):.1f})"
    print(f"  {r['age_group']:<12} {m_str:<30} {f_str:<30}")

# ============================================================================
# PASO 5: Datos de mortalidad de Argentina 2019
# ============================================================================
print("\n" + "=" * 70)
print("PASO 5: Datos de mortalidad Argentina 2019")
print("=" * 70)

# Fuente: DEIS - Dirección de Estadísticas e Información de Salud
# Ministerio de Salud de Argentina
# Defunciones por grupo de edad y sexo, Argentina 2019
# https://www.argentina.gob.ar/salud/deis
#
# NOTA: Estos datos deben verificarse con las estadísticas oficiales de DEIS.
# Los valores aquí son estimaciones basadas en datos publicados por DEIS para 2019.
# Total defunciones Argentina 2019: ~338,000 (todas las edades)
# Defunciones 30-69 años: ~100,000

# Muertes por grupo de edad y sexo (Argentina 2019 - DEIS)
# Fuente: Estadísticas Vitales 2019, DEIS, Ministerio de Salud
mortality_data = {
    '30-34': {'M': 3328, 'F': 1376},
    '35-39': {'M': 3918, 'F': 1790},
    '40-44': {'M': 4613, 'F': 2424},
    '45-49': {'M': 6057, 'F': 3307},
    '50-54': {'M': 8362, 'F': 4465},
    '55-59': {'M': 11174, 'F': 5899},
    '60-64': {'M': 14437, 'F': 7631},
    '65-69': {'M': 17625, 'F': 10013},
}

total_deaths = sum(v['M'] + v['F'] for v in mortality_data.values())
total_M = sum(v['M'] for v in mortality_data.values())
total_F = sum(v['F'] for v in mortality_data.values())

print(f"  Total muertes 30-69 años (2019): {total_deaths:,}")
print(f"  Hombres: {total_M:,} ({total_M/total_deaths*100:.1f}%)")
print(f"  Mujeres: {total_F:,} ({total_F/total_deaths*100:.1f}%)")
print()
print("  IMPORTANTE: Verificar estos datos con las estadísticas oficiales de DEIS")
print("  https://www.argentina.gob.ar/salud/deis")

# ============================================================================
# PASO 6: Modelo de Evaluación Comparativa de Riesgos (CRA)
# ============================================================================
print("\n" + "=" * 70)
print("PASO 6: Modelo de Evaluación Comparativa de Riesgos")
print("=" * 70)

# RR de meta-análisis (Pagliai et al., 2021)
# RR = 1.25 (95% CI: 1.14, 1.37) para comparación máx vs mín de UPF
# Esta RR corresponde a una contribución de 35.7% de UPF a la energía total
# vs 0% (mínimo teórico)
# Se modela como función log-lineal: RR(x) = exp(beta * x)
# donde beta = ln(1.25) / 35.7

RR_META = 1.25
RR_LO = 1.14
RR_HI = 1.37
UPF_REF = 35.7  # % de energía que produce RR_META

beta_central = np.log(RR_META) / UPF_REF
beta_lo = np.log(RR_LO) / UPF_REF
beta_hi = np.log(RR_HI) / UPF_REF

print(f"  RR meta-análisis: {RR_META} (95% CI: {RR_LO}, {RR_HI})")
print(f"  Beta (ln(RR)/UPF%): {beta_central:.6f}")
print(f"  RR por 10% energía de UPF: {np.exp(beta_central * 10):.3f}")


def rr_function(x, beta):
    """RR como función de % de UPF en la energía total."""
    return np.exp(beta * x)


def compute_paf_analytical(mean_upf, sd_upf, beta, reduction_pct=1.0):
    """
    Calcula PAF usando fórmula analítica para distribución log-normal con RR log-lineal.

    Para X ~ LogNormal(mu, sigma^2) y RR(x) = exp(beta*x):
    E[RR(X)] = exp(beta * exp(mu + sigma^2/2) * exp(beta^2 * sigma^2 / 2))
    Simplificación: E[exp(beta*X)] cuando X es log-normal tiene solución cerrada.

    Usamos aproximación basada en E[RR] = integral de RR(x)*f(x)dx
    con discretización numérica rápida (no quad).
    """
    if mean_upf <= 0 or sd_upf <= 0 or beta <= 0:
        return 0.0

    # Discretizar la distribución en N puntos (rápido)
    N_POINTS = 500
    cv = max(sd_upf / mean_upf, 0.01)
    sigma2 = np.log(1 + cv**2)
    sigma = np.sqrt(sigma2)
    mu = np.log(mean_upf) - sigma2 / 2

    # Puntos de evaluación (percentiles 0.1 a 99.9)
    x_points = np.linspace(0.001, 0.999, N_POINTS)
    x_vals = stats.lognorm.ppf(x_points, s=sigma, scale=np.exp(mu))
    pdf_vals = stats.lognorm.pdf(x_vals, s=sigma, scale=np.exp(mu))

    # E[RR(X)] baseline
    rr_vals = np.exp(beta * x_vals)
    dx = np.diff(x_vals, prepend=0)
    e_rr_baseline = np.sum(rr_vals * pdf_vals * dx)

    # Distribución contrafactual
    mean_c = mean_upf * (1 - reduction_pct)
    if mean_c <= 0.001:
        # Si reducción es ~100%, el contrafactual es ~0, E[RR] = 1
        e_rr_counter = 1.0
    else:
        sd_c = sd_upf * (1 - reduction_pct)
        sd_c = max(sd_c, 0.001)
        cv_c = sd_c / mean_c
        sigma2_c = np.log(1 + cv_c**2)
        sigma_c = np.sqrt(sigma2_c)
        mu_c = np.log(mean_c) - sigma2_c / 2

        x_vals_c = stats.lognorm.ppf(x_points, s=sigma_c, scale=np.exp(mu_c))
        pdf_vals_c = stats.lognorm.pdf(x_vals_c, s=sigma_c, scale=np.exp(mu_c))
        rr_vals_c = np.exp(beta * x_vals_c)
        dx_c = np.diff(x_vals_c, prepend=0)
        e_rr_counter = np.sum(rr_vals_c * pdf_vals_c * dx_c)

    if e_rr_baseline <= 0:
        return 0.0

    paf = (e_rr_baseline - e_rr_counter) / e_rr_baseline
    return max(0, min(paf, 1.0))


# ============================================================================
# PASO 7: Simulación Monte Carlo
# ============================================================================
print("\n" + "=" * 70)
print("PASO 7: Simulación Monte Carlo (n=10,000)")
print("=" * 70)

N_SIM = 10000
np.random.seed(42)

# Preparar distribuciones para sampling
# Beta (RR): normal en escala log
beta_se = (beta_hi - beta_lo) / (2 * 1.96)
beta_samples = np.random.normal(beta_central, beta_se, N_SIM)

# Resultados por escenario
scenarios = {
    'baseline': 1.0,     # Eliminación total (TMRL = 0%)
    'red_10': 0.10,      # Reducción del 10%
    'red_20': 0.20,      # Reducción del 20%
    'red_50': 0.50,      # Reducción del 50%
}

# Almacenar resultados
results_table2 = {}  # PAF y muertes atribuibles (baseline)
results_table3 = {}  # Muertes evitables por escenario

print("  Calculando PAFs y muertes atribuibles...")

for ag in labels:
    for sex in ['M', 'F']:
        key = (ag, sex)
        subset = df_analysis[(df_analysis['age_group'] == ag) & (df_analysis['sexo'] == sex)]
        if len(subset) < 5:
            results_table2[key] = {'paf': 0, 'paf_lo': 0, 'paf_hi': 0,
                                   'deaths': 0, 'deaths_lo': 0, 'deaths_hi': 0}
            for sc_name in scenarios:
                if sc_name not in results_table3:
                    results_table3[sc_name] = {}
                results_table3[sc_name][key] = {'deaths': 0, 'deaths_lo': 0, 'deaths_hi': 0}
            continue

        mean_upf = subset['upf_pct'].mean()
        sd_upf = subset['upf_pct'].std()
        se_upf = sd_upf / np.sqrt(len(subset))
        n_deaths = mortality_data[ag][sex]

        # Monte Carlo
        paf_sims = np.zeros(N_SIM)
        deaths_sims = np.zeros(N_SIM)
        sc_deaths_sims = {sc: np.zeros(N_SIM) for sc in scenarios if sc != 'baseline'}

        for i in range(N_SIM):
            # Sample UPF mean
            mean_sample = np.random.normal(mean_upf, se_upf)
            mean_sample = max(0.1, mean_sample)
            sd_sample = sd_upf  # SD estable

            # Sample beta
            beta_i = beta_samples[i]

            # Sample deaths (Poisson)
            deaths_i = np.random.poisson(n_deaths)

            # PAF baseline (eliminación total de UPF)
            paf_i = compute_paf_analytical(mean_sample, sd_sample, beta_i, reduction_pct=1.0)
            paf_sims[i] = paf_i
            deaths_sims[i] = paf_i * deaths_i

            # Escenarios contrafactuales
            for sc_name, red_pct in scenarios.items():
                if sc_name == 'baseline':
                    continue
                paf_sc = compute_paf_analytical(mean_sample, sd_sample, beta_i, reduction_pct=red_pct)
                sc_deaths_sims[sc_name][i] = paf_sc * deaths_i

        # Guardar resultados baseline
        results_table2[key] = {
            'paf': np.percentile(paf_sims, 50) * 100,
            'paf_lo': np.percentile(paf_sims, 2.5) * 100,
            'paf_hi': np.percentile(paf_sims, 97.5) * 100,
            'deaths': np.percentile(deaths_sims, 50),
            'deaths_lo': np.percentile(deaths_sims, 2.5),
            'deaths_hi': np.percentile(deaths_sims, 97.5),
            'mean_upf': mean_upf,
            'sd_upf': sd_upf,
            'n': len(subset),
        }

        # Guardar escenarios
        for sc_name in sc_deaths_sims:
            if sc_name not in results_table3:
                results_table3[sc_name] = {}
            results_table3[sc_name][key] = {
                'deaths': np.percentile(sc_deaths_sims[sc_name], 50),
                'deaths_lo': np.percentile(sc_deaths_sims[sc_name], 2.5),
                'deaths_hi': np.percentile(sc_deaths_sims[sc_name], 97.5),
            }

    print(f"  Completado: {ag}")

print("  Simulación finalizada.")

# ============================================================================
# PASO 8: RESULTADOS - Tabla 2 (PAF y muertes atribuibles)
# ============================================================================
print("\n" + "=" * 70)
print("TABLA 2: Muertes atribuibles al consumo de UPF, Argentina 2019")
print("=" * 70)

print(f"\n  {'Grupo':<8} {'PAF Hombres %':<22} {'PAF Mujeres %':<22} "
      f"{'Muertes H':<24} {'Muertes M':<24} {'Total':<20}")
print("  " + "-" * 120)

total_deaths_m = 0
total_deaths_m_lo = 0
total_deaths_m_hi = 0
total_deaths_f = 0
total_deaths_f_lo = 0
total_deaths_f_hi = 0

for ag in labels:
    rm = results_table2.get((ag, 'M'), {})
    rf = results_table2.get((ag, 'F'), {})

    paf_m = f"{rm.get('paf', 0):.1f} ({rm.get('paf_lo', 0):.1f}, {rm.get('paf_hi', 0):.1f})"
    paf_f = f"{rf.get('paf', 0):.1f} ({rf.get('paf_lo', 0):.1f}, {rf.get('paf_hi', 0):.1f})"

    dm = rm.get('deaths', 0)
    dm_lo = rm.get('deaths_lo', 0)
    dm_hi = rm.get('deaths_hi', 0)
    df_d = rf.get('deaths', 0)
    df_lo = rf.get('deaths_lo', 0)
    df_hi = rf.get('deaths_hi', 0)

    total_d = dm + df_d
    total_lo = dm_lo + df_lo
    total_hi = dm_hi + df_hi

    total_deaths_m += dm
    total_deaths_m_lo += dm_lo
    total_deaths_m_hi += dm_hi
    total_deaths_f += df_d
    total_deaths_f_lo += df_lo
    total_deaths_f_hi += df_hi

    deaths_m_str = f"{dm:,.0f} ({dm_lo:,.0f}, {dm_hi:,.0f})"
    deaths_f_str = f"{df_d:,.0f} ({df_lo:,.0f}, {df_hi:,.0f})"
    total_str = f"{total_d:,.0f} ({total_lo:,.0f}, {total_hi:,.0f})"

    print(f"  {ag:<8} {paf_m:<22} {paf_f:<22} {deaths_m_str:<24} {deaths_f_str:<24} {total_str:<20}")

total_all = total_deaths_m + total_deaths_f
total_all_lo = total_deaths_m_lo + total_deaths_f_lo
total_all_hi = total_deaths_m_hi + total_deaths_f_hi

print("  " + "-" * 120)
print(f"  {'Total':<8} {'':22} {'':22} "
      f"{total_deaths_m:,.0f} ({total_deaths_m_lo:,.0f}, {total_deaths_m_hi:,.0f})   "
      f"{total_deaths_f:,.0f} ({total_deaths_f_lo:,.0f}, {total_deaths_f_hi:,.0f})   "
      f"{total_all:,.0f} ({total_all_lo:,.0f}, {total_all_hi:,.0f})")

pct_of_total = total_all / total_deaths * 100
print(f"\n  Muertes atribuibles a UPF: {total_all:,.0f} de {total_deaths:,} ({pct_of_total:.1f}% de muertes prematuras)")

# ============================================================================
# PASO 9: RESULTADOS - Tabla 3 (Escenarios contrafactuales)
# ============================================================================
print("\n" + "=" * 70)
print("TABLA 3: Muertes prematuras evitables por escenarios de reducción de UPF")
print("=" * 70)

print(f"\n  {'Sexo':<10} {'Red. 10%':<28} {'Red. 20%':<28} {'Red. 50%':<28}")
print("  " + "-" * 94)

for sex, sex_label in [('M', 'Hombres'), ('F', 'Mujeres'), (None, 'Total')]:
    row_parts = [f"  {sex_label:<10}"]
    for sc_name in ['red_10', 'red_20', 'red_50']:
        total_d = 0
        total_lo = 0
        total_hi = 0
        for ag in labels:
            if sex is None:
                for s in ['M', 'F']:
                    r = results_table3[sc_name].get((ag, s), {})
                    total_d += r.get('deaths', 0)
                    total_lo += r.get('deaths_lo', 0)
                    total_hi += r.get('deaths_hi', 0)
            else:
                r = results_table3[sc_name].get((ag, sex), {})
                total_d += r.get('deaths', 0)
                total_lo += r.get('deaths_lo', 0)
                total_hi += r.get('deaths_hi', 0)
        row_parts.append(f"{total_d:,.0f} ({total_lo:,.0f}, {total_hi:,.0f})")
    print(f"  {sex_label:<10} " + "  ".join(
        f"{p:<26}" for p in [row_parts[1], row_parts[2], row_parts[3]]
    ))

# ============================================================================
# GUARDAR RESULTADOS
# ============================================================================
print("\n" + "=" * 70)
print("Guardando resultados...")
print("=" * 70)

output_dir = r'c:\Users\admin\Documents\Papers\Ultra-processed_Mortality'

# Guardar Tabla 1
table1.to_csv(os.path.join(output_dir, 'tabla1_upf_contribution.csv'), index=False)

# Guardar datos individuales
df_analysis.to_csv(os.path.join(output_dir, 'individual_upf_data.csv'), index=False)

# Guardar resumen de resultados
summary = {
    'total_premature_deaths_2019': total_deaths,
    'deaths_attributable_upf': round(total_all),
    'deaths_attributable_upf_lo': round(total_all_lo),
    'deaths_attributable_upf_hi': round(total_all_hi),
    'pct_of_premature_deaths': round(pct_of_total, 1),
    'RR_meta': RR_META,
    'RR_CI': f"{RR_LO}-{RR_HI}",
    'n_simulations': N_SIM,
}
with open(os.path.join(output_dir, 'summary_results.json'), 'w') as f:
    json.dump(summary, f, indent=2)

print(f"  Resultados guardados en: {output_dir}")
print("\n  ARCHIVOS GENERADOS:")
print("  - tabla1_upf_contribution.csv")
print("  - individual_upf_data.csv")
print("  - summary_results.json")

print("\n" + "=" * 70)
print("ANÁLISIS COMPLETADO")
print("=" * 70)
print("""
NOTAS IMPORTANTES:
1. Los datos de mortalidad deben verificarse con estadísticas oficiales de DEIS
   (https://www.argentina.gob.ar/salud/deis)
2. El RR utilizado proviene de Pagliai et al. (2021): RR=1.25 (1.14-1.37)
3. La clasificación NOVA fue realizada previamente (clasificar_NOVA.py)
4. La composición energética de algunos alimentos fue estimada por categoría
   cuando no se disponía de datos directos
5. El modelo asume portabilidad del RR de la meta-análisis a Argentina
""")
