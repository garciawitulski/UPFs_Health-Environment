# -*- coding: utf-8 -*-
"""
Réplica del paper: Premature Deaths Attributable to the Consumption of 
Ultraprocessed Foods - Adaptado para Argentina (ENNyS 2)

Basado en: Nilson EAF et al. Am J Prev Med 2023;64(1):129-136

Metodología:
- Clasificación NOVA: alimentos_clasificados_NOVA.csv (salida de clasificar_NOVA.py)
- Composición energética: composicion_quimica.csv (tabla oficial ENNyS, sin imputación)
- Denominador %UPF: tot_energia_kcal de Base_Nutrientes (energía total oficial del recall)
- PAF: Evaluación comparativa de riesgos (CRA) con distribución log-normal
- Incertidumbre: Monte Carlo (10,000 simulaciones)
- Defunciones: DEIS 2019 por estrato (agregación oficial desde defunciones_2019.csv)

Uso: python replicar_paper_UPF.py [--sample N]
     --sample N  usa solo N filas para prueba rápida (default: todo)
"""

import pandas as pd
import numpy as np
from scipy import stats
from pathlib import Path
import warnings
import sys
warnings.filterwarnings('ignore')

# Seed fijo al inicio para reproducibilidad (edad/sexo si faltan y Monte Carlo)
np.random.seed(42)

BASE = Path(__file__).parent / "ENNyS"
USE_SAMPLE = None
if '--sample' in sys.argv:
    i = sys.argv.index('--sample')
    USE_SAMPLE = int(sys.argv[i+1]) if i+1 < len(sys.argv) else 2000
OUT = Path(__file__).parent / "output"
OUT.mkdir(exist_ok=True)

# Parámetros epidemiológicos (Pagliai et al. 2021; Nilson et al. 2023)
RR_POINT = 1.25
RR_LOW = 1.14
RR_HIGH = 1.37
UPF_REF = 35.7  # % de energía UPF para el cual RR=1.25
AGE_GROUPS = [(30,34),(35,39),(40,44),(45,49),(50,54),(55,59),(60,64),(65,69)]
N_SIM = 10000

# Coeficientes log-lineales: beta = ln(RR) / UPF_REF
BETA_CENTRAL = np.log(RR_POINT) / UPF_REF
BETA_LO = np.log(RR_LOW) / UPF_REF
BETA_HI = np.log(RR_HIGH) / UPF_REF
BETA_SE = (BETA_HI - BETA_LO) / (2 * 1.96)


def compute_paf_distributional(mean_upf, sd_upf, beta, reduction_pct=1.0):
    """
    PAF con modelo CRA: integra E[RR(X)] sobre distribución log-normal de exposición.
    
    X ~ LogNormal(mu, sigma^2)  con media=mean_upf, sd=sd_upf
    RR(x) = exp(beta * x)
    PAF = (E[RR(X)] - E[RR(X_cf)]) / E[RR(X)]
    
    Donde X_cf es la distribución contrafactual (reducida por reduction_pct).
    """
    if mean_upf <= 0 or sd_upf <= 0 or beta <= 0:
        return 0.0

    N_POINTS = 500
    cv = max(sd_upf / mean_upf, 0.01)
    sigma2 = np.log(1 + cv**2)
    sigma = np.sqrt(sigma2)
    mu = np.log(mean_upf) - sigma2 / 2

    x_points = np.linspace(0.001, 0.999, N_POINTS)
    x_vals = stats.lognorm.ppf(x_points, s=sigma, scale=np.exp(mu))
    pdf_vals = stats.lognorm.pdf(x_vals, s=sigma, scale=np.exp(mu))

    rr_vals = np.exp(beta * x_vals)
    dx = np.diff(x_vals, prepend=0)
    e_rr_baseline = np.sum(rr_vals * pdf_vals * dx)

    mean_c = mean_upf * (1 - reduction_pct)
    if mean_c <= 0.001:
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


def main():
    print("="*70)
    print("Réplica Paper UPF - Argentina ENNyS 2")
    print("Método: CRA distribucional + Monte Carlo")
    print("="*70)

    # ================================================================
    # 1. Cargar clasificación NOVA (salida de clasificar_NOVA.py)
    # ================================================================
    alimentos_path = BASE / "alimentos_clasificados_NOVA.csv"
    if not alimentos_path.exists():
        alimentos_path = BASE / "alimentos_clasificados_sin_suplementos.csv"
    if not alimentos_path.exists():
        alimentos_path = BASE / "alimentos_clasificados.csv"
    alimentos = pd.read_csv(alimentos_path, encoding='utf-8-sig')
    alimentos = alimentos.rename(columns={c: c.strip('\ufeff') for c in alimentos.columns})
    alimentos['codigo'] = alimentos['codigo'].astype(str).str.strip()

    # Excluir suplementos (S)
    alimentos = alimentos[~alimentos['codigo'].str.startswith('S')]
    n_nova4 = (alimentos['NOVA'] == 4).sum()
    print(f"  Clasificación NOVA: {alimentos_path.name}")
    print(f"  Ítems sin suplementos: {len(alimentos)} (NOVA 4: {n_nova4})")
    
    # ================================================================
    # 2. Cargar composición energética (solo oficial, sin imputación)
    # ================================================================
    comp = None
    for f in ["composicion_SARA2.csv", "composicion_quimica.csv"]:
        if (BASE / f).exists():
            df = pd.read_csv(BASE / f, encoding='utf-8')
            if 'kcal_100g' in df.columns and 'codigo' in df.columns and len(df) > 0:
                comp = df.dropna(subset=['codigo'])
                print(f"  Composición: {f} ({len(comp)} alimentos)")
                break
    if comp is None:
        print("ERROR: No se encontró tabla de composición.")
        return
    
    cod_to_nova = dict(zip(alimentos['codigo'], alimentos['NOVA']))
    comp_codigos = set(comp['codigo'].astype(str).str.strip().values)
    
    def kcal_per_100g(cod):
        cod = str(cod).strip()
        if cod in comp_codigos:
            return float(comp[comp['codigo'].astype(str)==cod]['kcal_100g'].iloc[0])
        return np.nan

    # ================================================================
    # 3. Cargar Base Alimentos y calcular energía UPF por recall
    # ================================================================
    print("\nCargando Base Alimentos...")
    meta = ['informe_id','miembro_id','clave','fecha_realizacion','nro_R24H',
            'dia_anterior','UPM','EUPM','F_STG_calib']
    food_cols = [c for c in pd.read_csv(BASE / "Base_Alimentos_Bebidas_Suplementos.csv", nrows=0).columns 
                 if c not in meta and len(c)>=4 and len(c)<=6 
                 and c[0] in 'VFAABCDGHILOPQRSYZ' and c[1:].isdigit()]

    valid_codes = set(alimentos['codigo'].values)
    food_cols = [c for c in food_cols if (c in valid_codes) and (not c.startswith('S'))]
    
    usecols = ['informe_id','miembro_id','clave','F_STG_calib'] + food_cols
    chunksize = 2000 if USE_SAMPLE is None else 500
    nrows = USE_SAMPLE
    if USE_SAMPLE:
        print(f"  [Modo muestra: {USE_SAMPLE} filas]")

    agg_chunks = []
    n_before = 0
    n_after = 0
    g_before = 0.0
    g_after = 0.0

    reader = pd.read_csv(
        BASE / "Base_Alimentos_Bebidas_Suplementos.csv",
        usecols=usecols, low_memory=False,
        chunksize=chunksize, nrows=nrows
    )
    for chunk in reader:
        melt = chunk.melt(
            id_vars=['informe_id','miembro_id','clave','F_STG_calib'],
            value_vars=food_cols, var_name='codigo', value_name='gramos'
        )
        melt['gramos'] = pd.to_numeric(melt['gramos'], errors='coerce').fillna(0)
        melt = melt[melt['gramos'] > 0]
        if len(melt) == 0:
            continue
        n_before += len(melt)
        g_before += float(melt['gramos'].sum())

        melt['NOVA'] = melt['codigo'].map(cod_to_nova).fillna(3)
        melt['kcal_100g'] = melt['codigo'].apply(kcal_per_100g)
        melt = melt.dropna(subset=['kcal_100g'])
        if len(melt) == 0:
            continue
        n_after += len(melt)
        g_after += float(melt['gramos'].sum())

        melt['energia'] = melt['gramos'] * melt['kcal_100g'] / 100
        melt['energia_upf'] = np.where(melt['NOVA'] == 4, melt['energia'], 0)

        agg = melt.groupby(['informe_id','miembro_id','clave','F_STG_calib'], as_index=False).agg(
            energia_upf=('energia_upf', 'sum'),
            energia_total_calc=('energia', 'sum')
        )
        agg_chunks.append(agg)

    if not agg_chunks:
        print("ERROR: No se pudo calcular energía desde Base_Alimentos.")
        return
    agg_ali = pd.concat(agg_chunks, ignore_index=True)
    agg_ali = agg_ali.groupby(['informe_id','miembro_id','clave','F_STG_calib'], as_index=False).agg(
        energia_upf=('energia_upf', 'sum'),
        energia_total_calc=('energia_total_calc', 'sum')
    )

    if n_before > 0:
        print(f"  Cobertura kcal oficial: {n_after/n_before*100:.1f}% de filas "
              f"({(g_after/g_before*100 if g_before>0 else 0):.1f}% de gramos)")
    
    # ================================================================
    # 4. Calcular %UPF por recall (denominador = tot_energia_kcal oficial)
    # ================================================================
    print("Cargando Base Nutrientes...")
    nut = pd.read_csv(BASE / "Base_Nutrientes.csv", 
                      usecols=['informe_id','miembro_id','clave','F_STG_calib','tot_energia_kcal'],
                      low_memory=False)
    nut = nut.dropna(subset=['tot_energia_kcal'])
    nut['tot_energia_kcal'] = pd.to_numeric(nut['tot_energia_kcal'], errors='coerce')
    nut = nut[nut['tot_energia_kcal']>0]
    
    nut = nut.merge(agg_ali, on=['informe_id','miembro_id','clave'], how='left', suffixes=('','_y'))
    if 'F_STG_calib_y' in nut.columns:
        nut = nut.drop(columns=['F_STG_calib_y'], errors='ignore')
    
    nut['pct_upf'] = np.where(nut['tot_energia_kcal'].fillna(0) > 0,
                              nut['energia_upf'].fillna(0) / nut['tot_energia_kcal'] * 100,
                              np.nan)
    nut = nut.dropna(subset=['pct_upf'])
    nut = nut[(nut['pct_upf']>=0) & (nut['pct_upf']<=100)]
    print(f"  Recalls válidos: {len(nut)}")

    # ================================================================
    # 5. Cargar encuesta para edad y sexo
    # ================================================================
    print("Cargando encuesta (edad/sexo)...")
    enc_cols_req = ['SD_MIEMBRO_SORTEADO_clave', 'SD_MIEMBRO_SORTEADO_SD_4', 'SD_MIEMBRO_SORTEADO_SD_3']
    try:
        enc = pd.read_csv(BASE / "ENNyS2_encuesta.csv", usecols=enc_cols_req, low_memory=False)
    except Exception:
        enc = pd.read_csv(BASE / "ENNyS2_encuesta.csv", nrows=500, low_memory=False)
        enc_cols = [c for c in enc.columns if c in enc_cols_req]
        enc = pd.read_csv(BASE / "ENNyS2_encuesta.csv", usecols=enc_cols, low_memory=False)

    clave_col = 'SD_MIEMBRO_SORTEADO_clave'
    edad_col = 'SD_MIEMBRO_SORTEADO_SD_4'
    sexo_col = 'SD_MIEMBRO_SORTEADO_SD_3'

    if clave_col in enc.columns and edad_col in enc.columns and sexo_col in enc.columns:
        enc_slim = enc[[clave_col, edad_col, sexo_col]].drop_duplicates()
        enc_slim = enc_slim.rename(columns={clave_col:'clave', edad_col:'edad', sexo_col:'sexo'})
        enc_slim['edad'] = pd.to_numeric(enc_slim['edad'], errors='coerce')
        enc_slim = enc_slim.dropna(subset=['edad'])
        nut = nut.merge(enc_slim, on='clave', how='left')

    if 'edad' not in nut.columns or nut['edad'].isna().all():
        nut['edad'] = np.random.randint(30, 70, len(nut))
    if 'sexo' not in nut.columns or nut['sexo'].isna().all():
        nut['sexo'] = np.random.choice(['Masculino','Femenino'], len(nut))
    mask_na = nut['edad'].isna()
    if mask_na.any():
        nut.loc[mask_na, 'edad'] = np.random.randint(30, 70, mask_na.sum())
    nut['edad'] = pd.to_numeric(nut['edad'], errors='coerce')
    mask_year = (nut['edad'] >= 1920) & (nut['edad'] <= 2000)
    nut.loc[mask_year, 'edad'] = 2019 - nut.loc[mask_year, 'edad']
    mask_bad = nut['edad'].isna() | (nut['edad']<30) | (nut['edad']>69)
    if mask_bad.any():
        nut.loc[mask_bad, 'edad'] = np.random.randint(30, 70, mask_bad.sum())

    nut['sexo'] = nut['sexo'].fillna('').astype(str).str.strip()
    nut['sexo_m'] = (nut['sexo'].astype(str).str.match(r'^1$') | 
                     nut['sexo'].str.lower().str.contains('masc|varon|hombre')).astype(int)

    def age_grp(edad):
        for lo, hi in AGE_GROUPS:
            if lo <= edad <= hi:
                return f"{lo}-{hi}"
        return None
    nut['age_group'] = nut['edad'].apply(age_grp)
    nut = nut.dropna(subset=['age_group'])
    nut = nut[(nut['edad']>=30) & (nut['edad']<=69)]

    # ================================================================
    # 6. Promediar %UPF por persona (si tiene 2+ R24H)
    #    Luego calcular media ponderada por estrato
    # ================================================================
    print("Promediando por persona y luego por estrato...")
    nut['w'] = pd.to_numeric(nut['F_STG_calib'].fillna(1), errors='coerce').fillna(1)
    
    # Promedio por persona (múltiples recalls)
    df_person = nut.groupby(['miembro_id','sexo_m','age_group']).agg(
        pct_upf=('pct_upf', 'mean'),
        w=('w', 'first'),
        n_recalls=('pct_upf', 'count')
    ).reset_index()
    
    print(f"  Personas únicas 30-69: {len(df_person)}")
    print(f"  Con 2+ recalls: {(df_person['n_recalls']>=2).sum()}")

    # Media ponderada por estrato sexo × edad
    res = []
    for (sx, ag), g in df_person.groupby(['sexo_m','age_group']):
        m = np.average(g['pct_upf'], weights=g['w'])
        sd = np.sqrt(np.average((g['pct_upf'] - m)**2, weights=g['w'])) if len(g) > 1 else 0
        se = sd / np.sqrt(len(g)) if len(g) > 1 else 0
        ci_lo = m - 1.96 * se
        ci_hi = m + 1.96 * se
        res.append({'sexo_m': sx, 'age_group': ag, 'pct_upf_mean': m,
                    'pct_upf_sd': sd, 'pct_upf_se': se,
                    'pct_upf_ci_lo': ci_lo, 'pct_upf_ci_hi': ci_hi, 'n': len(g)})
    upf_strata = pd.DataFrame(res)

    upf_strata.to_csv(OUT / "upf_por_estrato.csv", index=False)
    print("\n  TABLA 1: Contribución de UPF a la energía total (%)")
    print("  " + "-" * 80)
    print(f"  {'Grupo edad':<12} {'Hombres % (95% CI)':<30} {'Mujeres % (95% CI)':<30}")
    print("  " + "-" * 80)
    for ag_lo, ag_hi in AGE_GROUPS:
        ag = f"{ag_lo}-{ag_hi}"
        rm = upf_strata[(upf_strata['sexo_m']==1) & (upf_strata['age_group']==ag)]
        rf = upf_strata[(upf_strata['sexo_m']==0) & (upf_strata['age_group']==ag)]
        m_str = f"{rm['pct_upf_mean'].values[0]:.1f} ({rm['pct_upf_ci_lo'].values[0]:.1f}, {rm['pct_upf_ci_hi'].values[0]:.1f})" if len(rm) else "N/A"
        f_str = f"{rf['pct_upf_mean'].values[0]:.1f} ({rf['pct_upf_ci_lo'].values[0]:.1f}, {rf['pct_upf_ci_hi'].values[0]:.1f})" if len(rf) else "N/A"
        print(f"  {ag:<12} {m_str:<30} {f_str:<30}")
    print(f"\n  Guardado: {OUT / 'upf_por_estrato.csv'}")

    # ================================================================
    # 7. Cargar defunciones DEIS 2019
    # ================================================================
    DEF_PATH = Path(__file__).parent / "data" / "defunciones_2019_por_estrato.csv"
    if not DEF_PATH.exists():
        print("ERROR: No existe defunciones_2019_por_estrato.csv.")
        return
    def19 = pd.read_csv(DEF_PATH, encoding='utf-8-sig')
    def19 = def19.dropna(subset=['sexo_m'])
    def19['sexo_m'] = def19['sexo_m'].astype(int)
    total_deaths = def19['deaths'].sum()
    print(f"\n  Defunciones DEIS 2019 (30-69): {total_deaths:,}")

    # Merge mortalidad con exposición
    deaths_df = upf_strata.merge(def19[['sexo_m','age_group','deaths']], 
                                  on=['sexo_m','age_group'], how='left')
    deaths_df['deaths'] = deaths_df['deaths'].fillna(0).astype(int)

    # ================================================================
    # 8. Monte Carlo: PAF distribucional con incertidumbre
    # ================================================================
    print(f"\n  Simulación Monte Carlo ({N_SIM:,} iteraciones)...")
    np.random.seed(42)
    beta_samples = np.random.normal(BETA_CENTRAL, BETA_SE, N_SIM)

    labels = [f"{lo}-{hi}" for lo,hi in AGE_GROUPS]
    scenarios = {'baseline': 1.0, 'red_10': 0.10, 'red_20': 0.20, 'red_50': 0.50}

    mc_results = []
    for _, row in deaths_df.iterrows():
        sx = int(row['sexo_m'])
        ag = row['age_group']
        mean_upf = row['pct_upf_mean']
        sd_upf = row['pct_upf_sd']
        se_upf = row['pct_upf_se']
        n_deaths = int(row['deaths'])
        n_obs = int(row['n'])

        paf_sims = np.zeros(N_SIM)
        deaths_sims = np.zeros(N_SIM)
        sc_deaths_sims = {sc: np.zeros(N_SIM) for sc in scenarios if sc != 'baseline'}

        for i in range(N_SIM):
            mean_sample = max(0.1, np.random.normal(mean_upf, se_upf))
            sd_sample = sd_upf
            beta_i = beta_samples[i]
            deaths_i = np.random.poisson(n_deaths)

            paf_i = compute_paf_distributional(mean_sample, sd_sample, beta_i, reduction_pct=1.0)
            paf_sims[i] = paf_i
            deaths_sims[i] = paf_i * deaths_i

            for sc_name, red_pct in scenarios.items():
                if sc_name == 'baseline':
                    continue
                paf_sc = compute_paf_distributional(mean_sample, sd_sample, beta_i, reduction_pct=red_pct)
                sc_deaths_sims[sc_name][i] = paf_sc * deaths_i

        result = {
            'sexo_m': sx, 'age_group': ag,
            'pct_upf_mean': mean_upf, 'pct_upf_sd': sd_upf, 'n': n_obs,
            'deaths': n_deaths,
            'PAF': np.percentile(paf_sims, 50),
            'PAF_lo': np.percentile(paf_sims, 2.5),
            'PAF_hi': np.percentile(paf_sims, 97.5),
            'deaths_attr': np.percentile(deaths_sims, 50),
            'deaths_attr_low': np.percentile(deaths_sims, 2.5),
            'deaths_attr_high': np.percentile(deaths_sims, 97.5),
        }
        for sc_name in sc_deaths_sims:
            result[f'{sc_name}_deaths'] = np.percentile(sc_deaths_sims[sc_name], 50)
            result[f'{sc_name}_deaths_lo'] = np.percentile(sc_deaths_sims[sc_name], 2.5)
            result[f'{sc_name}_deaths_hi'] = np.percentile(sc_deaths_sims[sc_name], 97.5)

        mc_results.append(result)
        print(f"    {ag} {'H' if sx==1 else 'M'}: PAF={result['PAF']*100:.1f}% "
              f"({result['PAF_lo']*100:.1f}, {result['PAF_hi']*100:.1f}), "
              f"muertes={result['deaths_attr']:.0f} ({result['deaths_attr_low']:.0f}-{result['deaths_attr_high']:.0f})")

    results = pd.DataFrame(mc_results)

    # ================================================================
    # 9. Resultados principales
    # ================================================================
    total_attr = results['deaths_attr'].sum()
    total_attr_lo = results['deaths_attr_low'].sum()
    total_attr_hi = results['deaths_attr_high'].sum()
    pct_attr = 100 * total_attr / total_deaths if total_deaths > 0 else 0

    print("\n" + "="*70)
    print("RESULTADOS - Muertes atribuibles a UPF (Argentina 30-69 años)")
    print("="*70)
    print(f"  Defunciones premature totales: {total_deaths:,}")
    print(f"  Muertes atribuibles a UPF:     {total_attr:,.0f} ({total_attr_lo:,.0f} – {total_attr_hi:,.0f})")
    print(f"  % de muertes prematuras:       {pct_attr:.1f}%")

    # Tabla por sexo
    for sx, sx_label in [(1, 'Hombres'), (0, 'Mujeres')]:
        sub = results[results['sexo_m']==sx]
        d = sub['deaths_attr'].sum()
        d_lo = sub['deaths_attr_low'].sum()
        d_hi = sub['deaths_attr_high'].sum()
        print(f"  {sx_label}: {d:,.0f} ({d_lo:,.0f} – {d_hi:,.0f})")

    # Escenarios
    print("\n  ESCENARIOS DE REDUCCIÓN:")
    for sc_name, sc_label in [('red_10','10%'), ('red_20','20%'), ('red_50','50%')]:
        d = results[f'{sc_name}_deaths'].sum()
        d_lo = results[f'{sc_name}_deaths_lo'].sum()
        d_hi = results[f'{sc_name}_deaths_hi'].sum()
        print(f"    Reducción {sc_label}: {d:,.0f} ({d_lo:,.0f} – {d_hi:,.0f}) muertes evitables")

    # Guardar resultado principal
    results.to_csv(OUT / "resultados_paper_argentina.csv", index=False)
    print(f"\n  Guardado: {OUT / 'resultados_paper_argentina.csv'}")


if __name__ == "__main__":
    main()
