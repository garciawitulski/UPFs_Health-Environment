# -*- coding: utf-8 -*-
"""
Calcula costos económicos indirectos de muertes prematuras atribuibles a UPF
usando el enfoque de capital humano (human capital approach).

Metodología:
- PVLE = Present Value of Lost Earnings (valor actual de ingresos perdidos)
- Por cada muerte atribuible: años de vida productiva perdidos × ingreso anual esperado
- Edad de retiro: 65 años
- Tasa de descuento: 3% (estándar OMS)
- Ingresos: promedio ponderado por EPH 2019 por estrato edad-sexo (SOLO datos oficiales)
- Tasa de participación laboral: desde EPH 2019

Datos necesarios (OBLIGATORIOS - no se usan datos inventados):
- data/EPH/individual_2019.csv o usu_individual de EPH 2019
  Variables: CH06 (edad), CH04 (sexo), P21 (ingreso ocupación principal) o P47T (ingreso total),
  PONDERA (ponderador), ESTADO o PP04E (ocupado=1)
  
Alternativa: data/EPH/ingresos_por_estrato.csv con columnas:
  sexo_m, age_group, ingreso_promedio, tasa_ocupacion

Uso: python calcular_costos_indirectos.py [--eph ruta]
"""

import pandas as pd
import numpy as np
from pathlib import Path

OUT = Path(__file__).parent / "output"
DATA = Path(__file__).parent / "data"
EPH_DIR = DATA / "EPH"

# Edad de retiro y tasa de descuento
RETIREMENT_AGE = 65
DISCOUNT_RATE = 0.03

# PPP conversion factor GDP (ARS per intl.$), Argentina 2019 — World Bank PA.NUS.PPP
# https://data.worldbank.org/indicator/PA.NUS.PPP?locations=AR
PPP_ARS_PER_INTL_USD_2019 = 20.38

AGE_GROUPS = ['30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69']
AGE_MIDPOINT = {ag: (int(ag.split('-')[0]) + int(ag.split('-')[1]))//2 for ag in AGE_GROUPS}


def load_eph_earnings(eph_path=None):
    """
    Carga ingresos promedio y tasa de ocupación por estrato desde EPH.
    Espera CSV con columnas compatibles con EPH (CH06, CH04, P21, PONDERA, etc.)
    o un archivo pre-procesado ingresos_por_estrato.csv
    """
    # Buscar archivo pre-procesado
    preproc = EPH_DIR / "ingresos_por_estrato.csv"
    if preproc.exists():
        df = pd.read_csv(preproc, encoding='utf-8-sig')
        if 'sexo_m' in df.columns and 'age_group' in df.columns:
            return df

    # Buscar EPH individual
    candidates = []
    if eph_path and Path(eph_path).exists():
        candidates.append(Path(eph_path))
    if EPH_DIR.exists():
        candidates.extend(EPH_DIR.glob("**/individual*.csv"))
        candidates.extend(EPH_DIR.glob("**/usu_individual*.csv"))
        candidates.extend(EPH_DIR.glob("**/usu_individual*.txt"))

    for path in candidates:
        try:
            sep = ';' if path.suffix == '.txt' else ','
            df = pd.read_csv(path, sep=sep, encoding='latin-1', low_memory=False, nrows=50000)
            # Detectar columnas
            age_col = next((c for c in df.columns if c.upper() in ('CH06', 'EDAD', 'AGE')), None)
            sex_col = next((c for c in df.columns if c.upper() in ('CH04', 'SEXO', 'SEX')), None)
            inc_col = next((c for c in df.columns if c.upper() in ('P21', 'P47T', 'INGRESO', 'P21_PRI')), None)
            wgt_col = next((c for c in df.columns if c.upper() in ('PONDERA', 'PONDII', 'PONDERADOR')), None)
            occ_col = next((c for c in df.columns if c.upper() in ('ESTADO', 'PP04E', 'CONDACT', 'CH15')), None)

            if not all([age_col, sex_col, wgt_col]):
                continue
            if inc_col is None:
                inc_col = next((c for c in df.columns if 'P47' in str(c).upper() or 'P21' in str(c).upper()), None)

            return process_eph(df, age_col, sex_col, inc_col, wgt_col, occ_col, path)
        except Exception as e:
            print(f"  Error leyendo {path}: {e}")
            continue

    return None


def process_eph(df, age_col, sex_col, inc_col, wgt_col, occ_col, path):
    """Procesa EPH y genera ingresos por estrato."""
    df = df.copy()
    df['edad'] = pd.to_numeric(df[age_col], errors='coerce')
    df = df[(df['edad'] >= 30) & (df['edad'] < 70)]

    # Sexo: 1=hombre típicamente
    sx = df[sex_col].astype(str)
    df['sexo_m'] = (sx.str.match(r'^1$') | sx.str.lower().str.contains('varon|masc|male|1')).astype(int)

    def age_grp(e):
        for ag in AGE_GROUPS:
            lo, hi = int(ag.split('-')[0]), int(ag.split('-')[1])
            if lo <= e <= hi:
                return ag
        return None
    df['age_group'] = df['edad'].apply(age_grp)
    df = df.dropna(subset=['age_group'])

    df['w'] = pd.to_numeric(df[wgt_col], errors='coerce').fillna(1)
    if inc_col:
        df['ingreso'] = pd.to_numeric(df[inc_col], errors='coerce').fillna(0)
    else:
        df['ingreso'] = 0

    # Ocupado: ESTADO 1 = Ocupado (EPH registro: 2=Desocupado, 3=Inactivo)
    if occ_col:
        occ = pd.to_numeric(df[occ_col], errors='coerce')
        df['ocupado'] = (occ == 1)
    else:
        df['ocupado'] = df['ingreso'] > 0

    # Ingreso promedio ponderado por estrato (solo ocupados con ingreso>0)
    res = []
    for (sx, ag), g in df.groupby(['sexo_m', 'age_group']):
        emp = g[g['ocupado'] & (g['ingreso'] > 0)]
        tot_w = g['w'].sum()
        emp_w = emp['w'].sum()
        # P21 = ingreso mensual; convertir a anual (x12)
        ingreso_prom = (emp['ingreso'] * emp['w']).sum() / emp_w * 12 if emp_w > 0 else 0
        tasa_ocup = emp_w / tot_w if tot_w > 0 else 0
        res.append({'sexo_m': sx, 'age_group': ag, 'ingreso_promedio': ingreso_prom, 'tasa_ocupacion': tasa_ocup})
    out = pd.DataFrame(res)
    out.to_csv(EPH_DIR / "ingresos_por_estrato.csv", index=False, encoding='utf-8-sig')
    print(f"  EPH procesado. Guardado: {EPH_DIR / 'ingresos_por_estrato.csv'}")
    return out


def pv_earnings(annual_earnings, employment_rate, years_remaining, r=DISCOUNT_RATE):
    """Valor actual de flujo de ingresos durante años restantes."""
    if years_remaining <= 0:
        return 0
    effective = annual_earnings * employment_rate
    if r <= 0:
        return effective * years_remaining
    pv = effective * (1 - (1 + r) ** (-years_remaining)) / r
    return pv


def main():
    print("="*70)
    print("COSTOS INDIRECTOS - ENFOQUE CAPITAL HUMANO")
    print("Muertes prematuras atribuibles a UPF - Argentina 2019")
    print("="*70)

    # 1. Cargar ingresos EPH
    import sys
    eph_path = None
    if '--eph' in sys.argv:
        i = sys.argv.index('--eph')
        if i + 1 < len(sys.argv):
            eph_path = sys.argv[i + 1]

    earnings = load_eph_earnings(eph_path)
    if earnings is None:
        print("\nERROR: No se encontraron datos EPH 2019 de fuente oficial.")
        print("Este script SOLO usa datos reales. Pasos:")
        print("  1. Descargar EPH 2019: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos")
        print("  2. Colocar usu_individual en data/EPH/ (usu_individual_t219.txt, etc.)")
        print("  3. Ejecutar: python data/extraer_ingresos_EPH.py data/EPH/usu_individual_t219.txt")
        return

    # 2. Cargar muertes atribuibles
    res = pd.read_csv(OUT / "resultados_paper_argentina.csv")
    res = res.merge(earnings, on=['sexo_m', 'age_group'], how='left')
    res['ingreso_promedio'] = res['ingreso_promedio'].fillna(0)
    res['tasa_ocupacion'] = res['tasa_ocupacion'].fillna(0)  # sin EPH real, no asumir

    # 3. Años de vida productiva restantes (hasta 65)
    res['años_productivos'] = res['age_group'].apply(
        lambda ag: max(0, RETIREMENT_AGE - AGE_MIDPOINT.get(ag, 35))
    )

    # 4. PV de ingresos perdidos por muerte
    res['PV_por_muerte'] = res.apply(
        lambda r: pv_earnings(r['ingreso_promedio'], r['tasa_ocupacion'], r['años_productivos']),
        axis=1
    )

    # 5. Costo total por estrato
    res['costo_indirecto'] = res['deaths_attr'] * res['PV_por_muerte']
    costo_total = res['costo_indirecto'].sum()

    # Resultados
    print("\n--- Ingresos y costos por estrato ---")
    print(res[['sexo_m', 'age_group', 'deaths_attr', 'ingreso_promedio', 'tasa_ocupacion',
              'años_productivos', 'PV_por_muerte', 'costo_indirecto']].to_string(index=False))

    costo_PPP_intl = costo_total / PPP_ARS_PER_INTL_USD_2019

    print(f"\nCosto indirecto total (PVLE): ${costo_total:,.0f} (pesos 2019)")
    print(f"  Equivalente: ~{costo_PPP_intl/1e9:.2f} billion intl. $ (PPP, World Bank 2019)")
    print(f"  Por muerte atribuible: ${costo_total/res['deaths_attr'].sum():,.0f}")

    res.to_csv(OUT / "costos_indirectos_por_estrato.csv", index=False, encoding='utf-8-sig')
    pd.DataFrame([{'costo_total_PVLE': costo_total, 'costo_PPP_millones_intl_USD': costo_PPP_intl / 1e6,
                   'deaths_attr_total': res['deaths_attr'].sum(),
                   'costo_por_muerte': costo_total/res['deaths_attr'].sum()}]).to_csv(
        OUT / "resumen_costos_indirectos.csv", index=False, encoding='utf-8-sig')
    print(f"\nGuardado: {OUT / 'costos_indirectos_por_estrato.csv'}")
    print(f"Guardado: {OUT / 'resumen_costos_indirectos.csv'}")


if __name__ == "__main__":
    main()
