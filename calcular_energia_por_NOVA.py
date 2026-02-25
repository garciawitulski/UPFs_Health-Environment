# -*- coding: utf-8 -*-
"""
Calcula % de energía por categoría NOVA a partir de datos oficiales.
Solo usa: ENNyS 2, alimentos_clasificados_NOVA.csv, composicion_quimica.

Uso: python calcular_energia_por_NOVA.py
Salida: output/energia_por_NOVA.csv
"""
import pandas as pd
import numpy as np
from pathlib import Path

BASE = Path(__file__).parent / "ENNyS"
OUT = Path(__file__).parent / "output"

def main():
    alimentos_path = BASE / "alimentos_clasificados_NOVA.csv"
    if not alimentos_path.exists():
        alimentos_path = BASE / "alimentos_clasificados_sin_suplementos.csv"
    if not alimentos_path.exists():
        alimentos_path = BASE / "alimentos_clasificados.csv"
    alimentos = pd.read_csv(alimentos_path, encoding='utf-8-sig')
    alimentos = alimentos.rename(columns={c: c.strip('\ufeff') for c in alimentos.columns})
    alimentos['codigo'] = alimentos['codigo'].astype(str).str.strip()
    alimentos = alimentos[~alimentos['codigo'].str.startswith('S')]
    cod_to_nova = dict(zip(alimentos['codigo'], alimentos['NOVA']))
    valid_codes = set(alimentos['codigo'].values)

    comp = None
    for f in ["composicion_quimica.csv"]:
        if (BASE / f).exists():
            df = pd.read_csv(BASE / f, encoding='utf-8')
            if 'kcal_100g' in df.columns and 'codigo' in df.columns and len(df) > 0:
                comp = df.dropna(subset=['codigo'])
                break
    if comp is None:
        print("ERROR: No se encontró tabla de composición oficial (composicion_quimica.csv).")
        print("Ejecutar: python data/extraer_composicion_excel.py")
        return
    comp_codigos = set(comp['codigo'].astype(str).str.strip().values)

    def kcal_per_100g(cod):
        cod = str(cod).strip()
        if cod in comp_codigos:
            return float(comp[comp['codigo'].astype(str) == cod]['kcal_100g'].iloc[0])
        return np.nan

    meta = ['informe_id', 'miembro_id', 'clave', 'fecha_realizacion', 'nro_R24H', 'dia_anterior', 'UPM', 'EUPM', 'F_STG_calib']
    food_cols = [c for c in pd.read_csv(BASE / "Base_Alimentos_Bebidas_Suplementos.csv", nrows=0).columns
                 if c not in meta and len(c) >= 4 and len(c) <= 6
                 and c[0] in 'VFAABCDGHILOPQRSYZ' and c[1:].isdigit()]
    # Mantener solo códigos clasificados y excluir suplementos (prefijo S)
    food_cols = [c for c in food_cols if (c in valid_codes) and (not c.startswith('S'))]

    # Procesar en chunks para no saturar memoria y sin imputación de kcal
    usecols = ['informe_id', 'miembro_id', 'clave', 'F_STG_calib'] + food_cols
    chunksize = 2000
    n_before = 0
    n_after = 0
    g_before = 0.0
    g_after = 0.0

    total_w_e = 0.0
    w_e = {1: 0.0, 2: 0.0, 3: 0.0, 4: 0.0}

    reader = pd.read_csv(
        BASE / "Base_Alimentos_Bebidas_Suplementos.csv",
        usecols=usecols,
        low_memory=False,
        chunksize=chunksize
    )
    for chunk in reader:
        melt = chunk.melt(
            id_vars=['informe_id', 'miembro_id', 'clave', 'F_STG_calib'],
            value_vars=food_cols,
            var_name='codigo',
            value_name='gramos'
        )
        melt['gramos'] = pd.to_numeric(melt['gramos'], errors='coerce').fillna(0)
        melt = melt[melt['gramos'] > 0]
        if len(melt) == 0:
            continue
        n_before += len(melt)
        g_before += float(melt['gramos'].sum())

        melt['kcal_100g'] = melt['codigo'].apply(kcal_per_100g)
        melt = melt.dropna(subset=['kcal_100g'])
        if len(melt) == 0:
            continue
        n_after += len(melt)
        g_after += float(melt['gramos'].sum())

        melt['NOVA'] = melt['codigo'].map(cod_to_nova).fillna(3).astype(int)
        melt['energia'] = melt['gramos'] * melt['kcal_100g'] / 100

        # Sumar por recall para aplicar peso F_STG_calib una vez por unidad
        per = melt.groupby(['informe_id', 'miembro_id', 'clave', 'F_STG_calib', 'NOVA'], as_index=False)['energia'].sum()
        # Pivot: columnas NOVA 1..4
        pv = per.pivot_table(index=['informe_id', 'miembro_id', 'clave', 'F_STG_calib'], columns='NOVA', values='energia', aggfunc='sum').fillna(0.0)
        w = pd.to_numeric(pv.index.get_level_values('F_STG_calib'), errors='coerce')
        w = np.where(np.isfinite(w), w, 1.0)

        # total energía calculada por recall (solo con kcal oficial)
        e_tot = pv.sum(axis=1).to_numpy()
        total_w_e += float((e_tot * w).sum())
        for k in (1, 2, 3, 4):
            if k in pv.columns:
                w_e[k] += float((pv[k].to_numpy() * w).sum())

    if n_before > 0:
        print(f"Cobertura kcal oficial: {n_after/n_before*100:.1f}% de filas de consumo ({(g_after/g_before*100 if g_before>0 else 0):.1f}% de gramos)")

    pct1 = 100 * w_e[1] / total_w_e if total_w_e > 0 else 0
    pct2 = 100 * w_e[2] / total_w_e if total_w_e > 0 else 0
    pct3 = 100 * w_e[3] / total_w_e if total_w_e > 0 else 0
    pct4 = 100 * w_e[4] / total_w_e if total_w_e > 0 else 0

    counts = alimentos['NOVA'].value_counts().sort_index()
    out = pd.DataFrame([
        {'NOVA': 1, 'descripcion': 'Sin procesar o mínimamente procesados', 'n_items': int(counts.get(1, 0)), 'pct_energia': round(pct1, 1)},
        {'NOVA': 2, 'descripcion': 'Ingredientes culinarios procesados', 'n_items': int(counts.get(2, 0)), 'pct_energia': round(pct2, 1)},
        {'NOVA': 3, 'descripcion': 'Procesados', 'n_items': int(counts.get(3, 0)), 'pct_energia': round(pct3, 1)},
        {'NOVA': 4, 'descripcion': 'Ultraprocesados (UPF)', 'n_items': int(counts.get(4, 0)), 'pct_energia': round(pct4, 1)},
    ])
    out.to_csv(OUT / "energia_por_NOVA.csv", index=False, encoding='utf-8-sig')
    print("Energía por NOVA (datos oficiales ENNyS 2):")
    print(out.to_string(index=False))
    print(f"\nTotal items clasificados: {len(alimentos)}")
    print(f"Guardado: {OUT / 'energia_por_NOVA.csv'}")


if __name__ == "__main__":
    main()
