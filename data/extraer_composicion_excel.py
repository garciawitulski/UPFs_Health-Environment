# -*- coding: utf-8 -*-
"""
Extrae composición energética (kcal/100g) desde composicion_quimica_alimentos.xlsx
y la mapea a códigos ENNyS mediante alimentos_clasificados.csv.

Fuente oficial: tabla de composición química vinculada a ENNyS.
Guarda: ENNyS/composicion_quimica.csv (codigo, descripcion, kcal_100g)

Uso: python data/extraer_composicion_excel.py
"""
import pandas as pd
from pathlib import Path

BASE = Path(__file__).parent.parent / "ENNyS"
EXCEL = BASE / "composicion_quimica_alimentos.xlsx"
ALIMENTOS = BASE / "alimentos_clasificados.csv"
OUT_CSV = BASE / "composicion_quimica.csv"


def main():
    if not EXCEL.exists():
        print(f"ERROR: No existe {EXCEL}")
        return
    if not ALIMENTOS.exists():
        print(f"ERROR: No existe {ALIMENTOS}")
        return

    comp = pd.read_excel(EXCEL, sheet_name='Table 1')
    ali = pd.read_csv(ALIMENTOS, encoding='utf-8-sig')
    ali = ali.rename(columns={c: c.strip('\ufeff') for c in ali.columns})

    # Columna de energía (puede tener caracteres especiales)
    ec = next((c for c in comp.columns if 'Valor' in c and 'energ' in c.lower()), None)
    if ec is None:
        ec = next((c for c in comp.columns if 'energ' in c.lower() or 'kcal' in c.lower()), None)
    if ec is None:
        print("ERROR: No se encontró columna de valor energético")
        print("Columnas:", list(comp.columns))
        return

    # Match exacto primero
    comp['desc_norm'] = comp['descripcion'].astype(str).str.strip().str.lower()
    comp['kcal'] = pd.to_numeric(comp[ec], errors='coerce')
    comp_valid = comp[comp['kcal'].notna() & (comp['kcal'] > 0)].copy()

    ali['desc_norm'] = ali['descripcion'].astype(str).str.strip().str.lower()
    # Merge exacto
    m1 = comp_valid.merge(ali[['codigo', 'desc_norm']], on='desc_norm', how='inner', suffixes=('', '_y'))
    exactos = m1[['codigo', 'descripcion', 'kcal']].drop_duplicates(subset=['codigo'])

    # Para el resto: (1) ali_d in comp_d (ej: "Acelga" matchea "Acelga, cruda")
    # (2) comp_d in ali_d (ej: "Leche entera" matchea "Leche entera pasteurizada"), prefiriendo comp_d más largo
    ya = set(exactos['codigo'].astype(str))
    resto = ali[~ali['codigo'].astype(str).isin(ya)]
    comp_dict = comp_valid.set_index('desc_norm')[['descripcion', 'kcal']].to_dict('index')
    comp_sorted = sorted(comp_dict.items(), key=lambda x: -len(x[0]))  # más específico primero

    results = exactos.rename(columns={'kcal': 'kcal_100g'}).to_dict('records')
    for _, row in resto.iterrows():
        cod = str(row['codigo']).strip()
        ali_d = row['desc_norm']
        if len(ali_d) < 3:
            continue
        matched = False
        for comp_d, v in comp_dict.items():
            if ali_d in comp_d:
                results.append({'codigo': cod, 'descripcion': v['descripcion'], 'kcal_100g': float(v['kcal'])})
                matched = True
                break
        if matched:
            continue
        for comp_d, v in comp_sorted:
            if len(comp_d) >= 5 and comp_d in ali_d:
                results.append({'codigo': cod, 'descripcion': v['descripcion'], 'kcal_100g': float(v['kcal'])})
                break

    out = pd.DataFrame(results).dropna(subset=['codigo']).drop_duplicates(subset=['codigo'], keep='first')
    out = out[['codigo', 'descripcion', 'kcal_100g']]
    out.to_csv(OUT_CSV, index=False, encoding='utf-8-sig')

    print(f"Composición extraída desde {EXCEL.name}")
    print(f"  Códigos mapeados: {len(out)} de {len(ali)} en alimentos_clasificados")
    print(f"  Guardado: {OUT_CSV}")
    print(out.head(10).to_string(index=False))


if __name__ == "__main__":
    main()
