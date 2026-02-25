# -*- coding: utf-8 -*-
"""
Agrega defunciones 2019 por SEXO y GRUPEDAD (30-69 años).
Fuente: DEIS - defunciones_2019.csv (descargar con descargar_defunciones_2019.py)
"""
import pandas as pd
from pathlib import Path

BASE = Path(__file__).parent

def main():
    path = BASE / "defunciones_2019.csv"
    if not path.exists():
        print("ERROR: No existe defunciones_2019.csv. Ejecutar: python data/descargar_defunciones_2019.py")
        return
    df = pd.read_csv(path, encoding='latin-1')
    grupos_30_69 = [f"{i:02d}_" for i in range(7, 15)]
    mask = df['GRUPEDAD'].astype(str).str.startswith(tuple(grupos_30_69))
    df = df[mask]
    df = df[~df['GRUPEDAD'].astype(str).str.contains('99')]
    df = df[df['SEXO'].isin([1, 2])]
    agg = df.groupby(['SEXO', 'GRUPEDAD'], as_index=False)['CUENTA'].sum()
    map_grupo = {
        '07_30 a 34': '30-34', '08_35 a 39': '35-39', '09_40 a 44': '40-44',
        '10_45 a 49': '45-49', '11_50 a 54': '50-54', '12_55 a 59': '55-59',
        '13_60 a 64': '60-64', '14_65 a 69': '65-69'
    }
    agg['age_group'] = agg['GRUPEDAD'].map(map_grupo)
    agg['sexo_m'] = agg['SEXO'].map({1: 1, 2: 0})
    agg = agg.rename(columns={'CUENTA': 'deaths'})
    out = agg[['sexo_m', 'age_group', 'deaths']]
    out.to_csv(BASE / "defunciones_2019_por_estrato.csv", index=False, encoding='utf-8-sig')
    print(out.to_string())
    print(f"\nTotal muertes 30-69 (2019): {out['deaths'].sum():,}")
    print(f"Guardado: {BASE / 'defunciones_2019_por_estrato.csv'}")


if __name__ == "__main__":
    main()
