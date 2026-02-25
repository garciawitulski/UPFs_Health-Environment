# -*- coding: utf-8 -*-
"""
Agrega defunciones 2019 por SEXO, GRUPEDAD y CAUSA (ICD-10).
Fuente: DEIS - defunciones_2019.csv
"""
import pandas as pd
from pathlib import Path

BASE = Path(__file__).parent
OUT = BASE.parent / "output"

def main():
    path = BASE / "defunciones_2019.csv"
    if not path.exists():
        print("ERROR: No existe defunciones_2019.csv. Ejecutar: python data/descargar_defunciones_2019.py")
        return
    df = pd.read_csv(path, encoding='latin-1')
    df = df[df['SEXO'].isin([1, 2])]
    df = df[~df['GRUPEDAD'].astype(str).str.contains('99')]
    grupos_30_69 = [f"{i:02d}_" for i in range(7, 15)]
    mask_edad = df['GRUPEDAD'].astype(str).str.startswith(tuple(grupos_30_69))
    df = df[mask_edad]
    map_grupo = {
        '07_30 a 34': '30-34', '08_35 a 39': '35-39', '09_40 a 44': '40-44',
        '10_45 a 49': '45-49', '11_50 a 54': '50-54', '12_55 a 59': '55-59',
        '13_60 a 64': '60-64', '14_65 a 69': '65-69'
    }
    def clasificar_causa(cod):
        cod = str(cod).strip().upper()
        if len(cod) < 3:
            return None
        try:
            if cod.startswith('E') and cod[:3] in ['E10','E11','E12','E13','E14']:
                return 'diabetes'
            if cod.startswith('I'):
                n = int(cod[1:3])
                if 60 <= n <= 69:
                    return 'cerebrovascular'
                if 0 <= n <= 59 or 70 <= n <= 99:
                    return 'cvd'
            if cod.startswith('C') and int(cod[1:3]) <= 97:
                return 'cancer'
        except (ValueError, IndexError):
            pass
        return None
    df['causa'] = df['CAUSA'].apply(clasificar_causa)
    df = df.dropna(subset=['causa'])
    agg = df.groupby(['SEXO', 'GRUPEDAD', 'causa'], as_index=False)['CUENTA'].sum()
    agg['age_group'] = agg['GRUPEDAD'].map(map_grupo)
    agg['sexo_m'] = agg['SEXO'].map({1: 1, 2: 0})
    out_df = agg[['sexo_m', 'age_group', 'causa', 'CUENTA']].rename(columns={'CUENTA': 'deaths'})
    OUT.mkdir(exist_ok=True)
    out_df.to_csv(OUT / "defunciones_2019_por_causa.csv", index=False, encoding='utf-8-sig')
    print(out_df.groupby('causa')['deaths'].sum())
    print(f"\nGuardado: {OUT / 'defunciones_2019_por_causa.csv'}")


if __name__ == "__main__":
    main()
