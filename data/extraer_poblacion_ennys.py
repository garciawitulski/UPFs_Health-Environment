# -*- coding: utf-8 -*-
"""
Extrae población 30-69 años por sexo y grupo etario desde ENNyS 2.
Usa F_STG_calib (pesos calibrados INDEC) - la suma por estrato estima
la población nacional en localidades ≥5000 habitantes.
"""
import pandas as pd
from pathlib import Path

BASE = Path(__file__).parent.parent / "ENNyS"
AGE_GROUPS = [(30,34),(35,39),(40,44),(45,49),(50,54),(55,59),(60,64),(65,69)]

def main():
    nut = pd.read_csv(BASE / "Base_Nutrientes.csv",
                      usecols=['clave', 'F_STG_calib', 'tot_energia_kcal'],
                      low_memory=False)
    nut = nut.dropna(subset=['tot_energia_kcal', 'F_STG_calib'])
    nut['tot_energia_kcal'] = pd.to_numeric(nut['tot_energia_kcal'], errors='coerce')
    nut = nut[nut['tot_energia_kcal'] > 0].drop_duplicates(subset=['clave'])
    
    enc = pd.read_csv(BASE / "ENNyS2_encuesta.csv",
                      usecols=['SD_MIEMBRO_SORTEADO_clave', 'SD_MIEMBRO_SORTEADO_SD_4', 'SD_MIEMBRO_SORTEADO_SD_3'],
                      low_memory=False)
    enc = enc.rename(columns={'SD_MIEMBRO_SORTEADO_clave': 'clave',
                              'SD_MIEMBRO_SORTEADO_SD_4': 'edad',
                              'SD_MIEMBRO_SORTEADO_SD_3': 'sexo'})
    enc['edad'] = pd.to_numeric(enc['edad'], errors='coerce')
    enc = enc.dropna(subset=['edad', 'sexo']).drop_duplicates(subset=['clave'])
    
    nut = nut.merge(enc, on='clave', how='inner')
    nut = nut[(nut['edad'] >= 30) & (nut['edad'] < 70)]
    
    def age_grp(e):
        for lo, hi in AGE_GROUPS:
            if lo <= e <= hi:
                return f"{lo}-{hi}"
        return None
    nut['age_group'] = nut['edad'].apply(age_grp)
    nut = nut.dropna(subset=['age_group'])
    
    nut['sexo_m'] = (nut['sexo'].astype(str).str.match(r'^1$') |
                     nut['sexo'].str.lower().str.contains('masc|varon|hombre', na=False)).astype(int)
    nut['w'] = pd.to_numeric(nut['F_STG_calib'], errors='coerce').fillna(1)
    
    pop = nut.groupby(['sexo_m', 'age_group'])['w'].sum().reset_index()
    pop = pop.rename(columns={'w': 'population'})
    pop['population'] = pop['population'].round(0).astype(int)
    
    out_path = Path(__file__).parent / "poblacion_argentina_2018_ennys.csv"
    pop.to_csv(out_path, index=False, encoding='utf-8-sig')
    print("Población extraída desde ENNyS 2 (pesos F_STG_calib):")
    print(pop.to_string(index=False))
    print(f"\nTotal 30-69: {pop['population'].sum():,.0f}")
    print(f"Guardado: {out_path}")

if __name__ == "__main__":
    main()
