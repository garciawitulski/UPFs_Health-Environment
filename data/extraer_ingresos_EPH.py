# -*- coding: utf-8 -*-
"""
Extrae ingresos promedio y tasa de ocupación por edad-sexo desde microdatos EPH 2018.

Colocar archivos EPH individual en data/EPH/:
- usu_individual_t119.txt, t219, t319, t419 (trimestres 2019)
- o individual_2019.csv
- Formato: CSV/TXT con sep=';', encoding latin-1

Variables EPH (nombres pueden variar):
- CH06 o CH03: edad
- CH04: sexo (1=varón, 2=mujer)
- P21: ingreso ocupación principal (ocupados)
- P47T: ingreso total individual
- PONDERA: ponderador
- ESTADO: condición de actividad (1=ocupado, 2=desocupado, 3=inactivo) — solo 1=ocupado
- PP04E: similar, codificación EPH continua

Uso: python data/extraer_ingresos_EPH.py [ruta_archivo]
"""
import pandas as pd
import sys
from pathlib import Path

EPH_DIR = Path(__file__).parent / "EPH"
AGE_GROUPS = ['30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69']


def find_eph_files():
    """Busca archivos EPH 2019 en data/EPH."""
    files = []
    if EPH_DIR.exists():
        for pattern in ['*individual*19*', '*usu_individual*19*', '*individual*2019*']:
            files.extend(EPH_DIR.glob(pattern))
    return sorted(set(files))


def process_file(path):
    """Procesa un archivo EPH y retorna DataFrame con estratos."""
    sep = ';' if path.suffix.lower() in ('.txt',) else ','
    try:
        df = pd.read_csv(path, sep=sep, encoding='latin-1', low_memory=False)
    except Exception:
        df = pd.read_csv(path, sep=sep, encoding='utf-8', low_memory=False)

    # Mapeo flexible de columnas
    col_map = {}
    for c in df.columns:
        cu = str(c).upper().strip()
        if cu in ('CH06', 'EDAD', 'AGE'): col_map['edad'] = c
        elif cu in ('CH04', 'SEXO', 'SEX'): col_map['sexo'] = c
        elif cu in ('P21', 'P21_PRI', 'INGRESO_OCUP'): col_map['ingreso'] = c
        elif cu in ('P47T', 'P47', 'INGRESO_TOT'): col_map['ingreso'] = col_map.get('ingreso', c)
        elif cu in ('PONDERA', 'PONDII'): col_map['peso'] = c
        elif cu in ('ESTADO', 'PP04E', 'CONDACT'): col_map['estado'] = c

    if 'edad' not in col_map or 'peso' not in col_map:
        print(f"  No se encontraron columnas necesarias en {path.name}. Columnas: {list(df.columns)[:15]}...")
        return None

    df = df.rename(columns={v: k for k, v in col_map.items()})
    df['edad'] = pd.to_numeric(df['edad'], errors='coerce')
    df = df[(df['edad'] >= 30) & (df['edad'] < 70)]
    df['peso'] = pd.to_numeric(df['peso'], errors='coerce').fillna(1)
    df['ingreso'] = pd.to_numeric(df.get('ingreso', 0), errors='coerce').fillna(0)

    if 'estado' in df.columns:
        est = pd.to_numeric(df['estado'], errors='coerce')
        # ESTADO: 1=Ocupado, 2=Desocupado, 3=Inactivo, 4=Menor 10 años (EPH registro 4t19)
        df['ocupado'] = (est == 1)
    else:
        df['ocupado'] = df['ingreso'] > 0

    if 'sexo' not in df.columns:
        df['sexo'] = 1
    df['sexo_m'] = (df['sexo'].astype(str).str.match(r'^1$') |
                    df['sexo'].astype(str).str.lower().str.contains('varon|masc|male')).astype(int)

    def ag(e):
        for g in AGE_GROUPS:
            lo, hi = int(g.split('-')[0]), int(g.split('-')[1])
            if lo <= e <= hi:
                return g
        return None
    df['age_group'] = df['edad'].apply(ag)
    df = df.dropna(subset=['age_group'])

    res = []
    for (sx, ag), g in df.groupby(['sexo_m', 'age_group']):
        emp = g[g['ocupado'] & (g['ingreso'] > 0)]
        tw = g['peso'].sum()
        ew = emp['peso'].sum()
        # P21 = ingreso mensual ocupación principal (EPH); convertimos a anual (x12)
        ing = (emp['ingreso'] * emp['peso']).sum() / ew * 12 if ew > 0 else 0
        tasa = ew / tw if tw > 0 else 0
        res.append({'sexo_m': sx, 'age_group': ag, 'ingreso_promedio': ing, 'tasa_ocupacion': tasa})
    return pd.DataFrame(res)


def main():
    EPH_DIR.mkdir(exist_ok=True)
    path_arg = sys.argv[1] if len(sys.argv) > 1 else None
    files = [Path(path_arg)] if path_arg and Path(path_arg).exists() else find_eph_files()

    if not files:
        print("No se encontraron archivos EPH en data/EPH/")
        print("Colocar usu_individual_t218.txt (o similar) y ejecutar de nuevo.")
        return

    print(f"Procesando {len(files)} archivo(s) EPH...")
    dfs = []
    for f in files:
        d = process_file(f)
        if d is not None:
            dfs.append(d)
    if not dfs:
        return

    # Promediar si hay varios trimestres
    out = pd.concat(dfs).groupby(['sexo_m', 'age_group']).mean().reset_index()
    out_path = EPH_DIR / "ingresos_por_estrato.csv"
    out.to_csv(out_path, index=False, encoding='utf-8-sig')
    print(f"Guardado: {out_path}")
    print(out.to_string(index=False))


if __name__ == "__main__":
    main()
