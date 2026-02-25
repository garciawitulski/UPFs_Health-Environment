# -*- coding: utf-8 -*-
"""
Descarga y prepara microdatos EPH 2019 para el cálculo de costos indirectos.

Opciones:
1. Descarga desde INDEC (FTP) - requiere conexión y formato DBF/ZIP
2. Descarga desde portal datos.gob.ar si está disponible
3. Colocar manualmente: si ya tenés los CSV de EPH 2019, colocarlos en:
   data/EPH/ con nombres: usu_individual_t119.txt, usu_individual_t219.txt, etc.
   (o individual_2019.csv)

Variables EPH necesarias: CH06 (edad), CH04 (sexo), P21 o P47T (ingreso), 
PONDERA (ponderador), ESTADO o PP04E (condición ocupacional).
"""
import os
import sys
import urllib.request
from pathlib import Path

DATA = Path(__file__).parent
EPH_DIR = DATA / "EPH"
EPH_DIR.mkdir(exist_ok=True)

# URL típica INDEC EPH (puede variar)
INDEC_EPH_BASE = "https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/"

# Trimestres 2019
TRIMESTRES_2019 = ["eph_cont_1trim19", "eph_cont_2trim19", "eph_cont_3trim19", "eph_cont_4trim19"]


def descargar_indec():
    """Intenta descargar EPH desde INDEC."""
    for t in TRIMESTRES_2019:
        url = f"{INDEC_EPH_BASE}{t}.zip"
        out = EPH_DIR / f"{t}.zip"
        if out.exists():
            print(f"  Ya existe: {out.name}")
            continue
        try:
            print(f"  Descargando {url}...")
            urllib.request.urlretrieve(url, out)
            print(f"  Guardado: {out}")
        except Exception as e:
            print(f"  Error: {e}")
            print("  Descargar manualmente desde: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos")
            return False
    return True


def main():
    print("="*60)
    print("Descarga EPH 2019 - Argentina")
    print("="*60)
    print(f"\nDirectorio destino: {EPH_DIR.absolute()}")

    if len(sys.argv) > 1 and sys.argv[1] == "--manual":
        print("\nModo manual:")
        print("  1. Ir a https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos")
        print("  2. Buscar 'Encuesta Permanente de Hogares'")
        print("  3. Descargar microdatos 2019 (trimestres 1-4)")
        print("  4. Descomprimir en", EPH_DIR)
        print("  5. Si son DBF: convertir a CSV/TXT (sep=';') con nombre usu_individual_*.csv")
        return

    print("\nIntentando descarga desde INDEC...")
    ok = descargar_indec()
    if not ok:
        print("\nPara descarga manual, ejecutar: python data/descargar_EPH_2018.py --manual")

    print("\nAlternativa: usar pyeph para Python:")
    print("  pip install pyeph")
    print("  from eph import get_microdata")
    print("  df = get_microdata(year=2019, trimester=2)")


if __name__ == "__main__":
    main()
