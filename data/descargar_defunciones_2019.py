# -*- coding: utf-8 -*-
"""
Descarga defunciones 2019 desde DEIS (fuente oficial).
Fuente: https://www.argentina.gob.ar/salud/deis/datos/defunciones
"""
import urllib.request
from pathlib import Path

BASE = Path(__file__).parent
URL = "https://www.argentina.gob.ar/sites/default/files/2021/03/defweb19.csv"
OUT = BASE / "defunciones_2019.csv"


def main():
    print("Descargando defunciones 2019 desde DEIS...")
    req = urllib.request.Request(URL, headers={'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'})
    with urllib.request.urlopen(req) as r:
        OUT.write_bytes(r.read())
    print(f"Guardado: {OUT}")


if __name__ == "__main__":
    main()
