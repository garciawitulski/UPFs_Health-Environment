# -*- coding: utf-8 -*-
"""
Extrae valor energético (kcal/100g) de SARA 2 PDF para mapear a códigos ENNyS.
Fuente: https://iah.salud.gob.ar/doc/720.pdf
Ejecutar: python data/extraer_SARA2.py
"""
import pdfplumber
import re
import pandas as pd
from pathlib import Path

BASE = Path(__file__).parent
PDF_PATH = BASE / "SARA2_composicion.pdf"
ENNYS_ALIMENTOS = Path(__file__).parent.parent / "ENNyS" / "alimentos_clasificados.csv"
OUT_CSV = Path(__file__).parent.parent / "ENNyS" / "composicion_SARA2.csv"

def normalizar_nombre(s):
    """Normaliza nombre para matching: minúsculas, quitar extras."""
    s = re.sub(r'\s+', ' ', str(s).lower().strip())
    s = re.sub(r'[,.]', '', s)
    return s

def main():
    if not PDF_PATH.exists():
        print(f"Colocar PDF en {PDF_PATH}")
        print("  wget/curl https://iah.salud.gob.ar/doc/720.pdf -o data/SARA2_composicion.pdf")
        return

    alimentos = pd.read_csv(ENNYS_ALIMENTOS, encoding='utf-8-sig')
    alimentos.columns = [c.strip('\ufeff') for c in alimentos.columns]
    desc_to_cod = dict(zip(
        alimentos['descripcion'].astype(str).str.strip().str.lower(),
        alimentos['codigo'].astype(str)
    ))

    results = []
    with pdfplumber.open(PDF_PATH) as pdf:
        # Tablas de alimentos suelen estar en páginas 17-100
        for i in range(17, min(100, len(pdf.pages))):
            page = pdf.pages[i]
            text = page.extract_text()
            if not text:
                continue
            # Buscar líneas con formato: "Alimento, estado VALOR ..."
            # El valor energético suele ser el primer número entero (ej: "Acelga, cruda 18")
            for line in text.split('\n'):
                line = line.strip()
                # Patrón: texto que termina en número(s) - último token antes de secuencia numérica puede ser kcal
                # Formato común: "Nombre alimento 18 92,7 1,8 ..."  (18 = kcal)
                m = re.search(r'^(.{5,80}?)\s+(\d{1,3})(?:\s+[\d,]+\s|$)', line)
                if m:
                    name_part = m.group(1).strip()
                    kcal_val = int(m.group(2))
                    # Evitar falsos positivos (headers, números de página)
                    if kcal_val > 500 or kcal_val < 3:
                        continue
                    if any(skip in name_part.lower() for skip in ['animatiV','kcal','Alimento','valor']):
                        continue
                    # Limpiar nombre
                    name_clean = re.sub(r'\s+', ' ', name_part).strip()
                    if len(name_clean) < 4:
                        continue
                    results.append({'alimento_sara': name_clean, 'kcal_100g': kcal_val})

    if not results:
        print("No se extrajeron filas. Intentando extracción alternativa por tablas...")
        with pdfplumber.open(PDF_PATH) as pdf:
            for i in range(min(50, len(pdf.pages))):
                tables = pdf.pages[i].extract_tables()
                for t in tables or []:
                    for row in t:
                        if row and len(row) >= 2:
                            first = str(row[0] or '').strip()
                            second = str(row[1] or '').strip()
                            if first and second.isdigit() and 3 <= int(second) <= 500:
                                results.append({'alimento_sara': first, 'kcal_100g': int(second)})

    if not results:
        print("No se pudo extraer. Ejecutar: python data/extraer_composicion_excel.py")
        return

    df_sara = pd.DataFrame(results).drop_duplicates(subset=['alimento_sara'], keep='first')
    print(f"Extraídos {len(df_sara)} alimentos de SARA 2")

    # Intentar mapeo a códigos ENNyS por similitud de nombre
    def match_codigo(desc_sara):
        desc_norm = normalizar_nombre(desc_sara)
        for ennys_desc, cod in desc_to_cod.items():
            ennys_norm = normalizar_nombre(ennys_desc)
            if ennys_norm in desc_norm or desc_norm in ennys_norm:
                return cod
            if desc_norm[:15] == ennys_norm[:15]:
                return cod
        return None

    df_sara['codigo'] = df_sara['alimento_sara'].apply(match_codigo)
    mapped = df_sara.dropna(subset=['codigo'])
    print(f"Mapeados a ENNyS: {len(mapped)} de {len(df_sara)}")

    # Salida: codigo, kcal_100g para uso en composicion
    out = mapped[['codigo', 'kcal_100g']].drop_duplicates(subset=['codigo'])
    out = out.merge(alimentos[['codigo', 'descripcion']], on='codigo', how='left')
    out = out[['codigo', 'descripcion', 'kcal_100g']]
    out.to_csv(OUT_CSV, index=False, encoding='utf-8-sig')
    print(f"Guardado: {OUT_CSV}")
    print(out.head(15).to_string())

if __name__ == "__main__":
    main()
