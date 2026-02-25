# Pipeline 2019 - Solo datos oficiales (sin imputación)

Este pipeline usa **únicamente fuentes oficiales** para datos y parámetros (ENNyS 2, DEIS, EPH, OMS, Banco Mundial, meta-análisis).  
**No se imputan kcal/100g** cuando un alimento no figura en tablas de composición: esos consumos se **excluyen** del cálculo energético y se reporta cobertura.

Además, para comparar con listados sin suplementos, el modelo usa `ENNyS/alimentos_clasificados_sin_suplementos.csv` (excluye códigos con prefijo `S`).

## 0. Composición energética (pre-requisito)

```bash
python data/extraer_composicion_excel.py
```

Fuente: `ENNyS/composicion_quimica_alimentos.xlsx` (tabla oficial). Genera `composicion_quimica.csv`.
**No se usa** composicion_estimada.csv (origen no documentado).

## 1. Defunciones 2019 (DEIS)

```bash
python data/descargar_defunciones_2019.py
python data/agregar_defunciones_2019.py
python data/agregar_defunciones_por_causa_2019.py
```

Fuente: https://www.argentina.gob.ar/salud/deis/datos/defunciones

## 2. Modelo principal (ENNyS 2 + DEIS)

```bash
python replicar_paper_UPF.py
```

Usa: ENNyS 2 (dieta), `data/defunciones_2019_por_estrato.csv` y `ENNyS/alimentos_clasificados_sin_suplementos.csv`.

## 3. Muertes por causa, YLL, esperanza de vida

```bash
python calcular_muertes_por_causa.py
python calcular_YLL_DALY.py
python calcular_esperanza_vida.py
```

## 4. Costos indirectos (EPH 2019 - OBLIGATORIO)

Los costos **solo se calculan** con datos reales de EPH. No hay valores de referencia.

**Pasos:**

1. Descargar EPH 2019 desde INDEC:  
   https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos  
   Buscar "Encuesta Permanente de Hogares" → microdatos 2019.

2. Colocar en `data/EPH/` los archivos usu_individual (formato TXT separado por `;`):
   - usu_individual_t119.txt, t219.txt, t319.txt, t419.txt  
   - o el CSV equivalente.

3. Extraer ingresos:
   ```bash
   python data/extraer_ingresos_EPH.py
   ```
   (o pasar ruta del archivo como argumento)

4. Calcular costos:
   ```bash
   python calcular_costos_indirectos.py
   ```

## 5. Generar tablas para el paper

```bash
python generar_tablas_paper.py
```

## 6. Energía por NOVA (opcional)

```bash
python calcular_energia_por_NOVA.py
```
Salida: `output/energia_por_NOVA.csv`

## Fuentes de datos

| Dato | Fuente |
|------|--------|
| Dieta / UPF | ENNyS 2 (Ministerio de Salud) |
| Composición energética | composicion_quimica_alimentos.xlsx (ENNyS) → composicion_quimica.csv |
| Defunciones | DEIS (Ministerio de Salud) |
| Población | ENNyS 2 pesos F_STG_calib (INDEC) |
| Ingresos/tasa ocupación | EPH 2019 (INDEC) |
| RR | Pagliai et al. Br J Nutr 2021 |
| Tabla vida (YLL) | OMS GHE 2021 |
