# UPFs_Health-Environment

Repositorio con los codigos y datasets usados en el paper sobre consumo de UPF, salud y ambiente en Argentina.

## Contenido

- `R/`: pipeline principal en R para resultados de salud, costos y modulo ambiental.
- `data/`: datos de entrada y scripts de preparacion (DEIS, EPH, poblacion, huella ambiental).
- `ENNyS/`: datasets de ENNyS 2 usados por los modelos (base alimentos, base nutrientes, encuesta, clasificacion NOVA y composicion).
- `*.py` (raiz): scripts Python para replica y tablas del paper.
- `PIPELINE_2019.md`: guia paso a paso del flujo Python.

## Requisitos

- Git + Git LFS (hay archivos de datos grandes versionados con LFS).
- Python 3.10+ recomendado.
- R 4.0+ recomendado.

Dependencias minimas usadas por scripts:
- Python: `pandas`, `numpy`, `scipy`.
- R: `readr`, `dplyr`, `tidyr`.

## Clonar y descargar datos grandes

```bash
git clone https://github.com/garciawitulski/UPFs_Health-Environment.git
cd UPFs_Health-Environment
git lfs install
git lfs pull
```

## Ejecucion rapida

### Pipeline en R (recomendado)

Desde la raiz del proyecto:

```r
source("R/run_all.R")
```

### Pipeline en Python

Ver detalle en `PIPELINE_2019.md`. Flujo tipico:

```bash
python data/descargar_defunciones_2019.py
python data/agregar_defunciones_2019.py
python data/agregar_defunciones_por_causa_2019.py
python replicar_paper_UPF.py
python calcular_muertes_por_causa.py
python calcular_YLL_DALY.py
python calcular_esperanza_vida.py
python calcular_costos_indirectos.py
python generar_tablas_paper.py
```

## Notas

- El directorio `output/` contiene resultados generados y no se versiona en Git.
- Los datos ENNyS 2 y EPH se incluyen para reproducibilidad del paper.
- Para costos indirectos, el flujo usa microdatos EPH en `data/EPH/`.
