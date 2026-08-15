# Pipeline de reproducibilidad — Tesis Pregrado

Orden de ejecución para regenerar todos los paneles y estimaciones desde los
datos crudos en `data/`.

## Etapa 1 — Construcción del panel (`R/01_construccion_panel/`)

| # | Script | Input principal | Output |
|---|--------|-----------------|--------|
| 1 | `01_panel_base.R` | SERNAPESCA (desembarques) | `data/panel.base.csv` |
| 2 | `02_integrar_env_puerto.R` | Copernicus SST/CHL/Wind | `data/panel_con_env_puerto.csv` |
| 3 | `03_panel_maes.R` | IPC INE, Diésel CNE, TC | `data/panel_maestro_definitivo.csv` |
| 4 | `04_integrar_panel_maestro.R` | biomasa IFOP | `data/panel_maestro_integrado.csv` |
| 4b| `04_preparacion_instrumentos.R` | ambientales macrozonal | `data/base_integrada3_IV.csv` |
| 5 | `05_integrar_fob_peru_y_veda.R` | FOB BCRP, veda SUBPESCA | `data/panel_con_alternativas.csv` |
| **6** | **`06_rebasar_deflactor_jun2024.R`** *(NUEVO)* | panel_con_alternativas | **`data/panel_correcto_base_junio_2024.csv`** |

**Nota clave:** el DEFLACTOR generado en el script 3 tiene base ene-2012.
El script 6 lo reescala a jun-2024 multiplicando las 8 columnas de precios
reales por `DEFLACTOR[jun-2024] = 1.6736095`. Esto NO altera las series
nominales, biomasas, cantidades ni variables ambientales.

## Etapa 2 — Estimación (`R/02_estimacion/`)

| # | Script | Input | Output |
|---|--------|-------|--------|
| 7 | `07_estimacion_iv_optimizada.R` | panel_correcto_base_junio_2024 | `data/panel_con_ivs_optimizados.csv` |
| 8 | `08_modelo_optimizado_test.R` | idem | tabla de coeficientes |
| 9 | `09_estimacion_adicional.R` | idem | modelos con POST_2020 |
| **16** | **`16_TAC_instrumento_v2.R`** *(REEMPLAZO)* | panel_correcto_base_junio_2024 + `TAC_anual.xlsx` | **`data/panel_upgrade.csv`** + modelos M0–M3 |

**Cambios vs `16_TAC_instrumento.R` original:**

1. Lee `panel_correcto_base_junio_2024.csv` (no `panel_con_alternativas.csv`).
2. Normaliza tildes con `chartr()` antes de filtrar recursos (fix "SARDINA COMÚN" 2017).
3. Filtra `"sardina comun"` + `"sardina"` en vez de `"sardina común"`.
4. Añade `POST_2020` y `ln_h_X_POST` al panel (antes se calculaban dentro de cada modelo).
5. **Guarda `data/panel_upgrade.csv`** (el original no escribía).

## Etapa 3 — Diagnósticos y robustez (`R/03_diagnosticos_robustez/`)

| Script | Input |
|--------|-------|
| `10_verificaciones.R`, `15_robustez_panel_desbalanceado.R`, `17_robustez_M2_TAC.R`, `17b_robustez_M2_adicional_2.R` | `panel_upgrade.csv` |

## Etapa 4 — Visualización y reportes

`R/04_visualizacion/` y `R/reportes/resultados_finales.Rmd`.

---

## Comando único de reproducción

```r
setwd("~/Tesis-Pregrado")
source("R/01_construccion_panel/01_panel_base.R")
source("R/01_construccion_panel/02_integrar_env_puerto.R")
source("R/01_construccion_panel/03_panel_maes.R")
source("R/01_construccion_panel/04_integrar_panel_maestro.R")
source("R/01_construccion_panel/04_preparacion_instrumentos.R")
source("R/01_construccion_panel/05_integrar_fob_peru_y_veda.R")
source("R/01_construccion_panel/06_rebasar_deflactor_jun2024.R")   # nuevo
source("R/02_estimacion/16_TAC_instrumento_v2.R")                  # reemplazo
# … resto de scripts de estimación y robustez
```
