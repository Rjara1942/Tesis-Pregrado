# Tesis de Pregrado — Precios del complejo pelágico centro-sur

Repositorio de la tesis de pregrado: estimación de la respuesta de precios
del complejo industrial pelágico (anchoveta-sardina común-jurel) a
variables ambientales, de oferta macrozonal e instrumentos de costos
(diésel, FOB internacional, TAC), usando un panel planta-mes 2012–2024
con estimación por variables instrumentales (IV-2SLS).

## Estructura del repositorio

```
.
├── docs/                        # documento de tesis y material de referencia
│   ├── escrito.pdf
│   ├── bibliografia.bib
│   └── instrucciones_modelo_precios_v3.pdf
├── data/                        # datos crudos y paneles intermedios (NO versionados, ver abajo)
├── R/
│   ├── 01_construccion_panel/   # parseo de fuentes crudas, integracion ambiental/economica
│   ├── 02_estimacion/           # modelos OLS e IV-2SLS
│   ├── 03_diagnosticos_robustez/# tests de autocorrelacion, multicolinealidad, robustez
│   ├── 04_visualizacion/        # graficos descriptivos, mapas, sankey
│   └── reportes/                # Rmd y su salida (html/pdf) con resultados finales
├── maps/                        # mapas de variables ambientales y ubicacion de puertos/ZEE
├── outputs/
│   ├── figures/                 # graficos generados por los scripts (PNG/PDF/SVG)
│   └── reportes_intermedios/    # reportes de robustez y comparaciones en md/pdf/html
└── tesis_datos.Rproj
```

## Datos

Los archivos de datos (`.csv`, `.xlsx`, `.rds`) **no se versionan** en este
repositorio (ver `.gitignore`) por su tamaño y por restricciones de uso de
algunas fuentes (SERNAPESCA, IFOP, Banco Central). Todos los scripts en
`R/` asumen que estos archivos viven en una carpeta `data/` en la raíz del
proyecto, y todas las rutas usan `here::here("data", "archivo.csv")` para
no depender del directorio desde el que se ejecute cada script.

Fuentes usadas (debes colocarlas en `data/` con estos nombres exactos
para que los scripts corran sin modificación):

| Archivo | Fuente |
|---|---|
| `DESEMBARQUES_SERNAPESCA.xlsx` | SERNAPESCA |
| `2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx` | IFOP |
| `data_env_puertos.csv` | variables ambientales por puerto (Copernicus/satelital) |
| `TC_2012-2024.xlsx` | Tipo de cambio, Banco Central |
| `harina_pescado_FOB.xlsx` | Precio FOB harina de pescado |
| `IPC_2012_2024.xlsx` | IPC, INE/Banco Central |
| `precios_combustibles.xlsx` | Precio diésel regionalizado, CNE/ENAP |
| `BD_PA_2013-2025-sep.xlsx` | Precio anchoveta Perú |
| `biomass_para_costos.xlsx` | Biomasa para costos |
| `precio_peru.csv` | FOB Perú, BCRP |
| `TAC_anual.xlsx` | Cuotas anuales (TAC), industrial y artesanal |
| `data_ambiental_por_puerto_*.RDS` | Variables ambientales crudas por puerto |

Los paneles intermedios (`panel.base.csv`, `panel_con_env_puerto.csv`,
`panel_maestro_definitivo.csv`, `panel_con_alternativas.csv`, etc.) los
genera el propio pipeline al correr los scripts en orden — no necesitas
crearlos a mano.

## Pipeline: orden de ejecución

Los scripts están numerados según su lugar en el flujo. El orden general
es:

1. **`R/01_construccion_panel/`**: `01_panel_base.R` →
   `02_integrar_env_puerto.R` → `03_panel_maes.R` →
   `04_integrar_panel_maestro.R` / `04_preparacion_instrumentos.R` →
   `05_integrar_fob_peru_y_veda.R`. (`envdta_puerto.R` genera los mapas
   ambientales de forma independiente.)
2. **`R/02_estimacion/`**: modelos sobre `panel_con_alternativas.csv`,
   desde el baseline (`02_estimacion_test.R`) hasta las versiones
   optimizadas con instrumentos (`07_estimacion_iv_optimizada.R`,
   `16_TAC_instrumento.R`).
3. **`R/03_diagnosticos_robustez/`**: tests de especificación y
   verificaciones de robustez sobre los mismos paneles.
4. **`R/04_visualizacion/`**: gráficos descriptivos y de resultados,
   independientes entre sí (cada uno se puede correr una vez que existe
   el panel que usa como input).

No existe todavía un script único que orqueste todo el pipeline de
principio a fin (a diferencia de un paquete más empaquetado); cada script
se corre individualmente desde RStudio con el `.Rproj` abierto, en el
orden indicado arriba.

## Reproducir el proyecto

```bash
git clone https://github.com/Rjara1942/Tesis-Pregrado.git
cd Tesis-Pregrado
```

1. Abre `tesis_datos.Rproj` en RStudio.
2. Crea la carpeta `data/` (si no existe) y coloca ahí los archivos
   listados en la tabla de la sección **Datos**.
3. Corre los scripts de `R/01_construccion_panel/` en el orden indicado.
4. Continúa con `R/02_estimacion/` y `R/03_diagnosticos_robustez/` según
   lo que necesites reproducir.
5. Los gráficos y reportes intermedios se generan automáticamente en
   `outputs/figures/` y `outputs/reportes_intermedios/`; los mapas, en
   `maps/`.

## Documento de tesis

El documento completo está en `docs/escrito.pdf`. La bibliografía en
formato BibTeX está en `docs/bibliografia.bib`.

## Notas sobre reportes intermedios

`outputs/reportes_intermedios/` contiene múltiples versiones de reportes
de estimación y robustez (`reporte_estimaciones.pdf`,
`reporte_estimaciones_commentsFQE.pdf`,
`REPORTE_ESTIMACION_IV_FINAL_v2.pdf`, etc.). Se conservan todas
deliberadamente como historial de las revisiones del proceso con el
profesor guía/comité, no son duplicados a eliminar.
