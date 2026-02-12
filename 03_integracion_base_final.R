################################################################################
# 03_INTEGRACION_BASE_FINAL.R
# Objetivo: Integrar precios y desembarques en base única
# Inputs: precios_clean.csv, desembarques_clean.csv
# Output: base_integrada.csv
################################################################################

library(tidyverse)
library(lubridate)

# ==============================================================================
# 1. CARGAR DATOS LIMPIOS
# ==============================================================================

cat("\n=== CARGANDO DATOS LIMPIOS ===\n")

df_precios <- read_csv("precios_clean.csv", show_col_types = FALSE)
df_desembarques <- read_csv("desembarques_clean.csv", show_col_types = FALSE)

cat("Precios:", nrow(df_precios), "filas\n")
cat("Desembarques:", nrow(df_desembarques), "filas\n")

# ==============================================================================
# 2. ESTRATEGIA DE INTEGRACIÓN
# ==============================================================================

cat("\n=== ESTRATEGIA DE INTEGRACIÓN ===\n")
cat("
La integración se realizará en DOS NIVELES:

NIVEL 1: REGIONAL (Base desagregada)
  - Join: ANIO + MES + RG + NM_RECURSO
  - Uso: Análisis de heterogeneidad regional
  - Ventaja: Captura variación espacial
  - Desventaja: Panel desbalanceado

NIVEL 2: MACROZONAL (Base agregada - RECOMENDADA PARA IAIDS)
  - Join: ANIO + MES + NM_RECURSO
  - Uso: Estimación del modelo principal
  - Ventaja: Mayor completitud, panel más balanceado
  - Desventaja: Pierde variación regional

Generaremos AMBAS bases para flexibilidad analítica.
\n")

# ==============================================================================
# 3. INTEGRACIÓN NIVEL 1: REGIONAL
# ==============================================================================

cat("\n=== NIVEL 1: INTEGRACIÓN REGIONAL ===\n")

df_regional <- df_precios %>%
  full_join(
    df_desembarques,
    by = c("ANIO", "MES", "RG", "NM_RECURSO", "FECHA"),
    suffix = c("_precio", "_desemb")
  ) %>%
  # Crear indicadores de disponibilidad
  mutate(
    tiene_precio = !is.na(PRECIO_PONDERADO),
    tiene_cantidad = !is.na(Q_TOTAL)
  )

# Diagnóstico de match
cat("\nDiagnóstico de Match Regional:\n")
cat("  Obs con PRECIO y CANTIDAD:", 
    sum(df_regional$tiene_precio & df_regional$tiene_cantidad), "\n")
cat("  Obs solo con PRECIO:", 
    sum(df_regional$tiene_precio & !df_regional$tiene_cantidad), "\n")
cat("  Obs solo con CANTIDAD:", 
    sum(!df_regional$tiene_precio & df_regional$tiene_cantidad), "\n")
cat("  Total observaciones:", nrow(df_regional), "\n")

# Filtrar solo observaciones completas
df_regional_completo <- df_regional %>%
  filter(tiene_precio, tiene_cantidad) %>%
  select(
    -tiene_precio, -tiene_cantidad,
    -FECHA  # Evitar duplicados (está en ambas bases)
  ) %>%
  mutate(FECHA = make_date(year = ANIO, month = MES, day = 1))

cat("\nObservaciones regionales completas:", nrow(df_regional_completo), "\n")

# Análisis de completitud regional
completitud_regional <- df_regional_completo %>%
  group_by(ANIO, MES, RG) %>%
  summarise(n_especies = n_distinct(NM_RECURSO), .groups = "drop") %>%
  count(n_especies, name = "n_meses")

cat("\nCompletitud del panel regional:\n")
print(completitud_regional)

# ==============================================================================
# 4. INTEGRACIÓN NIVEL 2: MACROZONAL
# ==============================================================================

cat("\n=== NIVEL 2: INTEGRACIÓN MACROZONAL ===\n")

# Re-agregar precios a nivel macrozonal
cat("Paso 1: Re-ponderando precios a nivel macrozonal...\n")

df_precios_macro <- df_precios %>%
  group_by(ANIO, MES, NM_RECURSO) %>%
  summarise(
    # Precio ponderado por cantidad de muestra REGIONAL
    PRECIO_MACRO = weighted.mean(
      PRECIO_PONDERADO, 
      w = Q_MUESTRA_PLANTAS, 
      na.rm = TRUE
    ),
    
    # Agregados de cantidad de muestra
    Q_MUESTRA_TOTAL = sum(Q_MUESTRA_PLANTAS, na.rm = TRUE),
    N_REGIONES_PRECIO = n_distinct(RG),
    N_PLANTAS_TOTAL = sum(N_PLANTAS, na.rm = TRUE),
    
    # Indicadores de calidad
    CV_PRECIO_REGIONAL = sd(PRECIO_PONDERADO, na.rm = TRUE) / 
                         mean(PRECIO_PONDERADO, na.rm = TRUE),
    N_OUTLIERS_PRECIO = sum(N_OUTLIERS, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(FECHA = make_date(year = ANIO, month = MES, day = 1))

cat("  Observaciones precio macrozonal:", nrow(df_precios_macro), "\n")

# Re-agregar desembarques a nivel macrozonal
cat("Paso 2: Agregando desembarques a nivel macrozonal...\n")

df_desemb_macro <- df_desembarques %>%
  group_by(ANIO, MES, NM_RECURSO) %>%
  summarise(
    Q_MACRO = sum(Q_TOTAL, na.rm = TRUE),
    Q_INDUSTRIAL_MACRO = sum(Q_INDUSTRIAL, na.rm = TRUE),
    Q_ARTESANAL_MACRO = sum(Q_ARTESANAL, na.rm = TRUE),
    N_REGIONES_DESEMB = n_distinct(RG),
    N_OUTLIERS_CANTIDAD = sum(N_OUTLIERS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    SHARE_INDUSTRIAL = Q_INDUSTRIAL_MACRO / Q_MACRO,
    SHARE_ARTESANAL = Q_ARTESANAL_MACRO / Q_MACRO,
    FECHA = make_date(year = ANIO, month = MES, day = 1)
  )

cat("  Observaciones desembarques macrozonal:", nrow(df_desemb_macro), "\n")

# Join macrozonal
df_macrozonal <- df_precios_macro %>%
  inner_join(
    df_desemb_macro,
    by = c("ANIO", "MES", "NM_RECURSO", "FECHA")
  )

cat("\nDiagnóstico de Match Macrozonal:\n")
cat("  Observaciones con precio Y cantidad:", nrow(df_macrozonal), "\n")

# Análisis de completitud macrozonal
completitud_macro <- df_macrozonal %>%
  group_by(ANIO, MES) %>%
  summarise(n_especies = n_distinct(NM_RECURSO), .groups = "drop") %>%
  count(n_especies, name = "n_meses")

cat("\nCompletitud del panel macrozonal:\n")
print(completitud_macro)

meses_completos_macro <- df_macrozonal %>%
  group_by(ANIO, MES) %>%
  filter(n_distinct(NM_RECURSO) == 3) %>%
  ungroup()

cat("\nMeses con 3 especies (panel balanceado):", 
    n_distinct(meses_completos_macro$FECHA), "\n")

# ==============================================================================
# 5. ANÁLISIS COMPARATIVO: REGIONAL VS MACROZONAL
# ==============================================================================

cat("\n=== COMPARACIÓN: REGIONAL VS MACROZONAL ===\n")

comparacion <- tibble(
  Característica = c(
    "Observaciones totales",
    "Meses únicos",
    "Meses con 3 especies",
    "% Completitud panel",
    "Período inicio",
    "Período fin"
  ),
  Regional = c(
    nrow(df_regional_completo),
    n_distinct(df_regional_completo$FECHA),
    sum(df_regional_completo %>% 
        group_by(ANIO, MES, RG) %>% 
        summarise(n = n_distinct(NM_RECURSO), .groups = "drop") %>% 
        pull(n) == 3) / 
      n_distinct(df_regional_completo %>% select(ANIO, MES, RG)),
    NA,  # Calcularemos después
    min(df_regional_completo$ANIO),
    max(df_regional_completo$ANIO)
  ),
  Macrozonal = c(
    nrow(df_macrozonal),
    n_distinct(df_macrozonal$FECHA),
    n_distinct(meses_completos_macro$FECHA),
    n_distinct(meses_completos_macro$FECHA) / n_distinct(df_macrozonal$FECHA),
    min(df_macrozonal$ANIO),
    max(df_macrozonal$ANIO)
  )
)

print(comparacion)

cat("\nRECOMENDACIÓN: Usar base MACROZONAL para estimación principal\n")
cat("  Razón: Mayor completitud y coherencia con hipótesis de mercado único\n")

# ==============================================================================
# 6. VALIDACIONES DE CONSISTENCIA
# ==============================================================================

cat("\n=== VALIDACIONES DE CONSISTENCIA ===\n")

# Check 1: Precios macrozonas vs regionales
if(nrow(df_regional_completo) > 0 & nrow(df_macrozonal) > 0) {
  
  # Comparar agregados
  check_agregacion <- df_regional_completo %>%
    group_by(ANIO, MES, NM_RECURSO) %>%
    summarise(
      P_regional_pond = weighted.mean(PRECIO_PONDERADO, w = Q_MUESTRA_PLANTAS),
      Q_regional_sum = sum(Q_TOTAL),
      .groups = "drop"
    ) %>%
    inner_join(
      df_macrozonal %>% select(ANIO, MES, NM_RECURSO, PRECIO_MACRO, Q_MACRO),
      by = c("ANIO", "MES", "NM_RECURSO")
    ) %>%
    mutate(
      diff_precio = abs(P_regional_pond - PRECIO_MACRO) / PRECIO_MACRO * 100,
      diff_cantidad = abs(Q_regional_sum - Q_MACRO) / Q_MACRO * 100
    )
  
  cat("Diferencias precio (regional vs macro):\n")
  cat("  Media:", round(mean(check_agregacion$diff_precio, na.rm = TRUE), 2), "%\n")
  cat("  Máxima:", round(max(check_agregacion$diff_precio, na.rm = TRUE), 2), "%\n")
  
  cat("Diferencias cantidad (regional vs macro):\n")
  cat("  Media:", round(mean(check_agregacion$diff_cantidad, na.rm = TRUE), 2), "%\n")
  cat("  Máxima:", round(max(check_agregacion$diff_cantidad, na.rm = TRUE), 2), "%\n")
}

# Check 2: Rangos razonables
cat("\nRangos de variables macrozonas:\n")
rangos <- df_macrozonal %>%
  group_by(NM_RECURSO) %>%
  summarise(
    N = n(),
    P_min = min(PRECIO_MACRO),
    P_max = max(PRECIO_MACRO),
    Q_min = min(Q_MACRO),
    Q_max = max(Q_MACRO),
    .groups = "drop"
  )
print(rangos)

# ==============================================================================
# 7. TRATAMIENTO DE CASOS ESPECIALES
# ==============================================================================

cat("\n=== TRATAMIENTO DE CASOS ESPECIALES ===\n")

# Identificar meses con alta dispersión regional de precios
alta_dispersion_regional <- df_macrozonal %>%
  filter(CV_PRECIO_REGIONAL > 0.3, N_REGIONES_PRECIO > 1) %>%
  arrange(desc(CV_PRECIO_REGIONAL))

if(nrow(alta_dispersion_regional) > 0) {
  cat("ADVERTENCIA:", nrow(alta_dispersion_regional), 
      "meses con alta dispersión regional de precios (CV > 30%)\n")
  cat("  Esto puede indicar segmentación de mercado o diferencias de calidad\n")
}

# Identificar meses con outliers múltiples
casos_outliers <- df_macrozonal %>%
  filter(N_OUTLIERS_PRECIO > 0 | N_OUTLIERS_CANTIDAD > 0) %>%
  select(ANIO, MES, NM_RECURSO, N_OUTLIERS_PRECIO, N_OUTLIERS_CANTIDAD)

if(nrow(casos_outliers) > 0) {
  cat("\nMeses con outliers detectados:", nrow(casos_outliers), "\n")
  cat("  Nota: Outliers se mantienen pero están flaggeados para análisis de sensibilidad\n")
}

# ==============================================================================
# 8. GUARDAR BASES FINALES
# ==============================================================================

# Base regional
write_csv(df_regional_completo, "base_regional.csv")

# Base macrozonal completa
write_csv(df_macrozonal, "base_macrozonal_completa.csv")

# Base macrozonal SOLO meses con 3 especies (RECOMENDADA)
write_csv(meses_completos_macro, "base_macrozonal_balanceada.csv")

cat("\n=== ARCHIVOS GUARDADOS ===\n")
cat("✓ base_regional.csv\n")
cat("✓ base_macrozonal_completa.csv\n")
cat("✓ base_macrozonal_balanceada.csv (RECOMENDADA)\n")

# ==============================================================================
# 9. RESUMEN EJECUTIVO
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("RESUMEN DE INTEGRACIÓN\n")
cat(rep("=", 80), "\n\n")

cat("INPUTS:\n")
cat("  - precios_clean.csv:", nrow(df_precios), "obs\n")
cat("  - desembarques_clean.csv:", nrow(df_desembarques), "obs\n")

cat("\nOUTPUTS GENERADOS:\n")
cat("  1. base_regional.csv\n")
cat("     • Observaciones:", nrow(df_regional_completo), "\n")
cat("     • Nivel: Región-Mes-Especie\n")
cat("     • Uso: Análisis de heterogeneidad espacial\n\n")

cat("  2. base_macrozonal_completa.csv\n")
cat("     • Observaciones:", nrow(df_macrozonal), "\n")
cat("     • Nivel: Mes-Especie (agregado zona centro-sur)\n")
cat("     • Uso: Análisis general\n\n")

cat("  3. base_macrozonal_balanceada.csv ⭐ RECOMENDADA\n")
cat("     • Observaciones:", nrow(meses_completos_macro), "\n")
cat("     • Meses únicos:", n_distinct(meses_completos_macro$FECHA), "\n")
cat("     • Panel: PERFECTAMENTE BALANCEADO (3 especies × N meses)\n")
cat("     • Período:", min(meses_completos_macro$ANIO), "-", 
    max(meses_completos_macro$ANIO), "\n")
cat("     • Uso: ESTIMACIÓN IAIDS\n")

cat("\n", rep("=", 80), "\n")
cat("INTEGRACIÓN COMPLETADA\n")
cat("Siguiente paso: Ejecutar 04_analisis_exploratorio.R\n")
cat(rep("=", 80), "\n\n")
