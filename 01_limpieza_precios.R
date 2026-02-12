################################################################################
# 01_LIMPIEZA_PRECIOS.R
# Objetivo: Procesar y validar datos de precios ex-vessel
# Input: 2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx
# Output: precios_clean.csv
################################################################################

library(tidyverse)
library(readxl)
library(lubridate)

# ==============================================================================
# 1. CARGA DE DATOS BRUTOS
# ==============================================================================

cat("\n=== CARGANDO DATOS DE PRECIOS ===\n")

df_precio_raw <- read_excel(
  "2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx", 
  sheet = "PRECIO", 
  guess_max = 10000
)

df_proceso_raw <- read_excel(
  "2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx",
  sheet = "PROCESO", 
  guess_max = 10000
)

cat("PRECIO:", nrow(df_precio_raw), "filas\n")
cat("PROCESO:", nrow(df_proceso_raw), "filas\n")

# ==============================================================================
# 2. EXPLORACIÓN INICIAL
# ==============================================================================

cat("\n=== DIAGNÓSTICO INICIAL ===\n")

cat("\n--- Especies en PRECIO ---\n")
print(table(df_precio_raw$NM_RECURSO))

cat("\n--- Especies en PROCESO ---\n")
print(table(df_proceso_raw$NM_RECURSO))

cat("\n--- Tipo de Industria (PROCESO) ---\n")
print(table(df_proceso_raw$CLASE_INDUSTRIA))

cat("\n--- Tipo de Industria (PRECIO) ---\n")
print(table(df_precio_raw$CLASE_INDUSTRIA_II))

cat("\n--- Regiones en datos ---\n")
cat("PRECIO:", paste(sort(unique(df_precio_raw$RG)), collapse=", "), "\n")
cat("PROCESO:", paste(sort(unique(df_proceso_raw$RG)), collapse=", "), "\n")

# ==============================================================================
# 3. FILTROS Y LIMPIEZA - PRECIO
# ==============================================================================

cat("\n=== APLICANDO FILTROS A PRECIOS ===\n")

df_precio_clean <- df_precio_raw %>%
  # 1. Filtrar solo las 3 especies de estudio
  filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN", "JUREL")) %>%
  
  # 2. Filtrar solo industria de REDUCCIÓN (ANIMAL)
  filter(CLASE_INDUSTRIA_II == "ANIMAL") %>%
  
  # 3. Filtrar solo zona centro-sur
  filter(RG %in% c(5, 7, 8, 9, 14, 10)) %>%
  
  # 4. Eliminar precios faltantes o cero
  filter(!is.na(PRECIO), PRECIO > 0) %>%
  
  # 5. Seleccionar variables relevantes
  select(
    ANIO,
    MES,
    RG,
    NUI,
    NM_RECURSO,
    PRECIO,
    TIPO_MP
  ) %>%
  
  # 6. Estandarizar nombres de especies
  mutate(
    NM_RECURSO = str_trim(toupper(NM_RECURSO)),
    NM_RECURSO = case_when(
      NM_RECURSO == "SARDINA COMÚN" ~ "SARDINA COMUN",
      TRUE ~ NM_RECURSO
    )
  )

cat("Filas después de filtros:", nrow(df_precio_clean), "\n")
cat("Pérdida:", nrow(df_precio_raw) - nrow(df_precio_clean), "filas\n")

# ==============================================================================
# 4. FILTROS Y LIMPIEZA - PROCESO
# ==============================================================================

cat("\n=== APLICANDO FILTROS A PROCESO ===\n")

df_proceso_clean <- df_proceso_raw %>%
  # 1. Filtrar solo las 3 especies de estudio
  filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN", "JUREL")) %>%
  
  # 2. Filtrar solo industria de REDUCCIÓN (ANIMAL)
  filter(CLASE_INDUSTRIA == "ANIMAL") %>%
  
  # 3. Filtrar solo zona centro-sur
  filter(RG %in% c(5, 7, 8, 9, 14, 10)) %>%
  
  # 4. Eliminar cantidades faltantes o cero
  filter(!is.na(MP_TOTAL), MP_TOTAL > 0) %>%
  
  # 5. Seleccionar variables relevantes
  select(
    ANIO,
    MES,
    RG,
    NUI,
    NM_RECURSO,
    MP_TOTAL
  ) %>%
  
  # 6. Estandarizar nombres de especies
  mutate(
    NM_RECURSO = str_trim(toupper(NM_RECURSO)),
    NM_RECURSO = case_when(
      NM_RECURSO == "SARDINA COMÚN" ~ "SARDINA COMUN",
      TRUE ~ NM_RECURSO
    )
  )

cat("Filas después de filtros:", nrow(df_proceso_clean), "\n")
cat("Pérdida:", nrow(df_proceso_raw) - nrow(df_proceso_clean), "filas\n")

# ==============================================================================
# 5. UNIR PRECIO Y PROCESO (MATCH A NIVEL TRANSACCIÓN)
# ==============================================================================

cat("\n=== UNIENDO PRECIO CON PROCESO ===\n")

# Join completo (conservar ambas fuentes)
df_transacciones <- df_proceso_clean %>%
  full_join(
    df_precio_clean,
    by = c("ANIO", "MES", "NUI", "RG", "NM_RECURSO"),
    suffix = c("_proceso", "_precio")
  )

cat("Filas totales después del join:", nrow(df_transacciones), "\n")

# Diagnóstico de match
cat("\n--- Diagnóstico de Match ---\n")
cat("Transacciones con PRECIO y CANTIDAD:", 
    sum(!is.na(df_transacciones$PRECIO) & !is.na(df_transacciones$MP_TOTAL)), "\n")
cat("Transacciones solo con PRECIO:", 
    sum(!is.na(df_transacciones$PRECIO) & is.na(df_transacciones$MP_TOTAL)), "\n")
cat("Transacciones solo con CANTIDAD:", 
    sum(is.na(df_transacciones$PRECIO) & !is.na(df_transacciones$MP_TOTAL)), "\n")

# Para el cálculo de precios ponderados, necesitamos ambos
df_transacciones_validas <- df_transacciones %>%
  filter(!is.na(PRECIO), !is.na(MP_TOTAL))

cat("\nTransacciones válidas (con precio Y cantidad):", 
    nrow(df_transacciones_validas), "\n")

# ==============================================================================
# 6. DETECCIÓN DE OUTLIERS EN PRECIOS
# ==============================================================================

cat("\n=== ANÁLISIS DE OUTLIERS ===\n")

# Calcular límites por especie usando IQR
outlier_analysis <- df_transacciones_validas %>%
  group_by(NM_RECURSO) %>%
  summarise(
    N = n(),
    Media = mean(PRECIO),
    Mediana = median(PRECIO),
    Q1 = quantile(PRECIO, 0.25),
    Q3 = quantile(PRECIO, 0.75),
    IQR = Q3 - Q1,
    Limite_Inferior = Q1 - 3 * IQR,  # 3 IQR es más conservador que 1.5
    Limite_Superior = Q3 + 3 * IQR,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 0)))

print(outlier_analysis)

# Identificar outliers
df_con_outlier_flag <- df_transacciones_validas %>%
  left_join(
    outlier_analysis %>% select(NM_RECURSO, Limite_Inferior, Limite_Superior),
    by = "NM_RECURSO"
  ) %>%
  mutate(
    is_outlier = PRECIO < Limite_Inferior | PRECIO > Limite_Superior
  )

cat("\nOutliers detectados por especie:\n")
print(table(df_con_outlier_flag$NM_RECURSO, df_con_outlier_flag$is_outlier))

# DECISIÓN: Mantener outliers pero flaggearlos para análisis de sensibilidad
# Justificación: En mercados con shocks de oferta, precios extremos son informativos

# ==============================================================================
# 7. CALCULAR PRECIOS PONDERADOS POR MES-REGIÓN-ESPECIE
# ==============================================================================

cat("\n=== CALCULANDO PRECIOS PONDERADOS ===\n")

df_precios_ponderados <- df_con_outlier_flag %>%
  # Calcular monto de transacción
  mutate(MONTO_TRANSACCION = PRECIO * MP_TOTAL) %>%
  
  # Agrupar por mes-región-especie
  group_by(ANIO, MES, RG, NM_RECURSO) %>%
  
  # Calcular precio ponderado y estadísticas
  summarise(
    PRECIO_PONDERADO = sum(MONTO_TRANSACCION) / sum(MP_TOTAL),
    
    Q_MUESTRA_PLANTAS = sum(MP_TOTAL),
    N_PLANTAS = n_distinct(NUI),
    N_TRANSACCIONES = n(),
    
    # Indicadores de calidad de precio
    PRECIO_MIN = min(PRECIO),
    PRECIO_MAX = max(PRECIO),
    CV_PRECIO = sd(PRECIO) / mean(PRECIO),  # Dispersión entre plantas
    N_OUTLIERS = sum(is_outlier),
    
    .groups = "drop"
  ) %>%
  
  # Crear variable de fecha
  mutate(FECHA = make_date(year = ANIO, month = MES, day = 1))

cat("Observaciones precio-región-mes-especie:", nrow(df_precios_ponderados), "\n")

# ==============================================================================
# 8. VALIDACIÓN DE PRECIOS
# ==============================================================================

cat("\n=== VALIDACIÓN DE PRECIOS ===\n")

# Check 1: Rango razonable por especie
validacion_rango <- df_precios_ponderados %>%
  group_by(NM_RECURSO) %>%
  summarise(
    N_obs = n(),
    Min = min(PRECIO_PONDERADO),
    P25 = quantile(PRECIO_PONDERADO, 0.25),
    Mediana = median(PRECIO_PONDERADO),
    P75 = quantile(PRECIO_PONDERADO, 0.75),
    Max = max(PRECIO_PONDERADO),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric) & !N_obs, ~scales::dollar(., prefix = "$", big.mark = ".")))

cat("\nDistribución de precios ponderados:\n")
print(validacion_rango)

# Check 2: Identificar meses con alta dispersión entre plantas
alta_dispersion <- df_precios_ponderados %>%
  filter(CV_PRECIO > 0.3, N_PLANTAS > 1) %>%  # CV > 30% con más de 1 planta
  arrange(desc(CV_PRECIO))

if(nrow(alta_dispersion) > 0) {
  cat("\nADVERTENCIA:", nrow(alta_dispersion), 
      "observaciones con alta dispersión de precios entre plantas (CV > 30%)\n")
  cat("Primeros casos:\n")
  print(head(alta_dispersion %>% select(ANIO, MES, RG, NM_RECURSO, N_PLANTAS, CV_PRECIO), 10))
}

# Check 3: Cobertura temporal
cobertura_temporal <- df_precios_ponderados %>%
  group_by(ANIO, NM_RECURSO) %>%
  summarise(Meses_con_Datos = n(), .groups = "drop") %>%
  pivot_wider(names_from = NM_RECURSO, values_from = Meses_con_Datos, values_fill = 0)

cat("\nCobertura temporal (meses con datos por año):\n")
print(cobertura_temporal)

# ==============================================================================
# 9. GUARDAR DATOS LIMPIOS
# ==============================================================================

write_csv(df_precios_ponderados, "precios_clean.csv")

# También guardar transacciones individuales para análisis de robustez
write_csv(df_con_outlier_flag, "transacciones_individuales.csv")

cat("\n=== ARCHIVOS GUARDADOS ===\n")
cat("✓ precios_clean.csv\n")
cat("✓ transacciones_individuales.csv\n")

# ==============================================================================
# 10. RESUMEN EJECUTIVO
# ==============================================================================

cat("\n" , rep("=", 80), "\n")
cat("RESUMEN DE LIMPIEZA DE PRECIOS\n")
cat(rep("=", 80), "\n\n")

cat("DATOS ORIGINALES:\n")
cat("  - Filas PRECIO:", nrow(df_precio_raw), "\n")
cat("  - Filas PROCESO:", nrow(df_proceso_raw), "\n")

cat("\nFILTROS APLICADOS:\n")
cat("  1. Solo especies: ANCHOVETA, SARDINA COMUN, JUREL\n")
cat("  2. Solo industria de REDUCCIÓN (ANIMAL)\n")
cat("  3. Solo regiones centro-sur (5, 7, 8, 9, 14, 10)\n")
cat("  4. Solo observaciones con precio Y cantidad positivos\n")

cat("\nRESULTADO FINAL:\n")
cat("  - Observaciones válidas:", nrow(df_precios_ponderados), "\n")
cat("  - Período:", min(df_precios_ponderados$ANIO), "-", 
    max(df_precios_ponderados$ANIO), "\n")
cat("  - Especies:", paste(unique(df_precios_ponderados$NM_RECURSO), collapse = ", "), "\n")

cat("\nCALIDAD DE DATOS:\n")
cat("  - Promedio plantas por obs:", 
    round(mean(df_precios_ponderados$N_PLANTAS), 1), "\n")
cat("  - Obs con >1 planta:", 
    sum(df_precios_ponderados$N_PLANTAS > 1), 
    "(", round(sum(df_precios_ponderados$N_PLANTAS > 1)/nrow(df_precios_ponderados)*100, 1), "%)\n")
cat("  - Outliers detectados:", sum(df_con_outlier_flag$is_outlier), "\n")

cat("\n", rep("=", 80), "\n")
cat("LIMPIEZA DE PRECIOS COMPLETADA\n")
cat("Siguiente paso: Ejecutar 02_limpieza_desembarques.R\n")
cat(rep("=", 80), "\n\n")
