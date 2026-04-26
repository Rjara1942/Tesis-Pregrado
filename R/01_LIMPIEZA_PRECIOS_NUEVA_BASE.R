################################################################################
# LIMPIEZA PRECIOS NUEVA BASE IFOP - CON CRUCE PRECIO-PROCESO
# Estrategia: Ponderación por volumen de materia prima procesada
# Autor: Ricardo Jara Valencia
################################################################################

library(tidyverse)
library(readxl)
library(lubridate)

cat("\n", rep("=", 80), "\n")
cat("LIMPIEZA PRECIOS - NUEVA BASE IFOP (CON PONDERACIÓN)\n")
cat(rep("=", 80), "\n\n")

# ==============================================================================
# 1. CARGA DE DATOS BRUTOS
# ==============================================================================

cat("=== 1. CARGANDO DATOS ===\n\n")

df_precio_raw <- read_excel(
  "2025_04_21_pelagicos_proceso-precios_mp_2012-2024.xlsx", 
  sheet = "PRECIO", 
  guess_max = 10000
)

df_proceso_raw <- read_excel(
  "2025_04_21_pelagicos_proceso-precios_mp_2012-2024.xlsx",
  sheet = "PROCESO", 
  guess_max = 10000
)

cat("Datos cargados:\n")
cat("  PRECIO:", nrow(df_precio_raw), "filas\n")
cat("  PROCESO:", nrow(df_proceso_raw), "filas\n\n")

# ==============================================================================
# 2. EXPLORACIÓN INICIAL
# ==============================================================================

cat("=== 2. EXPLORACIÓN INICIAL ===\n\n")

cat("Especies en PRECIO:\n")
print(table(df_precio_raw$NM_RECURSO))

cat("\nEspecies en PROCESO:\n")
print(table(df_proceso_raw$NM_RECURSO))

cat("\nClase Industria (PRECIO):\n")
print(table(df_precio_raw$CLASE_INDUSTRIA_II))

cat("\nClase Industria (PROCESO):\n")
print(table(df_proceso_raw$CLASE_INDUSTRIA))

cat("\nRegiones disponibles:\n")
cat("  PRECIO:", paste(sort(unique(df_precio_raw$RG)), collapse = ", "), "\n")
cat("  PROCESO:", paste(sort(unique(df_proceso_raw$RG)), collapse = ", "), "\n\n")

# ==============================================================================
# 3. FILTROS PRECIO
# ==============================================================================

cat("=== 3. FILTRANDO PRECIO ===\n\n")

df_precio_clean <- df_precio_raw %>%
  # Solo 3 especies
  filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN", "JUREL")) %>%
  
  # Solo reducción (ANIMAL)
  filter(CLASE_INDUSTRIA_II == "ANIMAL") %>%
  
  # Solo zona centro-sur
  filter(RG %in% c(5, 7, 8, 9, 10, 14)) %>%
  
  # Precios positivos
  filter(!is.na(PRECIO), PRECIO > 0) %>%
  
  # Estandarizar nombres
  mutate(
    NM_RECURSO = str_trim(toupper(NM_RECURSO)),
    NM_RECURSO = case_when(
      NM_RECURSO == "SARDINA COMÚN" ~ "SARDINA COMUN",
      TRUE ~ NM_RECURSO
    )
  ) %>%
  
  # Seleccionar variables
  select(ANIO, MES, RG, NUI, NM_RECURSO, PRECIO)

cat("PRECIO después de filtros:", nrow(df_precio_clean), "filas\n")
cat("  Pérdida:", nrow(df_precio_raw) - nrow(df_precio_clean), "filas\n\n")

# ==============================================================================
# 4. FILTROS PROCESO
# ==============================================================================

cat("=== 4. FILTRANDO PROCESO ===\n\n")

df_proceso_clean <- df_proceso_raw %>%
  # Solo 3 especies
  filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN", "JUREL")) %>%
  
  # Solo reducción (ANIMAL)
  filter(CLASE_INDUSTRIA == "ANIMAL") %>%
  
  # Solo zona centro-sur
  filter(RG %in% c(5, 7, 8, 9, 10, 14)) %>%
  
  # MP_TOTAL positiva
  filter(!is.na(MP_TOTAL), MP_TOTAL > 0) %>%
  
  # Estandarizar nombres
  mutate(
    NM_RECURSO = str_trim(toupper(NM_RECURSO)),
    NM_RECURSO = case_when(
      NM_RECURSO == "SARDINA COMÚN" ~ "SARDINA COMUN",
      TRUE ~ NM_RECURSO
    )
  ) %>%
  
  # Seleccionar variables
  select(ANIO, MES, RG, NUI, NM_RECURSO, MP_TOTAL)

cat("PROCESO después de filtros:", nrow(df_proceso_clean), "filas\n")
cat("  Pérdida:", nrow(df_proceso_raw) - nrow(df_proceso_clean), "filas\n\n")

# ==============================================================================
# 5. CRUCE PRECIO-PROCESO (A NIVEL TRANSACCIÓN)
# ==============================================================================

cat("=== 5. CRUZANDO PRECIO CON PROCESO ===\n\n")

# Full join para conservar ambas fuentes
df_transacciones <- df_proceso_clean %>%
  full_join(
    df_precio_clean,
    by = c("ANIO", "MES", "NUI", "RG", "NM_RECURSO"),
    suffix = c("_proceso", "_precio")
  )

cat("Transacciones totales después del join:", nrow(df_transacciones), "\n\n")

# Diagnóstico de match
cat("Diagnóstico de match:\n")
cat("  Con PRECIO y CANTIDAD:", 
    sum(!is.na(df_transacciones$PRECIO) & !is.na(df_transacciones$MP_TOTAL)), "\n")
cat("  Solo PRECIO (sin cantidad):", 
    sum(!is.na(df_transacciones$PRECIO) & is.na(df_transacciones$MP_TOTAL)), "\n")
cat("  Solo CANTIDAD (sin precio):", 
    sum(is.na(df_transacciones$PRECIO) & !is.na(df_transacciones$MP_TOTAL)), "\n\n")

# Match por especie
cat("Match por especie:\n")
match_especie <- df_transacciones %>%
  group_by(NM_RECURSO) %>%
  summarise(
    Total = n(),
    Con_ambos = sum(!is.na(PRECIO) & !is.na(MP_TOTAL)),
    Solo_precio = sum(!is.na(PRECIO) & is.na(MP_TOTAL)),
    Solo_cantidad = sum(is.na(PRECIO) & !is.na(MP_TOTAL)),
    Tasa_match = round(Con_ambos / Total * 100, 1)
  )
print(match_especie)
cat("\n")

# Filtrar transacciones válidas (con precio Y cantidad)
df_validas <- df_transacciones %>%
  filter(!is.na(PRECIO), !is.na(MP_TOTAL))

cat("Transacciones válidas (precio Y cantidad):", nrow(df_validas), "\n\n")

# ==============================================================================
# 6. DETECCIÓN DE OUTLIERS
# ==============================================================================

cat("=== 6. DETECTANDO OUTLIERS ===\n\n")

# Límites por especie (IQR × 3)
outlier_limits <- df_validas %>%
  group_by(NM_RECURSO) %>%
  summarise(
    N = n(),
    Media = mean(PRECIO),
    Mediana = median(PRECIO),
    Q1 = quantile(PRECIO, 0.25),
    Q3 = quantile(PRECIO, 0.75),
    IQR = Q3 - Q1,
    Lim_Inf = Q1 - 3 * IQR,
    Lim_Sup = Q3 + 3 * IQR,
    .groups = "drop"
  )

cat("Límites de outliers:\n")
print(outlier_limits %>% 
        mutate(across(c(Media:Lim_Sup), ~scales::comma(round(., 0)))))
cat("\n")

# Flaggear outliers
df_validas <- df_validas %>%
  left_join(
    outlier_limits %>% select(NM_RECURSO, Lim_Inf, Lim_Sup),
    by = "NM_RECURSO"
  ) %>%
  mutate(
    IS_OUTLIER = PRECIO < Lim_Inf | PRECIO > Lim_Sup
  )

cat("Outliers detectados:\n")
print(table(df_validas$NM_RECURSO, df_validas$IS_OUTLIER))
cat("\n")

# Mostrar algunos outliers
if(sum(df_validas$IS_OUTLIER) > 0) {
  cat("Ejemplos de outliers:\n")
  outliers_sample <- df_validas %>%
    filter(IS_OUTLIER) %>%
    select(ANIO, MES, RG, NM_RECURSO, PRECIO, MP_TOTAL) %>%
    arrange(desc(PRECIO)) %>%
    head(10)
  print(outliers_sample)
  cat("\n")
}

# DECISIÓN: Eliminar outliers extremos
df_validas_sin_outliers <- df_validas %>%
  filter(!IS_OUTLIER)

cat("Transacciones después de eliminar outliers:", 
    nrow(df_validas_sin_outliers), "\n")
cat("  Eliminadas:", sum(df_validas$IS_OUTLIER), "\n\n")

# ==============================================================================
# 7. CALCULAR PRECIOS PONDERADOS
# ==============================================================================

cat("=== 7. CALCULANDO PRECIOS PONDERADOS ===\n\n")

# Estrategia: Precio ponderado por volumen de MP procesada
# Agregación: MES × REGIÓN × ESPECIE

df_precios_ponderados <- df_validas_sin_outliers %>%
  # Calcular valor de transacción
  mutate(VALOR_TRANSACCION = PRECIO * MP_TOTAL) %>%
  
  # Agrupar
  group_by(ANIO, MES, RG, NM_RECURSO) %>%
  
  # Calcular precio ponderado y estadísticas
  summarise(
    # Precio ponderado = Σ(precio × cantidad) / Σ(cantidad)
    PRECIO_PONDERADO = sum(VALOR_TRANSACCION) / sum(MP_TOTAL),
    
    # Volumen de muestra
    Q_MUESTRA_PLANTAS = sum(MP_TOTAL),
    
    # Número de transacciones
    N_PLANTAS = n_distinct(NUI),
    N_TRANSACCIONES = n(),
    
    # Dispersión de precios
    PRECIO_MIN = min(PRECIO),
    PRECIO_MAX = max(PRECIO),
    PRECIO_SD = sd(PRECIO),
    CV_PRECIO = ifelse(n() > 1, sd(PRECIO) / mean(PRECIO), 0),
    
    .groups = "drop"
  ) %>%
  
  # Crear fecha
  mutate(FECHA = make_date(ANIO, MES, 1))

cat("Observaciones precio-región-mes-especie:", nrow(df_precios_ponderados), "\n\n")

# Distribución por especie
cat("Distribución por especie:\n")
dist_especie <- df_precios_ponderados %>%
  group_by(NM_RECURSO) %>%
  summarise(
    N_obs = n(),
    Anio_min = min(ANIO),
    Anio_max = max(ANIO),
    Meses_unicos = n_distinct(paste(ANIO, MES))
  )
print(dist_especie)
cat("\n")

# ==============================================================================
# 8. VALIDACIÓN DE PRECIOS
# ==============================================================================

cat("=== 8. VALIDACIÓN DE PRECIOS ===\n\n")

# Rango de precios ponderados
cat("Distribución de precios ponderados:\n")
validacion <- df_precios_ponderados %>%
  group_by(NM_RECURSO) %>%
  summarise(
    N = n(),
    Min = min(PRECIO_PONDERADO),
    P25 = quantile(PRECIO_PONDERADO, 0.25),
    Mediana = median(PRECIO_PONDERADO),
    Media = mean(PRECIO_PONDERADO),
    P75 = quantile(PRECIO_PONDERADO, 0.75),
    Max = max(PRECIO_PONDERADO),
    CV = sd(PRECIO_PONDERADO) / mean(PRECIO_PONDERADO)
  ) %>%
  mutate(across(c(Min:Max), ~scales::comma(round(., 0))))

print(validacion)
cat("\n")

# Cobertura temporal
cat("Cobertura temporal (meses con datos por año):\n")
cobertura <- df_precios_ponderados %>%
  group_by(ANIO, NM_RECURSO) %>%
  summarise(N_meses = n(), .groups = "drop") %>%
  pivot_wider(names_from = NM_RECURSO, values_from = N_meses, values_fill = 0)

print(cobertura)
cat("\n")

# Alta dispersión entre plantas
alta_disp <- df_precios_ponderados %>%
  filter(CV_PRECIO > 0.3, N_PLANTAS > 1) %>%
  arrange(desc(CV_PRECIO))

if(nrow(alta_disp) > 0) {
  cat("⚠️ ADVERTENCIA:", nrow(alta_disp), 
      "obs con alta dispersión (CV > 30%) entre plantas\n")
  cat("   Primeros casos:\n")
  print(head(alta_disp %>% 
               select(ANIO, MES, RG, NM_RECURSO, N_PLANTAS, CV_PRECIO), 5))
  cat("\n")
}

# ==============================================================================
# 9. GUARDAR RESULTADOS
# ==============================================================================

cat("=== 9. GUARDANDO RESULTADOS ===\n\n")

# Archivo principal
write_csv(df_precios_ponderados, "precios_ponderados_nueva_base.csv")
cat("✓ precios_ponderados_nueva_base.csv\n")

# Transacciones individuales (para análisis)
write_csv(df_validas_sin_outliers, "transacciones_individuales_nueva_base.csv")
cat("✓ transacciones_individuales_nueva_base.csv\n")

# Estadísticas de validación
write_csv(validacion, "validacion_precios.csv")
cat("✓ validacion_precios.csv\n\n")

# ==============================================================================
# 10. RESUMEN EJECUTIVO
# ==============================================================================

cat(rep("=", 80), "\n")
cat("RESUMEN LIMPIEZA PRECIOS - NUEVA BASE IFOP\n")
cat(rep("=", 80), "\n\n")

cat("DATOS ORIGINALES:\n")
cat("  PRECIO:", nrow(df_precio_raw), "filas\n")
cat("  PROCESO:", nrow(df_proceso_raw), "filas\n\n")

cat("FILTROS APLICADOS:\n")
cat("  1. Especies: ANCHOVETA, SARDINA COMUN, JUREL\n")
cat("  2. Industria: ANIMAL (reducción)\n")
cat("  3. Regiones: 5, 7, 8, 9, 10, 14 (centro-sur)\n")
cat("  4. Transacciones con precio Y cantidad positivos\n")
cat("  5. Outliers eliminados (IQR × 3)\n\n")

cat("CRUCE PRECIO-PROCESO:\n")
cat("  Transacciones válidas:", nrow(df_validas), "\n")
cat("  Tasa de match:\n")
for(i in 1:nrow(match_especie)) {
  cat(sprintf("    %s: %.1f%%\n", 
              match_especie$NM_RECURSO[i], 
              match_especie$Tasa_match[i]))
}
cat("\n")

cat("RESULTADO FINAL:\n")
cat("  Observaciones:", nrow(df_precios_ponderados), "\n")
cat("  Período:", min(df_precios_ponderados$ANIO), "-", 
    max(df_precios_ponderados$ANIO), "\n")
cat("  Distribución por especie:\n")
for(i in 1:nrow(dist_especie)) {
  cat(sprintf("    %s: %d obs (%d meses únicos)\n",
              dist_especie$NM_RECURSO[i],
              dist_especie$N_obs[i],
              dist_especie$Meses_unicos[i]))
}
cat("\n")

cat("CALIDAD DE DATOS:\n")
cat("  Promedio plantas/obs:", 
    round(mean(df_precios_ponderados$N_PLANTAS), 1), "\n")
cat("  Obs con >1 planta:", 
    sum(df_precios_ponderados$N_PLANTAS > 1), 
    sprintf("(%.1f%%)", 
            sum(df_precios_ponderados$N_PLANTAS > 1) / 
            nrow(df_precios_ponderados) * 100), "\n")
cat("  Outliers eliminados:", sum(df_validas$IS_OUTLIER), "\n\n")

cat(rep("=", 80), "\n")
cat("LIMPIEZA COMPLETADA\n")
cat("Siguiente paso: Integrar con desembarques_clean.csv\n")
cat(rep("=", 80), "\n\n")
