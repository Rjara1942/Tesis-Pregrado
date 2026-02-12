################################################################################
# 02_LIMPIEZA_DESEMBARQUES.R
# Objetivo: Procesar y validar datos de desembarques (SERNAPESCA)
# Input: DESEMBARQUES.xlsx
# Output: desembarques_clean.csv
################################################################################

library(tidyverse)
library(readxl)
library(lubridate)

# ==============================================================================
# 1. FUNCIÓN PARA PROCESAR RANGOS DE EXCEL
# ==============================================================================

cat("\n=== DEFINIENDO FUNCIÓN DE PROCESAMIENTO ===\n")

procesar_rango_excel <- function(hoja, rango, especie, flota, archivo = "DESEMBARQUES.xlsx") {
  
  # Leer rango específico
  df <- read_excel(archivo, sheet = hoja, range = rango)
  
  # Renombrar columnas clave
  colnames(df)[1] <- "ANIO"
  colnames(df)[2] <- "MES_TXT"
  
  # Regiones de interés (Zona Centro-Sur)
  regiones_interes <- c("5", "7", "8", "9", "14", "10")
  
  # Verificar cuáles regiones existen en este rango
  cols_existentes <- intersect(names(df), regiones_interes)
  
  if(length(cols_existentes) == 0) {
    message("  ADVERTENCIA: No se encontraron regiones centro-sur en ", hoja, " ", rango)
    return(NULL)
  }
  
  # Pivotar a formato largo
  df_long <- df %>%
    select(ANIO, MES_TXT, all_of(cols_existentes)) %>%
    pivot_longer(
      cols = all_of(cols_existentes),
      names_to = "RG",
      values_to = "TONELADAS"
    ) %>%
    mutate(
      NM_RECURSO = especie,
      FLOTA = flota,
      RG = as.numeric(RG),
      TONELADAS = as.numeric(TONELADAS)
    ) %>%
    # Eliminar NAs y ceros
    filter(!is.na(TONELADAS), TONELADAS > 0, !is.na(ANIO))
  
  return(df_long)
}

# ==============================================================================
# 2. EXTRAER TODOS LOS RANGOS
# ==============================================================================

cat("\n=== EXTRAYENDO DATOS DE DESEMBARQUES ===\n")

lista_datos <- list()

# --- FLOTA INDUSTRIAL ---
cat("\nProcesando INDUSTRIAL...\n")
lista_datos[[1]] <- procesar_rango_excel("INDUSTRIAL (nacional)", "A2:L293", "JUREL", "INDUSTRIAL")
lista_datos[[2]] <- procesar_rango_excel("INDUSTRIAL (nacional)", "N2:W196", "SARDINA COMUN", "INDUSTRIAL")
lista_datos[[3]] <- procesar_rango_excel("INDUSTRIAL (nacional)", "Y2:AJ273", "ANCHOVETA", "INDUSTRIAL")

# --- LANCHAS (Artesanal) ---
cat("\nProcesando LANCHAS...\n")
lista_datos[[4]] <- procesar_rango_excel("LANCHAS (CentroSur)", "A2:I143", "JUREL", "ARTESANAL")
lista_datos[[5]] <- procesar_rango_excel("LANCHAS (CentroSur)", "K2:S170", "SARDINA COMUN", "ARTESANAL")
lista_datos[[6]] <- procesar_rango_excel("LANCHAS (CentroSur)", "U2:AC169", "ANCHOVETA", "ARTESANAL")

# --- BOTES (Artesanal) ---
cat("\nProcesando BOTES...\n")
lista_datos[[7]] <- procesar_rango_excel("BOTES (CentroSur)", "A2:I146", "JUREL", "ARTESANAL")
lista_datos[[8]] <- procesar_rango_excel("BOTES (CentroSur)", "K2:S163", "SARDINA COMUN", "ARTESANAL")
lista_datos[[9]] <- procesar_rango_excel("BOTES (CentroSur)", "U2:AD109", "ANCHOVETA", "ARTESANAL")

# Unir todo
df_desembarques_raw <- bind_rows(lista_datos)

cat("\nFilas totales extraídas:", nrow(df_desembarques_raw), "\n")

# ==============================================================================
# 3. LIMPIEZA Y ESTANDARIZACIÓN
# ==============================================================================

cat("\n=== LIMPIANDO Y ESTANDARIZANDO ===\n")

df_desembarques_clean <- df_desembarques_raw %>%
  
  # 1. Convertir texto de mes a número
  mutate(
    MES_TXT = str_trim(tolower(MES_TXT)),
    MES = case_when(
      MES_TXT %in% c("ene", "enero") ~ 1,
      MES_TXT %in% c("feb", "febrero") ~ 2,
      MES_TXT %in% c("mar", "marzo") ~ 3,
      MES_TXT %in% c("abr", "abril") ~ 4,
      MES_TXT %in% c("may", "mayo") ~ 5,
      MES_TXT %in% c("jun", "junio") ~ 6,
      MES_TXT %in% c("jul", "julio") ~ 7,
      MES_TXT %in% c("ago", "agosto") ~ 8,
      MES_TXT %in% c("sep", "sept", "septiembre") ~ 9,
      MES_TXT %in% c("oct", "octubre") ~ 10,
      MES_TXT %in% c("nov", "noviembre") ~ 11,
      MES_TXT %in% c("dic", "diciembre") ~ 12,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # 2. Filtrar casos inválidos
  filter(
    !is.na(ANIO), 
    !is.na(MES),
    ANIO >= 2012,  # Limitar a período de análisis
    ANIO <= 2024
  ) %>%
  
  # 3. Estandarizar nombres de especies
  mutate(
    NM_RECURSO = str_trim(toupper(NM_RECURSO)),
    NM_RECURSO = case_when(
      NM_RECURSO == "SARDINA COMÚN" ~ "SARDINA COMUN",
      TRUE ~ NM_RECURSO
    )
  ) %>%
  
  # 4. Crear variable de fecha
  mutate(FECHA = make_date(year = ANIO, month = MES, day = 1))

cat("Filas después de limpieza:", nrow(df_desembarques_clean), "\n")

# ==============================================================================
# 4. VERIFICAR MESES MAL CODIFICADOS
# ==============================================================================

cat("\n=== VERIFICANDO CODIFICACIÓN DE MESES ===\n")

casos_mes_na <- df_desembarques_raw %>%
  mutate(MES_TXT_lower = str_trim(tolower(MES_TXT))) %>%
  filter(
    !(MES_TXT_lower %in% c("ene", "enero", "feb", "febrero", "mar", "marzo", 
                          "abr", "abril", "may", "mayo", "jun", "junio",
                          "jul", "julio", "ago", "agosto", "sep", "sept", "septiembre",
                          "oct", "octubre", "nov", "noviembre", "dic", "diciembre"))
  )

if(nrow(casos_mes_na) > 0) {
  cat("ADVERTENCIA:", nrow(casos_mes_na), "casos con mes mal codificado\n")
  cat("Valores únicos encontrados:\n")
  print(unique(casos_mes_na$MES_TXT))
} else {
  cat("✓ Todos los meses están bien codificados\n")
}

# ==============================================================================
# 5. DETECCIÓN DE OUTLIERS EN DESEMBARQUES
# ==============================================================================

cat("\n=== ANÁLISIS DE OUTLIERS EN DESEMBARQUES ===\n")

# Outliers por especie-región-flota
outlier_analysis_Q <- df_desembarques_clean %>%
  group_by(NM_RECURSO, FLOTA) %>%
  summarise(
    N = n(),
    Media = mean(TONELADAS),
    Mediana = median(TONELADAS),
    Q1 = quantile(TONELADAS, 0.25),
    Q3 = quantile(TONELADAS, 0.75),
    IQR = Q3 - Q1,
    Limite_Superior = Q3 + 3 * IQR,  # Solo límite superior (no hay desembarques negativos)
    Max_Observado = max(TONELADAS),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric) & !c(N), ~round(., 0)))

print(outlier_analysis_Q)

# Identificar outliers
df_desembarques_clean <- df_desembarques_clean %>%
  left_join(
    outlier_analysis_Q %>% select(NM_RECURSO, FLOTA, Limite_Superior),
    by = c("NM_RECURSO", "FLOTA")
  ) %>%
  mutate(
    is_outlier_Q = TONELADAS > Limite_Superior
  )

cat("\nOutliers detectados:\n")
print(table(df_desembarques_clean$NM_RECURSO, 
            df_desembarques_clean$FLOTA, 
            df_desembarques_clean$is_outlier_Q))

# Casos extremos (para revisión manual)
casos_extremos <- df_desembarques_clean %>%
  filter(is_outlier_Q) %>%
  arrange(desc(TONELADAS)) %>%
  select(ANIO, MES, RG, NM_RECURSO, FLOTA, TONELADAS, Limite_Superior)

if(nrow(casos_extremos) > 0) {
  cat("\nCasos extremos (top 10):\n")
  print(head(casos_extremos, 10))
}

# ==============================================================================
# 6. AGREGAR POR MES-REGIÓN-ESPECIE (CONSOLIDAR FLOTAS)
# ==============================================================================

cat("\n=== AGREGANDO DESEMBARQUES TOTALES ===\n")

# Justificación: El mercado ve la oferta TOTAL, independiente de origen de flota
df_desembarques_agregado <- df_desembarques_clean %>%
  group_by(ANIO, MES, RG, NM_RECURSO, FECHA) %>%
  summarise(
    Q_TOTAL = sum(TONELADAS),
    Q_INDUSTRIAL = sum(TONELADAS[FLOTA == "INDUSTRIAL"], na.rm = TRUE),
    Q_ARTESANAL = sum(TONELADAS[FLOTA == "ARTESANAL"], na.rm = TRUE),
    N_FLOTAS_ACTIVAS = n_distinct(FLOTA),
    N_OUTLIERS = sum(is_outlier_Q),
    .groups = "drop"
  ) %>%
  # Calcular participación de cada flota
  mutate(
    SHARE_INDUSTRIAL = Q_INDUSTRIAL / Q_TOTAL,
    SHARE_ARTESANAL = Q_ARTESANAL / Q_TOTAL
  )

cat("Observaciones agregadas:", nrow(df_desembarques_agregado), "\n")

# ==============================================================================
# 7. ANÁLISIS DE COBERTURA
# ==============================================================================

cat("\n=== ANÁLISIS DE COBERTURA TEMPORAL ===\n")

# Por especie y región
cobertura_region <- df_desembarques_agregado %>%
  group_by(NM_RECURSO, RG) %>%
  summarise(
    N_Meses = n(),
    Periodo_Inicio = min(ANIO),
    Periodo_Fin = max(ANIO),
    Q_Promedio = mean(Q_TOTAL),
    .groups = "drop"
  ) %>%
  arrange(NM_RECURSO, desc(N_Meses))

cat("\nCobertura por especie-región:\n")
print(cobertura_region)

# Por especie y año
cobertura_anual <- df_desembarques_agregado %>%
  group_by(ANIO, NM_RECURSO) %>%
  summarise(
    N_Meses = n(),
    Q_Total_Anual = sum(Q_TOTAL),
    N_Regiones = n_distinct(RG),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = ANIO,
    names_from = NM_RECURSO,
    values_from = c(N_Meses, Q_Total_Anual),
    values_fill = 0
  )

cat("\nCobertura anual:\n")
print(cobertura_anual)

# ==============================================================================
# 8. IDENTIFICAR VEDAS BIOLÓGICAS
# ==============================================================================

cat("\n=== IDENTIFICANDO VEDAS ===\n")

# Vedas típicas: Agosto-Septiembre para anchoveta y sardina
# Esto explica ausencia de datos en esos meses

vedas_detectadas <- df_desembarques_agregado %>%
  group_by(NM_RECURSO, MES) %>%
  summarise(N_Años_Activos = n_distinct(ANIO), .groups = "drop") %>%
  pivot_wider(names_from = MES, values_from = N_Años_Activos, values_fill = 0)

cat("\nFrecuencia de desembarques por mes (N° años con datos):\n")
print(vedas_detectadas)

# ==============================================================================
# 9. VALIDACIONES FINALES
# ==============================================================================

cat("\n=== VALIDACIONES FINALES ===\n")

# Check 1: Suma de flotas = total
check_suma_flotas <- df_desembarques_agregado %>%
  mutate(diff = abs((Q_INDUSTRIAL + Q_ARTESANAL) - Q_TOTAL)) %>%
  summarise(
    Max_Diferencia = max(diff),
    N_Diferencias = sum(diff > 0.01)
  )

cat("Check flotasuma: Max diferencia =", check_suma_flotas$Max_Diferencia, "toneladas\n")

# Check 2: Participaciones suman 1
check_shares <- df_desembarques_agregado %>%
  mutate(suma_shares = SHARE_INDUSTRIAL + SHARE_ARTESANAL) %>%
  summarise(
    Share_OK = all(abs(suma_shares - 1) < 0.001)
  )

cat("Check participaciones suman 1:", check_shares$Share_OK, "\n")

# Check 3: Rango de años
cat("Período cubierto:", 
    min(df_desembarques_agregado$ANIO), "-", 
    max(df_desembarques_agregado$ANIO), "\n")

# ==============================================================================
# 10. GUARDAR DATOS LIMPIOS
# ==============================================================================

# Base agregada (para merge con precios)
write_csv(df_desembarques_agregado, "desembarques_clean.csv")

# Base desagregada por flota (para análisis complementarios)
write_csv(df_desembarques_clean, "desembarques_por_flota.csv")

cat("\n=== ARCHIVOS GUARDADOS ===\n")
cat("✓ desembarques_clean.csv\n")
cat("✓ desembarques_por_flota.csv\n")

# ==============================================================================
# 11. RESUMEN EJECUTIVO
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("RESUMEN DE LIMPIEZA DE DESEMBARQUES\n")
cat(rep("=", 80), "\n\n")

cat("DATOS EXTRAÍDOS:\n")
cat("  - Fuentes: INDUSTRIAL, LANCHAS, BOTES\n")
cat("  - Filas brutas:", nrow(df_desembarques_raw), "\n")

cat("\nPROCESAMIENTO:\n")
cat("  1. Conversión de meses textuales a numéricos\n")
cat("  2. Filtro de años 2012-2024\n")
cat("  3. Agregación de flotas (ARTESANAL = LANCHAS + BOTES)\n")
cat("  4. Detección de outliers (flaggeados, no eliminados)\n")

cat("\nRESULTADO FINAL:\n")
cat("  - Observaciones agregadas:", nrow(df_desembarques_agregado), "\n")
cat("  - Período:", min(df_desembarques_agregado$ANIO), "-", 
    max(df_desembarques_agregado$ANIO), "\n")
cat("  - Especies:", paste(unique(df_desembarques_agregado$NM_RECURSO), collapse = ", "), "\n")
cat("  - Regiones:", paste(sort(unique(df_desembarques_agregado$RG)), collapse = ", "), "\n")

cat("\nDISTRIBUCIÓN DE OFERTA:\n")
resumen_Q <- df_desembarques_agregado %>%
  group_by(NM_RECURSO) %>%
  summarise(
    N_obs = n(),
    Q_Media = mean(Q_TOTAL),
    Q_Total = sum(Q_TOTAL),
    Pct_Industrial = mean(SHARE_INDUSTRIAL) * 100,
    .groups = "drop"
  )

for(i in 1:nrow(resumen_Q)) {
  cat("  -", resumen_Q$NM_RECURSO[i], ":\n")
  cat("     N obs:", resumen_Q$N_obs[i], "\n")
  cat("     Q media:", scales::comma(round(resumen_Q$Q_Media[i], 0)), "ton/mes\n")
  cat("     % Industrial:", round(resumen_Q$Pct_Industrial[i], 1), "%\n")
}

cat("\n", rep("=", 80), "\n")
cat("LIMPIEZA DE DESEMBARQUES COMPLETADA\n")
cat("Siguiente paso: Ejecutar 03_integracion_base_final.R\n")
cat(rep("=", 80), "\n\n")
