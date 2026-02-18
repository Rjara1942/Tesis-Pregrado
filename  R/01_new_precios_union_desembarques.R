################################################################################
# 03_GENERACION_PANEL_FINAL.R
# Objetivo: Generar el panel P (Precio Ponderado) y Q (Desembarques) 
# Estrategia: "Híbrida Inclusiva" (No filtrar Jurel por industria)
# Inputs: 
#   1. Excel Raw: 2025.04.21.pelagicos_proceso-precios...xlsx
#   2. CSV Limpio: desembarques_clean.csv
################################################################################

library(tidyverse)
library(readxl)
library(lubridate)

cat("\n", rep("=", 80), "\n")
cat("GENERACIÓN DE PANEL FINAL (ESTRATEGIA INCLUSIVA)\n")
cat(rep("=", 80), "\n\n")

# ==============================================================================
# 1. CARGAR DATOS
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
# B. Datos de Desembarques (CLEAN)
df_desembarques <- read_csv("desembarques_clean.csv", show_col_types = FALSE)

cat("Datos cargados:\n")
cat("  - Precios Raw:", nrow(df_precio_raw), "\n")
cat("  - Desembarques Clean:", nrow(df_desembarques), "\n\n")

# ==============================================================================
# 2. PROCESAMIENTO DE PRECIOS (P)
# ==============================================================================
cat("=== 2. CONSTRUYENDO VARIABLE PRECIO (P) ===\n")

# 2.1 Estandarización
clean_precios <- df_precio_raw %>%
  mutate(
    NM_RECURSO = str_trim(NM_RECURSO),
    NUI = as.numeric(NUI)
  ) %>%
  filter(NM_RECURSO %in% c("ANCHOVETA", "JUREL", "SARDINA COMUN"))

clean_proceso <- df_proceso_raw %>%
  mutate(
    NM_RECURSO = str_trim(NM_RECURSO),
    NUI = as.numeric(NUI)
  ) %>%
  filter(NM_RECURSO %in% c("ANCHOVETA", "JUREL", "SARDINA COMUN"))

# 2.2 Unión (Match de transacciones)
# NOTA: Aquí NO filtramos por CLASE_INDUSTRIA. Aceptamos Humano y Animal.
df_join <- inner_join(
  clean_precios,
  clean_proceso,
  by = c("ANIO", "MES", "RG", "NUI", "NM_RECURSO")
)

cat("  Matches encontrados (Transacciones):", nrow(df_join), "\n")

# 2.3 Cálculo del Precio Ponderado Macrozonal
# P_weighted = Sum(Precio * Volumen) / Sum(Volumen)
df_P_final <- df_join %>%
  group_by(ANIO, MES, NM_RECURSO) %>%
  summarise(
    # Precio Ponderado
    PRECIO_W = weighted.mean(PRECIO, w = MP_TOTAL, na.rm = TRUE),
    
    # Métricas de control
    Q_MUESTRA_PRECIO = sum(MP_TOTAL, na.rm = TRUE), # Cuánta captura respalda este precio
    N_TRANSACCIONES = n(),
    PRECIO_SD = sd(PRECIO, na.rm = TRUE),
    .groups = "drop"
  )

cat("  Precios mensuales calculados:\n")
print(table(df_P_final$NM_RECURSO))

# ==============================================================================
# 3. PROCESAMIENTO DE CANTIDADES (Q)
# ==============================================================================
cat("\n=== 3. CONSTRUYENDO VARIABLE CANTIDAD (Q) ===\n")

# Agregamos los desembarques (que están por región) a nivel Macrozona
df_Q_final <- df_desembarques %>%
  group_by(ANIO, MES, NM_RECURSO) %>%
  summarise(
    Q_MACRO = sum(Q_TOTAL, na.rm = TRUE),
    Q_INDUSTRIAL = sum(Q_INDUSTRIAL, na.rm = TRUE),
    Q_ARTESANAL = sum(Q_ARTESANAL, na.rm = TRUE),
    N_REGIONES_ACTIVAS = n_distinct(RG),
    .groups = "drop"
  ) %>%
  mutate(
    SHARE_INDUSTRIAL = Q_INDUSTRIAL / Q_MACRO
  )

cat("  Volúmenes mensuales calculados:\n")
print(table(df_Q_final$NM_RECURSO))

# ==============================================================================
# 4. INTEGRACIÓN FINAL (PANEL)
# ==============================================================================
cat("\n=== 4. INTEGRACIÓN P + Q ===\n")

# Usamos Full Join para no perder nada inicialmente, luego inspeccionamos
df_panel <- full_join(
  df_Q_final,
  df_P_final,
  by = c("ANIO", "MES", "NM_RECURSO")
) %>%
  arrange(NM_RECURSO, ANIO, MES) %>%
  mutate(
    FECHA = make_date(ANIO, MES, 1)
  )

# ==============================================================================
# 5. DIAGNÓSTICO Y GUARDADO
# ==============================================================================

# Diagnóstico de Cobertura Efectiva (Donde tenemos P y Q)
cat("\nCOBERTURA EFECTIVA (Meses con Precio Y Cantidad):\n")
cobertura <- df_panel %>%
  filter(!is.na(PRECIO_W) & !is.na(Q_MACRO)) %>%
  count(NM_RECURSO)

print(cobertura)

# Guardar
write_csv(df_panel, "base_integrada_macrozonal_v3.csv")

cat("\n", rep("=", 80), "\n")
cat("ÉXITO: Base guardada como 'base_integrada_macrozonal_v3.csv'\n")
cat("Esta base contiene la serie recuperada del Jurel.\n")
cat(rep("=", 80), "\n")
