################################################################################
# 07_PREPARACION_INSTRUMENTOS.R
# Objetivo: Integrar variables ambientales (SST, Chl-a, Viento) y precio diesel
# Autor: Ricardo Jara Valencia
# Fecha: Febrero 2026
################################################################################

library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)

cat("\n", rep("=", 80), "\n")
cat("PREPARACIÓN DE INSTRUMENTOS PARA IAIDS\n")
cat(rep("=", 80), "\n\n")

# ==============================================================================
# 1. CARGAR DATOS AMBIENTALES
# ==============================================================================

cat("=== 1. CARGANDO DATOS AMBIENTALES ===\n\n")

# Leer CSV directamente desde ZIP
data_env <- read_csv(
  unz("data_env_puertos_csv.zip", "data_env_puertos.csv"),
  show_col_types = FALSE
)

cat("Datos ambientales cargados:\n")
cat("  Observaciones:", nrow(data_env), "\n")
cat("  Variables:", ncol(data_env), "\n")
cat("  Puertos:", n_distinct(data_env$Puerto), "\n")
cat("  Período:", min(data_env$date), "a", max(data_env$date), "\n\n")

cat("Puertos disponibles:\n")
print(table(data_env$Puerto))
cat("\n")

# ==============================================================================
# 2. MAPEO PUERTO → REGIÓN
# ==============================================================================

cat("=== 2. MAPEANDO PUERTOS A REGIONES ===\n\n")

mapa_regiones <- tibble(
  Puerto = c(
    "San Antonio",
    "Talcahuano (San Vicente)",
    "Coronel",
    "Calbuco",
    "Lota",
    "Corral",
    "Puerto Montt",
    "Region 7 (puerto)", 
    "Region 9 (puerto)"
  ),
  REGION = c(5, 8, 8, 10, 8, 14, 10, 7, 9)
)

print(mapa_regiones)
cat("\n")

# ==============================================================================
# 3. PROCESAR DATOS AMBIENTALES
# ==============================================================================

cat("=== 3. PROCESANDO DATOS AMBIENTALES ===\n\n")

data_env_procesado <- data_env %>%
  left_join(mapa_regiones, by = "Puerto") %>%
  filter(!is.na(REGION)) %>%
  mutate(
    date = ymd(date),
    ANIO = year(date),
    MES = month(date)
  ) %>%
  filter(ANIO >= 2012, ANIO <= 2024)

cat("Después de filtros:\n")
cat("  Observaciones:", nrow(data_env_procesado), "\n")
cat("  Regiones:", n_distinct(data_env_procesado$REGION), "\n\n")

# ==============================================================================
# 4. AGREGACIÓN MACROZONAL MENSUAL
# ==============================================================================

cat("=== 4. AGREGANDO A NIVEL MACROZONAL-MENSUAL ===\n\n")

env_macrozonal <- data_env_procesado %>%
  group_by(ANIO, MES) %>%
  summarise(
    # Promedios (0-60km según tu elección)
    SST_MACRO = mean(sst_0_60km, na.rm = TRUE),
    CHL_A_MACRO = mean(chl_0_60km, na.rm = TRUE),
    WIND_SPEED_MACRO = mean(speed_mean_0_60km, na.rm = TRUE),
    WIND_MAX_MACRO = mean(speed_max_0_60km, na.rm = TRUE),
    
    # Desviación estándar (variabilidad)
    SST_SD = sd(sst_0_60km, na.rm = TRUE),
    CHL_A_SD = sd(chl_0_60km, na.rm = TRUE),
    
    # Control
    N_REGIONES = n_distinct(REGION),
    N_DIAS = sum(!is.na(sst_0_60km)),
    
    .groups = "drop"
  ) %>%
  
  # Transformaciones no lineales
  mutate(
    SST2 = SST_MACRO^2,
    CHL_A2 = CHL_A_MACRO^2,
    WIND2 = WIND_SPEED_MACRO^2,
    SST_X_CHL = SST_MACRO * CHL_A_MACRO,
    SST_X_WIND = SST_MACRO * WIND_SPEED_MACRO
  )

cat("Variables macrozonas:\n")
cat("  Observaciones:", nrow(env_macrozonal), "\n\n")

# ==============================================================================
# 5. CARGAR PRECIO COMBUSTIBLE
# ==============================================================================

cat("=== 5. PROCESANDO PRECIO COMBUSTIBLE ===\n\n")

# Leer Excel
hojas <- excel_sheets("precios_combustibles.xlsx")
cat("Hojas Excel:\n")
print(hojas)
cat("\n")

combustible_raw <- read_excel("precios_combustibles.xlsx", sheet = 1)

cat("Estructura:\n")
cat("  Filas:", nrow(combustible_raw), "\n")
cat("  Columnas:", ncol(combustible_raw), "\n\n")

print(head(combustible_raw, 3))
cat("\n")

# ADAPTAR SEGÚN ESTRUCTURA REAL
# Si tiene años como columnas:
if (any(grepl("^[0-9]{4}$", names(combustible_raw)))) {
  
  cat("Formato detectado: AÑOS COMO COLUMNAS\n\n")
  
  combustible_long <- combustible_raw %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "ANIO",
      values_to = "PRECIO"
    ) %>%
    mutate(ANIO = as.integer(ANIO), PRECIO = as.numeric(PRECIO))
  
  # Expandir a meses
  combustible_macro <- combustible_long %>%
    filter(!is.na(PRECIO)) %>%
    expand_grid(MES = 1:12) %>%
    select(ANIO, MES, PRECIO_DIESEL_MACRO = PRECIO)
  
} else {
  # Datos sintéticos (AJUSTAR MANUALMENTE)
  cat("ADVERTENCIA: Usando datos sintéticos de diesel\n\n")
  
  combustible_macro <- expand_grid(ANIO = 2012:2024, MES = 1:12) %>%
    mutate(PRECIO_DIESEL_MACRO = 600 + (ANIO - 2012) * 45 + rnorm(n(), 0, 15))
}

cat("Combustible procesado:\n")
cat("  Observaciones:", nrow(combustible_macro), "\n\n")

# ==============================================================================
# 6. INTEGRAR INSTRUMENTOS
# ==============================================================================

cat("=== 6. INTEGRANDO INSTRUMENTOS ===\n\n")

instrumentos_macro <- env_macrozonal %>%
  left_join(combustible_macro, by = c("ANIO", "MES"))

cat("Instrumentos integrados:", nrow(instrumentos_macro), "obs\n\n")

# ==============================================================================
# 7. MERGE CON BASE INTEGRADA
# ==============================================================================

cat("=== 7. MERGE CON BASE INTEGRADA ===\n\n")

df_base <- read_csv("base_integrada.csv", show_col_types = FALSE)

df_final <- df_base %>%
  left_join(instrumentos_macro, by = c("ANIO", "MES"))

cat("Base final:", nrow(df_final), "obs\n\n")

# Verificar missings
vars_iv <- c("SST_MACRO", "CHL_A_MACRO", "WIND_SPEED_MACRO", "PRECIO_DIESEL_MACRO")
cat("Valores faltantes:\n")
print(sapply(df_final[vars_iv], function(x) sum(is.na(x))))
cat("\n")

# ==============================================================================
# 8. IMPUTACIÓN (SI NECESARIO)
# ==============================================================================

if (any(is.na(df_final[vars_iv]))) {
  cat("=== 8. IMPUTANDO VALORES FALTANTES ===\n\n")
  
  df_final <- df_final %>%
    arrange(ANIO, MES) %>%
    mutate(across(all_of(vars_iv), ~zoo::na.approx(., rule = 2)))
  
  cat("✓ Imputación completada\n\n")
}

# ==============================================================================
# 9. CREAR LOGARITMOS Y REZAGOS
# ==============================================================================

cat("=== 9. CREANDO TRANSFORMACIONES ===\n\n")

df_final <- df_final %>%
  arrange(ANIO, MES) %>%
  mutate(
    # Logaritmos
    ln_SST = log(SST_MACRO + 20),
    ln_CHL_A = log(CHL_A_MACRO + 0.01),
    ln_WIND = log(WIND_SPEED_MACRO + 0.1),
    ln_DIESEL = log(PRECIO_DIESEL_MACRO),
    
    # Rezagos
    SST_LAG1 = lag(SST_MACRO, 1),
    CHL_A_LAG1 = lag(CHL_A_MACRO, 1),
    DIESEL_LAG1 = lag(PRECIO_DIESEL_MACRO, 1)
  )

cat("✓ Transformaciones creadas\n\n")

# ==============================================================================
# 10. ESTADÍSTICAS
# ==============================================================================

cat("=== 10. ESTADÍSTICAS DESCRIPTIVAS ===\n\n")

stats <- df_final %>%
  select(SST_MACRO, CHL_A_MACRO, WIND_SPEED_MACRO, PRECIO_DIESEL_MACRO) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    N = n(),
    Media = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE)
  )

print(stats)
cat("\n")

# ==============================================================================
# 11. CORRELACIONES
# ==============================================================================

cat("=== 11. CORRELACIONES ===\n\n")

cors <- cor(df_final[c("SST_MACRO", "CHL_A_MACRO", "WIND_SPEED_MACRO", "PRECIO_DIESEL_MACRO")], 
            use = "complete.obs")

print(round(cors, 3))
cat("\n")

# ==============================================================================
# 12. GUARDAR
# ==============================================================================

cat("=== 12. GUARDANDO RESULTADOS ===\n\n")

write_csv(df_final, "base_integrada_con_instrumentos.csv")
write_csv(instrumentos_macro, "instrumentos_macrozonal.csv")
write_csv(stats, "estadisticas_instrumentos.csv")

cat("✓ base_integrada_con_instrumentos.csv\n")
cat("✓ instrumentos_macrozonal.csv\n")
cat("✓ estadisticas_instrumentos.csv\n\n")

# ==============================================================================
# 13. VISUALIZACIONES
# ==============================================================================

cat("=== 13. VISUALIZACIONES ===\n\n")

library(ggplot2)

# Evolución temporal
g1 <- df_final %>%
  select(FECHA, SST_MACRO, CHL_A_MACRO, WIND_SPEED_MACRO) %>%
  pivot_longer(-FECHA) %>%
  ggplot(aes(x = FECHA, y = value, color = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  labs(title = "Evolución de Instrumentos Ambientales") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("grafico_evolucion_instrumentos.png", g1, width = 10, height = 8)

# Precio diesel
g2 <- ggplot(df_final, aes(x = FECHA, y = PRECIO_DIESEL_MACRO)) +
  geom_line(color = "darkblue", size = 1) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Evolución Precio Diesel") +
  theme_minimal()

ggsave("grafico_precio_diesel.png", g2, width = 10, height = 6)

cat("✓ grafico_evolucion_instrumentos.png\n")
cat("✓ grafico_precio_diesel.png\n\n")

# ==============================================================================
# RESUMEN
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("RESUMEN: INSTRUMENTOS PREPARADOS\n")
cat(rep("=", 80), "\n\n")

cat("INSTRUMENTOS DISPONIBLES:\n")
cat("  - SST_MACRO (°C)\n")
cat("  - CHL_A_MACRO (mg/m³)\n")
cat("  - WIND_SPEED_MACRO (m/s)\n")
cat("  - PRECIO_DIESEL_MACRO ($/L)\n")
cat("  - Cuadráticos: SST2, CHL_A2, WIND2\n")
cat("  - Interacciones: SST_X_CHL, SST_X_WIND\n")
cat("  - Logaritmos: ln_SST, ln_CHL_A, ln_WIND, ln_DIESEL\n")
cat("  - Rezagos: SST_LAG1, CHL_A_LAG1, DIESEL_LAG1\n\n")

cat("PRÓXIMO PASO:\n")
cat("  Ejecutar: 05_estimacion_IAIDS.R\n\n")

cat(rep("=", 80), "\n\n")
