################################################################################
# 07_PREPARACION_INSTRUMENTOS.R
# Integrar variables ambientales y precio diesel (deflactado)
################################################################################

library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)

cat("\n", rep("=", 80), "\n")
cat("PREPARACIÓN DE INSTRUMENTOS PARA IAIDS (CON DEFLACIÓN)\n")
cat(rep("=", 80), "\n\n")

# ==============================================================================
# 1. CARGAR DATOS AMBIENTALES
# ==============================================================================

cat("=== 1. CARGANDO DATOS AMBIENTALES ===\n\n")

data_env <- read_csv("data_env_puertos.csv",
  show_col_types = FALSE
)

cat("Datos ambientales cargados:\n")
cat("  Observaciones:", nrow(data_env), "\n")
cat("  Puertos:", n_distinct(data_env$Puerto), "\n")

# ==============================================================================
# 2. MAPEO PUERTO → REGIÓN
# ==============================================================================

cat("\n=== 2. MAPEANDO PUERTOS A REGIONES ===\n")

mapa_regiones <- tibble(
  Puerto = c(
    "San Antonio", "Talcahuano (San Vicente)", "Coronel",
    "Calbuco", "Lota", "Corral", "Puerto Montt",
    "Region 7 (puerto)", "Region 9 (puerto)"
  ),
  REGION = c(5, 8, 8, 10, 8, 14, 10, 7, 9)
)

# ==============================================================================
# 3. PROCESAR DATOS AMBIENTALES
# ==============================================================================

cat("=== 3. PROCESANDO DATOS AMBIENTALES ===\n")

data_env_procesado <- data_env %>%
  left_join(mapa_regiones, by = "Puerto") %>%
  filter(!is.na(REGION)) %>%
  mutate(
    date = ymd(date),
    ANIO = year(date),
    MES = month(date)
  ) %>%
  filter(ANIO >= 2012, ANIO <= 2024)

# ==============================================================================
# 4. AGREGACIÓN MACROZONAL MENSUAL
# ==============================================================================

cat("=== 4. AGREGANDO A NIVEL MACROZONAL-MENSUAL ===\n")

env_macrozonal <- data_env_procesado %>%
  group_by(ANIO, MES) %>%
  summarise(
    # Promedios
    SST_MACRO = mean(sst_0_60km, na.rm = TRUE),
    CHL_A_MACRO = mean(chl_0_60km, na.rm = TRUE),
    WIND_SPEED_MACRO = mean(speed_mean_0_60km, na.rm = TRUE),
    WIND_MAX_MACRO = mean(speed_max_0_60km, na.rm = TRUE),
    
    # Desviación estándar
    SST_SD = sd(sst_0_60km, na.rm = TRUE),
    CHL_A_SD = sd(chl_0_60km, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    SST2 = SST_MACRO^2,
    CHL_A2 = CHL_A_MACRO^2,
    WIND2 = WIND_SPEED_MACRO^2,
    SST_X_CHL = SST_MACRO * CHL_A_MACRO,
    SST_X_WIND = SST_MACRO * WIND_SPEED_MACRO
  )

# ==============================================================================
# 5. CARGAR PRECIO COMBUSTIBLE (NOMINAL)
# ==============================================================================

cat("=== 5. PROCESANDO PRECIO COMBUSTIBLE (NOMINAL) ===\n\n")

# 1. Leer el excel saltando las primeras 8 filas de metadatos
# y declarando qué strings significan valores perdidos en este archivo
combustible_raw <- read_excel(
  "precios_combustibles.xlsx", 
  sheet = "Petróleo Diesel",
  skip = 8, 
  na = c("ND", "NE", "NA", "-", "")
)

# 2. Limpiar nombres de columnas (quitar espacios en blanco al inicio/final)
names(combustible_raw) <- str_trim(names(combustible_raw))
colnames(combustible_raw)[2] <- "Fecha" # Asegurar el nombre de la columna 2

# 3. Procesar fechas y calcular el promedio Macrozonal
combustible_macro <- combustible_raw %>%
  # Quitar filas que no tengan fecha
  filter(!is.na(Fecha)) %>%
  mutate(
    # Parsear fecha: maneja tanto formato número de Excel (34335) como formato Date normal
    Fecha_clean = if_else(
      str_detect(as.character(Fecha), "^[0-9]+$"), 
      as.Date(as.numeric(Fecha), origin = "1899-12-30"), 
      as.Date(Fecha)
    ),
    ANIO = year(Fecha_clean),
    MES = month(Fecha_clean)
  ) %>%
  # Filtrar por el período de tu tesis
  filter(ANIO >= 2012 & ANIO <= 2024) %>%
  rowwise() %>%
  mutate(
    # Promediar solo los puertos/ciudades de la Macrozona Centro-Sur (R5 a R14)
    PRECIO_DIESEL_MACRO = mean(c_across(any_of(c(
      "VALPARAÍSO", "TALCA", "CHILLÁN", "CONCEPCIÓN", 
      "TEMUCO", "VALDIVIA", "PUERTO MONTT"
    ))), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Agrupar por mes (por si hay más de un reporte de precio en un mismo mes)
  group_by(ANIO, MES) %>%
  summarise(PRECIO_DIESEL_MACRO = mean(PRECIO_DIESEL_MACRO, na.rm = TRUE), .groups = "drop")

cat("Combustible procesado exitosamente:\n")
cat("  Observaciones:", nrow(combustible_macro), "meses\n\n")
# ==============================================================================
# 6. INTEGRAR INSTRUMENTOS
# ==============================================================================

cat("=== 6. INTEGRANDO INSTRUMENTOS ===\n")

instrumentos_macro <- env_macrozonal %>%
  left_join(combustible_macro, by = c("ANIO", "MES"))

# ==============================================================================
# 7. MERGE CON BASE INTEGRADA
# ==============================================================================

cat("=== 7. MERGE CON BASE INTEGRADA ===\n")

# NOTA: Cambiado a base_integrada2.csv que contiene el DEFLACTOR
df_base <- read_csv("base_integrada2.csv", show_col_types = FALSE)

df_final <- df_base %>%
  left_join(instrumentos_macro, by = c("ANIO", "MES"))

cat("Base final:", nrow(df_final), "obs\n")

# Verificar missings en variables clave (Nominales)
vars_iv <- c("SST_MACRO", "CHL_A_MACRO", "WIND_SPEED_MACRO", "PRECIO_DIESEL_MACRO")

# ==============================================================================
# 8. IMPUTACIÓN (SI NECESARIO)
# ==============================================================================

if (any(is.na(df_final[vars_iv]))) {
  cat("=== 8. IMPUTANDO VALORES FALTANTES ===\n")
  df_final <- df_final %>%
    arrange(ANIO, MES) %>%
    mutate(across(all_of(vars_iv), ~zoo::na.approx(., rule = 2)))
  cat("✓ Imputación completada\n")
}

# ==============================================================================
# 9. DEFLACIÓN Y TRANSFORMACIONES
# ==============================================================================

cat("=== 9. DEFLACTANDO Y TRANSFORMANDO ===\n")

df_final <- df_final %>%
  arrange(ANIO, MES) %>%
  mutate(
    # --- NUEVO: DEFLACIÓN DEL DIESEL ---
    # Usamos la columna DEFLACTOR presente en base_integrada2
    PRECIO_DIESEL_REAL = PRECIO_DIESEL_MACRO / DEFLACTOR,
    
    # Logaritmos (Usando variables REALES para precios)
    ln_SST = log(SST_MACRO + 20),
    ln_CHL_A = log(CHL_A_MACRO + 0.01),
    ln_WIND = log(WIND_SPEED_MACRO + 0.1),
    ln_DIESEL = log(PRECIO_DIESEL_REAL), # MODIFICADO: Log del precio real
    
    # Rezagos (Usando variable REAL para precio)
    SST_LAG1 = lag(SST_MACRO, 1),
    CHL_A_LAG1 = lag(CHL_A_MACRO, 1),
    DIESEL_LAG1 = lag(PRECIO_DIESEL_REAL, 1) # MODIFICADO: Lag del precio real
  )

cat("✓ Variables deflactadas creadas: PRECIO_DIESEL_REAL, ln_DIESEL\n\n")

# ==============================================================================
# 10. ESTADÍSTICAS
# ==============================================================================

cat("=== 10. ESTADÍSTICAS DESCRIPTIVAS ===\n")

stats <- df_final %>%
  select(SST_MACRO, CHL_A_MACRO, WIND_SPEED_MACRO, PRECIO_DIESEL_MACRO, PRECIO_DIESEL_REAL) %>%
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

cat("=== 11. CORRELACIONES ===\n")

cors <- cor(df_final[c("SST_MACRO", "CHL_A_MACRO", "WIND_SPEED_MACRO", "PRECIO_DIESEL_REAL")], 
            use = "complete.obs")

print(round(cors, 3))
cat("\n")

# ==============================================================================
# 12. GUARDAR
# ==============================================================================

cat("=== 12. GUARDANDO RESULTADOS ===\n")

write_csv(df_final, "base_integrada_con_instrumentos2.csv")
write_csv(instrumentos_macro, "instrumentos_macrozonal.csv")
write_csv(stats, "estadisticas_instrumentos.csv")

cat("✓ Archivos guardados exitosamente\n\n")

# ==============================================================================
# 13. VISUALIZACIONES ACTUALIZADAS
# ==============================================================================

cat("=== 13. VISUALIZACIONES ===\n")

# Gráfico 1: Ambientales
g1 <- df_final %>%
  select(FECHA, SST_MACRO, CHL_A_MACRO, WIND_SPEED_MACRO) %>%
  pivot_longer(-FECHA) %>%
  ggplot(aes(x = FECHA, y = value, color = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  labs(title = "Evolución de Instrumentos Ambientales") +
  theme_minimal() +
  theme(legend.position = "none")
print(g1)
ggsave("grafico_evolucion_instrumentos.png", g1, width = 10, height = 8)

# Gráfico 2: Comparación Diesel Nominal vs Real (NUEVO)

library(ggplot2)
g2 <- df_final %>%
  select(FECHA, NOMINAL = PRECIO_DIESEL_MACRO, REAL = PRECIO_DIESEL_REAL) %>%
  pivot_longer(-FECHA) %>%
  ggplot(aes(x = FECHA, y = value, color = name)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(
    title = "Precio del Diesel: Nominal vs Real",
    subtitle = "Efecto de la deflación (Índice de Precios)",
    y = "Precio ($/Lt)",
    x = "Fecha",
    color = "Tipo de Precio"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(g2)
ggsave("grafico_precio_diesel_comparativo.png", g2, width = 10, height = 6)

cat("✓ grafico_evolucion_instrumentos.png\n")
cat("✓ grafico_precio_diesel_comparativo.png\n\n")

cat("\n", rep("=", 80), "\n")
cat("LISTO: BASE PREPARADA PARA ESTIMACIÓN DE IAIDS\n")
cat(rep("=", 80), "\n\n")