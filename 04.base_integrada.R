# ==============================================================================
# INTEGRACIÓN Y ANÁLISIS DESCRIPTIVO 
# ==============================================================================



library(tidyverse)
library(lubridate)
library(readxl)

# ==============================================================================
# 2. CARGA DE DATOS
# ==============================================================================

# A. Base Macrozonal (Tu base limpia)
df_macro <- read_csv("base_macrozonal_completa.csv")

# B. Precio FOB (Harina)
df_fob <- read_csv("FOB_CLP.csv") %>%
  rename(ANIO = year, MES = month)

# C. IPC (Para Deflactor)
# El archivo tiene metadatos en las primeras filas, saltamos hasta el encabezado
df_ipc <- read_excel("IPC_2012_2024.xlsx", skip = 2) %>%
  rename(FECHA = 1, VAR_MENSUAL = 2) %>% # Renombrar columnas por posición
  select(FECHA, VAR_MENSUAL) %>%
  filter(!is.na(VAR_MENSUAL)) %>%
  mutate(
    FECHA = ymd(FECHA), # Convertir a fecha
    ANIO = year(FECHA),
    MES = month(FECHA)
  )

# ==============================================================================
# 3. CÁLCULO DEL DEFLACTOR (BASE 2024)
# ==============================================================================

# Construimos el Índice acumulado a partir de las variaciones mensuales
# Asumimos base 100 al inicio y acumulamos
df_ipc <- df_ipc %>%
  arrange(ANIO, MES) %>%
  mutate(
    FACTOR_MENSUAL = 1 + (VAR_MENSUAL / 100),
    INDICE_ACUMULADO = cumprod(FACTOR_MENSUAL)
  )

# Calculamos el promedio del índice en 2024 para usarlo como base (=1)
indice_base_2024 <- mean(df_ipc$INDICE_ACUMULADO[df_ipc$ANIO == 2024], na.rm = TRUE)

df_ipc <- df_ipc %>%
  mutate(DEFLACTOR = INDICE_ACUMULADO / indice_base_2024) %>%
  select(ANIO, MES, DEFLACTOR)

# ==============================================================================
# 4. INTEGRACIÓN FINAL
# ==============================================================================

df_integrada <- df_macro %>%
  left_join(df_fob, by = c("ANIO", "MES")) %>%
  left_join(df_ipc, by = c("ANIO", "MES")) %>%
  mutate(
    # Calcular Precios Reales (Pesos de 2024)
    PRECIO_REAL_MACRO = PRECIO_MACRO / DEFLACTOR,
    P_HARINA_REAL = P_harina_FOB_CLP / DEFLACTOR,
    
    # Crear fecha para gráficos
    FECHA = make_date(ANIO, MES, 1)
  )

# Guardar base integrada limpia
write_csv(df_integrada, "base_integrada.csv")

# ==============================================================================
# 5. GENERACIÓN DE GRÁFICOS
# ==============================================================================

# A. Evolución del Precio Real por Especie
g1 <- ggplot(df_integrada, aes(x = FECHA, y = PRECIO_REAL_MACRO, color = NM_RECURSO)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  theme_minimal() +
  labs(
    title = "Evolución del Precio Real Macrozonal (Ex-Vessel)",
    subtitle = "Pesos de 2024 (Deflactado por IPC)",
    y = "Precio ($/ton)",
    x = "Año",
    color = "Especie"
  ) +
  theme(legend.position = "bottom")

print(g1)
ggsave("grafico_evolucion_precios.png", plot = g1, width = 10, height = 6)

# B. Comparación con Precio FOB (Harina)
# Nota: El precio de la harina es mucho mayor, usaremos eje secundario o escala log
# Para ver la correlación visual, graficamos todo junto.

g2 <- ggplot() +
  # Precios de las especies (Líneas de color)
  geom_line(data = df_integrada, aes(x = FECHA, y = PRECIO_REAL_MACRO, color = NM_RECURSO), size = 0.8) +
  
  # Precio Harina FOB (Línea negra discontinua)
  geom_line(data = df_integrada, aes(x = FECHA, y = P_HARINA_REAL, linetype = "Harina FOB"), color = "black", size = 0.8) +
  
  theme_minimal() +
  labs(
    title = "Precio Ex-Vessel vs. Precio Internacional Harina (Real)",
    subtitle = "La línea negra muestra el precio FOB de Harina (Output)",
    y = "Precio Real ($/ton)",
    x = "Año",
    color = "Especie (Input)",
    linetype = "Referencia"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "bottom")

print(g2)
ggsave("grafico_precio_vs_fob.png", plot = g2, width = 10, height = 6)

# C. Evolución de Desembarques (Q_MACRO)
g3 <- ggplot(df_integrada, aes(x = FECHA, y = Q_MACRO, fill = NM_RECURSO)) +
  geom_area(alpha = 0.6, position = "identity") + # Usar 'identity' para ver superposición o 'stack' para total
  geom_line(aes(color = NM_RECURSO), size = 0.5) +
  theme_minimal() +
  labs(
    title = "Evolución de Desembarques Macrozonales (Q)",
    subtitle = "Toneladas mensuales procesadas",
    y = "Toneladas",
    x = "Año",
    fill = "Especie",
    color = "Especie"
  ) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~NM_RECURSO, scales = "free_y", ncol = 1) + # Separar para ver mejor las escalas
  theme(legend.position = "none")

print(g3)
ggsave("grafico_desembarques.png", plot = g3, width = 10, height = 8)

cat("Proceso completado. Gráficos guardados en el directorio de trabajo.\n")