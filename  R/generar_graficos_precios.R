################################################################################
#  Generación de Gráficos de Precios Ex-Vessel
#   1. Distribución de precios por especie (histogramas)
#   2. Estacionalidad de precios por mes
#   3. Evolución temporal 2012-2024
#   4. Relación precio vs cantidad (scatter plots)
################################################################################
# GENERACIÓN DE GRÁFICOS - ADAPTADO A TU ESTRUCTURA REAL
################################################################################

library(tidyverse)
library(lubridate)
library(scales)
library(ggplot2)
# ============================================================================ #
# 1. PREPARAR DATOS
# ============================================================================ #
# Configuración de tema global para los gráficos
theme_set(theme_minimal(base_size = 11))

# Paleta de colores por especie (consistente con tus gráficos)
colores_especies <- c(
  "ANCHOVETA" = "#5DADE2",      # Azul claro
  "JUREL" = "#AF7AC5",           # Morado
  "SARDINA COMUN" = "#F39C12"    # Naranja
)

# ============================================================================ #
# 1. CARGAR Y PREPARAR DATOS
# ============================================================================ #

# Cargar base de datos (ajustar ruta según tu directorio)
df <- read_csv("base_integrada_macrozonal_v3.csv",
               locale = locale(encoding = "UTF-8"))

# Suponiendo que 'df' ya está cargado
df_precios <- df %>%
  # Filtrar solo observaciones con precio válido
  filter(!is.na(PRECIO_W)) %>%
  # Crear variables auxiliares
  mutate(
    # Precio en miles de pesos
    PRECIO_MILES = PRECIO_W / 1000,
    # Cantidad en miles de toneladas
    Q_MILES_TON = Q_MACRO / 1000,
    # Mes como factor para estacionalidad
    MES_FACTOR = factor(MES, levels = 1:12, labels = month.name),
    # Etiqueta año-mes
    ANIO_MES = format(FECHA, "%Y-%m"),
    # Estandarizar nombres de especies (por si acaso)
    ESPECIE_NOMBRE = case_when(
      str_detect(NM_RECURSO, regex("anch", ignore_case = TRUE)) ~ "ANCHOVETA",
      str_detect(NM_RECURSO, regex("jur", ignore_case = TRUE)) ~ "JUREL",
      str_detect(NM_RECURSO, regex("sard", ignore_case = TRUE)) ~ "SARDINA COMUN",
      TRUE ~ NM_RECURSO
    )
  )

# Ver resumen
cat("\n=== RESUMEN DE DATOS ===\n")
df_precios %>%
  group_by(ESPECIE_NOMBRE) %>%
  summarise(
    N = n(),
    Media = mean(PRECIO_MILES, na.rm = TRUE),
    Mediana = median(PRECIO_MILES, na.rm = TRUE),
    Min_Precio = min(PRECIO_MILES, na.rm = TRUE),
    Max_Precio = max(PRECIO_MILES, na.rm = TRUE)
  ) %>%
  print()


# ============================================================================ #
#  GRÁFICO 1: DISTRIBUCIÓN DE PRECIOS
# ============================================================================ #

df_hist <- df_precios %>%
  filter(!is.na(PRECIO_MILES)) %>%
  group_by(ESPECIE_NOMBRE) %>%
  mutate(
    Media = mean(PRECIO_MILES),
    Mediana = median(PRECIO_MILES)
  ) %>%
  ungroup()

p1 <- ggplot(df_hist, aes(x = PRECIO_MILES, fill = ESPECIE_NOMBRE)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.9) +
  
  # Líneas de media y mediana
  geom_vline(aes(xintercept = Media), 
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = Mediana), 
             color = "darkgreen", linetype = "dotted", linewidth = 1) +
  
  # Etiquetas
  geom_label(
    data = df_hist %>% 
      group_by(ESPECIE_NOMBRE) %>% 
      summarise(Media = first(Media), 
                Mediana = first(Mediana),
                .groups = "drop"),
    aes(x = Inf, y = Inf, 
        label = paste0("Media: $", format(round(Media), big.mark = ","),
                       "\nMediana: $", format(round(Mediana), big.mark = ","))),
    hjust = 1.1, vjust = 1.5, size = 3.5,
    label.size = 0, fill = "white", alpha = 0.8
  ) +
  
  scale_x_continuous(
    labels = label_dollar(prefix = "$", suffix = "k", scale = 1)
  ) +
  scale_fill_manual(values = colores_especies) +
  facet_wrap(~ ESPECIE_NOMBRE, scales = "free", ncol = 3) +
  
  labs(
    title = "Distribución de Precios por Especie",
    x = "Precio (CLP/ton)",
    y = "Frecuencia"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA),
    panel.grid.minor = element_blank()
  )
print(p1)
ggsave("distribucion_precios.png", p1, width = 14, height = 5, dpi = 300, bg = "white")
cat("✓ Guardado: distribucion_precios.png\n")

# ============================================================================ #
# 4. GRÁFICO 2: ESTACIONALIDAD
# ============================================================================ #

cat("\n=== GENERANDO GRÁFICO 2: Estacionalidad Mensual ===\n")

# Calcular estadísticos mensuales por especie
df_estacional <- df_precios %>%
  filter(!is.na(PRECIO_MILES)) %>%
  group_by(ESPECIE_NOMBRE, MES) %>%
  summarise(
    Media = mean(PRECIO_MILES, na.rm = TRUE),
    Mediana = median(PRECIO_MILES, na.rm = TRUE),
    Q25 = quantile(PRECIO_MILES, 0.25, na.rm = TRUE),
    Q75 = quantile(PRECIO_MILES, 0.75, na.rm = TRUE),
    Min = min(PRECIO_MILES, na.rm = TRUE),
    Max = max(PRECIO_MILES, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  )


p2 <- ggplot(df_estacional, aes(x = factor(MES))) +
  geom_col(aes(y = Media, fill = ESPECIE_NOMBRE), 
           alpha = 0.7, color = "white") +
  geom_errorbar(aes(ymin = Q25, ymax = Q75, color = ESPECIE_NOMBRE),
                width = 0.3, linewidth = 0.8) +
  
  scale_y_continuous(
    labels = label_dollar(prefix = "$", suffix = "k", scale = 1)
  ) +
  scale_fill_manual(values = colores_especies) +
  scale_color_manual(values = colores_especies) +
  facet_wrap(~ ESPECIE_NOMBRE, scales = "free_y", ncol = 3) +
  
  labs(
    title = "Estacionalidad de Precios por Mes",
    x = "Mes",
    y = "Precio Promedio"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA)
  )
print(p2)
ggsave("estacionalidad_precios.png", p2, width = 14, height = 5, dpi = 300, bg = "white")
cat("✓ Guardado: estacionalidad_precios.png\n")

# ============================================================================ #
# 5. GRÁFICO 3: EVOLUCIÓN TEMPORAL
# ============================================================================ #

df_evolucion <- df_precios %>%
  filter(!is.na(PRECIO_MILES)) %>%
  arrange(ESPECIE_NOMBRE, FECHA) %>%
  group_by(ESPECIE_NOMBRE) %>%
  mutate(
    Media_Global = mean(PRECIO_MILES, na.rm = TRUE),
    Tendencia = predict(loess(PRECIO_MILES ~ as.numeric(FECHA), span = 0.3)),
    Limite_Sup = Media_Global + sd(PRECIO_MILES, na.rm = TRUE),
    Limite_Inf = pmax(0, Media_Global - sd(PRECIO_MILES, na.rm = TRUE)),
    N_Total = n()
  ) %>%
  ungroup()

p3 <- ggplot(df_evolucion, aes(x = FECHA, y = PRECIO_MILES, color = ESPECIE_NOMBRE)) +
  # Límites
  geom_ribbon(aes(ymin = Limite_Inf, ymax = Limite_Sup, fill = ESPECIE_NOMBRE),
              alpha = 0.15, color = NA) +
  geom_hline(aes(yintercept = Media_Global), 
             linetype = "dotted", color = "black", linewidth = 0.7) +
  
  # Serie
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_point(size = 1.8, alpha = 0.7) +
  
  # Tendencia
  geom_line(aes(y = Tendencia), linetype = "dashed", linewidth = 1.2) +
  
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "k")) +
  scale_color_manual(values = colores_especies) +
  scale_fill_manual(values = colores_especies) +
  
  facet_wrap(~ paste0(ESPECIE_NOMBRE, " (N=", N_Total, ")"),
             scales = "free_y", ncol = 1) +
  
  labs(
    title = "Evolución de Precios Ex-Vessel (2012-2024)",
    x = "Año",
    y = "Precio (miles CLP/ton)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 11, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA)
  )
print(p3)
ggsave("evolucion_precios.png", p3, width = 12, height = 10, dpi = 300, bg = "white")
cat("✓ Guardado: evolucion_precios.png\n")

# ============================================================================ #
# 6. GRÁFICO 4: SCATTER PRECIO vs CANTIDAD
# ============================================================================ #

df_scatter <- df_precios %>%
  filter(!is.na(PRECIO_MILES) & !is.na(Q_MILES_TON)) %>%
  group_by(ESPECIE_NOMBRE) %>%
  mutate(Corr = cor(PRECIO_MILES, Q_MILES_TON, use = "complete.obs")) %>%
  ungroup()

correlaciones <- df_scatter %>%
  group_by(ESPECIE_NOMBRE) %>%
  summarise(Corr = first(Corr), .groups = "drop")

p4 <- ggplot(df_scatter, aes(x = Q_MILES_TON, y = PRECIO_MILES, color = ESPECIE_NOMBRE)) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.2) +
  
  # Correlación
  geom_label(
    data = correlaciones,
    aes(x = -Inf, y = Inf, 
        label = paste0("Corr: ", 
                       ifelse(Corr >= 0, "+", ""),
                       sprintf("%.3f", Corr))),
    hjust = -0.05, vjust = 1.2, size = 4.5, fontface = "bold",
    label.size = 0.5, fill = "white", alpha = 0.9,
    show.legend = FALSE
  ) +
  
  scale_x_continuous(labels = label_number(big.mark = ",")) +
  scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "k")) +
  scale_color_manual(values = colores_especies) +
  
  facet_wrap(~ ESPECIE_NOMBRE, scales = "free", ncol = 3) +
  
  labs(
    title = "Relación Precio vs Cantidad por Especie",
    x = "Cantidad (miles ton)",
    y = "Precio (miles CLP/ton)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA)
  )
print(p4)
ggsave("scatter_precio_cantidad.png", p4, width = 14, height = 5, dpi = 300, bg = "white")
cat("✓ Guardado: scatter_precio_cantidad.png\n")

