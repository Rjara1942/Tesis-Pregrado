################################################################################
# MAPAS DE CHILE - VERSIÓN CORREGIDA PARA DATOS REALES
################################################################################

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(patchwork)
library(scales)

# ============================================================================ #
# 1. CARGAR DATOS
# ============================================================================ #

cat("=== CARGANDO DATOS ===\n")

df_env <- data_ambiental_por_puerto_15360263371915282388
print(unique(df_env$Puerto))

# ============================================================================ #
# 2. MAPEO AUTOMÁTICO DE PUERTOS A COORDENADAS
# ============================================================================ #

# Función para limpiar nombres de puertos
limpiar_nombre <- function(nombre) {
  nombre %>%
    str_trim() %>%
    str_to_upper() %>%
    str_replace_all("\\(.*\\)", "") %>%  # Quitar paréntesis
    str_trim()
}

# Crear tabla de coordenadas con múltiples variantes de nombres
puertos_coords <- tribble(
  ~Puerto_original,         ~Puerto_clean,      ~lat,      ~lon,     ~region,
  "San Antonio",            "SAN ANTONIO",      -33.593,   -71.610,  "Valparaíso",
  "Talcahuano",             "TALCAHUANO",       -36.725,   -73.116,  "Biobío",
  "Talcahuano (San Vicente)", "TALCAHUANO",     -36.725,   -73.116,  "Biobío",
  "Coronel",                "CORONEL",          -37.033,   -73.150,  "Biobío",
  "Lota",                   "LOTA",             -37.089,   -73.158,  "Biobío",
  "Corral",                 "CORRAL",           -39.887,   -73.429,  "Los Ríos",
  "Calbuco",                "CALBUCO",          -41.774,   -73.128,  "Los Lagos",
  "Puerto Montt",           "PUERTO MONTT",     -41.469,   -72.942,  "Los Lagos",
  "Region 7 (puerto)",      "REGION 7",         -35.500,   -72.500,  "Maule",
  "Region 9 (puerto)",      "REGION 9",         -38.500,   -73.000,  "Araucanía"
)

# ============================================================================ #
# 3. CALCULAR ESTADÍSTICAS POR PUERTO
# ============================================================================ #

cat("\n=== CALCULANDO ESTADÍSTICAS ===\n")

df_summary <- df_env %>%
  mutate(year = year(date)) %>%
  filter(year >= 2012 & year <= 2024) %>%
  group_by(Puerto) %>%
  summarise(
    SST_mean = mean(sst_0_60km, na.rm = TRUE),
    SST_sd = sd(sst_0_60km, na.rm = TRUE),
    CHL_mean = mean(chl_0_60km, na.rm = TRUE),
    CHL_sd = sd(chl_0_60km, na.rm = TRUE),
    WIND_mean = mean(speed_mean_0_60km, na.rm = TRUE),
    WIND_sd = sd(speed_mean_0_60km, na.rm = TRUE),
    SO_mean = mean(so_0_60km, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  # Hacer el join por nombre limpio
  mutate(Puerto_clean = limpiar_nombre(Puerto)) %>%
  left_join(
    puertos_coords %>% select(Puerto_clean, lat, lon, region) %>% distinct(),
    by = "Puerto_clean"
  )

# Verificar que todos los puertos tienen coordenadas
cat("\n✓ Puertos sin coordenadas (si hay):\n")
df_summary %>% filter(is.na(lat)) %>% select(Puerto) %>% print()

# Mostrar tabla final
cat("\n✓ TABLA DE ESTADÍSTICAS:\n")
print(df_summary %>% select(Puerto, region, SST_mean, CHL_mean, WIND_mean, n_obs))

# ============================================================================ #
# 4. CREAR ETIQUETAS OPTIMIZADAS
# ============================================================================ #

# Definir offsets de etiquetas para evitar sobreposición
df_summary <- df_summary %>%
  mutate(
    # Nombres cortos para display
    Puerto_short = case_when(
      str_detect(Puerto, "Region 7") ~ "R7",
      str_detect(Puerto, "Region 9") ~ "R9",
      str_detect(Puerto, "Talcahuano") ~ "Talcahuano",
      TRUE ~ Puerto
    ),
    # Offsets para etiquetas (ajustar según posición)
    label_offset_x = case_when(
      lat < -40 ~ 0.7,              # Sur: etiquetas a la derecha
      lon < -73 ~ -0.9,             # Oeste: etiquetas a la izquierda
      TRUE ~ 0.7                     # Resto: derecha
    ),
    label_offset_y = case_when(
      Puerto == "Puerto Montt" ~ -0.3,
      Puerto == "Calbuco" ~ 0.3,
      TRUE ~ 0
    )
  )

# ============================================================================ #
# 5. PREPARAR OBJETOS ESPACIALES
# ============================================================================ #

chile <- ne_countries(scale = "medium", country = "Chile", returnclass = "sf")

puertos_sf <- df_summary %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# ============================================================================ #
# 6. FUNCIÓN PARA CREAR MAPAS
# ============================================================================ #

crear_mapa <- function(variable, titulo, unidad, paleta = "viridis",
                       breaks_custom = waiver()) {
  
  coords <- st_coordinates(puertos_sf)
  df_labels <- puertos_sf %>%
    st_drop_geometry() %>%
    bind_cols(as.data.frame(coords)) %>%
    mutate(
      label_x = X + label_offset_x,
      label_y = Y + label_offset_y
    )
  
  p <- ggplot() +
    geom_sf(data = chile, fill = "gray95", color = "gray40", linewidth = 0.3) +
    
    geom_sf(data = puertos_sf, 
            aes(size = .data[[variable]], 
                color = .data[[variable]]),
            alpha = 0.85) +
    
    geom_label(data = df_labels,
               aes(x = label_x, y = label_y, label = Puerto_short),
               size = 2.8, fontface = "bold",
               label.padding = unit(0.15, "lines"),
               label.size = 0.2,
               fill = alpha("white", 0.85)) +
    
    scale_color_viridis_c(
      option = paleta, 
      name = unidad,
      direction = -1,
      breaks = breaks_custom
    ) +
    scale_size_continuous(range = c(4, 16), guide = "none") +
    
    coord_sf(xlim = c(-74.5, -70.5), ylim = c(-42.5, -33)) +
    
    labs(
      title = titulo,
      subtitle = "Promedio 2012-2024 (radio 0-60 km)",
      x = NULL,
      y = NULL
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5),
      legend.position = "right",
      panel.grid = element_line(color = "gray90", linewidth = 0.25),
      panel.background = element_rect(fill = "aliceblue", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  return(p)
}

# ============================================================================ #
# 7. GENERAR MAPAS
# ============================================================================ #

cat("\n=== GENERANDO MAPAS ===\n")

p_sst <- crear_mapa(
  "SST_mean",
  "Temperatura Superficial del Mar (SST)",
  "°C",
  "plasma",
  breaks_custom = seq(12, 15, 0.5)
)
print(p_sst)

p_chl <- crear_mapa(
  "CHL_mean",
  "Concentración de Clorofila-a",
  "mg/m³",
  "viridis",
  breaks_custom = c(1, 2, 3, 4, 5, 6)
)

p_wind <- crear_mapa(
  "WIND_mean",
  "Velocidad del Viento",
  "m/s",
  "cividis",
  breaks_custom = seq(6.5, 6.8, 0.1)
)

p_so <- crear_mapa(
  "SO_mean",
  "Salinidad Superficial",
  "PSU",
  "mako",
  breaks_custom = seq(31, 34, 0.5)
)

# Guardar
ggsave("mapa_SST_real.png", p_sst, width = 10, height = 12, dpi = 300, bg = "white")
ggsave("mapa_clorofila_real.png", p_chl, width = 10, height = 12, dpi = 300, bg = "white")
ggsave("mapa_viento_real.png", p_wind, width = 10, height = 12, dpi = 300, bg = "white")
ggsave("mapa_salinidad_real.png", p_so, width = 10, height = 12, dpi = 300, bg = "white")

cat("✓ Mapas individuales guardados\n")

# ============================================================================ #
# 8. PANEL COMPLETO
# ============================================================================ #
panel <- (p_sst | p_chl) / (p_wind | p_so) +
  plot_annotation(
    title = "Variables Ambientales - Puertos Pesqueros Centro-Sur Chile",
    subtitle = "Datos satelitales 2012-2024 | Copernicus Marine Service",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5)
    )
  )

ggsave("mapa_panel_completo_real.png", panel,
       width = 22, height = 22, dpi = 300, bg = "white")
cat("✓ Panel completo guardado\n")





