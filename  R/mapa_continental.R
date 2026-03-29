# ==============================================================================
# SCRIPT MAESTRO REVISADO: MAPEO SIN DATOS ORIGINALES DE GRILLA
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. LIMPIEZA DEL ENTORNO
# ------------------------------------------------------------------------------
rm(list = ls())
gc()

# ------------------------------------------------------------------------------
# 1. LIBRERÍAS
# ------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(tibble)
install.packages("ggrepel")
library(ggrepel)

# ------------------------------------------------------------------------------
# 2. DEFINICIÓN DE COORDENADAS DE PUERTOS
# ------------------------------------------------------------------------------
puertos_coords <- tribble(
  ~Puerto_original,           ~Puerto_clean,      ~lat,      ~lon,     ~region,
  "San Antonio",              "SAN ANTONIO",      -33.593,   -71.610,  "Valparaíso",
  "Talcahuano",               "TALCAHUANO",       -36.725,   -73.116,  "Biobío",
  "Talcahuano (San Vicente)", "TALCAHUANO",       -36.725,   -73.116,  "Biobío",
  "Coronel",                  "CORONEL",          -37.033,   -73.150,  "Biobío",
  "Lota",                     "LOTA",             -37.089,   -73.158,  "Biobío",
  "Corral",                   "CORRAL",           -39.887,   -73.429,  "Los Ríos",
  "Calbuco",                  "CALBUCO",          -41.774,   -73.128,  "Los Lagos",
  "Puerto Montt",             "PUERTO MONTT",     -41.469,   -72.942,  "Los Lagos",
  "Region 7 (puerto)",        "CONSTITUCION",     -35.333,   -72.416,  "Maule",
  "Region 9 (puerto)",        "QUEULE",           -39.388,   -73.203,  "Araucanía"
)

puertos_unicos <- puertos_coords %>%
  distinct(Puerto_clean, lat, lon)

# ------------------------------------------------------------------------------
# 3. CREACIÓN DE GEOMETRÍAS BASE (Continente y Mar)
# ------------------------------------------------------------------------------
# Generar Chile continental
chile_mainland <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin == "Chile") %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  (\(x) x[which.max(st_area(x))])() %>%
  st_transform(32719) # Transformar a UTM 19S (metros)

# Generar Sudamérica (para fondo y para restar tierra)
south_america <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent == "South America") %>%
  st_union() %>%
  st_transform(32719)

# Crear Buffer de 200 millas náuticas y aislar solo el mar
nm200 <- 200 * 1852   # metros
chile_buffer <- st_buffer(chile_mainland, dist = nm200)
sea_area <- st_difference(chile_buffer, south_america)

# ------------------------------------------------------------------------------
# 4. CREACIÓN MATEMÁTICA DE LA GRILLA DE 0.125 GRADOS (¡NUEVO!)
# ------------------------------------------------------------------------------
# Definimos una "caja" que cubra toda la costa oeste
bbox <- st_bbox(c(xmin = -85, xmax = -68, ymin = -45, ymax = -25), crs = 4326)

# Fabricamos la grilla espaciada cada 0.125 grados
grid_geom <- st_make_grid(bbox, cellsize = c(0.125, 0.125), what = "centers")

# Convertimos a formato sf y proyectamos a UTM 19S
grid_sf <- st_sf(geometry = grid_geom) %>% 
  st_transform(32719)

# Filtrar grilla para que solo queden puntos dentro de nuestra zona de mar (200 nm)
inside_sea <- st_intersects(grid_sf, sea_area, sparse = FALSE)[,1]
grid_filtered <- grid_sf[inside_sea, ]

# Calcular distancias exactas a la costa
grid_filtered$dist2coast_m  <- as.numeric(st_distance(grid_filtered, chile_mainland))
grid_filtered$dist2coast_km <- grid_filtered$dist2coast_m / 1000

# ------------------------------------------------------------------------------
# 5. PREPARACIÓN DE CAPAS PARA EL MAPA (Transformar a lat/lon EPSG:4326)
# ------------------------------------------------------------------------------
sea_plot           <- st_transform(sea_area, 4326)
grid_plot          <- st_transform(grid_filtered, 4326)
south_america_plot <- st_transform(south_america, 4326)

# ------------------------------------------------------------------------------
# 6. CREACIÓN DEL MAPA FINAL CON PUERTOS
# ------------------------------------------------------------------------------
mapa_completo <- ggplot() +
  
  geom_sf(data = grid_plot, aes(color = dist2coast_km), size = 1.6, shape = 16) +
  geom_sf(data = sea_plot, fill = NA, color = "blue", linewidth = 0.3) +
  geom_sf(data = south_america_plot, fill = "#8c8c8c", color = "black", linewidth = 0.4) +
  
  geom_point(
    data = puertos_unicos, aes(x = lon, y = lat),
    color = "red", fill = "white", shape = 21, size = 3.5, stroke = 1.2
  ) +
  
  geom_text_repel(
    data = puertos_unicos, aes(x = lon, y = lat, label = Puerto_clean),
    size = 4.5, fontface = "bold", color = "black", bg.color = "white", bg.r = 0.15,
    nudge_x = -1.2, direction = "y", hjust = 1, segment.color = "black", segment.size = 0.6
  ) +
  
  scale_color_viridis_c(
    name = "Distancia costa (km)", option = "plasma", direction = -1,
    breaks = c(100, 200, 300), limits = c(0, 370),
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0, barwidth = 1.8, barheight = 10,
      ticks.colour = "white", frame.colour = NA
    )
  ) +
  
  coord_sf(xlim = c(-82.5, -69.5), ylim = c(-42.6, -29.4), expand = FALSE) +
  scale_x_continuous(breaks = seq(-82, -70, by = 2), labels = function(x) paste0(abs(x), "°W")) +
  scale_y_continuous(breaks = seq(-42, -30, by = 2), labels = function(y) paste0(abs(y), "°S")) +
  
  labs(
    title = "Zona marítima chilena e infraestructura portuaria",
    subtitle = "Grid ambiental 0.125° (≤ 200 nm) frente a la costa continental",
    x = "Longitud", y = "Latitud"
  ) +
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#e8f1f5", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.6),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 16, color = "black", hjust = 0, margin = margin(b = 15)),
    axis.title = element_text(size = 18, face = "plain", color = "black"),
    axis.text = element_text(size = 14, color = "gray20"),
    legend.title = element_text(size = 16, face = "plain", color = "black"),
    legend.text = element_text(size = 14),
    plot.margin = margin(15, 15, 15, 15)
  )

print(mapa_completo)

# Guarda el mapa en alta resolución (300 dpi) en tu directorio de trabajo
ggsave(
  filename = "mapa_puertos_zee.png",
  plot = mapa_completo,
  width = 10,       # Ancho en pulgadas
  height = 12,      # Alto en pulgadas
  dpi = 300,        # Calidad óptima para documentos Word/PDF
  bg = "white"      # Asegura que el borde exterior sea blanco
)