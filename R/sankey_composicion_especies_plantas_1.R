# =============================================================================
# Sankey: Composición de especies procesadas por planta
# Transiciones entre 2012–2018 y 2019–2024
# Pesquería pelágica centro-sur de Chile
# Fuente: Encuesta de manufactura IFOP (hoja PROCESO)
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("ggalluvial")
library(ggalluvial)
library(stringr)
library(forcats)

# --- 1. Cargar y filtrar datos -----------------------------------------------

proc <-read_excel("2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx",
  sheet = "PROCESO"
)

# Regiones centro-sur y especies pelágicas
cs_regiones <- c(5, 6, 7, 8, 9, 10, 14, 16)
pelagicos   <- c("ANCHOVETA", "SARDINA COMUN", "JUREL")

df <- proc %>%
  filter(
    NM_UNIDAD == "PLANTA",
    RG %in% cs_regiones,
    trimws(NM_RECURSO) %in% pelagicos
  ) %>%
  mutate(
    NM_RECURSO = trimws(NM_RECURSO),
    PERIODO = ifelse(ANIO <= 2018, "2012–2018", "2019–2024")
  )

# --- 2. Determinar composición de especies por planta-período ----------------

# Para cada planta en cada período, ¿qué especies procesó?
plant_species <- df %>%
  group_by(NUI, PERIODO) %>%
  summarise(especies = list(sort(unique(NM_RECURSO))), .groups = "drop") %>%
  mutate(
    composicion = sapply(especies, function(sp) {
      sp_set <- sort(sp)
      if (identical(sp_set, c("ANCHOVETA", "JUREL", "SARDINA COMUN"))) {
        "Sardina, jurel\ny anchoveta"
      } else if (identical(sp_set, c("JUREL", "SARDINA COMUN"))) {
        "Sardina\ny jurel"
      } else if (identical(sp_set, c("ANCHOVETA", "SARDINA COMUN"))) {
        "Sardina\ny anchoveta"
      } else if (identical(sp_set, c("ANCHOVETA", "JUREL"))) {
        "Jurel\ny anchoveta"
      } else if (identical(sp_set, "JUREL")) {
        "Solo jurel"
      } else if (identical(sp_set, "SARDINA COMUN")) {
        "Solo sardina"
      } else if (identical(sp_set, "ANCHOVETA")) {
        "Solo anchoveta"
      } else {
        "Otro"
      }
    })
  )

# --- 3. Construir transiciones -----------------------------------------------

p1 <- plant_species %>%
  filter(PERIODO == "2012–2018") %>%
  select(NUI, COMP_1 = composicion)

p2 <- plant_species %>%
  filter(PERIODO == "2019–2024") %>%
  select(NUI, COMP_2 = composicion)

# Plantas en ambos períodos
ambos <- inner_join(p1, p2, by = "NUI")

# Plantas que salen (solo en período 1)
solo_p1 <- p1 %>%
  anti_join(p2, by = "NUI") %>%
  mutate(COMP_2 = "Sale del\npanel")

# Plantas que entran (solo en período 2)
solo_p2 <- p2 %>%
  anti_join(p1, by = "NUI") %>%
  mutate(COMP_1 = "Entra al\npanel")

transiciones <- bind_rows(
  ambos %>% select(COMP_1, COMP_2),
  solo_p1 %>% select(COMP_1, COMP_2),
  solo_p2 %>% select(COMP_1, COMP_2)
)

# Agregar conteos
flujos <- transiciones %>%
  group_by(COMP_1, COMP_2) %>%
  summarise(n = n(), .groups = "drop")

# --- 4. Preparar datos para ggalluvial ---------------------------------------

# Orden de categorías (de arriba a abajo)
orden_cats <- c(
  "Solo anchoveta",
  "Solo jurel",
  "Jurel\ny anchoveta",
  "Entra al\npanel",
  "Solo sardina",
  "Sardina\ny jurel",
  "Sardina\ny anchoveta",
  "Sardina, jurel\ny anchoveta",
  "Sale del\npanel"
)

# Solo mantener las que existen
cats_p1 <- unique(flujos$COMP_1)
cats_p2 <- unique(flujos$COMP_2)
orden_p1 <- orden_cats[orden_cats %in% cats_p1]
orden_p2 <- orden_cats[orden_cats %in% cats_p2]

flujos <- flujos %>%
  mutate(
    COMP_1 = factor(COMP_1, levels = orden_p1),
    COMP_2 = factor(COMP_2, levels = orden_p2)
  )

# --- 5. Colores --------------------------------------------------------------

colores <- c(
  "Sardina, jurel\ny anchoveta" = "#E8B952",
  "Solo jurel"                  = "#8BC78B",
  "Sardina\ny jurel"            = "#E8A0C0",
  "Sardina\ny anchoveta"        = "#D4A574",
  "Solo sardina"                = "#F2D166",
  "Solo anchoveta"              = "#7EC4CF",
  "Jurel\ny anchoveta"          = "#A8D5A2",
  "Entra al\npanel"             = "#C8C8C0",
  "Sale del\npanel"             = "#C8C8C0",
  "Otro"                        = "#999999"
)

# --- 6. Gráfico Sankey (alluvial) --------------------------------------------

p <- ggplot(flujos,
       aes(y = n, axis1 = COMP_1, axis2 = COMP_2)) +
  geom_alluvium(
    aes(fill = COMP_1),
    width = 1/6,
    alpha = 0.55,
    curve_type = "sigmoid"
  ) +
  geom_stratum(
    aes(fill = after_stat(stratum)),
    width = 1/6,
    color = "white",
    linewidth = 0.3
  ) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3,
    lineheight = 0.85,
    fontface = "plain"
  ) +
  scale_fill_manual(
    values = colores,
    guide = "none"
  ) +
  scale_x_discrete(
    limits = c("2012–2018", "2019–2024"),
    expand = c(0.15, 0.05),
    position = "bottom"
  ) +
  labs(
    title    = "Composición de especies procesadas por planta",
    subtitle = "Transiciones entre períodos — Pesquería pelágica centro-sur de Chile",
    caption  = "Fuente: Encuesta de manufactura IFOP (hoja PROCESO), 2012–2024. Elaboración propia.",
    y = "Número de plantas"
  ) +
  theme_minimal(base_family = "sans", base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle    = element_text(color = "grey45", size = 11, margin = margin(b = 16)),
    plot.caption     = element_text(color = "grey55", size = 8, hjust = 0, margin = margin(t = 12)),
    axis.text.x      = element_text(face = "bold", size = 12, color = "grey30"),
    axis.text.y      = element_blank(),
    axis.title       = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    plot.background  = element_rect(fill = "#fafaf7", color = NA),
    panel.background = element_rect(fill = "#fafaf7", color = NA),
    plot.margin      = margin(20, 20, 20, 20)
  )

# --- 7. Guardar --------------------------------------------------------------

ggsave(
  "sankey_composicion_especies_plantas.png",
  plot = p,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "#fafaf7"
)

ggsave(
  "sankey_composicion_especies_plantas.pdf",
  plot = p,
  width = 10,
  height = 7,
  bg = "#fafaf7"
)

cat("Guardado: sankey_composicion_especies_plantas.png / .pdf\n")

# --- 8. Tabla resumen (opcional) ---------------------------------------------

cat("\n--- Resumen de transiciones ---\n")
flujos %>%
  arrange(desc(n)) %>%
  mutate(
    COMP_1 = str_replace_all(COMP_1, "\n", " "),
    COMP_2 = str_replace_all(COMP_2, "\n", " ")
  ) %>%
  print(n = 30)
