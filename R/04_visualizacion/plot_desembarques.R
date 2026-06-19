library(tidyverse)
library(scales) 

df <- read_csv(here::here("data", "base_integrada3_IV.csv"), show_col_types = FALSE)

estacionalidad <- df %>%
  filter(!is.na(Q_MACRO)) %>%
  filter(NM_RECURSO %in% c("ANCHOVETA", "JUREL", "SARDINA COMUN")) %>%
  group_by(NM_RECURSO, MES) %>%
  summarise(Promedio_Q = mean(Q_MACRO, na.rm = TRUE), .groups = "drop") %>%
  # Convertir el MES numérico a factor con etiquetas de meses para el gráfico
  mutate(MES_FACTOR = factor(MES, 
                             levels = 1:12, 
                             labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                        "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")))

g_estacionalidad <- ggplot(estacionalidad, aes(x = MES_FACTOR, y = Promedio_Q, fill = NM_RECURSO)) +
  geom_col(alpha = 0.85) + 
  facet_wrap(~ NM_RECURSO, scales = "free_y", ncol = 1) + 
  scale_fill_manual(values = c("ANCHOVETA" = "#2ca02c", 
                               "JUREL" = "#1f77b4", 
                               "SARDINA COMUN" = "#ff7f0e")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Estacionalidad promedio de desembarques por especie, 2012–2024",
    x = "Mes del año",
    y = "Desembarque Promedio (Toneladas)",
    caption = "Nota: Las escalas del eje Y son independientes para cada especie, reflejando las diferencias de magnitud\nen los volúmenes capturados.\nFuente: Elaboración propia con datos IFOP."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # Ocultar leyenda, los títulos de los paneles ya indican la especie
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, color = "gray30", size = 9, margin = margin(t = 15)),
    strip.text = element_text(face = "bold", size = 11, color = "black"), # Títulos de cada cuadro
    panel.grid.major.x = element_blank(), # Limpiar líneas verticales del fondo
    panel.grid.minor.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )

# 4. Mostrar y guardar el gráfico en alta resolución
print(g_estacionalidad)
ggsave(here::here("outputs", "figures", "estacionalidad_desembarques.png"), plot = g_estacionalidad, width = 10, height = 8, dpi = 300)
