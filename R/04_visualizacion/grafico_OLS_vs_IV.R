library(ggplot2)

# Datos
datos_comparacion <- data.frame(
  Estimador = c("OLS\n(sin instrumentar)", "IV-2SLS\n(instrumentado)"),
  gamma = c(-0.04, -0.41),
  SE = c(0.016, 0.156)
)

# Gráfico
ggplot(datos_comparacion, aes(x = Estimador, y = gamma, fill = Estimador)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = gamma - 1.96*SE, ymax = gamma + 1.96*SE), 
                width = 0.2, linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = sprintf("%.2f", gamma)), 
            vjust = ifelse(datos_comparacion$gamma < -0.2, -0.5, 1.5),
            fontface = "bold", size = 5) +
  scale_fill_manual(values = c("#888888", "#2166AC")) +
  scale_y_continuous(limits = c(-0.8, 0.1), breaks = seq(-0.8, 0, 0.2)) +
  labs(
    title = "Sesgo de atenuación: OLS vs IV-2SLS",
    subtitle = "El coeficiente OLS subestima el efecto en ~10×",
    x = NULL,
    y = expression(paste("Elasticidad precio-cantidad (", gamma, ")"))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 12)
  )

# Guardar
ggsave(here::here("outputs", "figures", "figura_ols_vs_iv.png"), width = 7, height = 5, dpi = 300)
