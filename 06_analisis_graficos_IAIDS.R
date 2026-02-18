################################################################################
# 06_ANALISIS_GRAFICOS_IAIDS.R
# Objetivo: Visualización y análisis de resultados IAIDS
################################################################################

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(scales)

# ==============================================================================
# 1. CARGAR RESULTADOS
# ==============================================================================

df_pred <- read_csv("base_con_predicciones_IAIDS.csv", show_col_types = FALSE)
coefs <- read_csv("coeficientes_IAIDS.csv", show_col_types = FALSE)
gamma1 <- read.csv("flexibilidades_etapa1.csv", row.names = 1)
gamma2 <- read.csv("flexibilidades_etapa2.csv", row.names = 1)

# ==============================================================================
# 2. GRÁFICO 1: PRECIOS OBSERVADOS VS PREDICHOS
# ==============================================================================

# Transformar a formato largo para ggplot
df_plot <- df_pred %>%
  select(FECHA, 
         # Observados
         P_obs_anch = PRECIO_REAL_MACRO_ANCHOVETA,
         P_obs_jur = PRECIO_REAL_MACRO_JUREL,
         P_obs_sard = `PRECIO_REAL_MACRO_SARDINA COMUN`,
         # Predichos
         P_pred_anch = pred_P_ANCHOVETA,
         P_pred_jur = pred_P_JUREL,
         P_pred_sard = pred_P_SARDINA) %>%
  pivot_longer(
    cols = -FECHA,
    names_to = c(".value", "especie"),
    names_pattern = "P_(obs|pred)_(.*)"
  ) %>%
  mutate(
    especie = case_when(
      especie == "anch" ~ "Anchoveta",
      especie == "jur" ~ "Jurel",
      especie == "sard" ~ "Sardina"
    )
  )

g1 <- ggplot(df_plot, aes(x = FECHA)) +
  geom_line(aes(y = obs, color = "Observado"), size = 1) +
  geom_line(aes(y = pred, color = "Predicho"), size = 1, linetype = "dashed") +
  facet_wrap(~ especie, scales = "free_y", ncol = 1) +
  scale_color_manual(
    values = c("Observado" = "black", "Predicho" = "red"),
    name = ""
  ) +
  labs(
    title = "Precios Ex-Vessel: Observados vs Predichos (IAIDS Etapa 2)",
    subtitle = "Precios reales (pesos 2024)",
    x = "Fecha",
    y = "Precio ($/ton)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )

print(g1)
ggsave("iaids_precios_obs_vs_pred.png", g1, width = 12, height = 8, dpi = 300)

# ==============================================================================
# 3. GRÁFICO 2: RESIDUOS
# ==============================================================================

df_resid <- df_pred %>%
  select(FECHA,
         resid_anch = resid_P_ANCHOVETA,
         resid_jur = resid_P_JUREL,
         resid_sard = resid_P_SARDINA) %>%
  pivot_longer(
    cols = -FECHA,
    names_to = "especie",
    values_to = "residuo",
    names_prefix = "resid_"
  ) %>%
  mutate(
    especie = case_when(
      especie == "anch" ~ "Anchoveta",
      especie == "jur" ~ "Jurel",
      especie == "sard" ~ "Sardina"
    )
  )

g2 <- ggplot(df_resid, aes(x = FECHA, y = residuo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(aes(color = especie), size = 0.8) +
  geom_point(aes(color = especie), size = 1.5, alpha = 0.6) +
  facet_wrap(~ especie, scales = "free_y", ncol = 1) +
  labs(
    title = "Residuos del Modelo IAIDS",
    subtitle = "Diferencia entre precio observado y predicho",
    x = "Fecha",
    y = "Residuo ($/ton)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold")
  )

print(g2)
ggsave("iaids_residuos.png", g2, width = 12, height = 8, dpi = 300)

# ==============================================================================
# 4. GRÁFICO 3: MATRIZ DE FLEXIBILIDADES (HEATMAP)
# ==============================================================================

# Convertir matriz a formato largo
gamma2_long <- gamma2 %>%
  rownames_to_column("Precio") %>%
  pivot_longer(
    cols = -Precio,
    names_to = "Cantidad",
    values_to = "Flexibilidad"
  ) %>%
  mutate(
    Precio = factor(Precio, levels = c("Anchoveta", "Jurel", "Sardina")),
    Cantidad = recode(Cantidad,
                      "ln_Q_ANCH" = "Q_Anchoveta",
                      "ln_Q_JUR" = "Q_Jurel",
                      "ln_Q_SARD" = "Q_Sardina")
  )

g3 <- ggplot(gamma2_long, aes(x = Cantidad, y = Precio, fill = Flexibilidad)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Flexibilidad)), 
            color = "black", size = 5, fontface = "bold") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "γ_ij"
  ) +
  labs(
    title = "Matriz de Flexibilidades de Precio (IAIDS Etapa 2)",
    subtitle = "γ_ij = Elasticidad de P_i respecto a Q_j (controlando por precio internacional)",
    x = "Cambio en Cantidad (ln Q_j)",
    y = "Efecto sobre Precio (ln P_i)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

print(g3)
ggsave("iaids_matriz_flexibilidades.png", g3, width = 10, height = 6, dpi = 300)

# ==============================================================================
# 5. GRÁFICO 4: COMPARACIÓN ETAPA 1 vs ETAPA 2
# ==============================================================================

# Combinar matrices
gamma_comparacion <- bind_rows(
  gamma1 %>% 
    rownames_to_column("Precio") %>%
    pivot_longer(-Precio, names_to = "Cantidad", values_to = "Flexibilidad") %>%
    mutate(Modelo = "Etapa 1 (sin P_harina)"),
  
  gamma2 %>% 
    rownames_to_column("Precio") %>%
    pivot_longer(-Precio, names_to = "Cantidad", values_to = "Flexibilidad") %>%
    mutate(Modelo = "Etapa 2 (con P_harina)")
) %>%
  mutate(
    Interaccion = paste0(Precio, " - ", Cantidad)
  )

g4 <- ggplot(gamma_comparacion, aes(x = Interaccion, y = Flexibilidad, fill = Modelo)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Comparación: Flexibilidades Etapa 1 vs Etapa 2",
    subtitle = "¿Cómo cambian los coeficientes al controlar por precio internacional?",
    x = "",
    y = "Flexibilidad (γ_ij)",
    fill = "Modelo"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9)
  )

print(g4)
ggsave("iaids_comparacion_etapas.png", g4, width = 10, height = 8, dpi = 300)

# ==============================================================================
# 6. GRÁFICO 5: EFECTOS DEL PRECIO INTERNACIONAL
# ==============================================================================

# Extraer coeficientes de P_HARINA de Etapa 2
gamma_FM <- coefs %>%
  filter(Modelo == "Etapa2", grepl("ln_P_HARINA", Variable)) %>%
  mutate(
    Especie = case_when(
      grepl("anchoveta", Variable) ~ "Anchoveta",
      grepl("jurel", Variable) ~ "Jurel",
      grepl("sardina", Variable) ~ "Sardina"
    )
  )

g5 <- ggplot(gamma_FM, aes(x = Especie, y = Coeficiente)) +
  geom_col(aes(fill = Especie), alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = sprintf("%.3f", Coeficiente)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  labs(
    title = "Transmisión del Precio Internacional (γ_FM)",
    subtitle = "Efecto de ln(P_harina) sobre ln(P_ex-vessel)",
    x = "Especie",
    y = "Coeficiente (γ_FM)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(g5)
ggsave("iaids_efecto_precio_internacional.png", g5, width = 8, height = 6, dpi = 300)

# ==============================================================================
# 7. GRÁFICO 6: EVOLUCIÓN TEMPORAL DE RESIDUOS (BANDAS)
# ==============================================================================

df_resid_summary <- df_pred %>%
  select(ANIO, starts_with("resid_")) %>%
  group_by(ANIO) %>%
  summarise(
    across(starts_with("resid_"), 
           list(mean = ~mean(., na.rm = TRUE),
                sd = ~sd(., na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(
    cols = -ANIO,
    names_to = c("especie", "stat"),
    names_pattern = "resid_P_(.*?)_(mean|sd)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(
    especie = case_when(
      especie == "ANCHOVETA" ~ "Anchoveta",
      especie == "JUREL" ~ "Jurel",
      especie == "SARDINA" ~ "Sardina"
    )
  )

g6 <- ggplot(df_resid_summary, aes(x = ANIO)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = especie), alpha = 0.3) +
  geom_line(aes(y = mean, color = especie), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ especie, scales = "free_y", ncol = 1) +
  labs(
    title = "Evolución de Residuos por Año (Media ± SD)",
    subtitle = "¿Hay períodos con mayor desajuste?",
    x = "Año",
    y = "Residuo Promedio ($/ton)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold")
  )

print(g6)
ggsave("iaids_residuos_evolucion.png", g6, width = 12, height = 8, dpi = 300)

# ==============================================================================
# 8. TABLA RESUMEN DE ESTADÍSTICAS
# ==============================================================================

cat("\n=== ESTADÍSTICAS DE AJUSTE ===\n\n")

# RMSE y MAPE por especie
estadisticas <- df_pred %>%
  summarise(
    across(
      starts_with("resid_"),
      list(
        RMSE = ~sqrt(mean(.^2, na.rm = TRUE)),
        MAE = ~mean(abs(.), na.rm = TRUE),
        MAPE = ~mean(abs(. / get(paste0("PRECIO_REAL_MACRO_", 
                                         sub("resid_P_", "", cur_column())))), 
                     na.rm = TRUE) * 100
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("especie", "estadistica"),
    names_pattern = "resid_P_(.*?)_(.*)",
    values_to = "valor"
  ) %>%
  pivot_wider(
    names_from = estadistica,
    values_from = valor
  ) %>%
  mutate(
    especie = case_when(
      especie == "ANCHOVETA" ~ "Anchoveta",
      especie == "JUREL" ~ "Jurel",
      especie == "SARDINA" ~ "Sardina"
    )
  )

print(estadisticas)

# Guardar tabla
write_csv(estadisticas, "iaids_estadisticas_ajuste.csv")

# ==============================================================================
# 9. ANÁLISIS DE SUSTITUCIÓN/COMPLEMENTARIEDAD
# ==============================================================================

cat("\n=== ANÁLISIS DE SUSTITUCIÓN ===\n\n")

analisis_sustitucion <- gamma2_long %>%
  filter(Precio != str_remove(Cantidad, "Q_")) %>%  # Solo efectos cruzados
  mutate(
    Relacion = case_when(
      Flexibilidad > 0.01 ~ "SUSTITUTOS (γ > 0)",
      Flexibilidad < -0.01 ~ "COMPLEMENTOS (γ < 0)",
      TRUE ~ "INDEPENDIENTES (γ ≈ 0)"
    )
  ) %>%
  select(Precio, Cantidad, Flexibilidad, Relacion)

print(analisis_sustitucion)

cat("\nInterpretación:\n")
cat("  - SUSTITUTOS: ↑Q_j → ↑P_i (más de j disponible hace menos atractivo i)\n")
cat("  - COMPLEMENTOS: ↑Q_j → ↓P_i (más de j aumenta valor de conjunto)\n")
cat("  - INDEPENDIENTES: Q_j no afecta P_i\n\n")

# ==============================================================================
# 10. GRÁFICO FINAL: PANEL COMPLETO
# ==============================================================================

g_panel <- grid.arrange(
  g1, g3, g5,
  ncol = 1,
  top = "Resultados IAIDS: Formación de Precios Ex-Vessel"
)

ggsave("iaids_panel_completo.png", g_panel, width = 14, height = 16, dpi = 300)

cat("\n=== GRÁFICOS GENERADOS ===\n\n")
cat("  1. iaids_precios_obs_vs_pred.png\n")
cat("  2. iaids_residuos.png\n")
cat("  3. iaids_matriz_flexibilidades.png\n")
cat("  4. iaids_comparacion_etapas.png\n")
cat("  5. iaids_efecto_precio_internacional.png\n")
cat("  6. iaids_residuos_evolucion.png\n")
cat("  7. iaids_panel_completo.png\n")
cat("  8. iaids_estadisticas_ajuste.csv\n\n")
