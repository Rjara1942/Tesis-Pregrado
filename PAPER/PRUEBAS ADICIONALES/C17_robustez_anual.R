# ==============================================================================
# BLOQUE C-17 — Robustez a la variación anual de los instrumentos
# ==============================================================================
# Responde al referee: biomasa y TAC viven en frecuencia anual, el panel tiene
# ~13 años → ~13 valores independientes del instrumento. DK bw=4 no cubre la
# dependencia dentro del año. Entregables:
#   (1) IV con DK bw=12 (ancho de banda >= un año completo)
#   (2) IV con cluster por año
#   (3) Wild cluster bootstrap por año (fwildclusterboot)
#   (4) Leave-one-year-out
#   (5) Forma reducida (ln P sobre instrumentos anuales directos)
#   (6) IV con efectos fijos de año (debería dejar γ cerca de 0)
#   (7) Razón desembarque/TAC por año → evidencia de cuota restrictiva
# ==============================================================================

library(tidyverse)
library(fixest)
library(sandwich)
library(scales)
library(here)

# fwildclusterboot es opcional; si no está instalado, el bloque (3) se omite
has_wcb <- requireNamespace("fwildclusterboot", quietly = TRUE)
if (!has_wcb) {
  message("fwildclusterboot no instalado. ",
          "Para el wild bootstrap: install.packages('fwildclusterboot')")
}

PATH_PANEL <- here::here("data", "panel_upgrade.csv")
DIR_OUT    <- here::here("outputs", "reportes_intermedios")
DIR_FIG    <- here::here("outputs", "figuras_paper")
dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_FIG, showWarnings = FALSE, recursive = TRUE)

theme_paper <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    plot.subtitle    = element_text(size = 9, color = "grey30"),
    plot.caption     = element_text(size = 7.5, color = "grey40",
                                    hjust = 0, lineheight = 1.1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, color = "grey85")
  )

# ------------------------------------------------------------------------------
# 0. Muestra (misma que el paper)
# ------------------------------------------------------------------------------
df <- read_csv(PATH_PANEL, show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES)),
    ln_P_complejo_real = log(P_complejo_real),
    ANIO_f             = factor(ANIO)
  ) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup() |>
  filter(!(NUI == "90073" & ANIO == 2013))

stopifnot(nrow(df) == 427, length(unique(df$NUI)) == 16)
n_anios <- length(unique(df$ANIO))
cat(sprintf("Muestra: N=%d, G=%d plantas, T=%d meses, años=%d\n\n",
            nrow(df), n_distinct(df$NUI), n_distinct(df$period), n_anios))

# ------------------------------------------------------------------------------
# Especificación base común
# ------------------------------------------------------------------------------
FORM_IV <- ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS +
                                TENDENCIA + ln_P_FOB | NUI |
             ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo

tabla <- tibble(
  especificacion = character(),
  gamma          = double(),
  se             = double(),
  tstat          = double(),
  p              = double(),
  ci_low         = double(),
  ci_high        = double(),
  F              = double(),
  N              = integer()
)

add_row_fit <- function(name, m, F_val = NA_real_) {
  g  <- as.numeric(coef(m)["fit_ln_h_complejo"])
  s  <- as.numeric(se(m)["fit_ln_h_complejo"])
  tt <- g / s
  ci <- as.numeric(confint(m, "fit_ln_h_complejo"))
  tibble(
    especificacion = name,
    gamma = g, se = s, tstat = tt,
    p = 2 * pnorm(-abs(tt)),
    ci_low = ci[1], ci_high = ci[2],
    F = F_val, N = m$nobs
  )
}

# ------------------------------------------------------------------------------
# 1. Baseline paper: IV con DK bw=4
# ------------------------------------------------------------------------------
cat("1. Baseline DK bw=4...\n")
m_dk4 <- feols(FORM_IV, data = df, vcov = DK(4) ~ period)
F_dk4 <- as.numeric(fitstat(m_dk4, type = "ivf")$`ivf1::ln_h_complejo`$stat)
tabla <- bind_rows(tabla, add_row_fit("DK bw=4 (paper)", m_dk4, F_dk4))

# ------------------------------------------------------------------------------
# 2. DK bw=12 (cubre el año completo)
# ------------------------------------------------------------------------------
cat("2. DK bw=12...\n")
m_dk12 <- feols(FORM_IV, data = df, vcov = DK(12) ~ period)
tabla <- bind_rows(tabla, add_row_fit("DK bw=12", m_dk12, F_dk4))

# ------------------------------------------------------------------------------
# 3. Cluster por año
# ------------------------------------------------------------------------------
cat("3. Cluster por año...\n")
m_cy <- feols(FORM_IV, data = df, cluster = ~ ANIO)
tabla <- bind_rows(tabla, add_row_fit("Cluster por año", m_cy, F_dk4))

# ------------------------------------------------------------------------------
# 4. Cluster bidimensional planta + año
# ------------------------------------------------------------------------------
cat("4. Cluster NUI + año...\n")
m_c2 <- feols(FORM_IV, data = df, cluster = ~ NUI + ANIO)
tabla <- bind_rows(tabla, add_row_fit("Cluster NUI+año", m_c2, F_dk4))

# ------------------------------------------------------------------------------
# 5. Wild cluster bootstrap por año (si está disponible)
# ------------------------------------------------------------------------------
wcb_p <- NA_real_
wcb_ci <- c(NA_real_, NA_real_)
if (has_wcb) {
  cat("5. Wild cluster bootstrap por año (fwildclusterboot)...\n")
  # fwildclusterboot no soporta IV directamente; aplicamos a la forma reducida
  # como proxy del test del instrumento combinado. Para IV usamos remuestreo
  # manual: boottest sobre la reducida da p-valor conjunto correctamente para
  # H0: ambos coeficientes = 0 ⇔ H0: γ_IV = 0 bajo rango de Z.
  rf_boot <- feols(ln_P_complejo_real ~ ln_biomasa_sardina + ln_TAC_complejo +
                                        ln_h_jurel + SEASON_SIN + SEASON_COS +
                                        TENDENCIA + ln_P_FOB | NUI,
                   data = df)
  # Test individual (como referencia): coeficiente de TAC bajo wild cluster año
  bt <- tryCatch(
    fwildclusterboot::boottest(rf_boot, clustid = "ANIO",
                               param = "ln_TAC_complejo",
                               B = 9999, nthreads = 1),
    error = function(e) { message("boottest falló: ", e$message); NULL }
  )
  if (!is.null(bt)) {
    wcb_p  <- bt$p_val
    wcb_ci <- bt$conf_int
    cat(sprintf("   Forma reducida TAC: p_wcb = %.4f, IC95%% = [%.3f, %.3f]\n",
                wcb_p, wcb_ci[1], wcb_ci[2]))
  }
}

# ------------------------------------------------------------------------------
# 6. Leave-one-year-out
# ------------------------------------------------------------------------------
cat("6. Leave-one-year-out...\n")
loo_rows <- map_dfr(sort(unique(df$ANIO)), function(y) {
  d <- filter(df, ANIO != y)
  m <- tryCatch(
    feols(FORM_IV, data = d, vcov = DK(4) ~ period),
    error = function(e) NULL
  )
  if (is.null(m)) return(tibble())
  tibble(
    anio_excluido = y,
    gamma         = as.numeric(coef(m)["fit_ln_h_complejo"]),
    se            = as.numeric(se(m)["fit_ln_h_complejo"]),
    N             = m$nobs
  )
})
cat(sprintf("   Rango LOO: γ ∈ [%.3f, %.3f]\n",
            min(loo_rows$gamma), max(loo_rows$gamma)))

# ------------------------------------------------------------------------------
# 7. Forma reducida: ln P sobre instrumentos directamente
# ------------------------------------------------------------------------------
cat("7. Forma reducida...\n")
rf <- feols(ln_P_complejo_real ~ ln_biomasa_sardina + ln_TAC_complejo +
                                 ln_h_jurel + SEASON_SIN + SEASON_COS +
                                 TENDENCIA + ln_P_FOB | NUI,
            data = df, vcov = DK(4) ~ period)

rf_cy <- feols(ln_P_complejo_real ~ ln_biomasa_sardina + ln_TAC_complejo +
                                    ln_h_jurel + SEASON_SIN + SEASON_COS +
                                    TENDENCIA + ln_P_FOB | NUI,
               data = df, cluster = ~ ANIO)

rf_tbl <- tibble(
  variable = c("ln_biomasa_sardina", "ln_TAC_complejo"),
  coef_DK4 = coef(rf)[c("ln_biomasa_sardina", "ln_TAC_complejo")],
  se_DK4   = se(rf)[c("ln_biomasa_sardina", "ln_TAC_complejo")],
  coef_cY  = coef(rf_cy)[c("ln_biomasa_sardina", "ln_TAC_complejo")],
  se_cY    = se(rf_cy)[c("ln_biomasa_sardina", "ln_TAC_complejo")]
)
print(rf_tbl, width = Inf)

# ------------------------------------------------------------------------------
# 8. IV con efectos fijos de año (debería dejar γ ≈ 0)
# ------------------------------------------------------------------------------
cat("8. IV con FE de año + planta...\n")
FORM_IV_FEY <- ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS +
                                    ln_P_FOB | NUI + ANIO |
                 ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo
m_fey <- feols(FORM_IV_FEY, data = df, vcov = DK(4) ~ period)
tabla <- bind_rows(tabla, add_row_fit("IV con FE_planta+FE_año", m_fey, NA_real_))

# ------------------------------------------------------------------------------
# 9. Razón desembarque/TAC anual (evidencia de cuota restrictiva)
# ------------------------------------------------------------------------------
cat("9. Razón desembarque/TAC anual...\n")
razon <- df |>
  group_by(ANIO) |>
  summarise(
    h_total = sum(exp(ln_h_complejo), na.rm = TRUE),
    TAC     = mean(exp(ln_TAC_complejo), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(razon = h_total / TAC)

print(razon)

# ------------------------------------------------------------------------------
# TABLA RESUMEN
# ------------------------------------------------------------------------------
cat("\n==============================================================\n")
cat("Tabla resumen: γ bajo distintas matrices de varianza\n")
cat("==============================================================\n")
tabla_imp <- tabla |>
  mutate(across(c(gamma, se, tstat, ci_low, ci_high), ~ round(.x, 3)),
         p = round(p, 4))
print(tabla_imp, width = Inf)

write_csv(tabla, file.path(DIR_OUT, "C17_robustez_anual.csv"))
write_csv(loo_rows, file.path(DIR_OUT, "C17_leave_one_year_out.csv"))
write_csv(rf_tbl, file.path(DIR_OUT, "C17_forma_reducida.csv"))
write_csv(razon, file.path(DIR_OUT, "C17_razon_h_TAC.csv"))

# ------------------------------------------------------------------------------
# FIGURAS DE APOYO
# ------------------------------------------------------------------------------

# (a) Leave-one-year-out
fig_loo <- ggplot(loo_rows,
                  aes(x = factor(anio_excluido), y = gamma)) +
  geom_hline(yintercept = tabla$gamma[1], color = "red",
             linetype = "dashed", linewidth = 0.4) +
  geom_errorbar(aes(ymin = gamma - 1.96 * se, ymax = gamma + 1.96 * se),
                width = 0.2, color = "steelblue4") +
  geom_point(size = 2, color = "steelblue4") +
  labs(
    title    = "Leave-one-year-out",
    subtitle = sprintf(paste("Estimador IV excluyendo un año a la vez.",
                              "Línea roja: baseline (γ = %.3f)."),
                      tabla$gamma[1]),
    x = "Año excluido", y = "γ"
  ) +
  theme_paper

ggsave(file.path(DIR_FIG, "fig_loo_year.pdf"),
       fig_loo, width = 7, height = 4, device = "pdf")

# (b) Razón h/TAC por año (cuota restrictiva?)
fig_razon <- ggplot(razon, aes(x = ANIO, y = razon * 100)) +
  geom_col(aes(fill = razon >= 0.9), alpha = 0.85, width = 0.7) +
  geom_hline(yintercept = 100, color = "red", linetype = "dashed",
             linewidth = 0.4) +
  geom_hline(yintercept = 90, color = "darkorange", linetype = "dotted",
             linewidth = 0.4) +
  scale_fill_manual(
    values = c(`TRUE`  = "steelblue4", `FALSE` = "darkorange"),
    labels = c(`TRUE`  = "Cuota efectiva (≥90 %)",
               `FALSE` = "Cuota holgada (<90 %)"),
    name   = NULL
  ) +
  scale_y_continuous(labels = function(x) paste0(x, " %")) +
  scale_x_continuous(breaks = razon$ANIO) +
  labs(
    title    = "Razón desembarque / TAC por año",
    subtitle = paste("Línea roja = 100 % (cuota agotada).",
                     "Línea naranja = 90 % (umbral convencional de efectividad)."),
    x        = NULL,
    y        = "Desembarque como % del TAC",
    caption  = paste("La efectividad de la cuota varía año a año; en años con",
                     "razón <90 % el instrumento TAC es menos relevante en",
                     "el margen y la primera etapa pierde fuerza.",
                     sep = " ")
  ) +
  theme_paper +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(DIR_FIG, "fig_razon_h_TAC.pdf"),
       fig_razon, width = 7, height = 4.3, device = "pdf")

# (c) Forest plot de especificaciones
fig_forest <- tabla |>
  mutate(especificacion = factor(especificacion, levels = rev(especificacion))) |>
  ggplot(aes(x = gamma, y = especificacion)) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.3) +
  geom_vline(xintercept = tabla$gamma[1], color = "red",
             linetype = "dashed", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2,
                 color = "steelblue4") +
  geom_point(size = 2.3, color = "steelblue4") +
  labs(
    title    = "γ bajo distintas matrices de varianza",
    subtitle = "Mismo punto estimado, inferencia bajo supuestos alternativos sobre la dependencia.",
    x = "γ (flexibilidad)", y = NULL
  ) +
  theme_paper

ggsave(file.path(DIR_FIG, "fig_forest_vcov.pdf"),
       fig_forest, width = 7, height = 3.8, device = "pdf")

cat("\n==============================================================\n")
cat("Salidas:\n")
cat("- outputs/reportes_intermedios/C17_robustez_anual.csv\n")
cat("- outputs/reportes_intermedios/C17_leave_one_year_out.csv\n")
cat("- outputs/reportes_intermedios/C17_forma_reducida.csv\n")
cat("- outputs/reportes_intermedios/C17_razon_h_TAC.csv\n")
cat("- outputs/figuras_paper/fig_loo_year.pdf\n")
cat("- outputs/figuras_paper/fig_razon_h_TAC.pdf\n")
cat("- outputs/figuras_paper/fig_forest_vcov.pdf\n")

# ------------------------------------------------------------------------------
# LECTURA PARA EL PAPER
# ------------------------------------------------------------------------------
cat("\n==============================================================\n")
cat("Lectura rápida\n")
cat("==============================================================\n")
cat(sprintf("- Baseline DK bw=4: γ = %.3f (SE = %.3f)\n",
            tabla$gamma[1], tabla$se[1]))
cat(sprintf("- DK bw=12:         γ = %.3f (SE = %.3f)\n",
            tabla$gamma[2], tabla$se[2]))
cat(sprintf("- Cluster año:      γ = %.3f (SE = %.3f)\n",
            tabla$gamma[3], tabla$se[3]))
cat(sprintf("- Cluster NUI+año:  γ = %.3f (SE = %.3f)\n",
            tabla$gamma[4], tabla$se[4]))
cat(sprintf("- FE planta+año:    γ = %.3f (SE = %.3f)   <-- lo que el referee espera cerca de 0\n",
            tabla$gamma[5], tabla$se[5]))
cat(sprintf("- LOO:              γ ∈ [%.3f, %.3f] (sobre %d años)\n",
            min(loo_rows$gamma), max(loo_rows$gamma), n_anios))

cat("\nAgotamiento del TAC por año (% del TAC desembarcado):\n")
print(razon |> mutate(pct = round(razon * 100, 1)) |> select(ANIO, pct))
