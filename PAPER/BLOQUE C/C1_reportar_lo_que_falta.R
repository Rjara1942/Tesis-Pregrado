# ==============================================================================
# BLOQUE C.1:  LO QUE FALTA 
# ==============================================================================

library(tidyverse)
library(fixest)
library(AER)
library(sandwich)
library(lmtest)

# ------------------------------------------------------------------------------
#  CARGA 
# ------------------------------------------------------------------------------

df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ANIO_fact          = as.factor(ANIO),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  filter(!is.na(SST_PUERTO_L1)) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup()

stopifnot(nrow(df) == 418, length(unique(df$NUI)) == 15)

cat("Muestra de estimacion:", nrow(df), "obs,",
    length(unique(df$NUI)), "plantas,",
    length(unique(df$period)), "meses.\n\n")

# ==============================================================================
# 1. TEST DE HAUSMAN DEL FOB
# ==============================================================================
# Instrumentamos ln_P_FOB con una variable plausiblemente exogena al
# mercado local. El panel trae ln_P_FOB_PERU, que es el FOB peruano y
# responde al mismo mercado internacional de harina sin depender de la
# oferta local chilena.





# Primera etapa del FOB, controles idénticos al modelo principal
fs_fob <- feols(
  ln_P_FOB ~ ln_P_FOB_PERU + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
             NUI,
  data = df, cluster = ~ NUI
)
df$v_FOB <- residuals(fs_fob)

cat(sprintf("Primera etapa del FOB: coef ln_P_FOB_PERU = %.4f (t = %.2f, p = %.4f)\n",
            coef(fs_fob)["ln_P_FOB_PERU"],
            coef(fs_fob)["ln_P_FOB_PERU"] / se(fs_fob)["ln_P_FOB_PERU"],
            pvalue(fs_fob)["ln_P_FOB_PERU"]))

# Regresion aumentada con v_FOB (control function)
m_haus <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA + v_FOB | NUI |
                       ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                        ln_biomasa_sardina + ln_TAC_complejo,
  data = df, cluster = ~ NUI
)

t_haus <- coef(m_haus)["v_FOB"] / se(m_haus)["v_FOB"]
p_haus <- pvalue(m_haus)["v_FOB"]

cat("\nHausman del FOB (control-function con ln_P_FOB_PERU como instrumento):\n")
cat(sprintf("  coef v_FOB = %.4f   SE = %.4f   t = %.2f   p = %.4f\n",
            coef(m_haus)["v_FOB"], se(m_haus)["v_FOB"], t_haus, p_haus))
cat("  H0: FOB exogeno.  ",
    if (p_haus > 0.05) "No se rechaza H0 => FOB exogeno.\n"
    else "Se rechaza H0 => FOB endogeno.\n")

# Cross-check con AER::ivreg tratando FOB como segunda endogena
cat("\nCross-check: 2SLS con FOB tambien endogena, Wu-Hausman conjunto:\n")
m_ivreg_2endog <- ivreg(
  ln_P_complejo_real ~ ln_h_complejo + ln_P_FOB + ln_h_jurel +
                       SEASON_SIN + SEASON_COS + TENDENCIA + factor(NUI) |
                       SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                       ln_TAC_complejo + ln_P_FOB_PERU + ln_h_jurel +
                       SEASON_SIN + SEASON_COS + TENDENCIA + factor(NUI),
  data = df
)
diag_ivreg <- summary(m_ivreg_2endog, diagnostics = TRUE)$diagnostics
print(round(diag_ivreg, 4))

# ==============================================================================
# 2. ESTADISTICO J DE SARGAN-HANSEN (extraccion)
# ==============================================================================
cat("\n==============================================================\n")
cat("2. ESTADISTICO J DE SARGAN-HANSEN\n")
cat("==============================================================\n")

# Modelo principal (baseline) via AER::ivreg para tener diagnostics
m_ivreg_base <- ivreg(
  ln_P_complejo_real ~ ln_h_complejo + ln_P_FOB + ln_h_jurel +
                       SEASON_SIN + SEASON_COS + TENDENCIA + factor(NUI) |
                       SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                       ln_TAC_complejo + ln_P_FOB + ln_h_jurel +
                       SEASON_SIN + SEASON_COS + TENDENCIA + factor(NUI),
  data = df
)
diag_base <- summary(m_ivreg_base, diagnostics = TRUE)$diagnostics
cat("Diagnostics del modelo principal (AER::ivreg):\n")
print(round(diag_base, 4))

sargan_row <- diag_base["Sargan", ]
cat(sprintf("\nSargan J = %.4f  df = %d  p = %.4f\n",
            sargan_row["statistic"], sargan_row["df1"], sargan_row["p-value"]))
cat("Este es el estadistico que falta en la celda con guion en la Tabla 5.\n")

# ==============================================================================
# 3. ESPECIFICACION CON EFECTOS FIJOS DE ANO (para Tabla A.3 Panel B)
# ==============================================================================
cat("\n==============================================================\n")
cat("3. EFECTOS FIJOS DE ANO — reportar gamma\n")
cat("==============================================================\n")

m_yearFE <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA | NUI + ANIO |
                       ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                        ln_biomasa_sardina + ln_TAC_complejo,
  data = df, cluster = ~ NUI
)

nm <- if ("fit_ln_h_complejo" %in% names(coef(m_yearFE)))
        "fit_ln_h_complejo" else "ln_h_complejo"
cat(sprintf("Modelo con NUI + ANIO como efectos fijos:\n"))
cat(sprintf("  gamma = %.4f   SE = %.4f   p = %.4f\n",
            coef(m_yearFE)[nm], se(m_yearFE)[nm], pvalue(m_yearFE)[nm]))
cat(sprintf("  IC 95%%: [%.4f, %.4f]\n",
            confint(m_yearFE, parm = nm)[1, 1],
            confint(m_yearFE, parm = nm)[1, 2]))
cat("  FOB queda absorbido por los efectos fijos de ano (esperable).\n")
cat("  gamma NO se absorbe y este es el valor que va en la fila\n")
cat("  'Efectos fijos de ano' de la Tabla A.3 Panel B.\n")

# ==============================================================================
# 4. F DE PRIMERA ETAPA DE LA ESPECIFICACION "SIN TENDENCIA"
# ==============================================================================
cat("\n==============================================================\n")
cat("4. ESPECIFICACION 'SIN TENDENCIA' — F de primera etapa\n")
cat("==============================================================\n")

fs_sintend <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                  SO_PUERTO + SST_PUERTO_L1 +
                  ln_biomasa_sardina + ln_TAC_complejo | NUI,
  data = df, cluster = ~ NUI
)
F_sintend <- wald(fs_sintend,
                  keep = c("SO_PUERTO", "SST_PUERTO_L1",
                           "ln_biomasa_sardina", "ln_TAC_complejo"))

cat(sprintf("F primera etapa (sin tendencia) = %.4f   p = %.4f\n",
            F_sintend$stat, F_sintend$p))
cat(sprintf("Umbral Stock-Yogo 10%% con 4 instrumentos: 10.27\n"))
cat(sprintf("Evaluacion: %s\n",
            if (F_sintend$stat < 10.27)
              "F < 10.27 => primera etapa DEBIL, hay que anotarlo como con 'sin biomasa'"
            else
              "F >= 10.27 => primera etapa fuerte, no requiere nota"))

# Estimacion completa sin tendencia (para verificar gamma = -0.412 del paper)
m_sintend <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS | NUI |
                       ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                        ln_biomasa_sardina + ln_TAC_complejo,
  data = df, cluster = ~ NUI
)
nm2 <- if ("fit_ln_h_complejo" %in% names(coef(m_sintend)))
         "fit_ln_h_complejo" else "ln_h_complejo"
cat(sprintf("\nModelo sin tendencia:  gamma = %.4f  SE = %.4f  p = %.4f  N = %d\n",
            coef(m_sintend)[nm2], se(m_sintend)[nm2],
            pvalue(m_sintend)[nm2], nobs(m_sintend)))

# ==============================================================================
# 5. P-VALOR UNICO DEL RESULTADO CENTRAL
# ==============================================================================
cat("\n==============================================================\n")
cat("5. P-VALOR UNICO DEL RESULTADO CENTRAL\n")
cat("==============================================================\n")

# Modelo principal (cluster planta)
m_principal <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA | NUI |
                       ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                        ln_biomasa_sardina + ln_TAC_complejo,
  data = df, cluster = ~ NUI
)
nm3 <- if ("fit_ln_h_complejo" %in% names(coef(m_principal)))
         "fit_ln_h_complejo" else "ln_h_complejo"
gamma_hat <- as.numeric(coef(m_principal)[nm3])
se_hat    <- as.numeric(se(m_principal)[nm3])

G  <- length(unique(df$NUI))         # 15 clusters
df_t <- G - 1                        # 14 gl
z_stat <- gamma_hat / se_hat

p_normal <- 2 * (1 - pnorm(abs(z_stat)))
p_t14    <- 2 * (1 - pt(abs(z_stat), df = df_t))

cat(sprintf("gamma = %.4f, SE = %.4f, estadistico z = %.4f\n",
            gamma_hat, se_hat, z_stat))
cat(sprintf("  p con distribucion normal        (Seccion 5.3 / Tabla A.2) = %.4f\n",
            p_normal))
cat(sprintf("  p con distribucion t (%d gl)      (Seccion 5.2)             = %.4f\n",
            df_t, p_t14))
cat("\nRecomendacion: usar t con G-1 = 14 gl (0.030), coherente con Bell-McCaffrey\n")
cat("CR2 que tambien ajusta grados de libertad. Reemplazar 0.016 por 0.030 en la\n")
cat("Tabla A.2 fila CR1 y en la Seccion 5.3.\n")

# ==============================================================================
# 6. GUARDAR RESUMEN
# ==============================================================================
resumen <- tribble(
  ~item,                                       ~valor,
  "Hausman FOB — t (v_FOB)",                    round(t_haus, 4),
  "Hausman FOB — p",                            round(p_haus, 4),
  "Sargan J",                                   round(sargan_row["statistic"], 4),
  "Sargan p",                                   round(sargan_row["p-value"], 4),
  "Gamma con NUI + ANIO FE",                    round(coef(m_yearFE)[nm], 4),
  "SE gamma con FE ano",                        round(se(m_yearFE)[nm], 4),
  "p gamma con FE ano",                         round(pvalue(m_yearFE)[nm], 4),
  "F 1a etapa sin tendencia",                   round(F_sintend$stat, 4),
  "Gamma sin tendencia",                        round(coef(m_sintend)[nm2], 4),
  "p central con normal (Sec 5.3 / A.2)",       round(p_normal, 4),
  "p central con t14 (Sec 5.2)",                round(p_t14, 4)
)
dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(resumen,
          here::here("outputs", "reportes_intermedios", "C1_fixes.csv"))
cat("\nGuardado: outputs/reportes_intermedios/C1_fixes.csv\n")
print(resumen, n = Inf)
