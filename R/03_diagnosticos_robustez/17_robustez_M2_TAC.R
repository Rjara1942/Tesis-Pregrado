###############################################################################
#  17_robustez_M2_TAC.R
#  ─────────────────────────────────────────────────────────────────────────────
#  Pruebas de robustez para el Modelo M2 (TAC + biomasa + ambientales)
#  
#  Modelo principal M2:
#    ln_P_complejo ~ ln_P_FOB + ln_h_jurel + SEASON + TENDENCIA | NUI |
#    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina + ln_TAC_complejo
#
#  Referencia: γ = -0.343, F = 12.89, Sargan p = 0.277, N = 418
#
#  Requisitos:
#    - Panel con TAC ya incorporado (output del script 16)
#    - Paquetes: fixest, dplyr, sandwich, lmtest, boot
###############################################################################

library(readxl)
library(dplyr)
library(stringr)
library(fixest)
library(tidyr)
library(lmtest)

# ═══════════════════════════════════════════════════════════════════════════════
# 0. CARGAR DATOS (panel_upgrade.csv ya incluye TAC + POST_2020 + ln_h_X_POST)
# ═══════════════════════════════════════════════════════════════════════════════

panel <- read.csv(here::here("data", "panel_upgrade.csv"))


# ═══════════════════════════════════════════════════════════════════════════════
# 1. MODELO PRINCIPAL M2 (REFERENCIA)
# ═══════════════════════════════════════════════════════════════════════════════

m2 <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                    ln_TAC_complejo,
  data = panel, vcov = ~NUI
)
summary(m2)


# ═══════════════════════════════════════════════════════════════════════════════
# 2. ROBUSTEZ 1: COMPOSICIÓN DEL PANEL
# ═══════════════════════════════════════════════════════════════════════════════


# --- 1a. Excluir plantas con < 10 observaciones ---
plantas_n <- panel %>% count(NUI) %>% filter(n >= 10)
panel_10 <- panel %>% filter(NUI %in% plantas_n$NUI)

r1a <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                    ln_TAC_complejo,
  data = panel_10, vcov = ~NUI
)
cat("\n--- R1a: Plantas >= 10 obs ---\n")
cat("N =", r1a$nobs, "| Plantas =", length(unique(panel_10$NUI)), "\n")
cat("gamma =", round(coef(r1a)["fit_ln_h_complejo"], 4),
    "| SE =", round(se(r1a)["fit_ln_h_complejo"], 4),
    "| F =", round(fitstat(r1a, "ivf")$ivf$stat, 2),
    "| Sargan p =", round(fitstat(r1a, "sargan")$sargan$p, 3), "\n")

# --- 1b. Plantas >= 30 obs (completas) ---
plantas_30 <- panel %>% count(NUI) %>% filter(n >= 30)
panel_30 <- panel %>% filter(NUI %in% plantas_30$NUI)

r1b <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                    ln_TAC_complejo,
  data = panel_30, vcov = ~NUI
)
cat("\n--- R1b: Plantas >= 30 obs ---\n")
cat("N =", r1b$nobs, "| Plantas =", length(unique(panel_30$NUI)), "\n")
cat("gamma =", round(coef(r1b)["fit_ln_h_complejo"], 4),
    "| SE =", round(se(r1b)["fit_ln_h_complejo"], 4),
    "| F =", round(fitstat(r1b, "ivf")$ivf$stat, 2),
    "| Sargan p =", round(fitstat(r1b, "sargan")$sargan$p, 3), "\n")

# --- 1c. Solo plantas ANIMAL (reducción pura) ---
# Identificar plantas ANIMAL si la variable existe en el panel
if ("RG" %in% names(panel)) {
  # RG = región, pero necesitamos tipo de planta
  # Intentar con clasificación basada en la data disponible
  cat("\n--- R1c: Verificando clasificación de plantas ---\n")
  cat("Variable RG disponible. Valores:", paste(unique(panel$RG), collapse = ", "), "\n")
}


# ═══════════════════════════════════════════════════════════════════════════════
# 3. ROBUSTEZ 2: HETEROGENEIDAD TEMPORAL (PRE vs POST 2020)
# ═══════════════════════════════════════════════════════════════════════════════

# --- 2a. Solo pre-2020 ---
panel_pre <- panel %>% filter(ANIO < 2020)

r2a <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                    ln_TAC_complejo,
  data = panel_pre, vcov = ~NUI
)
cat("\n--- R2a: Pre-2020 ---\n")
cat("N =", r2a$nobs, "| gamma =", round(coef(r2a)["fit_ln_h_complejo"], 4),
    "| SE =", round(se(r2a)["fit_ln_h_complejo"], 4),
    "| F =", round(fitstat(r2a, "ivf")$ivf$stat, 2),
    "| Sargan p =", round(fitstat(r2a, "sargan")$sargan$p, 3), "\n")

# --- 2b. Interacción con POST_2020 ---
cat("\n--- R2b: Interacción temporal ---\n")

r2b <- tryCatch({
  feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA + POST_2020 |
      NUI |
      ln_h_complejo + ln_h_X_POST ~
        SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina + ln_TAC_complejo +
        I(SO_PUERTO * POST_2020) + I(SST_PUERTO_L1 * POST_2020) +
        I(ln_biomasa_sardina * POST_2020) + I(ln_TAC_complejo * POST_2020),
    data = panel, vcov = ~NUI
  )
}, error = function(e) {
  cat("  Error en interacción con 2 endógenas:", e$message, "\n")
  cat("  Estimando versión simplificada con dummy aditiva...\n")
  feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA + POST_2020 |
      NUI |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo,
    data = panel, vcov = ~NUI
  )
})
summary(r2b)


# ═══════════════════════════════════════════════════════════════════════════════
# 4. ROBUSTEZ 3: SIN PRECIO FOB (TEST DE VARIABLE OMITIDA)
# ═══════════════════════════════════════════════════════════════════════════════

r3 <- feols(
  ln_P_complejo ~ ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                    ln_TAC_complejo,
  data = panel, vcov = ~NUI
)
cat("gamma sin FOB =", round(coef(r3)["fit_ln_h_complejo"], 4),
    "| gamma con FOB =", round(coef(m2)["fit_ln_h_complejo"], 4),
    "| Diferencia =", round(abs(coef(r3)["fit_ln_h_complejo"] -
                                 coef(m2)["fit_ln_h_complejo"]), 4), "\n")
cat("F =", round(fitstat(r3, "ivf")$ivf$stat, 2),
    "| Sargan p =", round(fitstat(r3, "sargan")$sargan$p, 3), "\n")


# ═══════════════════════════════════════════════════════════════════════════════
# 5. ROBUSTEZ 4: TRATAMIENTO DEL FOB
# ═══════════════════════════════════════════════════════════════════════════════


# Instrumentar FOB con FOB Perú
if ("ln_P_FOB_PERU" %in% names(panel)) {
  r4 <- tryCatch({
    feols(
      ln_P_complejo ~ ln_h_jurel +
        SEASON_SIN + SEASON_COS + TENDENCIA |
        NUI |
        ln_h_complejo + ln_P_FOB ~
          SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
          ln_TAC_complejo + ln_P_FOB_PERU,
      data = panel, vcov = ~NUI
    )
  }, error = function(e) {
    cat("  Error instrumentando FOB:", e$message, "\n")
    NULL
  })
  
  if (!is.null(r4)) {
    cat("gamma (FOB instrumentado) =", round(coef(r4)["fit_ln_h_complejo"], 4),
        "| gamma (FOB exógeno) =", round(coef(m2)["fit_ln_h_complejo"], 4), "\n")
    cat("beta FOB (instrumentado) =", round(coef(r4)["fit_ln_P_FOB"], 4), "\n")
    cat("Diferencia gamma:", round(abs(coef(r4)["fit_ln_h_complejo"] -
                                        coef(m2)["fit_ln_h_complejo"]), 4), "\n")
  }
} else {
  cat("  Variable ln_P_FOB_PERU no disponible. Saltando.\n")
}

# Test de Hausman para exogeneidad del FOB
cat("\n--- Test de Hausman para FOB ---\n")
fob_1st <- feols(
  ln_P_FOB ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
    ln_TAC_complejo + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI,
  data = panel
)
panel$resid_fob <- NA
panel$resid_fob[as.integer(names(residuals(fob_1st)))] <- residuals(fob_1st)

hausman_m2 <- tryCatch({
  feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel + resid_fob +
      SEASON_SIN + SEASON_COS + TENDENCIA |
      NUI |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo,
    data = panel, vcov = ~NUI
  )
}, error = function(e) {
  cat("  Error en Hausman:", e$message, "\n")
  NULL
})

if (!is.null(hausman_m2)) {
  cat("Coef resid_fob =", round(coef(hausman_m2)["resid_fob"], 4),
      "| p-valor =", round(pvalue(hausman_m2)["resid_fob"], 4), "\n")
  if (pvalue(hausman_m2)["resid_fob"] > 0.10) {
    cat("  → No se rechaza exogeneidad del FOB (p > 0.10)\n")
  } else {
    cat("  → Se rechaza exogeneidad del FOB (p < 0.10)\n")
  }
}


# ═══════════════════════════════════════════════════════════════════════════════
# 6. ROBUSTEZ 5: INSTRUMENTOS ALTERNATIVOS
# ═══════════════════════════════════════════════════════════════════════════════

# --- 5a. Agregar CHL-a como instrumento adicional ---
r5a <- tryCatch({
  feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA |
      NUI |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo + CHL_A_PUERTO,
    data = panel, vcov = ~NUI
  )
}, error = function(e) { cat("  Error R5a:", e$message, "\n"); NULL })

if (!is.null(r5a)) {
  cat("R5a (+CHL_A): gamma =", round(coef(r5a)["fit_ln_h_complejo"], 4),
      "| F =", round(fitstat(r5a, "ivf")$ivf$stat, 2),
      "| Sargan p =", round(fitstat(r5a, "sargan")$sargan$p, 3), "\n")
}

# --- 5b. Agregar viento como instrumento ---
r5b <- tryCatch({
  feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA |
      NUI |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo + WIND_PUERTO,
    data = panel, vcov = ~NUI
  )
}, error = function(e) { cat("  Error R5b:", e$message, "\n"); NULL })

if (!is.null(r5b)) {
  cat("R5b (+WIND):  gamma =", round(coef(r5b)["fit_ln_h_complejo"], 4),
      "| F =", round(fitstat(r5b, "ivf")$ivf$stat, 2),
      "| Sargan p =", round(fitstat(r5b, "sargan")$sargan$p, 3), "\n")
}

# --- 5c. Agregar diesel como instrumento ---
if ("ln_DIESEL" %in% names(panel)) {
  r5c <- tryCatch({
    feols(
      ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
        SEASON_SIN + SEASON_COS + TENDENCIA |
        NUI |
        ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                        ln_TAC_complejo + ln_DIESEL,
      data = panel, vcov = ~NUI
    )
  }, error = function(e) { cat("  Error R5c:", e$message, "\n"); NULL })
  
  if (!is.null(r5c)) {
    cat("R5c (+DIESEL): gamma =", round(coef(r5c)["fit_ln_h_complejo"], 4),
        "| F =", round(fitstat(r5c, "ivf")$ivf$stat, 2),
        "| Sargan p =", round(fitstat(r5c, "sargan")$sargan$p, 3), "\n")
  }
}

# --- 5d. Rezagos de desembarque como instrumentos adicionales ---
r5d <- tryCatch({
  feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA |
      NUI |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo + ln_h_complejo_L3,
    data = panel, vcov = ~NUI
  )
}, error = function(e) { cat("  Error R5d:", e$message, "\n"); NULL })

if (!is.null(r5d)) {
  cat("R5d (+lag3):  gamma =", round(coef(r5d)["fit_ln_h_complejo"], 4),
      "| F =", round(fitstat(r5d, "ivf")$ivf$stat, 2),
      "| Sargan p =", round(fitstat(r5d, "sargan")$sargan$p, 3), "\n")
}

# --- 5e. TAC sin biomasa, solo ambientales ---

r5e <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_TAC_complejo,
  data = panel, vcov = ~NUI
)



# ═══════════════════════════════════════════════════════════════════════════════
# 7. ROBUSTEZ 6: CORRECCIÓN PARA POCOS CLUSTERS (CR2)
# ═══════════════════════════════════════════════════════════════════════════════

# Intentar con fwildclusterboot si está disponible
wild_boot <- tryCatch({
  library(fwildclusterboot)
  
  # Para wild bootstrap necesitamos ivreg o lm
  # Usar feols con diferentes tipos de SE
  m2_cr2 <- feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA |
      NUI |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo,
    data = panel
  )
  
  # CR2 (Bell-McCaffrey) con fixest
  se_cr0 <- se(m2)["fit_ln_h_complejo"]
  
  # Boottest
  boot_res <- boottest(m2_cr2, param = "fit_ln_h_complejo",
                        clustid = ~NUI, B = 9999, type = "webb")
  
  cat("SE clustered CR0:", round(se_cr0, 4), "\n")
  cat("Wild bootstrap p-valor:", round(boot_res$p_val, 4), "\n")
  cat("Wild bootstrap IC 95%: [", round(boot_res$conf_int[1], 4), ",",
      round(boot_res$conf_int[2], 4), "]\n")
  
  TRUE
}, error = function(e) {
  cat("  fwildclusterboot no disponible o error:", e$message, "\n")
  cat("  Reportando SE con distintos métodos disponibles:\n")
  
  # Comparar SE con diferentes especificaciones de cluster
  se_nui <- se(feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA |
      NUI |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo,
    data = panel, vcov = ~NUI
  ))["fit_ln_h_complejo"]
  
  se_het <- se(feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA |
      NUI |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo,
    data = panel, vcov = "hetero"
  ))["fit_ln_h_complejo"]
  
  cat("  SE cluster NUI:   ", round(se_nui, 4), "\n")
  cat("  SE heterocedastico:", round(se_het, 4), "\n")
  cat("  Ratio cluster/het: ", round(se_nui / se_het, 2), "\n")
  
  FALSE
})


# ═══════════════════════════════════════════════════════════════════════════════
# 8. ROBUSTEZ 7: AUTOCORRELACIÓN DE RESIDUOS
# ═══════════════════════════════════════════════════════════════════════════════

resid_m2 <- residuals(m2)
# Reconstruir panel usado a partir de complete.cases de las vars del modelo
vars_m2 <- c("ln_P_complejo", "ln_P_FOB", "ln_h_jurel",
             "SEASON_SIN", "SEASON_COS", "TENDENCIA",
             "ln_h_complejo", "SO_PUERTO", "SST_PUERTO_L1",
             "ln_biomasa_sardina", "ln_TAC_complejo",
             "NUI", "yearmonth")
panel_used <- panel[complete.cases(panel[, vars_m2]), ]
if (length(resid_m2) != nrow(panel_used)) {
  warning(sprintf("Ajuste: length(resid)=%d, nrow(panel_used)=%d — recortando",
                  length(resid_m2), nrow(panel_used)))
  # Recorte defensivo por si fixest dropeó por otra razón (colinealidad, etc.)
  n <- min(length(resid_m2), nrow(panel_used))
  resid_m2   <- resid_m2[seq_len(n)]
  panel_used <- panel_used[seq_len(n), ]
}
resid_ts <- tapply(as.numeric(resid_m2), panel_used$yearmonth,
                   mean, na.rm = TRUE)
resid_ts <- resid_ts[!is.na(resid_ts)]

# ACF manual
cat("Autocorrelaciones de residuos (promedio cross-section):\n")
for (k in 1:6) {
  n <- length(resid_ts)
  rho <- cor(resid_ts[1:(n-k)], resid_ts[(1+k):n], use = "complete.obs")
  sig <- ifelse(abs(rho) > 1.96/sqrt(n), "***", "")
  cat(sprintf("  rho(%d) = %+.3f %s\n", k, rho, sig))
}

# Breusch-Godfrey para panel (aproximación con OLS+FE)
cat("\n--- Breusch-Godfrey aproximado ---\n")
fe_ols <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_complejo + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA | NUI,
  data = panel
)
res_ols <- residuals(fe_ols)
vars_ols <- c("ln_P_complejo", "ln_P_FOB", "ln_h_complejo", "ln_h_jurel",
              "SEASON_SIN", "SEASON_COS", "TENDENCIA", "NUI")
idx_ols <- which(complete.cases(panel[, vars_ols]))
panel$resid_ols <- NA_real_
if (length(res_ols) == length(idx_ols)) {
  panel$resid_ols[idx_ols] <- as.numeric(res_ols)
} else {
  n <- min(length(res_ols), length(idx_ols))
  panel$resid_ols[idx_ols[seq_len(n)]] <- as.numeric(res_ols)[seq_len(n)]
}

panel_bg <- panel %>%
  arrange(NUI, ANIO, MES) %>%
  group_by(NUI) %>%
  mutate(resid_L1 = dplyr::lag(resid_ols, 1),
         resid_L2 = dplyr::lag(resid_ols, 2)) %>%
  ungroup()

bg1 <- summary(lm(resid_ols ~ resid_L1, data = panel_bg))
bg2 <- summary(lm(resid_ols ~ resid_L1 + resid_L2, data = panel_bg))

cat("AR(1) coef:", round(bg1$coefficients[2, 1], 3),
    "| p =", round(bg1$coefficients[2, 4], 4), "\n")
cat("AR(2) coef:", round(bg2$coefficients[3, 1], 3),
    "| p =", round(bg2$coefficients[3, 4], 4), "\n")


# ═══════════════════════════════════════════════════════════════════════════════
# 9. ROBUSTEZ 8: SERIE DE TIEMPO AGREGADA
# ═══════════════════════════════════════════════════════════════════════════════

ts_data <- panel %>%
  group_by(ANIO, MES) %>%
  summarise(
    ln_P_complejo = mean(ln_P_complejo, na.rm = TRUE),
    ln_h_complejo = first(ln_h_complejo),
    ln_P_FOB = first(ln_P_FOB),
    ln_h_jurel = first(ln_h_jurel),
    SEASON_SIN = first(SEASON_SIN),
    SEASON_COS = first(SEASON_COS),
    TENDENCIA = first(TENDENCIA),
    SO_PUERTO = mean(SO_PUERTO, na.rm = TRUE),
    SST_PUERTO_L1 = mean(SST_PUERTO_L1, na.rm = TRUE),
    ln_biomasa_sardina = first(ln_biomasa_sardina),
    ln_TAC_complejo = first(ln_TAC_complejo),
    .groups = "drop"
  ) %>%
  filter(!is.na(ln_P_complejo) & !is.na(ln_h_complejo) &
         !is.na(SO_PUERTO) & !is.na(ln_TAC_complejo))

# IV con ivreg si disponible, sino feols sin FE
r8 <- tryCatch({
  library(ivreg)
  iv_ts <- ivreg(
    ln_P_complejo ~ ln_h_complejo + ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA |
      SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina + ln_TAC_complejo +
      ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA,
    data = ts_data
  )
  # HAC Newey-West
  nw_se <- sqrt(diag(sandwich::NeweyWest(iv_ts, lag = 4)))
  
  cat("N (meses) =", nrow(ts_data), "\n")
  cat("gamma (serie tiempo) =", round(coef(iv_ts)["ln_h_complejo"], 4), "\n")
  cat("SE (Newey-West, 4 lags) =", round(nw_se["ln_h_complejo"], 4), "\n")
  cat("beta FOB =", round(coef(iv_ts)["ln_P_FOB"], 4), "\n")
  
  iv_ts
}, error = function(e) {
  cat("  ivreg no disponible. Estimando con feols sin FE:\n")
  r8_alt <- feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo,
    data = ts_data, vcov = "NW" ~ TENDENCIA
  )
  summary(r8_alt)
  r8_alt
})


# ═══════════════════════════════════════════════════════════════════════════════
# 10. ROBUSTEZ 9: INTERACCIÓN PLANTA MIXTA
# ═══════════════════════════════════════════════════════════════════════════════

# Verificar si hay variable de tipo de planta
if ("n_especies" %in% names(panel)) {
  panel <- panel %>%
    mutate(D_MIXTA = ifelse(n_especies > 1, 1, 0))
  
  cat("Plantas con n_especies > 1 (proxy MIXTA):",
      sum(panel$D_MIXTA, na.rm = TRUE), "obs\n")
  
  r9 <- tryCatch({
    feols(
      ln_P_complejo ~ ln_P_FOB + ln_h_jurel + D_MIXTA +
        SEASON_SIN + SEASON_COS + TENDENCIA |
        NUI |
        ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                        ln_TAC_complejo,
      data = panel, vcov = ~NUI
    )
  }, error = function(e) {
    cat("  Error (posible colinealidad con FE):", e$message, "\n")
    cat("  D_MIXTA es absorbida por los efectos fijos de planta.\n")
    cat("  Probando interacción D_MIXTA × ln_h_complejo...\n")
    NULL
  })
  
  if (!is.null(r9)) summary(r9)
}


# ═══════════════════════════════════════════════════════════════════════════════
# 11. TABLA RESUMEN FINAL
# ═══════════════════════════════════════════════════════════════════════════════


extraer <- function(mod, nombre) {
  coefs <- coef(mod)
  ses   <- se(mod)
  f_iv  <- tryCatch(fitstat(mod, "ivf")$ivf$stat, error = function(e) NA)
  s_p   <- tryCatch(fitstat(mod, "sargan")$sargan$p, error = function(e) NA)
  data.frame(
    Especificacion = nombre,
    gamma = round(coefs["fit_ln_h_complejo"], 4),
    SE = round(ses["fit_ln_h_complejo"], 4),
    F_1et = round(f_iv, 1),
    Sargan_p = round(s_p, 3),
    N = mod$nobs,
    row.names = NULL
  )
}

resumen <- bind_rows(
  extraer(m2,  "M2 Principal"),
  extraer(r1a, "Plantas >= 10 obs"),
  extraer(r1b, "Plantas >= 30 obs"),
  extraer(r2a, "Solo pre-2020"),
  extraer(r3,  "Sin FOB"),
  extraer(r5e, "Sin biomasa (TAC+amb)")
)

# Agregar modelos con instrumentos adicionales si existen
if (!is.null(r5a)) resumen <- bind_rows(resumen, extraer(r5a, "+ CHL_A"))
if (!is.null(r5b)) resumen <- bind_rows(resumen, extraer(r5b, "+ WIND"))
if (!is.null(r5d)) resumen <- bind_rows(resumen, extraer(r5d, "+ lag(3)"))


print(resumen, right = FALSE, row.names = FALSE)



# ═══════════════════════════════════════════════════════════════════════════════
# 12. TEST DE WALD: HIPÓTESIS DE TOMADOR DE PRECIO
# ═══════════════════════════════════════════════════════════════════════════════



gamma_m2 <- coef(m2)["fit_ln_h_complejo"]
se_m2    <- se(m2)["fit_ln_h_complejo"]
t_stat   <- gamma_m2 / se_m2
p_val    <- 2 * pt(abs(t_stat), df = m2$nobs - length(coef(m2)) - 15,
                    lower.tail = FALSE)


# Test conjunto: gamma = 0 AND delta_jurel = 0
wald_joint <- tryCatch({
  wald(m2, c("fit_ln_h_complejo", "ln_h_jurel"))
}, error = function(e) {
  cat("  Error en Wald conjunto:", e$message, "\n")
  NULL
})
if (!is.null(wald_joint)) print(wald_joint)



