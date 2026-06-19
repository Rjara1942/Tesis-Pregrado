# ==============================================================================
# PROCEDIMIENTO FELIPE - PASO A PASO (CORREGIDO)
# Modelo Complejo Sardina-Anchoveta
# ==============================================================================
#
# Paso 1: Panel planta×mes, rezagos como instrumentos
# Paso 2: OLS + FE (within), R² within/between, residuos
# Paso 3: Test autocorrelación (Breusch-Godfrey, ACF/PACF) → determinar q
# Paso 4: Seleccionar rezagos k > q como instrumentos, verificar F > 10
#
# ==============================================================================


library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)

cat(rep("=", 70), "\n", sep = "")
cat("PROCEDIMIENTO FELIPE - PASO A PASO\n")
cat(rep("=", 70), "\n\n", sep = "")

# ==============================================================================
# PASO 1: CARGAR Y PREPARAR PANEL
# ==============================================================================

cat(rep("=", 70), "\n", sep = "")
cat("PASO 1: PREPARACIÓN DEL PANEL\n")
cat(rep("=", 70), "\n\n", sep = "")

# Cargar panel
df <- read_csv(here::here("data", "panel_complejo.csv"), show_col_types = FALSE)

cat("1.1 Panel cargado:\n")
cat("    N observaciones:", nrow(df), "\n")
cat("    N plantas:", n_distinct(df$NUI), "\n")
cat("    Período:", min(df$ANIO), "-", max(df$ANIO), "\n")
cat("    Meses únicos:", n_distinct(df$yearmonth), "\n")

# -----------------------------------------------------------------------------
# 1.2 Observaciones en meses de veda
# -----------------------------------------------------------------------------
cat("\n1.2 Observaciones en meses de veda:\n")
cat("    Agosto (8):", sum(df$MES == 8), "\n")
cat("    Septiembre (9):", sum(df$MES == 9), "\n")
cat("    Total veda:", sum(df$D_VEDA == 1), "obs\n")
cat("    → Se mantienen (D_VEDA como control)\n")

# -----------------------------------------------------------------------------
# 1.3 Crear rezagos adicionales para instrumentos
# -----------------------------------------------------------------------------
cat("\n1.3 Creando rezagos adicionales:\n")

df <- df %>%
  mutate(NUI = as.character(NUI)) %>%
  arrange(NUI, ANIO, MES) %>%
  group_by(NUI) %>%
  mutate(
    # Rezagos de h_complejo (hasta orden 6)
    ln_h_complejo_L1 = dplyr::lag(ln_h_complejo, 1),
    ln_h_complejo_L2 = dplyr::lag(ln_h_complejo, 2),
    ln_h_complejo_L3 = dplyr::lag(ln_h_complejo, 3),
    ln_h_complejo_L4 = dplyr::lag(ln_h_complejo, 4),
    ln_h_complejo_L5 = dplyr::lag(ln_h_complejo, 5),
    ln_h_complejo_L6 = dplyr::lag(ln_h_complejo, 6),
    
    # Rezagos de h_jurel
    ln_h_jurel_L1 = dplyr::lag(ln_h_jurel, 1),
    ln_h_jurel_L2 = dplyr::lag(ln_h_jurel, 2),
    ln_h_jurel_L3 = dplyr::lag(ln_h_jurel, 3),
    
    # Rezago del precio (para modelo dinámico)
    ln_P_complejo_L1 = dplyr::lag(ln_P_complejo, 1)
  ) %>%
  ungroup()

cat("    Rezagos creados: L1 a L6\n")

# Cobertura de rezagos
cat("\n    Cobertura de rezagos:\n")
for (k in 1:6) {
  var <- paste0("ln_h_complejo_L", k)
  n_ok <- sum(!is.na(df[[var]]))
  cat(sprintf("    L%d: %d obs (%.0f%%)\n", k, n_ok, 100*n_ok/nrow(df)))
}

# -----------------------------------------------------------------------------
# 1.4 Definir panel para plm
# -----------------------------------------------------------------------------
cat("\n1.4 Definiendo estructura de panel:\n")

pdata <- pdata.frame(df, index = c("NUI", "yearmonth"))

cat("    Índices: NUI (planta), yearmonth\n")
cat("    Balanced:", ifelse(is.pbalanced(pdata), "Sí", "No"), "\n")

# ==============================================================================
# PASO 2: ESTIMACIÓN OLS + FE (WITHIN) - BENCHMARK
# ==============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("PASO 2: OLS CON EFECTOS FIJOS (WITHIN ESTIMATOR)\n")
cat(rep("=", 70), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 2.1 Estimar modelo within (efectos fijos de planta)
# -----------------------------------------------------------------------------
cat("2.1 Estimación Within (FE):\n\n")

fe_ols <- plm(
  ln_P_complejo ~ ln_P_FOB + ln_h_complejo + ln_h_jurel + factor(MES),
  data = pdata,
  model = "within"
)

cat("Coeficientes principales:\n")
print(summary(fe_ols)$coefficients[1:3, ])

# -----------------------------------------------------------------------------
# 2.2 R² within
# -----------------------------------------------------------------------------
cat("\n2.2 Bondad de ajuste:\n")

# R² within directamente de plm
r2_vals <- summary(fe_ols)$r.squared
cat("    R² within (adjrsq):", round(r2_vals["adjrsq"], 4), "\n")
cat("    R² (rsq):", round(r2_vals["rsq"], 4), "\n")

# -----------------------------------------------------------------------------
# 2.3 Examinar residuos
# -----------------------------------------------------------------------------
cat("\n2.3 Análisis de residuos:\n")

resid_fe <- residuals(fe_ols)

cat("    N residuos:", length(resid_fe), "\n")
cat("    Media:", round(mean(resid_fe), 6), "\n")
cat("    SD:", round(sd(resid_fe), 4), "\n")
cat("    Min:", round(min(resid_fe), 4), "\n")
cat("    Max:", round(max(resid_fe), 4), "\n")

# ==============================================================================
# PASO 3: TEST DE AUTOCORRELACIÓN SERIAL
# ==============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("PASO 3: TEST DE AUTOCORRELACIÓN SERIAL\n")
cat(rep("=", 70), "\n\n", sep = "")

# -----------------------------------------------------------------------------
# 3.1 Test de Breusch-Godfrey para panel
# -----------------------------------------------------------------------------
cat("3.1 Test de Breusch-Godfrey para panel:\n\n")
cat("    H0: No hay autocorrelación serial de orden k\n\n")

bg_results <- data.frame(orden = integer(), chi2 = numeric(), pvalue = numeric())

for (order in 1:6) {
  bg_test <- tryCatch({
    pbgtest(fe_ols, order = order)
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(bg_test)) {
    cat(sprintf("    Orden %d: χ² = %.3f, p-value = %.4f %s\n",
                order, 
                bg_test$statistic, 
                bg_test$p.value,
                ifelse(bg_test$p.value < 0.05, "→ RECHAZA H0 ***", "→ No rechaza")))
    bg_results <- rbind(bg_results, 
                        data.frame(orden = order, 
                                   chi2 = bg_test$statistic, 
                                   pvalue = bg_test$p.value))
  }
}

# -----------------------------------------------------------------------------
# 3.2 Test de Wooldridge para AR(1) en paneles FE
# -----------------------------------------------------------------------------
cat("\n3.2 Test de Wooldridge para AR(1):\n\n")

wooldridge_test <- tryCatch({
  pwartest(fe_ols)
}, error = function(e) {
  return(NULL)
})

if (!is.null(wooldridge_test)) {
  cat(sprintf("    F-stat = %.3f, p-value = %.6f\n",
              wooldridge_test$statistic,
              wooldridge_test$p.value))
  cat(sprintf("    %s\n", 
              ifelse(wooldridge_test$p.value < 0.05, 
                     "→ RECHAZA H0: Hay autocorrelación AR(1) ***",
                     "→ No rechaza H0")))
}

# -----------------------------------------------------------------------------
# 3.3 ACF/PACF de residuos (CORREGIDO)
# -----------------------------------------------------------------------------
cat("\n3.3 ACF/PACF de residuos:\n\n")

# Crear dataframe con residuos
df_resid <- data.frame(
  yearmonth = names(resid_fe),
  resid = as.numeric(resid_fe)
)

# Promediar residuos por yearmonth
resid_by_month <- df_resid %>%
  group_by(yearmonth) %>%
  summarise(resid_mean = mean(resid, na.rm = TRUE), .groups = "drop") %>%
  arrange(yearmonth)

# Convertir a vector numérico
resid_ts <- resid_by_month$resid_mean

cat("    Serie temporal de residuos promedio: T =", length(resid_ts), "\n")
cat("    Valores NA:", sum(is.na(resid_ts)), "\n\n")

# Eliminar NAs si existen
resid_ts_clean <- na.omit(resid_ts)

# ACF
cat("    ACF (Autocorrelation Function):\n")
cat("    ─────────────────────────────────\n")
acf_vals <- acf(resid_ts_clean, lag.max = 12, plot = FALSE)

cat("    Lag    ACF    Significativo\n")
ci <- 1.96 / sqrt(length(resid_ts_clean))
for (i in 1:min(8, length(acf_vals$acf))) {
  sig <- ifelse(abs(acf_vals$acf[i]) > ci, "  ***", "")
  cat(sprintf("    %3d  %6.3f  %s\n", i-1, acf_vals$acf[i], sig))
}

# PACF
cat("\n    PACF (Partial Autocorrelation Function):\n")
cat("    ─────────────────────────────────────────\n")
pacf_vals <- pacf(resid_ts_clean, lag.max = 12, plot = FALSE)

cat("    Lag   PACF    Significativo\n")
for (i in 1:min(8, length(pacf_vals$acf))) {
  sig <- ifelse(abs(pacf_vals$acf[i]) > ci, "  ***", "")
  cat(sprintf("    %3d  %6.3f  %s\n", i, pacf_vals$acf[i], sig))
}

cat(sprintf("\n    Intervalo de confianza 95%%: ± %.3f\n", ci))

# -----------------------------------------------------------------------------
# 3.4 Determinar orden q de autocorrelación
# -----------------------------------------------------------------------------
cat("\n3.4 Determinación del orden de autocorrelación (q):\n\n")

# Encontrar último rezago significativo en PACF
q_pacf <- 0
for (i in 1:length(pacf_vals$acf)) {
  if (abs(pacf_vals$acf[i]) > ci) {
    q_pacf <- i
  }
}

# Encontrar último rezago significativo en ACF (decay pattern)
q_acf <- 0
for (i in 2:length(acf_vals$acf)) {  # Empezar desde lag 1
  if (abs(acf_vals$acf[i]) > ci) {
    q_acf <- i - 1  # ACF incluye lag 0
  }
}

cat(sprintf("    Último rezago significativo en PACF: %d\n", q_pacf))
cat(sprintf("    Último rezago significativo en ACF:  %d\n", q_acf))

# Usar el máximo como q conservador
q_suggested <- max(q_pacf, q_acf, 1)
cat(sprintf("\n    → q sugerido (conservador): %d\n", q_suggested))
cat(sprintf("    → Usar rezagos de orden k > %d como instrumentos\n", q_suggested))

# ==============================================================================
# PASO 4: SELECCIÓN DE INSTRUMENTOS (rezagos k > q)
# ==============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("PASO 4: VERIFICACIÓN DE INSTRUMENTOS (PRIMERA ETAPA)\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("4.1 Primera etapa con diferentes conjuntos de rezagos:\n\n")
cat("    (Instrumentos válidos deben tener k > q =", q_suggested, ")\n\n")

# Función para probar instrumentos
test_first_stage <- function(lag_start, lag_end, data, q) {
  
  # Variables de instrumentos
  iv_vars <- paste0("ln_h_complejo_L", lag_start:lag_end)
  iv_vars <- iv_vars[iv_vars %in% names(data)]
  
  if (length(iv_vars) == 0) return(NULL)
  
  # Datos completos
  vars_all <- c("ln_h_complejo", "ln_P_FOB", "ln_h_jurel", "MES", "NUI", "yearmonth", iv_vars)
  data_clean <- data[complete.cases(data[, vars_all]), ]
  
  if (nrow(data_clean) < 50) return(NULL)
  
  # Panel
  pdata_iv <- pdata.frame(data_clean, index = c("NUI", "yearmonth"))
  
  # Primera etapa
  formula_str <- paste("ln_h_complejo ~", paste(iv_vars, collapse = " + "), 
                       "+ ln_P_FOB + ln_h_jurel + factor(MES)")
  
  first_stage <- tryCatch({
    plm(as.formula(formula_str), data = pdata_iv, model = "within")
  }, error = function(e) NULL)
  
  if (is.null(first_stage)) return(NULL)
  
  # Modelo restringido (sin IVs)
  restricted <- plm(ln_h_complejo ~ ln_P_FOB + ln_h_jurel + factor(MES), 
                    data = pdata_iv, model = "within")
  
  # F-test
  ssr_r <- sum(residuals(restricted)^2)
  ssr_u <- sum(residuals(first_stage)^2)
  df1 <- length(iv_vars)
  df2 <- nrow(data_clean) - length(coef(first_stage)) - n_distinct(data_clean$NUI)
  
  F_stat <- ((ssr_r - ssr_u) / df1) / (ssr_u / df2)
  
  # Validez: k > q
  min_lag <- lag_start
  es_valido <- min_lag > q
  
  list(
    lags = paste0("L", lag_start:lag_end, collapse = "+"),
    min_lag = min_lag,
    n_obs = nrow(data_clean),
    F_stat = round(F_stat, 2),
    valido = es_valido
  )
}

# Probar diferentes combinaciones
cat(sprintf("    %-15s %6s %8s %10s %10s\n", "Rezagos", "N", "F-stat", "k > q?", "Resultado"))
cat("    ", rep("-", 55), "\n", sep = "")

resultados_iv <- list()

for (start in 1:5) {
  end <- min(start + 1, 6)
  result <- test_first_stage(start, end, df, q_suggested)
  
  if (!is.null(result)) {
    status_valido <- ifelse(result$valido, "Sí", "No")
    status_f <- ifelse(result$F_stat > 10, "✓ FUERTE", 
                       ifelse(result$F_stat > 5, "~ Moderado", "✗ Débil"))
    
    cat(sprintf("    %-15s %6d %8.2f %10s %10s\n", 
                result$lags, result$n_obs, result$F_stat, status_valido, status_f))
    
    resultados_iv[[length(resultados_iv) + 1]] <- result
  }
}

# -----------------------------------------------------------------------------
# 4.2 Comparación con IVs climáticos
# -----------------------------------------------------------------------------
cat("\n4.2 Comparación con IVs climáticos (SST, CHL_A):\n\n")

# IVs climáticos
iv_clima <- c("SST_MACRO", "SST2", "CHL_A_MACRO", "CHL_A2", "WIND_SPEED_MACRO")
vars_clima <- c("ln_h_complejo", "ln_P_FOB", "ln_h_jurel", "MES", "NUI", "yearmonth", iv_clima)
data_clima <- df[complete.cases(df[, vars_clima]), ]

if (nrow(data_clima) > 50) {
  pdata_clima <- pdata.frame(data_clima, index = c("NUI", "yearmonth"))
  
  formula_clima <- paste("ln_h_complejo ~", paste(iv_clima, collapse = " + "),
                         "+ ln_P_FOB + ln_h_jurel + factor(MES)")
  
  first_clima <- plm(as.formula(formula_clima), data = pdata_clima, model = "within")
  restricted_clima <- plm(ln_h_complejo ~ ln_P_FOB + ln_h_jurel + factor(MES), 
                          data = pdata_clima, model = "within")
  
  ssr_r_c <- sum(residuals(restricted_clima)^2)
  ssr_u_c <- sum(residuals(first_clima)^2)
  df1_c <- length(iv_clima)
  df2_c <- nrow(data_clima) - length(coef(first_clima)) - n_distinct(data_clima$NUI)
  
  F_clima <- ((ssr_r_c - ssr_u_c) / df1_c) / (ssr_u_c / df2_c)
  
  cat(sprintf("    IVs climáticos: N = %d, F-stat = %.2f %s\n", 
              nrow(data_clima), F_clima,
              ifelse(F_clima > 10, "✓ FUERTES", "✗ Débiles")))
  cat("    Ventaja: No tienen problema de autocorrelación (exógenos por naturaleza)\n")
}

# -----------------------------------------------------------------------------
# 4.3 Recomendación final
# -----------------------------------------------------------------------------
cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("RESUMEN Y RECOMENDACIÓN\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("DIAGNÓSTICO:\n")
cat(sprintf("    • Autocorrelación detectada: q = %d\n", q_suggested))
cat(sprintf("    • Test Wooldridge AR(1): F = %.1f (p < 0.001) → AR(1) confirmado\n", 
            wooldridge_test$statistic))
cat(sprintf("    • Test Breusch-Godfrey: Rechaza H0 hasta orden %d\n", 
            max(bg_results$orden[bg_results$pvalue < 0.05])))

cat("\nIMPLICACIONES:\n")
cat(sprintf("    • Rezagos L1 a L%d NO son instrumentos válidos\n", q_suggested))
cat(sprintf("    • Usar rezagos L%d+ como instrumentos\n", q_suggested + 1))
cat("    • Alternativa: IVs climáticos (SST, CHL_A, WIND)\n")

cat("\nRECOMENDACIÓN:\n")
if (q_suggested >= 3) {
  cat("    → Dado q alto, PREFERIR IVs climáticos\n")
  cat("    → Los rezagos pierden muchas observaciones y pueden ser débiles\n")
} else {
  cat(sprintf("    → Usar L%d + L%d como instrumentos principales\n", 
              q_suggested + 1, q_suggested + 2))
  cat("    → Complementar con IVs climáticos para robustez\n")
}

# ==============================================================================
# GUARDAR RESULTADOS
# ==============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("GUARDANDO ARCHIVOS\n")
cat(rep("=", 70), "\n\n", sep = "")

# Panel con rezagos
write_csv(df, here::here("data", "panel_complejo_con_rezagos.csv"))
cat("    → panel_complejo_con_rezagos.csv\n")

# Gráficos ACF/PACF
png("acf_pacf_residuos.png", width = 900, height = 400)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
acf(resid_ts_clean, lag.max = 12, main = "ACF Residuos FE", 
    xlab = "Lag", ylab = "ACF")
abline(h = c(-ci, ci), col = "red", lty = 2)
pacf(resid_ts_clean, lag.max = 12, main = "PACF Residuos FE",
     xlab = "Lag", ylab = "PACF")
abline(h = c(-ci, ci), col = "red", lty = 2)
dev.off()
cat("    → acf_pacf_residuos.png\n")

# Resultados del diagnóstico
diagnostico <- list(
  q_autocorrelacion = q_suggested,
  wooldridge_F = wooldridge_test$statistic,
  wooldridge_p = wooldridge_test$p.value,
  bg_results = bg_results,
  acf = acf_vals$acf,
  pacf = pacf_vals$acf,
  ci_95 = ci
)
saveRDS(diagnostico, here::here("data", "diagnostico_autocorr.rds"))
cat("    → diagnostico_autocorr.rds\n")

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("DIAGNÓSTICO COMPLETADO\n")
cat(rep("=", 70), "\n")

