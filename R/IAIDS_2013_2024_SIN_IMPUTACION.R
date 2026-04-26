# ============================================================================
# MODELO IAIDS SIMPLE - SIN IMPUTACIÓN (PANEL BALANCEADO)
# Período: 2013-2024
# Método: OLS (sin instrumentos - modelo base)
# Variables exógenas: Solo precio FOB harina
# ============================================================================

library(tidyverse)
library(systemfit)
library(car)

# Función auxiliar para líneas separadoras
linea <- function() {
  cat(paste(rep("=", 80), collapse=""), "\n")
}

# ============================================================================
# 1. CARGAR Y FILTRAR PERÍODO 2013-2024
# ============================================================================

cat("\n")
linea()
cat("MODELO IAIDS SIMPLE - PANEL BALANCEADO (SIN IMPUTACIÓN)\n")
cat("Período: 2013-2024\n")
linea()
cat("\n")

df <- read_csv("base_integrada3_IV.csv") %>%
  filter(ANIO >= 2013)  # Solo 2013-2024

cat("Datos cargados:\n")
cat("  Período:", min(df$ANIO), "-", max(df$ANIO), "\n")
cat("  Observaciones totales:", nrow(df), "\n")
cat("  Especies:", paste(unique(df$NM_RECURSO), collapse=", "), "\n\n")

# ============================================================================
# 2. CREAR TRANSFORMACIONES LOGARÍTMICAS
# ============================================================================

df <- df %>%
  mutate(
    # Logaritmos
    ln_P = log(PRECIO_REAL_W),
    ln_Q = log(Q_MACRO),
    ln_P_HARINA = log(P_HARINA_REAL)
  )

cat("Transformaciones logarítmicas creadas\n\n")

# ============================================================================
# 3. PIVOTAR A FORMATO WIDE
# ============================================================================

df_wide <- df %>%
  select(ANIO, MES, NM_RECURSO, ln_P, ln_Q, ln_P_HARINA) %>%
  pivot_wider(
    names_from = NM_RECURSO,
    values_from = c(ln_P, ln_Q),
    names_sep = "_"
  )

# Renombrar a nombres cortos
df_wide <- df_wide %>%
  rename(
    ln_P_A = ln_P_ANCHOVETA,
    ln_P_J = ln_P_JUREL,
    ln_P_S = `ln_P_SARDINA COMUN`,
    ln_Q_A = ln_Q_ANCHOVETA,
    ln_Q_J = ln_Q_JUREL,
    ln_Q_S = `ln_Q_SARDINA COMUN`
  )

cat("Formato wide creado\n\n")

# ============================================================================
# 4. PANEL BALANCEADO (SIN IMPUTACIÓN)
# ============================================================================

linea()
cat("CREANDO PANEL BALANCEADO\n")
linea()
cat("\n")

# Estadísticas ANTES de balancear
cat("Observaciones por especie (formato wide):\n")
cat("  Anchoveta:", sum(!is.na(df_wide$ln_P_A)), "meses con precio\n")
cat("  Jurel:    ", sum(!is.na(df_wide$ln_P_J)), "meses con precio\n")
cat("  Sardina:  ", sum(!is.na(df_wide$ln_P_S)), "meses con precio\n\n")

# Filtrar solo meses con las 3 especies
df_balanced <- df_wide %>%
  filter(
    !is.na(ln_P_A) & !is.na(ln_Q_A) &
    !is.na(ln_P_J) & !is.na(ln_Q_J) &
    !is.na(ln_P_S) & !is.na(ln_Q_S) &
    !is.na(ln_P_HARINA)
  )

cat("PANEL BALANCEADO:\n")
cat("  Meses con 3 especies simultáneas:", nrow(df_balanced), "\n")
cat("  Período efectivo:", min(df_balanced$ANIO), "-", max(df_balanced$ANIO), "\n\n")

# Verificar si hay suficientes datos
if (nrow(df_balanced) < 30) {
  stop("⚠️ ERROR: Panel balanceado tiene < 30 observaciones. Insuficiente para estimación.")
}

# ============================================================================
# 5. ESTADÍSTICAS DESCRIPTIVAS PANEL BALANCEADO
# ============================================================================

linea()
cat("ESTADÍSTICAS DESCRIPTIVAS (Panel balanceado)\n")
linea()
cat("\n")

# Precios
cat("PRECIOS (log):\n")
cat("  Anchoveta: Media =", round(mean(df_balanced$ln_P_A), 3), 
    " SD =", round(sd(df_balanced$ln_P_A), 3), "\n")
cat("  Jurel:     Media =", round(mean(df_balanced$ln_P_J), 3), 
    " SD =", round(sd(df_balanced$ln_P_J), 3), "\n")
cat("  Sardina:   Media =", round(mean(df_balanced$ln_P_S), 3), 
    " SD =", round(sd(df_balanced$ln_P_S), 3), "\n\n")

# Cantidades
cat("CANTIDADES (log):\n")
cat("  Anchoveta: Media =", round(mean(df_balanced$ln_Q_A), 3), 
    " SD =", round(sd(df_balanced$ln_Q_A), 3), "\n")
cat("  Jurel:     Media =", round(mean(df_balanced$ln_Q_J), 3), 
    " SD =", round(sd(df_balanced$ln_Q_J), 3), "\n")
cat("  Sardina:   Media =", round(mean(df_balanced$ln_Q_S), 3), 
    " SD =", round(sd(df_balanced$ln_Q_S), 3), "\n\n")

# Precio FOB
cat("PRECIO FOB HARINA (log):\n")
cat("  Media =", round(mean(df_balanced$ln_P_HARINA), 3), 
    " SD =", round(sd(df_balanced$ln_P_HARINA), 3), "\n\n")

# ============================================================================
# 6. ESPECIFICACIÓN DEL SISTEMA IAIDS SIMPLE
# ============================================================================

linea()
cat("ESPECIFICACIÓN DEL MODELO\n")
linea()
cat("\n")

cat("Sistema IAIDS SIMPLE (sin instrumentos):\n")
cat("  ln_P_i = α_i + γ_i1·ln_Q_A + γ_i2·ln_Q_J + γ_i3·ln_Q_S + γ_FM·ln_P_HARINA + ε_i\n\n")

cat("Características:\n")
cat("  ✓ Sin instrumentos (cantidades tratadas como exógenas)\n")
cat("  ✓ Solo precio FOB como variable exógena adicional\n")
cat("  ✓ Estimación OLS (baseline)\n")
cat("  ✓ Sin restricciones de simetría/homogeneidad\n\n")

# Ecuaciones
eq_A <- ln_P_A ~ ln_Q_A + ln_Q_J + ln_Q_S + ln_P_HARINA
eq_J <- ln_P_J ~ ln_Q_A + ln_Q_J + ln_Q_S + ln_P_HARINA
eq_S <- ln_P_S ~ ln_Q_A + ln_Q_J + ln_Q_S + ln_P_HARINA

sistema <- list(anchoveta = eq_A, jurel = eq_J, sardina = eq_S)

cat("Ecuaciones definidas:\n")
cat("  [1] Anchoveta\n")
cat("  [2] Jurel\n")
cat("  [3] Sardina\n\n")

# ============================================================================
# 7. ESTIMACIÓN OLS (SIN RESTRICCIONES)
# ============================================================================

linea()
cat("ESTIMANDO MODELO OLS (SIN RESTRICCIONES)\n")
linea()
cat("\n")

modelo_ols <- systemfit(
  formula = sistema,
  method = "OLS",
  data = df_balanced
)

cat("Estimación completada\n\n")

# ============================================================================
# 8. RESULTADOS
# ============================================================================

linea()
cat("RESULTADOS DE ESTIMACIÓN\n")
linea()
cat("\n")

# Resumen completo
print(summary(modelo_ols))

# ============================================================================
# 9. EXTRACCIÓN DE COEFICIENTES CLAVE
# ============================================================================

cat("\n")
linea()
cat("MATRIZ DE FLEXIBILIDADES (γ_ij)\n")
linea()
cat("\n")

coefs <- coef(modelo_ols)

# Extraer gammas por posición
# Estructura: (Intercept), ln_Q_A, ln_Q_J, ln_Q_S, ln_P_HARINA
gamma_A_A <- coefs["anchoveta_ln_Q_A"]
gamma_A_J <- coefs["anchoveta_ln_Q_J"]
gamma_A_S <- coefs["anchoveta_ln_Q_S"]
gamma_FM_A <- coefs["anchoveta_ln_P_HARINA"]

gamma_J_A <- coefs["jurel_ln_Q_A"]
gamma_J_J <- coefs["jurel_ln_Q_J"]
gamma_J_S <- coefs["jurel_ln_Q_S"]
gamma_FM_J <- coefs["jurel_ln_P_HARINA"]

gamma_S_A <- coefs["sardina_ln_Q_A"]
gamma_S_J <- coefs["sardina_ln_Q_J"]
gamma_S_S <- coefs["sardina_ln_Q_S"]
gamma_FM_S <- coefs["sardina_ln_P_HARINA"]

# Crear matriz de flexibilidades
matriz_gamma <- matrix(
  c(gamma_A_A, gamma_A_J, gamma_A_S,
    gamma_J_A, gamma_J_J, gamma_J_S,
    gamma_S_A, gamma_S_J, gamma_S_S),
  nrow = 3, byrow = TRUE,
  dimnames = list(
    c("P_Anchoveta", "P_Jurel", "P_Sardina"),
    c("Q_Anch", "Q_Jurel", "Q_Sard")
  )
)

cat("Flexibilidades precio-cantidad:\n\n")
print(round(matriz_gamma, 4))
cat("\n")

# Sumas de fila (verificación homogeneidad)
cat("Suma por fila (Homogeneidad: debe ser ≈ 0):\n")
cat("  Anchoveta:", round(sum(matriz_gamma[1,]), 4), "\n")
cat("  Jurel:    ", round(sum(matriz_gamma[2,]), 4), "\n")
cat("  Sardina:  ", round(sum(matriz_gamma[3,]), 4), "\n\n")

# ============================================================================
# 10. ELASTICIDADES PRECIO FOB
# ============================================================================

linea()
cat("ELASTICIDADES PRECIO FOB HARINA (γ_FM)\n")
linea()
cat("\n")

cat("Transmisión precio internacional → precio local:\n\n")
cat("  Anchoveta: ", round(gamma_FM_A, 4), "\n")
cat("  Jurel:     ", round(gamma_FM_J, 4), "\n")
cat("  Sardina:   ", round(gamma_FM_S, 4), "\n\n")

cat("Interpretación:\n")
cat("  Si P_FOB ↑10% → P_Anchoveta ↑", round(gamma_FM_A * 10, 2), "%\n")
cat("  Si P_FOB ↑10% → P_Jurel ↑", round(gamma_FM_J * 10, 2), "%\n")
cat("  Si P_FOB ↑10% → P_Sardina ↑", round(gamma_FM_S * 10, 2), "%\n\n")

# ============================================================================
# 11. VERIFICACIÓN DE RESTRICCIONES (informativa)
# ============================================================================

linea()
cat("VERIFICACIÓN DE RESTRICCIONES TEÓRICAS (sin imponer)\n")
linea()
cat("\n")

# Simetría
cat("SIMETRÍA (γ_ij - γ_ji):\n")
cat("  γ_AJ - γ_JA = ", round(gamma_A_J - gamma_J_A, 4), "\n")
cat("  γ_AS - γ_SA = ", round(gamma_A_S - gamma_S_A, 4), "\n")
cat("  γ_JS - γ_SJ = ", round(gamma_J_S - gamma_S_J, 4), "\n\n")

# ============================================================================
# 12. DIAGNÓSTICO DEL MODELO
# ============================================================================

linea()
cat("DIAGNÓSTICO DEL MODELO\n")
linea()
cat("\n")

# R² por ecuación
r2_A <- summary(modelo_ols)$eq[[1]]$r.squared
r2_J <- summary(modelo_ols)$eq[[2]]$r.squared
r2_S <- summary(modelo_ols)$eq[[3]]$r.squared

cat("R² por ecuación:\n")
cat("  Anchoveta:", round(r2_A, 4), "\n")
cat("  Jurel:    ", round(r2_J, 4), "\n")
cat("  Sardina:  ", round(r2_S, 4), "\n\n")

# RMSE
rmse_A <- sqrt(summary(modelo_ols)$eq[[1]]$ssr / summary(modelo_ols)$eq[[1]]$df[2])
rmse_J <- sqrt(summary(modelo_ols)$eq[[2]]$ssr / summary(modelo_ols)$eq[[2]]$df[2])
rmse_S <- sqrt(summary(modelo_ols)$eq[[3]]$ssr / summary(modelo_ols)$eq[[3]]$df[2])

cat("RMSE (en logs):\n")
cat("  Anchoveta:", round(rmse_A, 4), "\n")
cat("  Jurel:    ", round(rmse_J, 4), "\n")
cat("  Sardina:  ", round(rmse_S, 4), "\n\n")

# ============================================================================
# 13. GUARDAR RESULTADOS
# ============================================================================

linea()
cat("GUARDANDO RESULTADOS\n")
linea()
cat("\n")

# Modelo
saveRDS(modelo_ols, "modelo_iaids_ols_sin_imputacion.rds")

# Coeficientes
write.csv(
  data.frame(
    Coeficiente = names(coefs),
    Valor = as.numeric(coefs)
  ),
  "coeficientes_ols_sin_imputacion.csv",
  row.names = FALSE
)

# Matriz flexibilidades
write.csv(matriz_gamma, "matriz_flexibilidades_sin_imputacion.csv")

# Elasticidades FOB
write.csv(
  data.frame(
    Especie = c("Anchoveta", "Jurel", "Sardina"),
    Elasticidad_FOB = c(gamma_FM_A, gamma_FM_J, gamma_FM_S)
  ),
  "elasticidades_fob_sin_imputacion.csv",
  row.names = FALSE
)

# R² y diagnóstico
write.csv(
  data.frame(
    Ecuacion = c("Anchoveta", "Jurel", "Sardina"),
    R2 = c(r2_A, r2_J, r2_S),
    RMSE = c(rmse_A, rmse_J, rmse_S),
    N = rep(nrow(df_balanced), 3)
  ),
  "diagnostico_sin_imputacion.csv",
  row.names = FALSE
)


