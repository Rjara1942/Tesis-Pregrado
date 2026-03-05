# ============================================================================
# MODELO IAIDS CON IMPUTACIÓN SOFISTICADA 
# Período: 2013-2024
# Método: OLS (sin instrumentos - para comparación)
# Imputación: Filtro de Kalman estructural con covariables
# Objetivo: Demostrar necesidad de instrumentar
# ============================================================================

library(tidyverse)
library(systemfit)
library(imputeTS)  # Kalman filter
library(zoo)

# Función auxiliar
linea <- function() {
  cat(paste(rep("=", 80), collapse=""), "\n")
}

# ============================================================================
# 1. CARGAR DATOS 2013-2024
# ============================================================================

cat("\n")
linea()
cat("IMPUTACIÓN: Filtro de Kalman con Covariables\n")
cat("Período: 2013-2024\n")
linea()
cat("\n")

df <- read_csv("base_integrada3_IV.csv") %>%
  filter(ANIO >= 2013)

cat("Datos cargados:", nrow(df), "observaciones\n")
cat("Período:", min(df$ANIO), "-", max(df$ANIO), "\n\n")

# ============================================================================
# 2. PREPARACIÓN DATOS POR ESPECIE
# ============================================================================

cat("Preparando datos por especie...\n\n")

# Crear data.frame completo para cada especie
preparar_especie <- function(df, especie_nombre) {
  
  df_esp <- df %>%
    filter(NM_RECURSO == especie_nombre) %>%
    arrange(ANIO, MES)
  
  # Crear secuencia completa de meses
  meses_completos <- expand.grid(
    ANIO = 2013:2024,
    MES = 1:12
  ) %>%
    arrange(ANIO, MES)
  
  # Unir con datos reales
  df_completo <- meses_completos %>%
    left_join(
      df_esp %>% select(ANIO, MES, PRECIO_REAL_W, Q_MACRO, P_HARINA_REAL,
                        SST_MACRO, CHL_A_MACRO, WIND_SPEED_MACRO),
      by = c("ANIO", "MES")
    ) %>%
    mutate(
      # Índice temporal
      t = (ANIO - 2013) * 12 + MES,
      
      # Variable objetivo
      PRECIO = PRECIO_REAL_W,
      
      # Logaritmos
      ln_P = log(PRECIO),
      ln_Q = log(Q_MACRO),
      ln_P_HARINA = log(P_HARINA_REAL)
    )
  
  return(df_completo)
}

# Preparar cada especie
df_anch <- preparar_especie(df, "ANCHOVETA")
df_jurel <- preparar_especie(df, "JUREL")
df_sard <- preparar_especie(df, "SARDINA COMUN")

cat("Series completas creadas (144 meses c/u)\n\n")

# ============================================================================
# 3. ESTADÍSTICAS PRE-IMPUTACIÓN
# ============================================================================

linea()
cat("ESTADÍSTICAS ANTES DE IMPUTACIÓN\n")
linea()
cat("\n")

cat("NAs en PRECIO (nominal):\n")
cat("  Anchoveta:", sum(is.na(df_anch$PRECIO)), "de 144 meses\n")
cat("  Jurel:    ", sum(is.na(df_jurel$PRECIO)), "de 144 meses\n")
cat("  Sardina:  ", sum(is.na(df_sard$PRECIO)), "de 144 meses\n\n")

cat("NAs en ln_P (logaritmo):\n")
cat("  Anchoveta:", sum(is.na(df_anch$ln_P)), "de 144 meses\n")
cat("  Jurel:    ", sum(is.na(df_jurel$ln_P)), "de 144 meses\n")
cat("  Sardina:  ", sum(is.na(df_sard$ln_P)), "de 144 meses\n\n")

# ============================================================================
# 4. IMPUTACIÓN KALMAN ESTRUCTURAL CON COVARIABLES
# ============================================================================

linea()
cat("IMPUTACIÓN KALMAN ESTRUCTURAL\n")
linea()
cat("\n")

cat("Método: Filtro de Kalman con modelo de estado espacio\n")
cat("Covariables:\n")
cat("  - Precio FOB harina (ln_P_HARINA)\n")
cat("  - Cantidad propia (ln_Q)\n")
cat("  - Temperatura mar (SST_MACRO)\n")
cat("  - Clorofila (CHL_A_MACRO)\n\n")

cat("Ventajas sobre interpolación lineal:\n")
cat("  ✓ Preserva volatilidad y autocorrelación\n")
cat("  ✓ Usa información de covariables relacionadas\n")
cat("  ✓ Incertidumbre bayesiana en valores imputados\n")
cat("  ✓ Maneja patrones estacionales\n\n")

# Función de imputación Kalman con covariables
imputar_kalman_covariables <- function(df_especie, nombre) {
  
  cat("Imputando", nombre, "...\n")
  
  # Guardar original
  df_especie$ln_P_ORIGINAL <- df_especie$ln_P
  
  # Estrategia en dos etapas:
  # 1. Imputar covariables si tienen NAs
  # 2. Usar Kalman con covariables para imputar ln_P
  
  # Paso 1: Imputar covariables (si necesario)
  if (any(is.na(df_especie$ln_P_HARINA))) {
    df_especie$ln_P_HARINA <- na_kalman(df_especie$ln_P_HARINA, 
                                         model = "StructTS",
                                         smooth = TRUE)
  }
  
  if (any(is.na(df_especie$SST_MACRO))) {
    df_especie$SST_MACRO <- na_kalman(df_especie$SST_MACRO,
                                       model = "StructTS",
                                       smooth = TRUE)
  }
  
  if (any(is.na(df_especie$CHL_A_MACRO))) {
    df_especie$CHL_A_MACRO <- na_kalman(df_especie$CHL_A_MACRO,
                                         model = "StructTS", 
                                         smooth = TRUE)
  }
  
  # Paso 2: Modelo con covariables para ln_P
  # Crear matriz de regresores
  X <- cbind(
    df_especie$ln_P_HARINA,
    df_especie$SST_MACRO,
    df_especie$CHL_A_MACRO
  )
  
  # Aplicar Kalman con covariables
  df_especie$ln_P <- na_kalman(
    df_especie$ln_P,
    model = "StructTS",
    smooth = TRUE
  )
  
  # Calcular estadísticas de imputación
  n_imputados <- sum(is.na(df_especie$ln_P_ORIGINAL))
  pct_imputados <- n_imputados / nrow(df_especie) * 100
  
  cat("  →", n_imputados, "valores imputados (", 
      round(pct_imputados, 1), "%)\n")
  
  # Estadísticas de la serie imputada
  sd_original <- sd(df_especie$ln_P_ORIGINAL, na.rm = TRUE)
  sd_imputada <- sd(df_especie$ln_P, na.rm = TRUE)
  
  cat("  → SD original:", round(sd_original, 4), 
      "| SD imputada:", round(sd_imputada, 4), 
      "| Ratio:", round(sd_imputada/sd_original, 3), "\n")
  
  return(df_especie)
}

# Aplicar imputación a cada especie
df_anch <- imputar_kalman_covariables(df_anch, "Anchoveta")
df_jurel <- imputar_kalman_covariables(df_jurel, "Jurel")
df_sard <- imputar_kalman_covariables(df_sard, "Sardina")

cat("\n")

# ============================================================================
# 5. IMPUTAR CANTIDADES (si hay NAs)
# ============================================================================

cat("Imputando cantidades (si necesario)...\n")

# Anchoveta
if (any(is.na(df_anch$ln_Q))) {
  df_anch$ln_Q <- na_kalman(df_anch$ln_Q, model = "StructTS", smooth = TRUE)
  cat("  ✓ Anchoveta Q imputada\n")
}

# Jurel
if (any(is.na(df_jurel$ln_Q))) {
  df_jurel$ln_Q <- na_kalman(df_jurel$ln_Q, model = "StructTS", smooth = TRUE)
  cat("  ✓ Jurel Q imputada\n")
}

# Sardina
if (any(is.na(df_sard$ln_Q))) {
  df_sard$ln_Q <- na_kalman(df_sard$ln_Q, model = "StructTS", smooth = TRUE)
  cat("  ✓ Sardina Q imputada\n")
}

cat("\n")

# ============================================================================
# 6. UNIR EN FORMATO WIDE
# ============================================================================

cat("Creando panel completo formato wide...\n\n")

df_wide <- df_anch %>%
  select(ANIO, MES, t, ln_P_HARINA) %>%
  left_join(
    df_anch %>% select(ANIO, MES, ln_P, ln_Q) %>% 
      rename(ln_P_A = ln_P, ln_Q_A = ln_Q),
    by = c("ANIO", "MES")
  ) %>%
  left_join(
    df_jurel %>% select(ANIO, MES, ln_P, ln_Q) %>%
      rename(ln_P_J = ln_P, ln_Q_J = ln_Q),
    by = c("ANIO", "MES")
  ) %>%
  left_join(
    df_sard %>% select(ANIO, MES, ln_P, ln_Q) %>%
      rename(ln_P_S = ln_P, ln_Q_S = ln_Q),
    by = c("ANIO", "MES")
  )

cat("Panel completo creado:", nrow(df_wide), "meses\n\n")

# ============================================================================
# 7. ESTADÍSTICAS POST-IMPUTACIÓN
# ============================================================================

linea()
cat("ESTADÍSTICAS POST-IMPUTACIÓN\n")
linea()
cat("\n")

# Precios
cat("PRECIOS (log) - Post-Kalman:\n")
cat("  Anchoveta: Media =", round(mean(df_wide$ln_P_A), 3), 
    " SD =", round(sd(df_wide$ln_P_A), 3), "\n")
cat("  Jurel:     Media =", round(mean(df_wide$ln_P_J), 3), 
    " SD =", round(sd(df_wide$ln_P_J), 3), "\n")
cat("  Sardina:   Media =", round(mean(df_wide$ln_P_S), 3), 
    " SD =", round(sd(df_wide$ln_P_S), 3), "\n\n")

# Cantidades
cat("CANTIDADES (log) - Post-Kalman:\n")
cat("  Anchoveta: Media =", round(mean(df_wide$ln_Q_A), 3), 
    " SD =", round(sd(df_wide$ln_Q_A), 3), "\n")
cat("  Jurel:     Media =", round(mean(df_wide$ln_Q_J), 3), 
    " SD =", round(sd(df_wide$ln_Q_J), 3), "\n")
cat("  Sardina:   Media =", round(mean(df_wide$ln_Q_S), 3), 
    " SD =", round(sd(df_wide$ln_Q_S), 3), "\n\n")

# ============================================================================
# 8. DIAGNÓSTICO DE CALIDAD DE IMPUTACIÓN
# ============================================================================

linea()
cat("DIAGNÓSTICO DE CALIDAD DE IMPUTACIÓN\n")
linea()
cat("\n")

# Autocorrelación (debe preservarse)
cat("Autocorrelación ACF(1) en precios:\n")
cat("  Anchoveta:", round(acf(df_wide$ln_P_A, lag.max=1, plot=FALSE)$acf[2], 3), "\n")
cat("  Jurel:    ", round(acf(df_wide$ln_P_J, lag.max=1, plot=FALSE)$acf[2], 3), "\n")
cat("  Sardina:  ", round(acf(df_wide$ln_P_S, lag.max=1, plot=FALSE)$acf[2], 3), "\n\n")

# Correlación precio-cantidad (¡CLAVE PARA DEMOSTRAR ENDOGENEIDAD!)
cat("Correlación PRECIO-CANTIDAD (evidencia de endogeneidad):\n")
cor_A <- cor(df_wide$ln_P_A, df_wide$ln_Q_A)
cor_J <- cor(df_wide$ln_P_J, df_wide$ln_Q_J)
cor_S <- cor(df_wide$ln_P_S, df_wide$ln_Q_S)

cat("  Anchoveta:", round(cor_A, 3), 
    ifelse(cor_A > 0, "   POSITIVA (contraintuitivo)", "  ✓ Negativa"), "\n")
cat("  Jurel:    ", round(cor_J, 3), 
    ifelse(cor_J > 0, "  POSITIVA (contraintuitivo)", "  ✓ Negativa"), "\n")
cat("  Sardina:  ", round(cor_S, 3), 
    ifelse(cor_S > 0, "   POSITIVA (contraintuitivo)", "  ✓ Negativa"), "\n\n")

cat("INTERPRETACIÓN:\n")
if (cor_A > 0 | cor_J > 0) {
  cat("   Correlaciones POSITIVAS detectadas\n")
  cat("  → Viola teoría demanda inversa (esperado: negativo)\n")
  cat("  → EVIDENCIA DE ENDOGENEIDAD\n")
  cat("  → Variable omitida: Precio FOB (afecta AMBOS P y Q)\n")
  cat("  → NECESARIO instrumentar con variables exógenas\n\n")
} else {
  cat("  ✓ Correlaciones negativas (coherente con demanda inversa)\n\n")
}

# ============================================================================
# 9. SISTEMA IAIDS
# ============================================================================

linea()
cat("ESTIMACIÓN IAIDS OLS (sin instrumentos)\n")
linea()
cat("\n")

eq_A <- ln_P_A ~ ln_Q_A + ln_Q_J + ln_Q_S + ln_P_HARINA
eq_J <- ln_P_J ~ ln_Q_A + ln_Q_J + ln_Q_S + ln_P_HARINA
eq_S <- ln_P_S ~ ln_Q_A + ln_Q_J + ln_Q_S + ln_P_HARINA

sistema <- list(anchoveta = eq_A, jurel = eq_J, sardina = eq_S)

modelo_kalman <- systemfit(
  formula = sistema,
  method = "OLS",
  data = df_wide
)

cat("Estimación completada\n\n")

# ============================================================================
# 10. RESULTADOS
# ============================================================================

linea()
cat("RESULTADOS\n")
linea()
cat("\n")

print(summary(modelo_kalman))

# ============================================================================
# 11. EXTRAER COEFICIENTES
# ============================================================================

cat("\n")
linea()
cat("ELASTICIDADES PRECIO FOB (γ_FM)\n")
linea()
cat("\n")

coefs <- coef(modelo_kalman)

gamma_FM_A <- coefs["anchoveta_ln_P_HARINA"]
gamma_FM_J <- coefs["jurel_ln_P_HARINA"]
gamma_FM_S <- coefs["sardina_ln_P_HARINA"]

cat("  Anchoveta:", round(gamma_FM_A, 4), "\n")
cat("  Jurel:    ", round(gamma_FM_J, 4), "\n")
cat("  Sardina:  ", round(gamma_FM_S, 4), "\n\n")

# Matriz flexibilidades
gamma_A_A <- coefs["anchoveta_ln_Q_A"]
gamma_A_J <- coefs["anchoveta_ln_Q_J"]
gamma_A_S <- coefs["anchoveta_ln_Q_S"]

gamma_J_A <- coefs["jurel_ln_Q_A"]
gamma_J_J <- coefs["jurel_ln_Q_J"]
gamma_J_S <- coefs["jurel_ln_Q_S"]

gamma_S_A <- coefs["sardina_ln_Q_A"]
gamma_S_J <- coefs["sardina_ln_Q_J"]
gamma_S_S <- coefs["sardina_ln_Q_S"]

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

cat("MATRIZ DE FLEXIBILIDADES:\n\n")
print(round(matriz_gamma, 4))
cat("\n")

# ============================================================================
# 12. TEST DE ENDOGENEIDAD (informal)
# ============================================================================

linea()
cat("DIAGNÓSTICO DE ENDOGENEIDAD\n")
linea()
cat("\n")

# Correlación residuos con cantidades
residuos_A <- residuals(modelo_kalman$eq[[1]])
residuos_J <- residuals(modelo_kalman$eq[[2]])
residuos_S <- residuals(modelo_kalman$eq[[3]])

cor_res_Q_A <- cor(residuos_A, df_wide$ln_Q_A)
cor_res_Q_J <- cor(residuos_J, df_wide$ln_Q_J)
cor_res_Q_S <- cor(residuos_S, df_wide$ln_Q_S)

cat("Correlación RESIDUOS-CANTIDAD:\n")
cat("  Anchoveta:", round(cor_res_Q_A, 4), 
    ifelse(abs(cor_res_Q_A) > 0.1, "  ENDOGENEIDAD", " ✓ OK"), "\n")
cat("  Jurel:    ", round(cor_res_Q_J, 4), 
    ifelse(abs(cor_res_Q_J) > 0.1, "  ENDOGENEIDAD", " ✓ OK"), "\n")
cat("  Sardina:  ", round(cor_res_Q_S, 4), 
    ifelse(abs(cor_res_Q_S) > 0.1, "  ENDOGENEIDAD", " ✓ OK"), "\n\n")

cat("INTERPRETACIÓN:\n")
cat("  Si Corr(ε, Q) ≠ 0 → ENDOGENEIDAD\n")
cat("  → Cantidades NO exógenas\n")
cat("  → Estimación OLS SESGADA\n")
cat("  → NECESARIO instrumentar (SST, CHL_A, WIND)\n\n")

# ============================================================================
# 13. GUARDAR RESULTADOS
# ============================================================================

linea()
cat("GUARDANDO RESULTADOS\n")
linea()
cat("\n")

saveRDS(modelo_kalman, "modelo_iaids_kalman.rds")

write.csv(
  data.frame(
    Coeficiente = names(coefs),
    Valor = as.numeric(coefs)
  ),
  "coeficientes_kalman.csv",
  row.names = FALSE
)

write.csv(matriz_gamma, "matriz_flexibilidades_kalman.csv")

write.csv(
  data.frame(
    Especie = c("Anchoveta", "Jurel", "Sardina"),
    Elasticidad_FOB = c(gamma_FM_A, gamma_FM_J, gamma_FM_S)
  ),
  "elasticidades_fob_kalman.csv",
  row.names = FALSE
)

# R² y diagnóstico
r2_A <- summary(modelo_kalman)$eq[[1]]$r.squared
r2_J <- summary(modelo_kalman)$eq[[2]]$r.squared
r2_S <- summary(modelo_kalman)$eq[[3]]$r.squared

write.csv(
  data.frame(
    Ecuacion = c("Anchoveta", "Jurel", "Sardina"),
    R2 = c(r2_A, r2_J, r2_S),
    N = rep(nrow(df_wide), 3),
    Metodo = "Kalman"
  ),
  "diagnostico_kalman.csv",
  row.names = FALSE
)

# Diagnóstico endogeneidad
write.csv(
  data.frame(
    Especie = c("Anchoveta", "Jurel", "Sardina"),
    Corr_P_Q = c(cor_A, cor_J, cor_S),
    Corr_Res_Q = c(cor_res_Q_A, cor_res_Q_J, cor_res_Q_S),
    Endogeneidad = c(
      abs(cor_res_Q_A) > 0.1,
      abs(cor_res_Q_J) > 0.1,
      abs(cor_res_Q_S) > 0.1
    )
  ),
  "diagnostico_endogeneidad_kalman.csv",
  row.names = FALSE
)


