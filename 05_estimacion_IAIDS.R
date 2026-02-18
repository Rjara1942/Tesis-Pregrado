################################################################################
# 05_ESTIMACION_IAIDS.R
# Objetivo: Estimar Inverse Almost Ideal Demand System (IAIDS) en dos etapas
# Autor: Ricardo Jara Valencia
# Fecha: Febrero 2026
################################################################################

library(tidyverse)
library(systemfit)
library(lmtest)
library(car)
library(lubridate)

cat("\n", rep("=", 80), "\n")
cat("ESTIMACIÓN IAIDS: FORMACIÓN DE PRECIOS EX-VESSEL\n")
cat(rep("=", 80), "\n\n")

# ==============================================================================
# 1. CARGAR Y TRANSFORMAR DATOS A FORMATO WIDE
# ==============================================================================

cat("=== 1. CARGANDO Y TRANSFORMANDO DATOS ===\n\n")

# Cargar base integrada (formato largo)
df_long <- read_csv("base_integrada.csv", show_col_types = FALSE)

cat("Base original (formato largo):\n")
cat("  Observaciones:", nrow(df_long), "\n")
cat("  Período:", min(df_long$ANIO), "-", max(df_long$ANIO), "\n")
cat("  Especies:", unique(df_long$NM_RECURSO), "\n\n")

# Transformar a formato wide (una fila por mes)
df_wide <- df_long %>%
  select(ANIO, MES, FECHA, NM_RECURSO, 
         PRECIO_REAL_MACRO, Q_MACRO, 
         SHARE_INDUSTRIAL, N_OUTLIERS_CANTIDAD,
         P_HARINA_REAL) %>%
  
  pivot_wider(
    names_from = NM_RECURSO,
    values_from = c(PRECIO_REAL_MACRO, Q_MACRO, SHARE_INDUSTRIAL, N_OUTLIERS_CANTIDAD),
    names_sep = "_"
  ) %>%
  
  # Crear variables agregadas y de control
  mutate(
    # Total desembarques (H)
    Q_TOTAL = rowSums(select(., starts_with("Q_MACRO_")), na.rm = TRUE),
    
    # Tendencia temporal
    TENDENCIA = (ANIO - 2012) * 12 + MES,  # Meses desde inicio
    
    # Dummies estacionales
    D_VEDA = (MES %in% c(8, 9)),  # Agosto-Septiembre
    D_TEMPORADA_ALTA = (MES %in% c(3, 4, 5, 6, 7)),  # Marzo-Julio
    D_VERANO = (MES %in% c(12, 1, 2)),
    
    # Trimestre
    TRIMESTRE = case_when(
      MES %in% 1:3 ~ 1,
      MES %in% 4:6 ~ 2,
      MES %in% 7:9 ~ 3,
      MES %in% 10:12 ~ 4
    ),
    
    # Factores para efectos fijos
    FACTOR_MES = factor(MES),
    FACTOR_TRIMESTRE = factor(TRIMESTRE),
    FACTOR_ANO = factor(ANIO),
    
    # Flags de disponibilidad
    tiene_ANCHOVETA = !is.na(PRECIO_REAL_MACRO_ANCHOVETA),
    tiene_JUREL = !is.na(PRECIO_REAL_MACRO_JUREL),
    tiene_SARDINA = !is.na(`PRECIO_REAL_MACRO_SARDINA COMUN`),
    
    N_ESPECIES = tiene_ANCHOVETA + tiene_JUREL + tiene_SARDINA
  )

cat("Base transformada (formato wide):\n")
cat("  Observaciones:", nrow(df_wide), "\n")
cat("  Meses únicos:", n_distinct(df_wide$FECHA), "\n")
cat("  Distribución por N especies:\n")
print(table(df_wide$N_ESPECIES))

# ==============================================================================
# 2. CREAR VARIABLES LOGARÍTMICAS
# ==============================================================================

cat("\n=== 2. CREANDO VARIABLES LOGARÍTMICAS ===\n\n")

df_wide <- df_wide %>%
  mutate(
    # Precios (variable dependiente)
    ln_P_ANCHOVETA = log(PRECIO_REAL_MACRO_ANCHOVETA),
    ln_P_JUREL = log(PRECIO_REAL_MACRO_JUREL),
    ln_P_SARDINA = log(`PRECIO_REAL_MACRO_SARDINA COMUN`),
    
    # Cantidades (variables endógenas)
    ln_Q_ANCHOVETA = log(Q_MACRO_ANCHOVETA + 1),  # +1 para evitar log(0)
    ln_Q_JUREL = log(Q_MACRO_JUREL + 1),
    ln_Q_SARDINA = log(`Q_MACRO_SARDINA COMUN` + 1),
    
    # Total
    ln_Q_TOTAL = log(Q_TOTAL + 1),
    
    # Precio harina internacional
    ln_P_HARINA = log(P_HARINA_REAL),
    
    # Shares de flota (para controles)
    SHARE_IND_ANCH = SHARE_INDUSTRIAL_ANCHOVETA,
    SHARE_IND_JUR = SHARE_INDUSTRIAL_JUREL,
    SHARE_IND_SARD = `SHARE_INDUSTRIAL_SARDINA COMUN`
  )

# ==============================================================================
# 3. CREAR INSTRUMENTOS (VARIABLES EXÓGENAS DE OFERTA)
# ==============================================================================

cat("=== 3. CREANDO INSTRUMENTOS ===\n\n")

# NOTA: Aquí usarás las variables climáticas cuando las tengas
# Por ahora, usamos:
# - Rezagos de cantidades (predeterminados)
# - Dummies de veda (exógenas)
# - Tendencia temporal

df_wide <- df_wide %>%
  arrange(ANIO, MES) %>%
  mutate(
    # Rezagos de cantidades (son predeterminadas, válidas como IV)
    ln_Q_ANCHOVETA_LAG1 = lag(ln_Q_ANCHOVETA, 1),
    ln_Q_JUREL_LAG1 = lag(ln_Q_JUREL, 1),
    ln_Q_SARDINA_LAG1 = lag(ln_Q_SARDINA, 1),
    
    # Rezago del precio harina (puede afectar decisiones con delay)
    ln_P_HARINA_LAG1 = lag(ln_P_HARINA, 1),
    
    # Interacciones de veda con tendencia (captura cambios en efectividad)
    D_VEDA_X_TREND = D_VEDA * TENDENCIA
  )

cat("Instrumentos disponibles:\n")
cat("  - Rezagos de cantidades (t-1)\n")
cat("  - Dummies de veda y temporada\n")
cat("  - Tendencia temporal\n")
cat("  - Rezago precio harina\n")
cat("  - Factor mes (efectos fijos)\n\n")

cat("NOTA: Cuando agregues SST, Chl-a, y precio diesel, inclúyelos aquí.\n\n")

# ==============================================================================
# 4. FILTRAR BASE COMPLETA (SOLO MESES CON 3 ESPECIES)
# ==============================================================================

cat("=== 4. PREPARANDO BASES DE ESTIMACIÓN ===\n\n")

# Base balanceada (solo meses con 3 especies)
df_balanceada <- df_wide %>%
  filter(N_ESPECIES == 3) %>%
  drop_na(ln_P_ANCHOVETA, ln_P_JUREL, ln_P_SARDINA,
          ln_Q_ANCHOVETA, ln_Q_JUREL, ln_Q_SARDINA)

cat("Base BALANCEADA (3 especies):\n")
cat("  Observaciones:", nrow(df_balanceada), "\n")
cat("  Período:", min(df_balanceada$ANIO), "-", max(df_balanceada$ANIO), "\n\n")

# Base completa (todas las obs con al menos precio y cantidad)
df_completa <- df_wide %>%
  filter(N_ESPECIES >= 1)

cat("Base COMPLETA (1+ especies):\n")
cat("  Observaciones:", nrow(df_completa), "\n")
cat("  Período:", min(df_completa$ANIO), "-", max(df_completa$ANIO), "\n\n")

# Verificar valores faltantes en variables clave
cat("Valores faltantes en variables clave (base balanceada):\n")
vars_clave <- c("ln_P_ANCHOVETA", "ln_P_JUREL", "ln_P_SARDINA",
                "ln_Q_ANCHOVETA", "ln_Q_JUREL", "ln_Q_SARDINA",
                "ln_P_HARINA", "D_VEDA")
sapply(df_balanceada[vars_clave], function(x) sum(is.na(x))) %>% print()

# ==============================================================================
# 5. ETAPA 1: MODELO SIN PRECIO INTERNACIONAL
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("ETAPA 1: MODELO SIN PRECIO INTERNACIONAL (DINÁMICA LOCAL)\n")
cat(rep("=", 80), "\n\n")

# Especificar sistema de 3 ecuaciones
# NOTA: Omitimos ln_Q_TOTAL para evitar multicolinealidad perfecta

eq_anchoveta_1 <- ln_P_ANCHOVETA ~ 
  ln_Q_ANCHOVETA + ln_Q_JUREL + ln_Q_SARDINA +
  TENDENCIA + D_VEDA + D_TEMPORADA_ALTA +
  FACTOR_MES + SHARE_IND_ANCH

eq_jurel_1 <- ln_P_JUREL ~ 
  ln_Q_ANCHOVETA + ln_Q_JUREL + ln_Q_SARDINA +
  TENDENCIA + D_VEDA + D_TEMPORADA_ALTA +
  FACTOR_MES + SHARE_IND_JUR

eq_sardina_1 <- ln_P_SARDINA ~ 
  ln_Q_ANCHOVETA + ln_Q_JUREL + ln_Q_SARDINA +
  TENDENCIA + D_VEDA + D_TEMPORADA_ALTA +
  FACTOR_MES + SHARE_IND_SARD

sistema_1 <- list(
  anchoveta = eq_anchoveta_1,
  jurel = eq_jurel_1,
  sardina = eq_sardina_1
)

# Instrumentos (variables exógenas)
instrumentos_1 <- ~ ln_Q_ANCHOVETA_LAG1 + ln_Q_JUREL_LAG1 + ln_Q_SARDINA_LAG1 +
  TENDENCIA + D_VEDA + D_TEMPORADA_ALTA + D_VEDA_X_TREND +
  FACTOR_MES

# Estimar con OLS primero (para comparar)
cat("Estimando con OLS (para comparación)...\n")
modelo_ols_1 <- systemfit(sistema_1, method = "OLS", data = df_balanceada)

cat("Estimando con 3SLS...\n")
modelo_3sls_1 <- systemfit(
  sistema_1,
  method = "3SLS",
  inst = instrumentos_1,
  data = df_balanceada
)

cat("\n=== RESULTADOS ETAPA 1 ===\n\n")
print(summary(modelo_3sls_1))

# Extraer coeficientes de interés (γ_ij)
coefs_1 <- coef(modelo_3sls_1)
gamma_matrix_1 <- matrix(NA, nrow = 3, ncol = 3,
                         dimnames = list(
                           c("Anchoveta", "Jurel", "Sardina"),
                           c("ln_Q_ANCH", "ln_Q_JUR", "ln_Q_SARD")
                         ))

# Llenar matriz (busca los coeficientes)
gamma_matrix_1[1, ] <- c(
  coefs_1["anchoveta_ln_Q_ANCHOVETA"],
  coefs_1["anchoveta_ln_Q_JUREL"],
  coefs_1["anchoveta_ln_Q_SARDINA"]
)
gamma_matrix_1[2, ] <- c(
  coefs_1["jurel_ln_Q_ANCHOVETA"],
  coefs_1["jurel_ln_Q_JUREL"],
  coefs_1["jurel_ln_Q_SARDINA"]
)
gamma_matrix_1[3, ] <- c(
  coefs_1["sardina_ln_Q_ANCHOVETA"],
  coefs_1["sardina_ln_Q_JUREL"],
  coefs_1["sardina_ln_Q_SARDINA"]
)

cat("\nMatriz de Flexibilidades de Precio (γ_ij) - ETAPA 1:\n")
cat("(Efecto de ln_Q_j sobre ln_P_i)\n\n")
print(round(gamma_matrix_1, 4))

# Test de restricción de homogeneidad (Σ γ_ij = 0)
cat("\n\nTest de homogeneidad (Σ γ_ij = 0 para cada ecuación):\n")
for (i in 1:3) {
  suma <- sum(gamma_matrix_1[i, ], na.rm = TRUE)
  cat(sprintf("  Ecuación %d: Σ γ = %.4f\n", i, suma))
}

# ==============================================================================
# 6. ETAPA 2: MODELO CON PRECIO INTERNACIONAL
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("ETAPA 2: MODELO CON PRECIO INTERNACIONAL (INTEGRACIÓN IMPERFECTA)\n")
cat(rep("=", 80), "\n\n")

eq_anchoveta_2 <- ln_P_ANCHOVETA ~ 
  ln_Q_ANCHOVETA + ln_Q_JUREL + ln_Q_SARDINA +
  ln_P_HARINA +  # ← AÑADIDO
  TENDENCIA + D_VEDA + D_TEMPORADA_ALTA +
  FACTOR_MES + SHARE_IND_ANCH

eq_jurel_2 <- ln_P_JUREL ~ 
  ln_Q_ANCHOVETA + ln_Q_JUREL + ln_Q_SARDINA +
  ln_P_HARINA +  # ← AÑADIDO
  TENDENCIA + D_VEDA + D_TEMPORADA_ALTA +
  FACTOR_MES + SHARE_IND_JUR

eq_sardina_2 <- ln_P_SARDINA ~ 
  ln_Q_ANCHOVETA + ln_Q_JUREL + ln_Q_SARDINA +
  ln_P_HARINA +  # ← AÑADIDO
  TENDENCIA + D_VEDA + D_TEMPORADA_ALTA +
  FACTOR_MES + SHARE_IND_SARD

sistema_2 <- list(
  anchoveta = eq_anchoveta_2,
  jurel = eq_jurel_2,
  sardina = eq_sardina_2
)

# Mismo set de instrumentos (P_HARINA se trata como exógeno)
cat("Estimando con 3SLS (modelo ampliado)...\n")
modelo_3sls_2 <- systemfit(
  sistema_2,
  method = "3SLS",
  inst = instrumentos_1,  # Mismo set
  data = df_balanceada
)

cat("\n=== RESULTADOS ETAPA 2 ===\n\n")
print(summary(modelo_3sls_2))

# Extraer coeficientes
coefs_2 <- coef(modelo_3sls_2)
gamma_matrix_2 <- matrix(NA, nrow = 3, ncol = 3,
                         dimnames = list(
                           c("Anchoveta", "Jurel", "Sardina"),
                           c("ln_Q_ANCH", "ln_Q_JUR", "ln_Q_SARD")
                         ))

gamma_matrix_2[1, ] <- c(
  coefs_2["anchoveta_ln_Q_ANCHOVETA"],
  coefs_2["anchoveta_ln_Q_JUREL"],
  coefs_2["anchoveta_ln_Q_SARDINA"]
)
gamma_matrix_2[2, ] <- c(
  coefs_2["jurel_ln_Q_ANCHOVETA"],
  coefs_2["jurel_ln_Q_JUREL"],
  coefs_2["jurel_ln_Q_SARDINA"]
)
gamma_matrix_2[3, ] <- c(
  coefs_2["sardina_ln_Q_ANCHOVETA"],
  coefs_2["sardina_ln_Q_JUREL"],
  coefs_2["sardina_ln_Q_SARDINA"]
)

cat("\nMatriz de Flexibilidades de Precio (γ_ij) - ETAPA 2:\n")
cat("(Efecto de ln_Q_j sobre ln_P_i, controlando por P_HARINA)\n\n")
print(round(gamma_matrix_2, 4))

# Coeficientes del precio internacional
gamma_FM <- c(
  coefs_2["anchoveta_ln_P_HARINA"],
  coefs_2["jurel_ln_P_HARINA"],
  coefs_2["sardina_ln_P_HARINA"]
)

cat("\n\nCoeficientes Precio Harina Internacional (γ_FM):\n")
cat(sprintf("  Anchoveta: %.4f\n", gamma_FM[1]))
cat(sprintf("  Jurel:     %.4f\n", gamma_FM[2]))
cat(sprintf("  Sardina:   %.4f\n", gamma_FM[3]))

# ==============================================================================
# 7. COMPARACIÓN ENTRE ETAPAS
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("COMPARACIÓN: ETAPA 1 vs ETAPA 2\n")
cat(rep("=", 80), "\n\n")

cat("Cambio en coeficientes γ_ij después de controlar por P_HARINA:\n\n")
diff_matrix <- gamma_matrix_2 - gamma_matrix_1
print(round(diff_matrix, 4))

cat("\n\nInterpretación:\n")
cat("  - Si γ_ij (Etapa 2) sigue siendo significativo → Integración IMPERFECTA\n")
cat("  - Si γ_ij (Etapa 2) pierde significancia → Integración PERFECTA (LOP)\n")
cat("  - Magnitud del cambio → Importancia relativa precio local vs internacional\n\n")

# Test de significancia conjunta de cantidades en Etapa 2
cat("Test de significancia conjunta de ln_Q en Etapa 2:\n")
cat("(H0: γ_ij = 0 para todas las cantidades)\n\n")

for (i in 1:3) {
  especies <- c("anchoveta", "jurel", "sardina")
  hipotesis <- paste0(
    especies[i], "_ln_Q_ANCHOVETA = 0, ",
    especies[i], "_ln_Q_JUREL = 0, ",
    especies[i], "_ln_Q_SARDINA = 0"
  )
  
  test_result <- try(
    linearHypothesis(modelo_3sls_2, hipotesis),
    silent = TRUE
  )
  
  if (!inherits(test_result, "try-error")) {
    cat(sprintf("Ecuación %s:\n", especies[i]))
    print(test_result)
    cat("\n")
  }
}

# ==============================================================================
# 8. DIAGNÓSTICOS
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("DIAGNÓSTICOS DEL MODELO\n")
cat(rep("=", 80), "\n\n")

# R² por ecuación
cat("R² por ecuación:\n")
r2_etapa1 <- summary(modelo_3sls_1)$eq[[1]]$r.squared
r2_etapa2 <- summary(modelo_3sls_2)$eq[[1]]$r.squared

for (i in 1:3) {
  especies <- c("Anchoveta", "Jurel", "Sardina")
  r2_1 <- summary(modelo_3sls_1)$eq[[i]]$r.squared
  r2_2 <- summary(modelo_3sls_2)$eq[[i]]$r.squared
  
  cat(sprintf("  %s: Etapa 1 = %.4f, Etapa 2 = %.4f (Δ = %.4f)\n",
              especies[i], r2_1, r2_2, r2_2 - r2_1))
}

# Test de Hausman (endogeneidad)
cat("\n\nTest de Hausman (OLS vs 3SLS) - Etapa 1:\n")
cat("(Si p-value < 0.05 → endogeneidad confirmada, usar 3SLS)\n\n")

# Este test es aproximado porque systemfit no implementa hausman directo
# Podemos comparar coeficientes manualmente

coefs_ols <- coef(modelo_ols_1)
coefs_3sls <- coef(modelo_3sls_1)

diff_coefs <- head(coefs_3sls - coefs_ols[names(coefs_3sls)], 12)
cat("Diferencias en primeros coeficientes (3SLS - OLS):\n")
print(round(diff_coefs, 4))

cat("\n\nSi diferencias son grandes → endogeneidad presente → usar 3SLS\n")

# ==============================================================================
# 9. CALCULAR ELASTICIDADES
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("ELASTICIDADES PRECIO-CANTIDAD\n")
cat(rep("=", 80), "\n\n")

# En log-log, los coeficientes SON las elasticidades directamente
cat("En especificación log-log: γ_ij = elasticidad de P_i respecto a Q_j\n\n")

cat("ETAPA 1 (sin P_HARINA):\n")
print(round(gamma_matrix_1, 4))

cat("\n\nETAPA 2 (con P_HARINA):\n")
print(round(gamma_matrix_2, 4))

cat("\n\nInterpretación:\n")
cat("  γ_ii < 0 → Aumento de Q_i reduce P_i (esperado)\n")
cat("  γ_ij > 0 → Especies i y j son SUSTITUTOS\n")
cat("  γ_ij < 0 → Especies i y j son COMPLEMENTOS\n\n")

# ==============================================================================
# 10. PREDICCIONES Y AJUSTE
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("PREDICCIONES Y AJUSTE DEL MODELO\n")
cat(rep("=", 80), "\n\n")

# Predicciones del modelo Etapa 2
pred_etapa2 <- predict(modelo_3sls_2)

# Añadir a la base
df_balanceada <- df_balanceada %>%
  mutate(
    pred_ln_P_ANCHOVETA = pred_etapa2[, "anchoveta"],
    pred_ln_P_JUREL = pred_etapa2[, "jurel"],
    pred_ln_P_SARDINA = pred_etapa2[, "sardina"],
    
    # Transformar de vuelta a niveles
    pred_P_ANCHOVETA = exp(pred_ln_P_ANCHOVETA),
    pred_P_JUREL = exp(pred_ln_P_JUREL),
    pred_P_SARDINA = exp(pred_ln_P_SARDINA),
    
    # Calcular residuos
    resid_P_ANCHOVETA = PRECIO_REAL_MACRO_ANCHOVETA - pred_P_ANCHOVETA,
    resid_P_JUREL = PRECIO_REAL_MACRO_JUREL - pred_P_JUREL,
    resid_P_SARDINA = `PRECIO_REAL_MACRO_SARDINA COMUN` - pred_P_SARDINA
  )

# MAPE (Mean Absolute Percentage Error)
mape_anch <- mean(abs(df_balanceada$resid_P_ANCHOVETA / df_balanceada$PRECIO_REAL_MACRO_ANCHOVETA), na.rm = TRUE) * 100
mape_jur <- mean(abs(df_balanceada$resid_P_JUREL / df_balanceada$PRECIO_REAL_MACRO_JUREL), na.rm = TRUE) * 100
mape_sard <- mean(abs(df_balanceada$resid_P_SARDINA / df_balanceada$`PRECIO_REAL_MACRO_SARDINA COMUN`), na.rm = TRUE) * 100

cat("Error de predicción (MAPE):\n")
cat(sprintf("  Anchoveta: %.2f%%\n", mape_anch))
cat(sprintf("  Jurel:     %.2f%%\n", mape_jur))
cat(sprintf("  Sardina:   %.2f%%\n", mape_sard))

# ==============================================================================
# 11. GUARDAR RESULTADOS
# ==============================================================================

cat("\n=== GUARDANDO RESULTADOS ===\n\n")

# Guardar base con predicciones
write_csv(df_balanceada, "base_con_predicciones_IAIDS.csv")
cat("✓ Base con predicciones: base_con_predicciones_IAIDS.csv\n")

# Guardar coeficientes
coeficientes <- tibble(
  Modelo = rep(c("Etapa1", "Etapa2"), each = length(coefs_1)),
  Variable = c(names(coefs_1), names(coefs_2)),
  Coeficiente = c(coefs_1, coefs_2)
)
write_csv(coeficientes, "coeficientes_IAIDS.csv")
cat("✓ Coeficientes: coeficientes_IAIDS.csv\n")

# Guardar matrices de flexibilidades
write.csv(gamma_matrix_1, "flexibilidades_etapa1.csv")
write.csv(gamma_matrix_2, "flexibilidades_etapa2.csv")
cat("✓ Matrices de flexibilidades guardadas\n")

# Guardar resumen del modelo
sink("resumen_modelo_IAIDS.txt")
cat("="*80, "\n")
cat("RESUMEN MODELO IAIDS\n")
cat("="*80, "\n\n")
cat("Base de datos:", nrow(df_balanceada), "observaciones\n")
cat("Período:", min(df_balanceada$ANIO), "-", max(df_balanceada$ANIO), "\n\n")
cat("ETAPA 1: SIN PRECIO INTERNACIONAL\n")
cat("-"*80, "\n")
print(summary(modelo_3sls_1))
cat("\n\nETAPA 2: CON PRECIO INTERNACIONAL\n")
cat("-"*80, "\n")
print(summary(modelo_3sls_2))
sink()
cat("✓ Resumen completo: resumen_modelo_IAIDS.txt\n")

cat("\n", rep("=", 80), "\n")
cat("ESTIMACIÓN COMPLETADA\n")
cat(rep("=", 80), "\n\n")

cat("Archivos generados:\n")
cat("  1. base_con_predicciones_IAIDS.csv\n")
cat("  2. coeficientes_IAIDS.csv\n")
cat("  3. flexibilidades_etapa1.csv\n")
cat("  4. flexibilidades_etapa2.csv\n")
cat("  5. resumen_modelo_IAIDS.txt\n\n")

cat("Próximos pasos:\n")
cat("  1. Revisar significancia de coeficientes\n")
cat("  2. Agregar SST, Chl-a, precio diesel como IVs\n")
cat("  3. Implementar restricciones de homogeneidad y simetría\n")
cat("  4. Análisis de robustez (con/sin outliers)\n")
cat("  5. Gráficos de precios observados vs predichos\n\n")
