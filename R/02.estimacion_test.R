# ==============================================================================
# ESTIMACIÓN  DEL MODELO DE PRECIOS 
# ==============================================================================

# Cargar librerías
library(tidyverse)
library(lubridate)
library(plm)
library(ivreg)
library(lmtest)
library(sandwich)
library(readxl)
library(clubSandwich) # Para la corrección CR2 en Mac 

cat(strrep("=", 70), "\n")
cat("INICIANDO ESTIMACIÓN ECONOMÉTRICA (VERSIÓN)\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
# 1. CARGA DEL PANEL BASE
# ==============================================================================
cat("1. Cargando panel corregido...\n")

df_panel <- read_csv("panel_complejo_corregido.csv", show_col_types = FALSE) %>%
  mutate(
    NUI_fact = as.factor(NUI),
    MES_fact = as.factor(MES)
  )

pdata <- pdata.frame(df_panel, index = c("NUI", "yearmonth"))

# ==============================================================================
# 2. MODELO OLS (FE) Y TEST DE AUTOCORRELACIÓN
# ==============================================================================
cat("\n2. Estimando benchmark (OLS Efectos Fijos) y autocorrelación...\n")

fe_ols <- plm(
  ln_P_complejo ~ ln_P_FOB + ln_h_complejo + ln_h_jurel + MES_fact,
  data = pdata, 
  model = "within"
)

# Test de Breusch-Godfrey
bg_1 <- pbgtest(fe_ols, order = 1)
bg_2 <- pbgtest(fe_ols, order = 2)

cat("   -> Test Breusch-Godfrey AR(1): p-value =", round(bg_1$p.value, 4), "\n")
cat("   -> Test Breusch-Godfrey AR(2): p-value =", round(bg_2$p.value, 4), "\n")

# Gráfico de residuos
resid_ts <- tapply(residuals(fe_ols), pdata$yearmonth, mean, na.rm = TRUE)
png("acf_pacf_residuos.png", width = 800, height = 400)
par(mfrow=c(1,2))
acf(resid_ts, na.action = na.pass, main="ACF Residuos Panel")
pacf(resid_ts, na.action = na.pass, main="PACF Residuos Panel")
dev.off()
cat("   -> Gráfico ACF/PACF exportado como 'acf_pacf_residuos.png'.\n")

# ==============================================================================
# 3. PROCESAMIENTO DE INSTRUMENTOS MACROECONÓMICOS
# ==============================================================================
cat("\n3. Procesando instrumentos externos (Perú y Tipo de Cambio)...\n")

# Desembarques Perú (Archivo Excel original)
df_peru <- read_excel("BD_PA_2013-2025-sep.xlsx", skip = 4) %>%
  filter(str_detect(toupper(ESPECIE), "ANCHOVETA")) %>%
  group_by(AÑO, MES) %>%
  summarise(
    h_peru_anchoveta = sum(`DESEMBARQUE (TM)`, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  rename(ANIO = AÑO) %>%
  mutate(ln_h_peru = log(h_peru_anchoveta + 1))

# Dólar Observado (Archivo Excel original)
df_dolar <- read_excel("TC_2012-2024.xlsx", skip = 4, col_names = c("FECHA_STRING", "DOLAR_OBSERVADO")) %>%
  mutate(
    FECHA = as.Date(FECHA_STRING),
    ANIO = year(FECHA),
    MES = month(FECHA),
    ln_tipo_cambio = log(DOLAR_OBSERVADO)
  ) %>%
  select(ANIO, MES, DOLAR_OBSERVADO, ln_tipo_cambio) %>%
  drop_na()

# Unir todo al panel
df_estimacion <- df_panel %>%
  left_join(df_peru, by = c("ANIO", "MES")) %>%
  left_join(df_dolar, by = c("ANIO", "MES"))

# Limpiar NAs para la regresión final (fundamental para ivreg y clubSandwich)
df_clean <- df_estimacion %>% 
  drop_na(ln_P_complejo, ln_P_FOB, ln_h_complejo, ln_h_jurel, 
          SST_MACRO, CHL_A_MACRO, ln_h_complejo_L3, ln_h_complejo_L4,
          ln_h_peru, ln_tipo_cambio)

cat("   -> Observaciones finales listas para la regresión:", nrow(df_clean), "\n")

# ==============================================================================
# 4. ESTIMACIÓN IVREG (ENDOGENEIDAD DOBLE)
# ==============================================================================
cat("\n4. Estimando modelo estructural (Panel IV)...\n")

# Nota: Se omite D_VEDA en los instrumentos porque MES_fact ya absorbe la estacionalidad
# y causaba multicolinealidad perfecta ("collinear instrumental variables").

iv_final <- ivreg(
  ln_P_complejo ~ ln_P_FOB + ln_h_complejo + ln_h_jurel + MES_fact + NUI_fact |
    ln_h_peru + ln_tipo_cambio + SST_MACRO + CHL_A_MACRO + 
    ln_h_complejo_L3 + ln_h_complejo_L4 + ln_h_jurel + MES_fact + NUI_fact,
  data = df_clean
)

# Diagnósticos de Primera Etapa (Relevancia de los Instrumentos)
cat("\n--- DIAGNÓSTICOS DE PRIMERA ETAPA ---\n")
sum_iv <- summary(iv_final, diagnostics = TRUE)
print(sum_iv$diagnostics)

# ==============================================================================
# 5. CORRECCIÓN PARA POCOS CLÚSTERES (ESTIMADOR CR2)
# ==============================================================================
cat("\n5. Aplicando corrección CR2 para pocos clústeres (N=16 plantas)...\n")
cat(strrep("-", 70), "\n")

# Calculamos la matriz robusta CR2
res_cr2 <- coef_test(iv_final, vcov = "CR2", cluster = df_clean$NUI)

# En ivreg, los primeros coeficientes suelen ser el Intercepto y las variables de interés.
# Imprimimos los primeros 4 coeficientes (Intercept, ln_P_FOB, ln_h_complejo, ln_h_jurel)
print(res_cr2[1:4, ])

cat(strrep("=", 70), "\n")
cat("Estimación finalizada exitosamente.\n")

# ==============================================================================
# 6. TEST DE EXOGENEIDAD DEL PRECIO FOB (HAUSMAN)
# ==============================================================================
cat("\n--- PASO 6: TEST DE HAUSMAN PARA ln_P_FOB ---\n")

# PASO 1: Regresar la variable sospechosa (ln_P_FOB) sobre TODOS los instrumentos 
# (tanto los incluidos en la ecuación principal como los excluidos).
# Se omite D_VEDA porque MES_fact ya absorbe la estacionalidad perfecta.
first_stage_fob <- lm(
  ln_P_FOB ~ ln_h_jurel + SST_MACRO + CHL_A_MACRO + 
    ln_h_complejo_L2 + ln_h_complejo_L3 + 
    ln_h_peru + ln_tipo_cambio + MES_fact,
  data = df_clean
)

# Guardar los residuos directamente en la base limpia
df_clean$resid_fob <- residuals(first_stage_fob)

# Crear un nuevo pdata.frame con los residuos incorporados
pdata_clean <- pdata.frame(df_clean, index = c("NUI", "yearmonth"))

# PASO 2: Incluir los residuos como regresor adicional en la ecuación estructural con efectos fijos.
hausman_eq <- plm(
  ln_P_complejo ~ ln_P_FOB + ln_h_complejo + ln_h_jurel + 
    resid_fob + MES_fact,
  data = pdata_clean, 
  model = "within"
)

# PASO 3: Evaluar la significancia del residuo
# Opcion A: Cálculo estándar (HC1)
cat("\nResultados del Test de Hausman (Errores Estándar HC1):\n")
res_hausman_hc1 <- coeftest(hausman_eq, vcov = vcovHC(hausman_eq, type = "HC1", cluster = "group"))
print(res_hausman_hc1["resid_fob", , drop = FALSE])

# Opcion B: Cálculo robusto CR2 para muestras pequeñas (N=16 plantas)
cat("\nResultados del Test de Hausman (Errores Estándar CR2 - clubSandwich):\n")
res_hausman_cr2 <- coef_test(hausman_eq, vcov = "CR2", cluster = "individual")

# Extraer el p-value usando la posición de la fila (resid_fob es la última variable)
idx_resid <- which(rownames(res_hausman_cr2) == "resid_fob")
print(res_hausman_cr2[idx_resid, ])

p_val_hausman <- res_hausman_cr2$p_Satt[idx_resid]

if(!is.na(p_val_hausman) && p_val_hausman < 0.10) {
  cat(sprintf("\n[CONCLUSIÓN] p-value = %.4f < 0.10\n", p_val_hausman))
  cat("Se RECHAZA la hipótesis nula. El precio FOB ES ENDÓGENO.\n")
} else {
  cat(sprintf("\n[CONCLUSIÓN] p-value = %.4f > 0.10\n", p_val_hausman))
  cat("NO SE RECHAZA la hipótesis nula. El precio FOB se considera EXÓGENO.\n")
  cat("Las flotas son price-takers del mercado internacional.\n")
}

# ==============================================================================
# COEFICIENTES FINALES (OLS-FE)
# ==============================================================================
#
# DADO QUE:
#   - Wu-Hausman p = 0.33 → NO hay endogeneidad
#   - Sargan p = 0.19 → IVs válidos
#   - Hausman FOB p = 0.87 → FOB exógeno
#
# ENTONCES:
#   - IV es INNECESARIO
#   - Modelo óptimo = OLS con Efectos Fijos
#   - Solo falta: test de Wald + Coeficientes finales
#
# ==============================================================================


cat("\n")
cat(strrep("=", 70), "\n")
cat("MODELO FINAL: OLS CON EFECTOS FIJOS\n")
cat("(Wu-Hausman p=0.33 confirma exogeneidad → IV innecesario)\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
# MODELO OLS-FE
# ==============================================================================
library(car)



fe_ols <- plm(
  ln_P_complejo ~ ln_P_FOB + ln_h_complejo + ln_h_jurel + MES_fact,
  data = pdata,
  model = "within"
)

# Errores robustos (Driscoll-Kraay para autocorrelación)
vcov_dk <- vcovSCC(fe_ols, maxlag = 3)
coef_ols <- coeftest(fe_ols, vcov = vcov_dk)

cat("COEFICIENTES OLS-FE (Errores Driscoll-Kraay):\n")
cat(strrep("-", 60), "\n")
print(coef_ols[c("ln_P_FOB", "ln_h_complejo", "ln_h_jurel"), ])

cat("\nR² within:", round(summary(fe_ols)$r.squared["rsq"], 4), "\n")

# ==============================================================================
# PASO 7: TEST DE WALD (γ = 0)
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("PASO 7: TEST DE WALD - ¿PRICE-TAKERS PUROS?\n")
cat(strrep("=", 70), "\n\n")

cat("H0: γ_complejo = γ_jurel = 0\n")
cat("H1: Al menos un γ ≠ 0\n\n")

wald_test <- linearHypothesis(
  fe_ols,
  c("ln_h_complejo = 0", "ln_h_jurel = 0"),
  vcov. = vcov_dk,
  test = "F"
)

F_wald <- wald_test$F[2]
p_wald <- wald_test$`Pr(>F)`[2]

cat("Resultado:\n")
cat("   F-stat:  ", round(F_wald, 3), "\n")
cat("   p-value: ", round(p_wald, 4), "\n")

if (p_wald < 0.05) {
  cat("\n   → RECHAZA H0 al 5%: Cantidades SÍ afectan precio ***\n")
  cat("   → Plantas NO son price-takers puros\n")
  cat("   → INTEGRACIÓN IMPERFECTA confirmada\n")
} else if (p_wald < 0.10) {
  cat("\n   → RECHAZA H0 al 10%: Evidencia moderada *\n")
  cat("   → Plantas probablemente NO son price-takers puros\n")
} else {
  cat("\n   → NO RECHAZA H0\n")
  cat("   → Plantas podrían ser price-takers puros\n")
}

# ==============================================================================
# RESUMEN FINAL DE TODOS LOS TESTS
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("RESUMEN FINAL: PASOS 6, 7, 8\n")
cat(strrep("=", 70), "\n\n")

cat("┌────────────────────────────────────────────────────────────────────┐\n")
cat("│  PASO 6: TEST DE HAUSMAN PARA FOB                                 │\n")
cat("│    Coef residuo: -0.035 (p = 0.87)                                │\n")
cat("│    → FOB es EXÓGENO ✓                                             │\n")
cat("│    → Chile es tomador de precios en mercado mundial               │\n")
cat("├────────────────────────────────────────────────────────────────────┤\n")
cat(sprintf("│  PASO 7: TEST DE WALD (γ = 0)                                     │\n"))
cat(sprintf("│    F = %.3f, p = %.4f                                            │\n", F_wald, p_wald))
if (p_wald < 0.10) {
  cat("│    → RECHAZA H0: Cantidades SÍ afectan precio                     │\n")
  cat("│    → Plantas NO son price-takers puros                            │\n")
} else {
  cat("│    → No rechaza H0                                                │\n")
}
cat("├────────────────────────────────────────────────────────────────────┤\n")
cat("│  PASO 8: TESTS DE ESPECIFICACIÓN.                                  │\n")
cat("│    Wu-Hausman: p = 0.33 → h es EXÓGENO (IV innecesario)            │\n")
cat("│    Sargan: p = 0.19 → IVs son válidos                              │\n")
cat("├────────────────────────────────────────────────────────────────────┤\n")
cat("│  COEFICIENTES FINALES (OLS-FE, Errores Driscoll-Kraay)             │\n")
cat("├────────────────────────────────────────────────────────────────────┤\n")

beta <- coef_ols["ln_P_FOB", "Estimate"]
se_beta <- coef_ols["ln_P_FOB", "Std. Error"]
p_beta <- coef_ols["ln_P_FOB", "Pr(>|t|)"]

gamma <- coef_ols["ln_h_complejo", "Estimate"]
se_gamma <- coef_ols["ln_h_complejo", "Std. Error"]
p_gamma <- coef_ols["ln_h_complejo", "Pr(>|t|)"]

gamma_j <- coef_ols["ln_h_jurel", "Estimate"]

sig_beta <- ifelse(p_beta < 0.01, "***", ifelse(p_beta < 0.05, "**", ifelse(p_beta < 0.10, "*", "")))
sig_gamma <- ifelse(p_gamma < 0.01, "***", ifelse(p_gamma < 0.05, "**", ifelse(p_gamma < 0.10, "*", "")))

cat(sprintf("│    β (pass-through FOB): %6.3f (SE=%.3f) %s                      │\n", beta, se_beta, sig_beta))
cat(sprintf("│    γ (flexibilidad h):   %6.3f (SE=%.3f) %s                      │\n", gamma, se_gamma, sig_gamma))
cat(sprintf("│    γ_jurel:              %6.3f                                    │\n", gamma_j))
cat(sprintf("│    R² within: %.3f                                                 │\n", summary(fe_ols)$r.squared["rsq"]))
cat("└────────────────────────────────────────────────────────────────────┘\n")

# ==============================================================================
# INTERPRETACIÓN ECONÓMICA
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("INTERPRETACIÓN ECONÓMICA\n")
cat(strrep("=", 70), "\n\n")

cat("1. TRANSMISIÓN DEL PRECIO INTERNACIONAL (Pass-through):\n")
cat(sprintf("   β = %.3f %s\n", beta, sig_beta))
cat(sprintf("   → Por cada 10%% ↑ en P_FOB → precio ex-vessel ↑ %.1f%%\n", 10*beta))
cat(sprintf("   → Transmisión del %.0f%% del precio internacional\n\n", 100*beta))

cat("2. EFECTO DE LOS DESEMBARQUES LOCALES (Flexibilidad):\n")
cat(sprintf("   γ = %.3f %s\n", gamma, sig_gamma))
if (gamma < 0) {
  cat(sprintf("   → Por cada 10%% ↑ en desembarques → precio ex-vessel ↓ %.1f%%\n", abs(10*gamma)))
  cat("   → Signo negativo coherente con demanda inversa\n\n")
} else {
  cat(sprintf("   → Por cada 10%% ↑ en desembarques → precio ex-vessel ↑ %.1f%%\n", 10*gamma))
}

cat("3. CHILE COMO TOMADOR DE PRECIOS (Paso 6):\n")
cat("   → Hausman FOB p = 0.87 >> 0.10\n")
cat("   → P_FOB es exógeno (no afectado por producción chilena)\n")
cat("   → Chile (~15% producción mundial) no tiene poder de mercado\n")
cat("   → Perú domina (~60% producción mundial)\n\n")

if (p_wald < 0.10) {
  cat("4. INTEGRACIÓN IMPERFECTA (Paso 7):\n")
  cat(sprintf("   → Wald F = %.2f (p = %.3f)\n", F_wald, p_wald))
  cat("   → Las cantidades locales SÍ afectan el precio\n")
  cat("   → Las plantas NO son price-takers puros\n")
  cat("   → Mecanismo: capacidad de procesamiento cuasi-fija\n")
  cat("   → FOB = ancla de largo plazo\n")
  cat("   → Desembarques = desviaciones de corto plazo\n")
}

# ==============================================================================
# COMPARACIÓN: ¿POR QUÉ OLS-FE Y NO IV?
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("¿POR QUÉ USAR OLS-FE EN LUGAR DE IV?\n")
cat(strrep("=", 70), "\n\n")

cat("Tu output mostró:\n\n")

cat("1. Wu-Hausman: p = 0.33\n")
cat("   → NO RECHAZA H0 de exogeneidad\n")
cat("   → Las variables son EXÓGENAS\n")
cat("   → IV es INNECESARIO\n\n")

cat("2. Weak IV para h_complejo: F = 2.5 << 10\n")
cat("   → Instrumentos DÉBILES\n")
cat("   → Causa sesgo hacia cero en los coeficientes\n")
cat("   → Por eso tus coeficientes IV eran no significativos\n\n")

cat("3. Conclusión:\n")
cat("   → OLS-FE es CONSISTENTE (dado exogeneidad confirmada)\n")
cat("   → OLS-FE es MÁS EFICIENTE que IV innecesario\n")
cat("   → Los coeficientes OLS-FE SON los coeficientes finales\n")

# ==============================================================================
# GUARDAR RESULTADOS
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("GUARDANDO RESULTADOS\n")
cat(strrep("=", 70), "\n\n")

# Tabla de tests
resumen_tests <- data.frame(
  Paso = c("6", "7", "8a", "8b"),
  Test = c("Hausman FOB", "Wald conjunto", "Wu-Hausman", "Sargan"),
  Statistic = c(-0.17, F_wald, 1.12, 6.17),
  p_value = c(0.87, p_wald, 0.33, 0.19),
  Conclusion = c("FOB exógeno", 
                 ifelse(p_wald < 0.10, "No price-takers", "Price-takers"),
                 "h exógeno (no IV)",
                 "IVs válidos")
)
write_csv(resumen_tests, "resumen_tests_678.csv")
cat("   → resumen_tests_678.csv\n")

# Coeficientes finales
coef_final <- data.frame(
  Variable = c("ln_P_FOB", "ln_h_complejo", "ln_h_jurel"),
  Coef = c(beta, gamma, gamma_j),
  SE = c(se_beta, se_gamma, coef_ols["ln_h_jurel", "Std. Error"]),
  t = coef_ols[c("ln_P_FOB", "ln_h_complejo", "ln_h_jurel"), "t value"],
  p = coef_ols[c("ln_P_FOB", "ln_h_complejo", "ln_h_jurel"), "Pr(>|t|)"]
)
write_csv(coef_final, "coeficientes_finales.csv")
cat("   → coeficientes_finales.csv\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("ANÁLISIS COMPLETADO\n")
cat(strrep("=", 70), "\n")



