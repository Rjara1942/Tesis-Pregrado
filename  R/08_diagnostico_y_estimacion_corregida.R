# =============================================================================
# SCRIPT 08: DIAGNÓSTICO COMPLETO Y ESTIMACIÓN IV CORREGIDA
# =============================================================================
#
# Este script:
#   1. Diagnostica los problemas identificados en el panel
#   2. Propone soluciones para cada problema
#   3. Estima el modelo IV con las correcciones apropiadas
#
# =============================================================================

library(tidyverse)
library(fixest)       # Para modelos IV con efectos fijos y clustered SE
library(lmtest)       # Tests diagnósticos
library(sandwich)     # Errores robustos

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("DIAGNÓSTICO Y ESTIMACIÓN CORREGIDA DEL MODELO IV\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# =============================================================================
# PARTE 1: CARGAR Y DIAGNOSTICAR
# =============================================================================

panel <- read_csv("panel_con_alternativas.csv", show_col_types = FALSE)

cat("RESUMEN DEL PANEL:\n")
cat("  Observaciones:", nrow(panel), "\n")
cat("  Variables:", ncol(panel), "\n")
cat("  Plantas:", n_distinct(panel$NUI), "\n")
cat("  Período:", min(panel$ANIO), "-", max(panel$ANIO), "\n\n")

# =============================================================================
# PARTE 2: PROBLEMA 1 - D_VEDA EXTREMADAMENTE DESBALANCEADO
# =============================================================================

cat(paste(rep("=", 70), collapse=""), "\n")
cat("PROBLEMA 1: D_VEDA DESBALANCEADO\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

cat("Distribución actual:\n")
table(panel$D_VEDA) %>% print()
cat("\nPor mes:\n")
panel %>% count(MES, D_VEDA) %>% pivot_wider(names_from = D_VEDA, values_from = n, values_fill = 0) %>% print()

cat("\n┌─────────────────────────────────────────────────────────────────────┐\n")
cat("│ DIAGNÓSTICO: Solo ", sum(panel$D_VEDA == 1), " obs con D_VEDA=1 (", 
    round(sum(panel$D_VEDA == 1)/nrow(panel)*100, 1), "%)              │\n", sep="")
cat("│                                                                     │\n")
cat("│ SOLUCIÓN: Usar SEASON_SIN + SEASON_COS (armónicos de Fourier)      │\n")
cat("│           Capturan estacionalidad sin el problema de pocas obs     │\n")
cat("└─────────────────────────────────────────────────────────────────────┘\n\n")

# =============================================================================
# PARTE 3: PROBLEMA 2 - PANEL DESBALANCEADO Y ATTRITION
# =============================================================================

cat(paste(rep("=", 70), collapse=""), "\n")
cat("PROBLEMA 2: PANEL DESBALANCEADO Y ATTRITION\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Observaciones por planta
obs_planta <- panel %>% count(NUI, name = "n_obs") %>% arrange(desc(n_obs))
cat("Observaciones por planta:\n")
print(obs_planta)

cat("\n  Plantas con <10 obs:", sum(obs_planta$n_obs < 10), "\n")
cat("  Plantas con <20 obs:", sum(obs_planta$n_obs < 20), "\n")

# Attrition
cat("\nObservaciones por año:\n")
panel %>% count(ANIO) %>% print()

cat("\n┌─────────────────────────────────────────────────────────────────────┐\n")
cat("│ DIAGNÓSTICO: Panel muy desbalanceado (2-61 obs por planta)         │\n")
cat("│              Attrition severo (74→4 obs de 2012 a 2024)            │\n")
cat("│                                                                     │\n")
cat("│ SOLUCIÓN:                                                          │\n")
cat("│   1. Usar ERRORES ESTÁNDAR CLUSTEREADOS por NUI                    │\n")
cat("│   2. Incluir TENDENCIA como control                                │\n")
cat("│   3. Considerar restringir a plantas con >15 obs (robusto)         │\n")
cat("└─────────────────────────────────────────────────────────────────────┘\n\n")

# =============================================================================
# PARTE 4: PROBLEMA 3 - RELEVANCIA DE IVs
# =============================================================================

cat(paste(rep("=", 70), collapse=""), "\n")
cat("PROBLEMA 3: RELEVANCIA DE INSTRUMENTOS\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Correlaciones IV vs endógena
ivs <- c("SST_PUERTO", "CHL_A_PUERTO", "WIND_PUERTO", "ln_DIESEL", 
         "ln_biomasa_sardina", "ln_h_peru", "SST_PUERTO_L1")

cat("Correlaciones con la ENDÓGENA (ln_h_complejo):\n")
for (iv in ivs) {
  if (iv %in% names(panel)) {
    corr <- cor(panel$ln_h_complejo, panel[[iv]], use = "complete.obs")
    cat(sprintf("  %-20s: r = %+.3f %s\n", iv, corr, 
                ifelse(abs(corr) > 0.2, "✓ RELEVANTE", 
                       ifelse(abs(corr) > 0.1, "⚠ DÉBIL", "✗ MUY DÉBIL"))))
  }
}

# Primera etapa rápida
cat("\nPRIMERA ETAPA (regresión reducida):\n")
fe1 <- feols(ln_h_complejo ~ SST_PUERTO + CHL_A_PUERTO + WIND_PUERTO + 
                             ln_DIESEL + ln_biomasa_sardina + 
                             ln_h_jurel + TENDENCIA | NUI + MES,
             data = panel, vcov = ~NUI)
cat("\nF-estadístico IVs excluidos:", round(fitstat(fe1, "ivf")[[1]], 2), "\n")
cat("  (Regla empírica: F > 10 para evitar instrumentos débiles)\n")

cat("\n┌─────────────────────────────────────────────────────────────────────┐\n")
cat("│ DIAGNÓSTICO: IVs ambientales tienen relevancia moderada            │\n")
cat("│              ln_biomasa_sardina es el IV más fuerte                │\n")
cat("│                                                                     │\n")
cat("│ SOLUCIÓN:                                                          │\n")
cat("│   1. Combinar múltiples IVs para aumentar relevancia               │\n")
cat("│   2. Incluir SST_PUERTO_L1 (rezago) como IV adicional              │\n")
cat("│   3. Reportar F-estadístico de primera etapa                       │\n")
cat("│   4. Usar inferencia robusta a instrumentos débiles (AR test)      │\n")
cat("└─────────────────────────────────────────────────────────────────────┘\n\n")

# =============================================================================
# PARTE 5: ESTIMACIÓN DEL MODELO CORREGIDO
# =============================================================================

cat(paste(rep("=", 70), collapse=""), "\n")
cat("ESTIMACIÓN DEL MODELO CORREGIDO\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Preparar datos (completar NAs en ln_h_peru con 0 o imputar)
panel_est <- panel %>%
  mutate(
    ln_h_peru = ifelse(is.na(ln_h_peru), mean(ln_h_peru, na.rm = TRUE), ln_h_peru)
  )

# Modelo 1: OLS base (para comparación)
cat("MODELO 1: OLS (referencia, sesgado)\n")
cat("-" %>% rep(50) %>% paste(collapse=""), "\n")

ols <- feols(ln_P_complejo ~ ln_h_complejo + ln_P_FOB + ln_h_jurel + 
                              SEASON_SIN + SEASON_COS + TENDENCIA | NUI,
             data = panel_est, vcov = ~NUI)
print(summary(ols))

# Modelo 2: IV con efectos fijos y errores clustereados
cat("\n\nMODELO 2: IV-2SLS CON EFECTOS FIJOS (CORREGIDO)\n")
cat("-" %>% rep(50) %>% paste(collapse=""), "\n")

iv_model <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |                                      # Efectos fijos planta
    ln_h_complejo ~ SST_PUERTO + SST_PUERTO_L1 + CHL_A_PUERTO + 
                    ln_DIESEL + ln_biomasa_sardina,  # Instrumentos
  data = panel_est,
  vcov = ~NUI  # Errores clustereados por planta
)
print(summary(iv_model))

# Estadísticos de diagnóstico
cat("\n\nDIAGNÓSTICOS IV:\n")
cat("-" %>% rep(50) %>% paste(collapse=""), "\n")

# F de primera etapa (Extraemos $stat)
ivf_stat <- fitstat(iv_model, "ivf")[[1]]$stat
cat("F de instrumentos (primera etapa):", round(ivf_stat, 2), "\n")
cat("  → ", ifelse(ivf_stat > 10, "✓ Instrumentos no son débiles (F>10)", 
                   "⚠ Posibles instrumentos débiles (F<10)"), "\n")

# Wu-Hausman (Extraemos $stat y $p)
wh <- fitstat(iv_model, "wh")[[1]]
cat("\nTest de Wu-Hausman (endogeneidad):", round(wh$stat, 2), 
    " (p=", round(wh$p, 4), ")\n", sep="")
cat("  → ", ifelse(wh$p < 0.05, "✓ Rechaza exogeneidad → IV es necesario",
                   "No rechaza exogeneidad → OLS podría ser consistente"), "\n")

# Sargan (Extraemos $stat y $p)
sargan <- fitstat(iv_model, "sargan")[[1]]
cat("\nTest de Sargan (sobreidentificación):", round(sargan$stat, 2),
    " (p=", round(sargan$p, 4), ")\n", sep="")
cat("  → ", ifelse(sargan$p > 0.05, "✓ No rechaza validez de IVs (p>0.05)",
                   "⚠ Posible invalidez de IVs"), "\n")
# =============================================================================
# PARTE 6: MODELO ALTERNATIVO CON FOB PERÚ
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse=""), "\n")
cat("MODELO 3: IV CON FOB PERÚ (ALTERNATIVA)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

iv_peru <- feols(
  ln_P_complejo ~ ln_P_FOB_PERU + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    # Usamos SOLO los 3 instrumentos fuertes
    ln_h_complejo ~ SST_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina,
  data = panel_est,
  vcov = ~NUI
)
print(summary(iv_peru))
# =============================================================================
# PARTE 7: ROBUSTEZ - RESTRINGIR A PLANTAS CON SUFICIENTES OBS
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse=""), "\n")
cat("MODELO 4: IV ROBUSTO (solo plantas con >15 obs)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# Identificar plantas con suficientes observaciones
plantas_ok <- panel_est %>% 
  count(NUI) %>% 
  filter(n > 15) %>% 
  pull(NUI)

cat("Plantas incluidas:", length(plantas_ok), "de", n_distinct(panel_est$NUI), "\n")
cat("Observaciones:", sum(panel_est$NUI %in% plantas_ok), "de", nrow(panel_est), "\n\n")

iv_robusto <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    # Usamos SOLO los 3 instrumentos fuertes
    ln_h_complejo ~ SST_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina,
  data = panel_est %>% filter(NUI %in% plantas_ok),
  vcov = ~NUI
)
print(summary(iv_robusto))

# =============================================================================
# PARTE 8: TABLA COMPARATIVA
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse=""), "\n")
cat("TABLA COMPARATIVA DE MODELOS\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

etable(ols, iv_model, iv_peru, iv_robusto,
       headers = c("OLS", "IV-Chile", "IV-Perú", "IV-Robusto"),
       se.below = TRUE,
       fitstat = ~n + r2 + ivf + wh + sargan,
       dict = c(ln_h_complejo = "ln(Desembarques)",
                ln_P_FOB = "ln(FOB Chile)",
                ln_P_FOB_PERU = "ln(FOB Perú)",
                ln_h_jurel = "ln(Jurel)",
                SEASON_SIN = "sin(2πm/12)",
                SEASON_COS = "cos(2πm/12)",
                TENDENCIA = "Tendencia"))

# =============================================================================
# PARTE 9: RESUMEN EJECUTIVO
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse=""), "\n")
cat("RESUMEN EJECUTIVO\n")
cat(paste(rep("=", 70), collapse=""), "\n")

coef_ols <- coef(ols)["ln_h_complejo"]
coef_iv <- coef(iv_model)["fit_ln_h_complejo"]

cat("
┌─────────────────────────────────────────────────────────────────────┐
│ COEFICIENTE DE INTERÉS: Elasticidad Precio-Cantidad                 │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│   OLS:  γ = ", sprintf("%+.3f", coef_ols), "  → Sesgo hacia cero (endogeneidad)           │
│   IV:   γ = ", sprintf("%+.3f", coef_iv), "  → Efecto causal estimado                     │
│                                                                     │
│   Interpretación: Un aumento del 10% en los desembarques            │
│   reduce el precio en aprox. ", sprintf("%.1f", abs(coef_iv)*10), "%                                   │
│                                                                     │
├─────────────────────────────────────────────────────────────────────┤
│ CORRECCIONES IMPLEMENTADAS:                                         │
│                                                                     │
│   1. ✓ Reemplazado D_VEDA por armónicos SEASON_SIN/COS              │
│   2. ✓ Errores estándar clustereados por planta (NUI)               │
│   3. ✓ Control por TENDENCIA temporal                               │
│   4. ✓ Múltiples IVs para aumentar relevancia                       │
│   5. ✓ Tests de diagnóstico IV reportados                           │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
", sep="")

cat("\n✓ Script completado\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

# =============================================================================
# PARTE 10: GUARDAR RESULTADOS
# =============================================================================

# Exportar tabla de resultados
resultados <- tibble(
  Modelo = c("OLS", "IV-Chile", "IV-Perú", "IV-Robusto"),
  Coef_Desembarques = c(coef(ols)["ln_h_complejo"],
                         coef(iv_model)["fit_ln_h_complejo"],
                         coef(iv_peru)["fit_ln_h_complejo"],
                         coef(iv_robusto)["fit_ln_h_complejo"]),
  SE = c(se(ols)["ln_h_complejo"],
         se(iv_model)["fit_ln_h_complejo"],
         se(iv_peru)["fit_ln_h_complejo"],
         se(iv_robusto)["fit_ln_h_complejo"]),
  N = c(nobs(ols), nobs(iv_model), nobs(iv_peru), nobs(iv_robusto)),
  F_IV = c(NA, fitstat(iv_model, "ivf")[[1]], 
           fitstat(iv_peru, "ivf")[[1]], 
           fitstat(iv_robusto, "ivf")[[1]])
)

write_csv(resultados, "resultados_modelos_IV.csv")
cat("\n✓ Resultados guardados en 'resultados_modelos_IV.csv'\n")




# =============================================================================
# modelo 2 corregido
#==============================================================================

rm(iv_model)
