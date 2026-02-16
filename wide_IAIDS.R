################################################################################
# ANÁLISIS DE BASE FINAL PARA ESTIMACIÓN IAIDS
######################################################

library(tidyverse)
library(lubridate)

cat("\n", rep("=", 80), "\n")
cat("ANÁLISIS DE BASE FINAL PARA IAIDS\n")
cat(rep("=", 80), "\n\n")

# ==============================================================================
# 1. CARGAR Y VERIFICAR DATOS
# ==============================================================================

cat("=== 1. CARGANDO BASE CORREGIDA ===\n\n")

df <- read_csv("base_integrada_con_instrumentos_CORREGIDA.csv", 
               show_col_types = FALSE)

cat("Estructura general:\n")
cat("  Observaciones:", nrow(df), "\n")
cat("  Variables:", ncol(df), "\n")
cat("  Especies:", paste(unique(df$NM_RECURSO), collapse = ", "), "\n")
cat("  Período:", min(df$ANIO), "-", max(df$ANIO), "\n\n")

# ==============================================================================
# 2. DISTRIBUCIÓN POR ESPECIE
# ==============================================================================

cat("=== 2. DISTRIBUCIÓN POR ESPECIE ===\n\n")

dist_especies <- df %>%
  group_by(NM_RECURSO) %>%
  summarise(
    N_obs = n(),
    Años = paste(range(ANIO), collapse = "-"),
    N_meses_unicos = n_distinct(paste(ANIO, MES))
  )

print(dist_especies)
cat("\n")

# Distribución por año-especie
dist_anual <- df %>%
  group_by(ANIO) %>%
  summarise(
    N_especies = n_distinct(NM_RECURSO),
    Especies = paste(unique(NM_RECURSO), collapse = ", ")
  )

cat("Especies por año:\n")
print(dist_anual)
cat("\n")

# ==============================================================================
# 3. PANEL BALANCEADO vs COMPLETO
# ==============================================================================

cat("=== 3. ANÁLISIS DE PANEL ===\n\n")

# Meses con 3 especies (panel balanceado)
meses_completos <- df %>%
  group_by(ANIO, MES) %>%
  summarise(N_especies = n(), .groups = "drop") %>%
  filter(N_especies == 3)

cat("PANEL BALANCEADO (3 especies simultáneas):\n")
cat("  Meses únicos:", nrow(meses_completos), "\n")
cat("  Observaciones:", nrow(meses_completos) * 3, "\n")
cat("  Período:", min(meses_completos$ANIO), "-", max(meses_completos$ANIO), "\n\n")

cat("PANEL COMPLETO (todas las observaciones):\n")
cat("  Meses únicos:", n_distinct(paste(df$ANIO, df$MES)), "\n")
cat("  Observaciones:", nrow(df), "\n\n")

cat("RECOMENDACIÓN IAIDS: Usar PANEL COMPLETO (no requiere balanceo)\n\n")

# ==============================================================================
# 4. VERIFICACIÓN DE PRECIOS REALES
# ==============================================================================

cat("=== 4. VERIFICACIÓN DE PRECIOS DEFLACTADOS ===\n\n")

# Verificar que todos estén en pesos 2024
precios_check <- df %>%
  summarise(
    `PRECIO_REAL_MACRO (ex-vessel)` = mean(PRECIO_REAL_MACRO, na.rm = TRUE),
    `P_HARINA_REAL (FOB)` = mean(P_HARINA_REAL, na.rm = TRUE),
    `PRECIO_DIESEL_REAL (combustible)` = mean(PRECIO_DIESEL_REAL, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Media_2024")

cat("Precios en pesos constantes 2024:\n")
print(precios_check)
cat("\n")

# Comparación diesel nominal vs real
cat("Comparación DIESEL (Nominal vs Real):\n")
diesel_comp <- df %>%
  group_by(ANIO) %>%
  summarise(
    Nominal = mean(PRECIO_DIESEL_NOMINAL, na.rm = TRUE),
    Real = mean(PRECIO_DIESEL_REAL, na.rm = TRUE),
    Dif_pct = round((Real - Nominal) / Nominal * 100, 1)
  ) %>%
  filter(ANIO %in% c(2012, 2015, 2018, 2021, 2024))

print(diesel_comp)
cat("\nNOTA: Precios antiguos AUMENTAN al deflactar (pesos valían más)\n\n")

# ==============================================================================
# 5. ESTADÍSTICAS DESCRIPTIVAS POR ESPECIE
# ==============================================================================

cat("=== 5. ESTADÍSTICAS DESCRIPTIVAS POR ESPECIE ===\n\n")

stats_especies <- df %>%
  group_by(NM_RECURSO) %>%
  summarise(
    N = n(),
    `Precio Real Media` = round(mean(PRECIO_REAL_MACRO, na.rm = TRUE)),
    `Precio Real SD` = round(sd(PRECIO_REAL_MACRO, na.rm = TRUE)),
    `Precio CV` = round(sd(PRECIO_REAL_MACRO, na.rm = TRUE) / 
                          mean(PRECIO_REAL_MACRO, na.rm = TRUE) * 100, 1),
    `Q Media (ton)` = round(mean(Q_MACRO, na.rm = TRUE)),
    `Q SD` = round(sd(Q_MACRO, na.rm = TRUE)),
    `Q CV` = round(sd(Q_MACRO, na.rm = TRUE) / mean(Q_MACRO, na.rm = TRUE) * 100, 1),
    `Share Industrial` = round(mean(SHARE_INDUSTRIAL, na.rm = TRUE) * 100, 1)
  )

print(stats_especies)
cat("\n")

# ==============================================================================
# 6. INSTRUMENTOS: ESTADÍSTICAS Y COBERTURA
# ==============================================================================

cat("=== 6. INSTRUMENTOS: ESTADÍSTICAS ===\n\n")

# Variables ambientales
cat("VARIABLES AMBIENTALES:\n")
instrumentos_amb <- df %>%
  summarise(
    across(
      c(SST_MACRO, CHL_A_MACRO, WIND_SPEED_MACRO),
      list(
        Media = ~round(mean(., na.rm = TRUE), 2),
        SD = ~round(sd(., na.rm = TRUE), 2),
        Min = ~round(min(., na.rm = TRUE), 2),
        Max = ~round(max(., na.rm = TRUE), 2),
        NA_pct = ~round(sum(is.na(.)) / n() * 100, 1)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(everything()) %>%
  separate(name, into = c("Variable", "Stat"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Stat, values_from = value)

print(instrumentos_amb)
cat("\n")

# Precio diesel
cat("PRECIO DIESEL REAL:\n")
diesel_stats <- df %>%
  summarise(
    Media = round(mean(PRECIO_DIESEL_REAL, na.rm = TRUE)),
    SD = round(sd(PRECIO_DIESEL_REAL, na.rm = TRUE)),
    Min = round(min(PRECIO_DIESEL_REAL, na.rm = TRUE)),
    Max = round(max(PRECIO_DIESEL_REAL, na.rm = TRUE)),
    NA_pct = round(sum(is.na(PRECIO_DIESEL_REAL)) / n() * 100, 1)
  )

print(diesel_stats)
cat("\n")

# ==============================================================================
# 7. CORRELACIONES ENTRE INSTRUMENTOS
# ==============================================================================

cat("=== 7. CORRELACIONES ENTRE INSTRUMENTOS ===\n\n")

vars_iv <- c("SST_MACRO", "CHL_A_MACRO", "WIND_SPEED_MACRO", 
             "PRECIO_DIESEL_REAL", "SST2", "CHL_A2")

cor_iv <- cor(df[vars_iv], use = "pairwise.complete.obs")

cat("Matriz de correlaciones:\n")
print(round(cor_iv, 3))
cat("\n")

# Detectar correlaciones altas
high_cor <- which(abs(cor_iv) > 0.85 & cor_iv != 1, arr.ind = TRUE)
if (nrow(high_cor) > 0) {
  cat("ADVERTENCIA: Correlaciones > 0.85 detectadas:\n")
  for (i in 1:nrow(high_cor)) {
    if (high_cor[i, 1] < high_cor[i, 2]) {
      cat(sprintf("  %s - %s: %.3f\n",
                  rownames(cor_iv)[high_cor[i, 1]],
                  colnames(cor_iv)[high_cor[i, 2]],
                  cor_iv[high_cor[i, 1], high_cor[i, 2]]))
    }
  }
  cat("\n")
} else {
  cat("✓ No se detectaron correlaciones problemáticas (>0.85)\n\n")
}

# ==============================================================================
# 8. VALORES FALTANTES
# ==============================================================================

cat("=== 8. VALORES FALTANTES ===\n\n")

# Variables clave para IAIDS
vars_clave <- c("PRECIO_REAL_MACRO", "Q_MACRO", "P_HARINA_REAL",
                "SST_MACRO", "CHL_A_MACRO", "WIND_SPEED_MACRO", 
                "PRECIO_DIESEL_REAL", "SHARE_INDUSTRIAL")

na_check <- sapply(df[vars_clave], function(x) {
  c(
    N_NA = sum(is.na(x)),
    Pct_NA = round(sum(is.na(x)) / length(x) * 100, 1)
  )
})

cat("Valores faltantes por variable:\n")
print(t(na_check))
cat("\n")

if (any(na_check["Pct_NA", ] > 5)) {
  cat("ADVERTENCIA: Algunas variables tienen >5% de NAs\n\n")
} else {
  cat("✓ Todas las variables clave tienen <5% de NAs\n\n")
}

# ==============================================================================
# 9. PREPARACIÓN PARA FORMATO WIDE
# ==============================================================================

cat("=== 9. TRANSFORMACIÓN A FORMATO WIDE ===\n\n")

# El IAIDS requiere formato wide (una fila por mes, columnas por especie)
df_wide <- df %>%
  select(ANIO, MES, FECHA, NM_RECURSO, PRECIO_REAL_MACRO, Q_MACRO, 
         P_HARINA_REAL, SST_MACRO, CHL_A_MACRO, WIND_SPEED_MACRO, 
         PRECIO_DIESEL_REAL, SHARE_INDUSTRIAL, DEFLACTOR) %>%
  pivot_wider(
    id_cols = c(ANIO, MES, FECHA, P_HARINA_REAL, SST_MACRO, CHL_A_MACRO, 
                WIND_SPEED_MACRO, PRECIO_DIESEL_REAL, DEFLACTOR),
    names_from = NM_RECURSO,
    values_from = c(PRECIO_REAL_MACRO, Q_MACRO, SHARE_INDUSTRIAL),
    names_sep = "_"
  )

cat("Formato WIDE para IAIDS:\n")
cat("  Filas:", nrow(df_wide), "(meses únicos)\n")
cat("  Columnas:", ncol(df_wide), "\n\n")

# Verificar cobertura por especie
cat("Cobertura por especie (en formato wide):\n")
cobertura <- tibble(
  Especie = c("ANCHOVETA", "JUREL", "SARDINA COMUN"),
  N_obs = c(
    sum(!is.na(df_wide$PRECIO_REAL_MACRO_ANCHOVETA)),
    sum(!is.na(df_wide$PRECIO_REAL_MACRO_JUREL)),
    sum(!is.na(df_wide$`PRECIO_REAL_MACRO_SARDINA COMUN`))
  ),
  Pct = round(c(
    sum(!is.na(df_wide$PRECIO_REAL_MACRO_ANCHOVETA)) / nrow(df_wide) * 100,
    sum(!is.na(df_wide$PRECIO_REAL_MACRO_JUREL)) / nrow(df_wide) * 100,
    sum(!is.na(df_wide$`PRECIO_REAL_MACRO_SARDINA COMUN`)) / nrow(df_wide) * 100
  ), 1)
)

print(cobertura)
cat("\n")

# ==============================================================================
# 10. CREAR VARIABLES PARA IAIDS
# ==============================================================================

cat("=== 10. CREANDO VARIABLES LOGARÍTMICAS ===\n\n")

df_wide_iaids <- df_wide %>%
  mutate(
    # Logaritmos de precios
    ln_P_ANCHOVETA = log(PRECIO_REAL_MACRO_ANCHOVETA),
    ln_P_JUREL = log(PRECIO_REAL_MACRO_JUREL),
    ln_P_SARDINA = log(`PRECIO_REAL_MACRO_SARDINA COMUN`),
    
    # Logaritmos de cantidades
    ln_Q_ANCHOVETA = log(Q_MACRO_ANCHOVETA),
    ln_Q_JUREL = log(Q_MACRO_JUREL),
    ln_Q_SARDINA = log(`Q_MACRO_SARDINA COMUN`),
    
    # Logaritmo precio harina
    ln_P_HARINA = log(P_HARINA_REAL),
    
    # Variables de control
    TENDENCIA = (ANIO - 2012) * 12 + MES,
    D_VEDA = MES %in% c(8, 9),
    D_TEMPORADA_ALTA = MES %in% c(3, 4, 5, 6, 7),
    FACTOR_MES = factor(MES),
    
    # Rezagos de cantidades (IVs predeterminados)
    ln_Q_ANCHOVETA_LAG1 = lag(ln_Q_ANCHOVETA, 1),
    ln_Q_JUREL_LAG1 = lag(ln_Q_JUREL, 1),
    ln_Q_SARDINA_LAG1 = lag(ln_Q_SARDINA, 1)
  )

cat("Variables creadas:\n")
cat("  Precios log: ln_P_ANCHOVETA, ln_P_JUREL, ln_P_SARDINA\n")
cat("  Cantidades log: ln_Q_ANCHOVETA, ln_Q_JUREL, ln_Q_SARDINA\n")
cat("  Controles: TENDENCIA, D_VEDA, D_TEMPORADA_ALTA, FACTOR_MES\n")
cat("  Instrumentos lag: ln_Q_*_LAG1\n\n")

# ==============================================================================
# 11. ESTADÍSTICAS FINALES
# ==============================================================================

cat("=== 11. ESTADÍSTICAS FINALES PRE-ESTIMACIÓN ===\n\n")

cat("OBSERVACIONES POR CONFIGURACIÓN:\n")
config <- df_wide_iaids %>%
  mutate(
    Config = case_when(
      !is.na(ln_P_ANCHOVETA) & !is.na(ln_P_JUREL) & !is.na(ln_P_SARDINA) ~ "3 especies",
      !is.na(ln_P_ANCHOVETA) & !is.na(ln_P_JUREL) ~ "Anch + Jurel",
      !is.na(ln_P_ANCHOVETA) & !is.na(ln_P_SARDINA) ~ "Anch + Sardina",
      !is.na(ln_P_JUREL) & !is.na(ln_P_SARDINA) ~ "Jurel + Sardina",
      !is.na(ln_P_ANCHOVETA) ~ "Solo Anchoveta",
      !is.na(ln_P_JUREL) ~ "Solo Jurel",
      !is.na(ln_P_SARDINA) ~ "Solo Sardina",
      TRUE ~ "Ninguna"
    )
  ) %>%
  count(Config, name = "N_meses")

print(config)
cat("\n")

# Guardar base wide para IAIDS
write_csv(df_wide_iaids, "base_wide_iaids.csv")
cat("✓ Base WIDE guardada: base_wide_iaids.csv\n\n")

# ==============================================================================
# 12. RESUMEN FINAL
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("RESUMEN: BASE LISTA PARA IAIDS\n")
cat(rep("=", 80), "\n\n")

cat("DATOS:\n")
cat("  Total observaciones (long):", nrow(df), "\n")
cat("  Meses únicos (wide):", nrow(df_wide_iaids), "\n")
cat("  Período:", min(df$ANIO), "-", max(df$ANIO), "\n")
cat("  Especies: Anchoveta, Jurel, Sardina Común\n\n")

cat("PRECIOS (todos en pesos 2024):\n")
cat("  ✓ PRECIO_REAL_MACRO (ex-vessel)\n")
cat("  ✓ P_HARINA_REAL (FOB)\n")
cat("  ✓ PRECIO_DIESEL_REAL (combustible)\n\n")

cat("INSTRUMENTOS:\n")
cat("  ✓ SST_MACRO, CHL_A_MACRO, WIND_SPEED_MACRO\n")
cat("  ✓ PRECIO_DIESEL_REAL\n")
cat("  ✓ Transformaciones: SST2, CHL_A2, SST_X_CHL\n")
cat("  ✓ Rezagos: ln_Q_*_LAG1\n")
cat("  ✓ Regulatorios: D_VEDA, D_TEMPORADA_ALTA\n\n")

cat("CONTROLES:\n")
cat("  ✓ TENDENCIA\n")
cat("  ✓ SHARE_INDUSTRIAL\n")
cat("  ✓ FACTOR_MES (efectos fijos)\n\n")

cat("CORRELACIONES INSTRUMENTOS:\n")
if (nrow(high_cor) > 0) {
  cat("Revisar correlaciones altas antes de estimar\n\n")
} else {
  cat("  ✓ Sin problemas de multicolinealidad\n\n")
}

cat("PRÓXIMO PASO:\n")
cat("  Ejecutar: source('05_estimacion_IAIDS.R')\n\n")

cat(rep("=", 80), "\n\n")