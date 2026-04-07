# =============================================================================
# SCRIPT 07: INTEGRAR FOB PERÚ Y ANALIZAR ALTERNATIVAS A D_VEDA
# =============================================================================
#   1. Incorpora precio FOB Perú (BCRP) al panel maestro
#   2. Aplica tipo de cambio y deflactación consistente con FOB Chile
#   3. Analiza alternativas al tratamiento de veda como dummy
# =============================================================================

library(tidyverse)
library(lubridate)

# =============================================================================
# PARTE 1: CARGAR DATOS
# =============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PARTE 1: CARGA DE DATOS\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Panel maestro
panel <- read_csv("panel_maestro_integrado.csv", show_col_types = FALSE)
cat("✓ Panel maestro:", nrow(panel), "obs ×", ncol(panel), "vars\n")

# FOB Perú (BCRP)
# El archivo tiene 2 filas de header, formato: "MesAño,Precio"
fob_peru_raw <- read_csv("precio_peru.csv", 
                          skip = 2, 
                          col_names = c("periodo", "FOB_PERU_USD"),
                          show_col_types = FALSE)

cat("✓ FOB Perú crudo:", nrow(fob_peru_raw), "observaciones\n")

# =============================================================================
# PARTE 2: PROCESAR FOB PERÚ
# =============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PARTE 2: PROCESAMIENTO FOB PERÚ\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Mapeo de meses español -> número
meses_map <- c(
  "Ene" = 1, "Feb" = 2, "Mar" = 3, "Abr" = 4, "May" = 5, "Jun" = 6,
  "Jul" = 7, "Ago" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dic" = 12
)

# Parsear periodo "Ene12" -> ANIO, MES
fob_peru <- fob_peru_raw %>%
  mutate(
    # Extraer mes (primeras 3 letras)
    mes_str = str_sub(periodo, 1, 3),
    # Extraer año (últimos 2 dígitos)
    anio_str = str_sub(periodo, 4, 5),
    # Convertir
    MES = meses_map[mes_str],
    ANIO = as.integer(anio_str) + 2000  # Asume 20XX
  ) %>%
  select(ANIO, MES, FOB_PERU_USD) %>%
  filter(!is.na(ANIO) & !is.na(MES))

cat("✓ FOB Perú procesado:", nrow(fob_peru), "meses\n")
cat("  Período:", min(fob_peru$ANIO), "-", max(fob_peru$ANIO), "\n")

# Verificar estadísticas
cat("\n  Estadísticas FOB Perú (USD/ton):\n")
cat("    Media:  $", round(mean(fob_peru$FOB_PERU_USD, na.rm=TRUE), 0), "\n")
cat("    Min:    $", round(min(fob_peru$FOB_PERU_USD, na.rm=TRUE), 0), "\n")
cat("    Max:    $", round(max(fob_peru$FOB_PERU_USD, na.rm=TRUE), 0), "\n")

# =============================================================================
# PARTE 3: MERGE CON PANEL Y APLICAR TC + DEFLACTOR
# =============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PARTE 3: INTEGRACIÓN AL PANEL\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Merge por ANIO-MES
panel_con_peru <- panel %>%
  left_join(fob_peru, by = c("ANIO", "MES"))

# Verificar match
n_matched <- sum(!is.na(panel_con_peru$FOB_PERU_USD))
cat("✓ Observaciones con FOB Perú:", n_matched, "de", nrow(panel_con_peru), 
    "(", round(n_matched/nrow(panel_con_peru)*100, 1), "%)\n")

# Aplicar tipo de cambio y deflactación (usando TC y DEFLACTOR del panel)
panel_con_peru <- panel_con_peru %>%
  mutate(
    # Convertir a CLP nominal
    P_FOB_PERU_CLP = FOB_PERU_USD * TC,
    # Deflactar a pesos reales
    P_FOB_PERU_REAL = P_FOB_PERU_CLP / DEFLACTOR,
    # Logaritmo
    ln_P_FOB_PERU = log(P_FOB_PERU_REAL)
  )

# Comparar con FOB Chile
cat("\n  Comparación FOB Chile vs Perú (en panel):\n")
comparison <- panel_con_peru %>%
  filter(!is.na(ln_P_FOB) & !is.na(ln_P_FOB_PERU)) %>%
  summarise(
    N = n(),
    FOB_Chile_mean = mean(P_FOB_REAL, na.rm=TRUE),
    FOB_Peru_mean = mean(P_FOB_PERU_REAL, na.rm=TRUE),
    Diferencia_pct = (FOB_Chile_mean - FOB_Peru_mean) / FOB_Peru_mean * 100,
    Correlacion = cor(ln_P_FOB, ln_P_FOB_PERU, use="complete.obs")
  )
print(comparison)

# =============================================================================
# PARTE 4: ANÁLISIS DE LA VEDA
# =============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PARTE 4: ANÁLISIS DE D_VEDA Y ALTERNATIVAS\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# 4.1 Distribución actual
cat("\n4.1 Distribución de D_VEDA:\n")
veda_dist <- panel_con_peru %>%
  group_by(D_VEDA) %>%
  summarise(
    N = n(),
    h_complejo_mean = mean(h_complejo, na.rm=TRUE),
    P_complejo_mean = mean(P_complejo, na.rm=TRUE),
    .groups = "drop"
  )
print(veda_dist)

# 4.2 Por mes
cat("\n4.2 Estadísticas por MES:\n")
mes_stats <- panel_con_peru %>%
  group_by(MES) %>%
  summarise(
    N = n(),
    D_VEDA = first(D_VEDA),
    h_complejo_mean = round(mean(h_complejo, na.rm=TRUE), 0),
    h_complejo_sd = round(sd(h_complejo, na.rm=TRUE), 0),
    P_complejo_mean = round(mean(P_complejo, na.rm=TRUE), 0),
    .groups = "drop"
  ) %>%
  mutate(CV_h = round(h_complejo_sd / h_complejo_mean * 100, 1))
print(mes_stats, n=12)

# 4.3 Problema identificado
cat("\n4.3 PROBLEMAS CON D_VEDA COMO DUMMY:\n")
cat("
┌─────────────────────────────────────────────────────────────────────┐
│ PROBLEMA 1: Desbalance extremo                                      │
│   - Solo ", sum(panel_con_peru$D_VEDA == 1), " obs con D_VEDA=1 vs ", 
    sum(panel_con_peru$D_VEDA == 0), " con D_VEDA=0                    │
│   - Coeficiente estimado con muy poca información                   │
│                                                                     │
│ PROBLEMA 2: Colinealidad con efectos fijos                          │
│   - D_VEDA = 1 siempre en meses 8-9                                 │
│   - Si incluyes factor(MES), D_VEDA es redundante                   │
│                                                                     │
│ PROBLEMA 3: El efecto ya está en ln_h_complejo                      │
│   - La veda reduce capturas → P sube                                │
│   - Este canal ya está capturado por la variable endógena           │
│   - D_VEDA captura efecto ADICIONAL (¿cuál?)                        │
│                                                                     │
│ PROBLEMA 4: Resultado contraintuitivo en IV                         │
│   - OLS: D_VEDA = -0.16 (precio 16% menor en veda)                  │
│   - IV:  D_VEDA = -0.76 (¡precio 76% menor!) ← SOSPECHOSO           │
└─────────────────────────────────────────────────────────────────────┘
")

# =============================================================================
# PARTE 5: CREAR ALTERNATIVAS A D_VEDA
# =============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PARTE 5: ALTERNATIVAS A D_VEDA\n")
cat(paste(rep("=", 70), collapse=""), "\n")

panel_con_peru <- panel_con_peru %>%
  mutate(
    # ALTERNATIVA 1: Separar tipos de veda
    D_VEDA_REPRO = ifelse(MES %in% c(8, 9), 1, 0),      # Veda reproductiva
    D_VEDA_RECLUT = ifelse(MES %in% c(1, 2), 1, 0),     # Veda reclutamiento
    
    # ALTERNATIVA 2: Estacionalidad continua (seno/coseno)
    # Captura ciclo anual sin dummies
    SEASON_SIN = sin(2 * pi * MES / 12),
    SEASON_COS = cos(2 * pi * MES / 12),
    
    # ALTERNATIVA 3: Intensidad de captura relativa al mes
    # (requiere calcular promedio mensual primero)
    MES_factor = factor(MES)
  )

# Calcular promedio mensual histórico para cada mes
promedios_mes <- panel_con_peru %>%
  group_by(MES) %>%
  summarise(h_promedio_mes = mean(h_complejo, na.rm=TRUE), .groups="drop")

panel_con_peru <- panel_con_peru %>%
  left_join(promedios_mes, by = "MES") %>%
  mutate(
    # ALTERNATIVA 4: Desviación del promedio mensual (z-score)
    h_desv_mensual = (h_complejo - h_promedio_mes) / sd(h_complejo, na.rm=TRUE),
    
    # ALTERNATIVA 5: Ratio respecto al promedio mensual
    h_ratio_mensual = h_complejo / h_promedio_mes
  )

cat("✓ Variables alternativas creadas:\n")
cat("  - D_VEDA_REPRO:    Dummy veda reproductiva (Ago-Sep)\n")
cat("  - D_VEDA_RECLUT:   Dummy veda reclutamiento (Ene-Feb)\n")
cat("  - SEASON_SIN/COS:  Armónicos estacionales (Fourier)\n")
cat("  - h_desv_mensual:  Z-score respecto al promedio del mes\n")
cat("  - h_ratio_mensual: Ratio h_complejo / h_promedio_mes\n")

# =============================================================================
# PARTE 6: RECOMENDACIONES
# =============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PARTE 6: RECOMENDACIONES\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("
┌─────────────────────────────────────────────────────────────────────┐
│                    RECOMENDACIONES                                   │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│ OPCIÓN A: ELIMINAR D_VEDA (PREFERIDA)                               │
│   - El efecto de la veda YA está capturado en ln_h_complejo         │
│   - La veda afecta P solo vía reducción de Q                        │
│   - Modelo más parsimonioso y sin problemas de identificación       │
│                                                                     │
│   Ecuación:                                                         │
│   ln_P = α + γ₁·ln_h_complejo + γ₂·ln_h_jurel + β·ln_P_FOB         │
│          + δ·TENDENCIA + ε                                          │
│                                                                     │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│ OPCIÓN B: USAR ESTACIONALIDAD CONTINUA                              │
│   - Reemplazar D_VEDA por SEASON_SIN + SEASON_COS                   │
│   - Captura ciclo anual completo, no solo veda                      │
│   - No tiene problemas de colinealidad con efectos fijos            │
│                                                                     │
│   Ecuación:                                                         │
│   ln_P = α + γ·ln_h_complejo + β·ln_P_FOB                          │
│          + θ₁·sin(2π·MES/12) + θ₂·cos(2π·MES/12) + ε               │
│                                                                     │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│ OPCIÓN C: EFECTOS FIJOS DE MES (SI HAY SUFICIENTES DATOS)          │
│   - Incluir factor(MES) como control                                │
│   - Captura TODA la estacionalidad (veda + otros efectos)           │
│   - Requiere suficientes obs por mes (mín ~30)                      │
│                                                                     │
│   ADVERTENCIA: Con solo 435 obs y 16 plantas, los EF de mes         │
│   pueden consumir muchos grados de libertad                         │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
")

# =============================================================================
# PARTE 7: GUARDAR PANEL ACTUALIZADO
# =============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PARTE 7: GUARDAR PANEL ACTUALIZADO\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Seleccionar columnas finales
panel_final <- panel_con_peru %>%
  select(
    # Identificadores
    ANIO, MES, NUI, Puerto, FECHA, yearmonth, MES_fact,
    
    # Variables dependiente y endógenas
    P_complejo, P_complejo_real, ln_P_complejo, ln_P_complejo_nominal,
    h_complejo, h_planta, ln_h_complejo, ln_h_planta,
    h_jurel, ln_h_jurel,
    n_especies, RG,
    
    # Rezagos
    starts_with("ln_h_complejo_L"), starts_with("ln_h_jurel_L"),
    
    # Precios FOB (Chile y Perú)
    TC, DEFLACTOR,
    FOB_USD, P_FOB_CLP, P_FOB_REAL, ln_P_FOB,                    # Chile
    FOB_PERU_USD, P_FOB_PERU_CLP, P_FOB_PERU_REAL, ln_P_FOB_PERU, # Perú
    
    # Diesel
    P_DIESEL_NOMINAL, PRECIO_DIESEL_REAL, ln_DIESEL,
    ln_tipo_cambio,
    
    # Biomasa
    sardine_biomass, anchoveta_biomass, biomasa_complejo,
    ln_biomasa_sardina, ln_biomasa_anchoveta, ln_biomasa_complejo,
    
    # Ambientales por puerto
    SST_PUERTO, CHL_A_PUERTO, WIND_PUERTO, SO_PUERTO, CURRENT_PUERTO, WIND_MAX_PUERTO,
    SST_MACRO, CHL_A_MACRO, WIND_MACRO,
    SST_PUERTO2, CHL_A_PUERTO2, WIND_PUERTO2,
    SST_X_CHL_PUERTO, SST_X_WIND_PUERTO,
    ln_SST_PUERTO, ln_CHL_A_PUERTO, ln_WIND_PUERTO,
    SST_PUERTO_L1, SST_PUERTO_L2, CHL_A_PUERTO_L1,
    
    # Veda y estacionalidad (TODAS las opciones)
    D_VEDA,                              # Original (para comparar)
    D_VEDA_REPRO, D_VEDA_RECLUT,         # Separadas
    SEASON_SIN, SEASON_COS,               # Armónicos
    D_TEMP_ALTA, TENDENCIA,
    h_promedio_mes, h_desv_mensual, h_ratio_mensual,
    
    # Perú
    h_peru, ln_h_peru,
    
    # Ratio
    ratio_exvessel_fob
  )

# Verificar
cat("✓ Panel final:", nrow(panel_final), "obs ×", ncol(panel_final), "vars\n")

# Guardar
write_csv(panel_final, "panel_con_alternativas.csv")


# =============================================================================
# PARTE 8: RESUMEN COMPARATIVO FOB
# =============================================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("PARTE 8: RESUMEN COMPARATIVO FOB CHILE vs PERÚ\n")
cat(paste(rep("=", 70), collapse=""), "\n")

resumen_fob <- panel_final %>%
  filter(!is.na(ln_P_FOB) & !is.na(ln_P_FOB_PERU)) %>%
  summarise(
    N = n(),
    `FOB Chile (CLP real)` = mean(P_FOB_REAL, na.rm=TRUE),
    `FOB Perú (CLP real)` = mean(P_FOB_PERU_REAL, na.rm=TRUE),
    `Diferencia %` = (mean(P_FOB_REAL) - mean(P_FOB_PERU_REAL)) / mean(P_FOB_PERU_REAL) * 100,
    `Correlación (ln)` = cor(ln_P_FOB, ln_P_FOB_PERU, use="complete.obs"),
    `Corr con ln_P_complejo (Chile)` = cor(ln_P_complejo, ln_P_FOB, use="complete.obs"),
    `Corr con ln_P_complejo (Perú)` = cor(ln_P_complejo, ln_P_FOB_PERU, use="complete.obs")
  )

cat("\nResumen:\n")
print(t(round(resumen_fob, 3)))

cat("\n✓ Script completado exitosamente\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")
