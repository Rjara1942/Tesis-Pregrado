# ==============================================================================
# INTEGRACIÓN COMPLETA DEL PANEL MAESTRO
# ==============================================================================

# Variables a agregar:
#   - ln_h_peru (desembarques Perú)
#   - ln_biomasa_sardina, ln_biomasa_anchoveta, ln_biomasa_complejo
#   - D_TEMP_ALTA, TENDENCIA, ratio_exvessel_fob
#
# ==============================================================================

library(tidyverse)
library(readxl)
library(lubridate)

cat(strrep("=", 70), "\n")
cat("INTEGRACIÓN DE VARIABLES AL PANEL MAESTRO\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
# 1. CARGAR PANEL MAESTRO DEFINITIVO
# ==============================================================================

cat("1. Cargando panel maestro definitivo...\n")

df_panel <- read_csv("panel_maestro_definitivo.csv", show_col_types = FALSE)

cat("   Observaciones:", nrow(df_panel), "\n")
cat("   Variables:", ncol(df_panel), "\n\n")

# ==============================================================================
# 2. CREAR ln_tipo_cambio (TC ya existe en el panel)
# ==============================================================================

cat("2. Creando ln_tipo_cambio desde TC...\n")

df_panel <- df_panel %>%
  mutate(ln_tipo_cambio = log(TC))

cat("   ✓ ln_tipo_cambio creado\n\n")

# ==============================================================================
# 3. CARGAR Y PROCESAR DESEMBARQUES PERÚ
# ==============================================================================

cat("3. Procesando desembarques Perú (BD_PA_2013-2025-sep.xlsx)...\n")

df_peru <- read_excel("BD_PA_2013-2025-sep.xlsx", skip = 4) %>%
  # Filtrar solo anchoveta
  filter(str_detect(toupper(ESPECIE), "ANCHOVETA")) %>%
  # Agregar por año-mes
  group_by(AÑO, MES) %>%
  summarise(
    h_peru = sum(`DESEMBARQUE (TM)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(ANIO = AÑO) %>%
  mutate(ln_h_peru = log(h_peru + 1))

cat("   Observaciones Perú:", nrow(df_peru), "\n")
cat("   Período:", min(df_peru$ANIO), "-", max(df_peru$ANIO), "\n")

# Unir al panel
df_panel <- df_panel %>%
  left_join(df_peru, by = c("ANIO", "MES"))

cat("   ✓ ln_h_peru agregado (NAs:", sum(is.na(df_panel$ln_h_peru)), ")\n\n")

# ==============================================================================
# 4. CARGAR Y PROCESAR BIOMASA
# ==============================================================================

cat("4. Procesando biomasa (biomass_para_costos.xlsx)...\n")

df_biomasa <- read_excel("biomass_para_costos.xlsx") %>%
  rename(ANIO = year) %>%
  mutate(
    # Biomasa del complejo (sardina + anchoveta)
    biomasa_complejo = sardine_biomass + anchoveta_biomass,
    
    # Logaritmos
    ln_biomasa_sardina = log(sardine_biomass),
    ln_biomasa_anchoveta = log(anchoveta_biomass),
    ln_biomasa_complejo = log(biomasa_complejo)
  ) %>%
  select(ANIO, 
         sardine_biomass, anchoveta_biomass, biomasa_complejo,
         ln_biomasa_sardina, ln_biomasa_anchoveta, ln_biomasa_complejo)

cat("   Observaciones biomasa:", nrow(df_biomasa), "\n")
cat("   Período:", min(df_biomasa$ANIO), "-", max(df_biomasa$ANIO), "\n")

# Unir al panel (por año, biomasa es anual)
df_panel <- df_panel %>%
  left_join(df_biomasa, by = "ANIO")

cat("   ✓ Biomasa agregada (NAs:", sum(is.na(df_panel$ln_biomasa_sardina)), ")\n\n")

# ==============================================================================
# 5. CREAR CONTROLES ADICIONALES
# ==============================================================================

cat("5. Creando controles adicionales...\n")

df_panel <- df_panel %>%
  mutate(
    # Dummy temporada alta (marzo-julio)
    D_TEMP_ALTA = ifelse(MES %in% c(3, 4, 5, 6, 7), 1, 0),
    
    # Tendencia temporal (meses desde enero 2012)
    TENDENCIA = (ANIO - 2012) * 12 + MES,
    
    # Ratio precio ex-vessel / FOB (en porcentaje)
    ratio_exvessel_fob = (P_complejo_real / P_FOB_REAL) * 100
  )

cat("   ✓ D_TEMP_ALTA creado\n")
cat("   ✓ TENDENCIA creado\n")
cat("   ✓ ratio_exvessel_fob creado\n\n")

# ==============================================================================
# 6. VERIFICACIÓN DEL PANEL INTEGRADO
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("VERIFICACIÓN DEL PANEL INTEGRADO\n")
cat(strrep("=", 70), "\n\n")

# Variables críticas para el modelo
vars_modelo <- c(
  "ln_P_complejo",      # Dependiente
  "ln_h_complejo",      # Endógena
  "ln_P_FOB",           # Endógena
  "SST_PUERTO",         # IV ambiental
  "CHL_A_PUERTO",       # IV ambiental
  "WIND_PUERTO",        # IV ambiental
  "SST_PUERTO_L1",      # IV ambiental (rezago)
  "ln_DIESEL",          # IV externo
  "ln_tipo_cambio",     # IV externo
  "ln_biomasa_sardina", # IV externo
  "ln_h_peru",          # IV externo
  "ln_h_jurel",         # Control
  "D_VEDA",             # Control
  "D_TEMP_ALTA",        # Control
  "TENDENCIA"           # Control
)

cat("NAs en variables críticas:\n\n")

for (v in vars_modelo) {
  if (v %in% names(df_panel)) {
    na_count <- sum(is.na(df_panel[[v]]))
    na_pct <- na_count / nrow(df_panel) * 100
    status <- ifelse(na_pct < 20, "✓", "⚠")
    cat(sprintf("   %s %-25s: %3d NAs (%5.1f%%)\n", status, v, na_count, na_pct))
  } else {
    cat(sprintf("   ✗ %-25s: NO DISPONIBLE\n", v))
  }
}

# ==============================================================================
# 7. ESTADÍSTICAS CLAVE
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("ESTADÍSTICAS CLAVE\n")
cat(strrep("=", 70), "\n\n")

# Ratio ex-vessel/FOB (esperado 11-13% según Dresdner et al., 2014)
cat("Ratio ex-vessel/FOB (esperado 11-13%):\n")
cat(sprintf("   Media: %.1f%%\n", mean(df_panel$ratio_exvessel_fob, na.rm = TRUE)))
cat(sprintf("   Min:   %.1f%%\n", min(df_panel$ratio_exvessel_fob, na.rm = TRUE)))
cat(sprintf("   Max:   %.1f%%\n", max(df_panel$ratio_exvessel_fob, na.rm = TRUE)))

# Variación SST entre puertos
cat("\nVariación SST_PUERTO (valores únicos por mes):\n")
sst_unique <- df_panel %>%
  group_by(ANIO, MES) %>%
  summarise(n_unique = n_distinct(SST_PUERTO), .groups = "drop") %>%
  pull(n_unique) %>%
  mean()
cat(sprintf("   Promedio: %.1f valores únicos por mes\n", sst_unique))

# Variación DIESEL por región
cat("\nPrecio diesel por región (ln_DIESEL promedio):\n")
df_panel %>%
  group_by(RG) %>%
  summarise(ln_DIESEL_mean = mean(ln_DIESEL, na.rm = TRUE)) %>%
  print()

# ==============================================================================
# 8. GUARDAR PANEL INTEGRADO
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("GUARDANDO PANEL INTEGRADO\n")
cat(strrep("=", 70), "\n\n")

write_csv(df_panel, "panel_maestro_integrado.csv")

cat("   Archivo: panel_maestro_integrado.csv\n")
cat("   Observaciones:", nrow(df_panel), "\n")
cat("   Variables:", ncol(df_panel), "\n")

# ==============================================================================
# 9. RESUMEN FINAL
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("PANEL LISTO PARA ESTIMACIÓN IV\n")
cat(strrep("=", 70), "\n")

cat("
VARIABLES DISPONIBLES PARA EL MODELO:

DEPENDIENTE:
   ln_P_complejo          : Log precio real ex-vessel

ENDÓGENAS:
   ln_h_complejo          : Log desembarques complejo (sardina + anchoveta)
   ln_P_FOB               : Log precio FOB real

IV AMBIENTALES (por puerto - variación cross-sectional):
   SST_PUERTO             : Temperatura superficial del mar
   CHL_A_PUERTO           : Clorofila-a
   WIND_PUERTO            : Velocidad del viento
   SST_PUERTO_L1/L2       : Rezagos de SST

IV EXTERNOS:
   ln_DIESEL              : Log precio diesel real (por región)
   ln_tipo_cambio         : Log tipo de cambio USD/CLP
   ln_biomasa_sardina     : Log biomasa sardina (anual)
   ln_h_peru              : Log desembarques Perú

CONTROLES:
   ln_h_jurel             : Log desembarques jurel
   D_VEDA                 : Dummy veda (ene, ago, sep)
   D_TEMP_ALTA            : Dummy temporada alta (mar-jul)
   TENDENCIA              : Tendencia temporal

EFECTOS FIJOS:
   NUI                    : Efectos fijos planta
   MES                    : Efectos fijos mes (estacionalidad)
")

cat(strrep("=", 70), "\n")

