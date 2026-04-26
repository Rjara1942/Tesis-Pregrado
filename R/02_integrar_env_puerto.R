# ==============================================================================
# INTEGRACION DE VARIABLES AMBIENTALES A NIVEL DE PUERTO
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("1. CARGANDO PANEL BASE\n")
cat(strrep("=", 70), "\n\n")

df_panel <- read_csv("panel.base.csv")

cat("   Observaciones:", nrow(df_panel), "\n")
cat("   Plantas:", n_distinct(df_panel$NUI), "\n")
cat("   Regiones:", paste(unique(df_panel$RG), collapse = ", "), "\n")

# ==============================================================================
# 2. CREAR MAPEO PLANTA -> PUERTO
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("2. MAPEO PLANTA -> PUERTO\n")
cat(strrep("=", 70), "\n\n")

# Mapeo basado en la región de cada planta
# NOTA: Verificar con información real si disponible

mapa_plantas <- tribble(
  ~NUI,   ~Puerto,
  # Region 5 - Valparaíso
  10452,  "San Antonio",
  

  # Region 8 - BioBío (asignación según ubicación aproximada)
  10973,  "Talcahuano (San Vicente)",
  90073,  "Coronel",
  10951,  "Talcahuano (San Vicente)",
  13702,  "Coronel",
  14815,  "Talcahuano (San Vicente)",
  14838,  "Lota",
  17003,  "Talcahuano (San Vicente)",
  90099,  "Coronel",
  2421,   "Talcahuano (San Vicente)",
  2743,   "Coronel",
  2332,   "Lota",
  15219,  "Talcahuano (San Vicente)",
  13476,  "Coronel",
  
  # Region 10 - Los Lagos
  1688,   "Calbuco",
  
  # Region 14 - Los Ríos
  14837,  "Corral"
)

# Unir mapeo al panel
df_panel <- df_panel %>%
  left_join(mapa_plantas, by = "NUI")

# Verificar mapeo
cat("Mapeo planta -> puerto:\n\n")
df_panel %>%
  select(NUI, RG, Puerto) %>%
  distinct() %>%
  arrange(RG, NUI) %>%
  print(n = 20)

# ==============================================================================
# 3. CARGAR DATOS AMBIENTALES POR PUERTO
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("3. CARGANDO DATOS AMBIENTALES POR PUERTO\n")
cat(strrep("=", 70), "\n\n")

data_env <- read_csv("data_env_puertos.csv")

cat("   Observaciones diarias:", nrow(data_env), "\n")
cat("   Puertos:", paste(unique(data_env$Puerto), collapse = ", "), "\n")

# ==============================================================================
# 4. AGREGAR A NIVEL PUERTO-MES
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("4. AGREGANDO A NIVEL PUERTO-MES\n")
cat(strrep("=", 70), "\n\n")

# Procesar fechas
data_env <- data_env %>%
  mutate(
    date = ymd(date),
    ANIO = year(date),
    MES = month(date)
  ) %>%
  filter(ANIO >= 2012, ANIO <= 2024)

# Agregar por Puerto-Mes (CLAVE: mantener variación entre puertos)
env_por_puerto <- data_env %>%
  group_by(ANIO, MES, Puerto) %>%
  summarise(
    # Variables principales (radio 0-60km)
    SST_PUERTO = mean(sst_0_60km, na.rm = TRUE),
    CHL_A_PUERTO = mean(chl_0_60km, na.rm = TRUE),
    WIND_PUERTO = mean(speed_mean_0_60km, na.rm = TRUE),
    
    # Variables adicionales
    SO_PUERTO = mean(so_0_60km, na.rm = TRUE),           # Salinidad
    CURRENT_PUERTO = mean(current_speed_0_60km, na.rm = TRUE), # Corrientes
    
    # Máximos y mínimos
    WIND_MAX_PUERTO = mean(speed_max_0_60km, na.rm = TRUE),
    
    .groups = "drop"
  )

cat("   Observaciones puerto-mes:", nrow(env_por_puerto), "\n")

# Verificar variación entre puertos
cat("\n   Ejemplo: SST por puerto en Enero 2020:\n\n")
env_por_puerto %>%
  filter(ANIO == 2020, MES == 1) %>%
  select(Puerto, SST_PUERTO, CHL_A_PUERTO) %>%
  arrange(SST_PUERTO) %>%
  print()

# ==============================================================================
# 5. TAMBIÉN CREAR VARIABLES MACRO (para comparación)
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("5. CREANDO VARIABLES MACRO (para comparación)\n")
cat(strrep("=", 70), "\n\n")

env_macro <- data_env %>%
  group_by(ANIO, MES) %>%
  summarise(
    SST_MACRO = mean(sst_0_60km, na.rm = TRUE),
    CHL_A_MACRO = mean(chl_0_60km, na.rm = TRUE),
    WIND_MACRO = mean(speed_mean_0_60km, na.rm = TRUE),
    .groups = "drop"
  )

cat("   Observaciones macro:", nrow(env_macro), "\n")

# ==============================================================================
# 6. UNIR AL PANEL
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("6. UNIENDO VARIABLES AMBIENTALES AL PANEL\n")
cat(strrep("=", 70), "\n\n")

# Unir por puerto (CORRECCIÓN CLAVE)
df_panel <- df_panel %>%
  left_join(
    env_por_puerto,
    by = c("ANIO", "MES", "Puerto")
  )

# También unir macro para comparación
df_panel <- df_panel %>%
  left_join(
    env_macro,
    by = c("ANIO", "MES")
  )

cat("   NAs en SST_PUERTO:", sum(is.na(df_panel$SST_PUERTO)), "\n")
cat("   NAs en SST_MACRO:", sum(is.na(df_panel$SST_MACRO)), "\n")

# ==============================================================================
# 7. VERIFICAR VARIACIÓN CROSS-SECTIONAL
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("7. VERIFICANDO VARIACIÓN CROSS-SECTIONAL\n")
cat(strrep("=", 70), "\n\n")

# Ejemplo: SST en Junio 2020 por planta
cat("   SST en Junio 2020 por planta:\n\n")
df_panel %>%
  filter(ANIO == 2020, MES == 6) %>%
  select(NUI, Puerto, SST_PUERTO, SST_MACRO) %>%
  distinct() %>%
  arrange(SST_PUERTO) %>%
  print()

# Calcular varianzas
var_puerto <- df_panel %>%
  group_by(NUI) %>%
  summarise(SST = mean(SST_PUERTO, na.rm = TRUE)) %>%
  pull(SST) %>%
  var(na.rm = TRUE)

var_macro <- df_panel %>%
  group_by(NUI) %>%
  summarise(SST = mean(SST_MACRO, na.rm = TRUE)) %>%
  pull(SST) %>%
  var(na.rm = TRUE)

cat(sprintf("\n   Varianza between-planta SST_PUERTO: %.4f\n", var_puerto))
cat(sprintf("   Varianza between-planta SST_MACRO:  %.4f\n", var_macro))
cat(sprintf("   Ratio (PUERTO/MACRO): %.1f veces más variación\n", var_puerto / max(var_macro, 0.0001)))

# ==============================================================================
# 8. CREAR TRANSFORMACIONES
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("8. CREANDO TRANSFORMACIONES\n")
cat(strrep("=", 70), "\n\n")

df_panel <- df_panel %>%
  mutate(
    # Cuadráticos
    SST_PUERTO2 = SST_PUERTO^2,
    CHL_A_PUERTO2 = CHL_A_PUERTO^2,
    WIND_PUERTO2 = WIND_PUERTO^2,
    
    # Interacciones
    SST_X_CHL_PUERTO = SST_PUERTO * CHL_A_PUERTO,
    SST_X_WIND_PUERTO = SST_PUERTO * WIND_PUERTO,
    
    # Logaritmos
    ln_SST_PUERTO = log(SST_PUERTO),
    ln_CHL_A_PUERTO = log(CHL_A_PUERTO + 0.01),  # +0.01 para evitar log(0)
    ln_WIND_PUERTO = log(WIND_PUERTO)
  )

# Crear rezagos de SST_PUERTO (por planta)
df_panel <- df_panel %>%
  arrange(NUI, ANIO, MES) %>%
  group_by(NUI) %>%
  mutate(
    SST_PUERTO_L1 = lag(SST_PUERTO, 1),
    SST_PUERTO_L2 = lag(SST_PUERTO, 2),
    CHL_A_PUERTO_L1 = lag(CHL_A_PUERTO, 1)
  ) %>%
  ungroup()

cat("   Transformaciones creadas:\n")
cat("   - Cuadráticos: SST_PUERTO2, CHL_A_PUERTO2, WIND_PUERTO2\n")
cat("   - Interacciones: SST_X_CHL_PUERTO, SST_X_WIND_PUERTO\n")
cat("   - Logaritmos: ln_SST_PUERTO, ln_CHL_A_PUERTO, ln_WIND_PUERTO\n")
cat("   - Rezagos: SST_PUERTO_L1, SST_PUERTO_L2, CHL_A_PUERTO_L1\n")

# ==============================================================================
# 9. GUARDAR PANEL CON SST_PUERTO
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("9. GUARDANDO PANEL\n")
cat(strrep("=", 70), "\n\n")

write_csv(df_panel, "panel_con_env_puerto.csv")
cat("   Guardado: panel_con_sst_puerto.csv\n")
cat("   Dimensiones:", nrow(df_panel), "x", ncol(df_panel), "\n")

