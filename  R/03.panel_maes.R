# ==============================================================================
# INTEGRACIÓN DE VARIABLES ECONÓMICAS RAW (DESDE ARCHIVOS EXCEL ORIGINALES)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(readxl)

cat(strrep("=", 70), "\n")
cat("INYECTANDO VARIABLES ECONÓMICAS DESDE EXCEL AL PANEL\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
# 1. CARGAR TIPO DE CAMBIO (Banco Central)
# ==============================================================================
cat("1. Procesando Tipo de Cambio...\n")
df_tc <- read_excel("TC_2012-2024.xlsx", skip = 2) %>%
  rename(Fecha = 1, TC = 2) %>%
  drop_na(Fecha, TC) %>%
  mutate(
    date = ymd(Fecha),
    ANIO = year(date),
    MES = month(date),
    TC = as.numeric(TC)
  ) %>%
  select(ANIO, MES, TC)

# ==============================================================================
# 2. CARGAR PRECIO FOB INTERNACIONAL (Harina de Pescado)
# ==============================================================================
cat("2. Procesando Precio FOB (USD/ton)...\n")
df_fob <- read_excel("harina_pescado_FOB.xlsx", skip = 2) %>%
  rename(Fecha = 1, FOB_USD = 2) %>%
  drop_na(Fecha, FOB_USD) %>%
  mutate(
    date = ymd(Fecha),
    ANIO = year(date),
    MES = month(date),
    FOB_USD = as.numeric(FOB_USD)
  ) %>%
  select(ANIO, MES, FOB_USD)

# ==============================================================================
# 3. CARGAR IPC Y CREAR DEFLACTOR (INE / Banco Central)
# ==============================================================================
cat("3. Procesando IPC y calculando Deflactor Base...\n")
df_ipc <- read_excel("IPC_2012_2024.xlsx", sheet = "Cuadro", skip = 2) %>%
  rename(Fecha = 1, Var_Mensual = 2) %>%
  drop_na(Fecha, Var_Mensual) %>%
  mutate(
    date = ymd(Fecha),
    ANIO = year(date),
    MES = month(date),
    Factor = 1 + (as.numeric(Var_Mensual) / 100),
    IPC_Index = 100 * cumprod(Factor),
    DEFLACTOR = IPC_Index / 100
  ) %>%
  select(ANIO, MES, DEFLACTOR)

# ==============================================================================
# 4. CARGAR PRECIO DEL DIÉSEL REGIONALIZADO (CNE / ENAP)
# ==============================================================================
cat("4. Procesando Precio del Diésel con precisión regional...\n")

df_diesel_raw <- read_excel("precios_combustibles.xlsx", sheet = "Petróleo Diesel", skip = 8)

# Buscamos dinámicamente las columnas
col_fecha <- grep("Fecha", names(df_diesel_raw), ignore.case = TRUE, value = TRUE)[1]
col_R5  <- grep("VALPARA", names(df_diesel_raw), ignore.case = TRUE, value = TRUE)[1]
col_R8  <- grep("CONCEPCI", names(df_diesel_raw), ignore.case = TRUE, value = TRUE)[1]
col_R10 <- grep("PUERTO MONTT", names(df_diesel_raw), ignore.case = TRUE, value = TRUE)[1]
col_R14 <- grep("VALDIVIA", names(df_diesel_raw), ignore.case = TRUE, value = TRUE)[1]

df_diesel <- df_diesel_raw %>%
  select(
    Fecha = all_of(col_fecha),
    `5`  = all_of(col_R5),
    `8`  = all_of(col_R8),
    `10` = all_of(col_R10),
    `14` = all_of(col_R14)
  ) %>%
  # Limpiamos las fechas inválidas (como notas al final del excel)
  mutate(date = suppressWarnings(ymd(Fecha))) %>%
  drop_na(date) %>%
  mutate(
    ANIO = year(date),
    MES = month(date),
    # CORRECCIÓN VITAL: Convertir todas las regiones a numérico antes de juntarlas
    across(c(`5`, `8`, `10`, `14`), ~ suppressWarnings(as.numeric(.x)))
  ) %>%
  pivot_longer(
    cols = c(`5`, `8`, `10`, `14`),
    names_to = "RG",
    values_to = "P_DIESEL_NOMINAL"
  ) %>%
  mutate(RG = as.numeric(RG)) %>%
  # Promediamos si hay más de un reporte por mes
  group_by(ANIO, MES, RG) %>%
  summarise(P_DIESEL_NOMINAL = mean(P_DIESEL_NOMINAL, na.rm = TRUE), .groups = "drop") %>%
  # Eliminamos los NAs que surgieron de los "ND" o "NE"
  drop_na()

# ==============================================================================
# 5. UNIÓN TOTAL AL PANEL AMBIENTAL
# ==============================================================================
cat("5. Cruzando datos económicos con el Panel Ambiental...\n")

df_panel_env <- read_csv("panel_con_env_puerto.csv", show_col_types = FALSE)

df_final <- df_panel_env %>%
  left_join(df_tc, by = c("ANIO", "MES")) %>%
  left_join(df_fob, by = c("ANIO", "MES")) %>%
  left_join(df_ipc, by = c("ANIO", "MES")) %>%
  left_join(df_diesel, by = c("ANIO", "MES", "RG")) %>% # Unión cruzada por Región
  
  # Cálculos Económicos Reales
  mutate(
    P_FOB_CLP = FOB_USD * TC,
    P_FOB_REAL         = P_FOB_CLP / DEFLACTOR,
    P_complejo_real    = P_complejo / DEFLACTOR,
    PRECIO_DIESEL_REAL = P_DIESEL_NOMINAL / DEFLACTOR,
    
    ln_P_complejo = log(P_complejo_real),
    ln_P_FOB      = log(P_FOB_REAL),
    ln_DIESEL     = log(PRECIO_DIESEL_REAL)
  ) %>%
  
  drop_na(ln_P_complejo, ln_P_FOB, ln_DIESEL)

# ==============================================================================
# 6. EXPORTAR BASE MAESTRA
# ==============================================================================
write_csv(df_final, "panel_maestro_definitivo.csv")

cat("\n")
cat(strrep("=", 70), "\n")
cat("¡ÉXITO! Base purificada guardada como 'panel_maestro_definitivo.csv'\n")
cat("Observaciones listas para el modelo IV:", nrow(df_final), "\n")
cat(strrep("=", 70), "\n")