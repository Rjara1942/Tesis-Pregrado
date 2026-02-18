# -----------------------------------------------------------------------------
# ESTIMACIÓN FINAL 3SLS
# -----------------------------------------------------------------------------

library(tidyverse)
library(systemfit)
library(readxl)


# Base principal (con Dummies y Deflactor)
df_main <- read_csv("BASE_CON_DUMMIES.csv")

# Base de Precio de Harina (FOB)
df_harina <- read_csv("FOB_CLP.csv") %>%
  rename(ANIO = year, MES = month) %>%
  select(ANIO, MES, P_HARINA_NOMINAL = P_harina_FOB_CLP)


# Diesel (Costo de Flota) 
# Usamos los nombres exactos de las columnas según la inspección del archivo
df_diesel <- read_excel("precios_combustibles.xlsx", 
                        sheet = "Petróleo Diesel", 
                        skip = 8, 
                        na = c("", "ND", "NE", "NaN")) %>%
  # Seleccionamos explícitamente la columna Fecha y las Ciudades
  select(
    Fecha_Txt = Fecha,           # La columna se llama "Fecha" en la fila 9 del Excel
    `5`  = `VALPARAÍSO`,         # Región 5
    `7`  = `TALCA`,              # Región 7
    `8`  = `CONCEPCIÓN`,         # Región 8
    `9`  = `TEMUCO`,             # Región 9
    `14` = `VALDIVIA`,           # Región 14
    `10` = `PUERTO MONTT`        # Región 10
  ) %>%
  filter(!is.na(Fecha_Txt)) %>%
  mutate(
    Fecha = as.Date(Fecha_Txt),
    ANIO = year(Fecha),
    MES = month(Fecha)
  ) %>%
  filter(ANIO >= 2012) %>% # Filtramos solo el periodo relevante
  
  # Convertimos todo a numérico para evitar errores
  mutate(across(c(`5`, `7`, `8`, `9`, `14`, `10`), as.numeric)) %>%
  
  # Pivotamos a formato largo (Una fila por Región-Mes)
  pivot_longer(
    cols = c(`5`, `7`, `8`, `9`, `14`, `10`),
    names_to = "REG",
    values_to = "Precio_Diesel_Nominal"
  ) %>%
  mutate(REG = as.numeric(REG))



# D. Variables Ambientales (SST, Viento) - TU CÓDIGO INCORPORADO

df_env <- read_csv("env_var_por_region.csv") %>%
  rename(ANIO = year, MES = month) %>%
  pivot_longer(
    cols = starts_with("SST") | starts_with("Chl") | starts_with("Wind"),
    names_to = "Variable_Reg",
    values_to = "Valor"
  ) %>%
  mutate(
    REG = as.numeric(str_extract(Variable_Reg, "\\d+")),
    Tipo = case_when(
      str_detect(Variable_Reg, "SST") ~ "SST",
      str_detect(Variable_Reg, "Wind_Regional") ~ "Viento",
      # Puedes agregar clorofila si quieres: str_detect(Variable_Reg, "Chl") ~ "Clorofila",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Tipo)) %>%
  pivot_wider(id_cols = c(ANIO, MES, REG), names_from = Tipo, values_from = Valor)




#  INTEGRACIÓN (CRUCE POR AÑO, MES Y REGIÓN)
# =============================================================================

df_integrada <- df_main %>%
  left_join(df_harina, by = c("ANIO", "MES")) %>%
  left_join(df_diesel, by = c("ANIO", "MES", "REG")) %>%
  left_join(df_env, by = c("ANIO", "MES", "REG")) %>%
  mutate(
    # Deflactamos costos a Pesos Reales de 2024
    PRECIO_HARINA_REAL = P_HARINA_NOMINAL / Deflactor_2024,
    PRECIO_DIESEL_REAL = Precio_Diesel_Nominal / Deflactor_2024
  )

# =============================================================================
# AGREGACIÓN MACROZONAL (ÍNDICES PONDERADOS)
# =============================================================================
# Aquí construimos el dato único mensual para el modelo 3SLS

df_agrupada <- df_integrada %>%
  group_by(ANIO, MES, NM_RECURSO) %>%
  summarise(
    # ENDÓGENAS: Precio y Cantidad Agregada
    Precio_Real = weighted.mean(PRECIO_REAL, w = Q_MERCADO_TOTAL, na.rm = TRUE),
    Q_Mercado   = sum(Q_MERCADO_TOTAL, na.rm = TRUE),
    D_Shock     = max(D_SHOCK_JUREL, na.rm = TRUE),
    
    # EXÓGENAS (INSTRUMENTOS): Promedios de la Macrozona
    P_Harina    = mean(PRECIO_HARINA_REAL, na.rm = TRUE),
    
    # Instrumentos Regionales (Diésel y Clima):
    # Usamos weighted.mean para que el clima de las zonas donde MÁS se pesca pese más.
    # (Si no hubo pesca, Q=0, usamos mean simple para no perder el dato)
    P_Diesel    = tryCatch(weighted.mean(PRECIO_DIESEL_REAL, w = Q_MERCADO_TOTAL, na.rm=TRUE), error=function(e) mean(PRECIO_DIESEL_REAL, na.rm=TRUE)),
    SST         = tryCatch(weighted.mean(SST, w = Q_MERCADO_TOTAL, na.rm=TRUE), error=function(e) mean(SST, na.rm=TRUE)),
    Viento      = tryCatch(weighted.mean(Viento, w = Q_MERCADO_TOTAL, na.rm=TRUE), error=function(e) mean(Viento, na.rm=TRUE)),
    
    .groups = "drop"
  )

write_csv(df_agrupada, "base_3SLS.csv")

