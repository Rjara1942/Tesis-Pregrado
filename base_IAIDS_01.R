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

#===========================================
# estimacion IAIDS
#=========================================
library(tidyverse)
library(zoo) # Necesario para interpolar precios (na.approx)

# 1. CARGAR BASE
df_base <- read_csv("base_3SLS.csv")

df_completa <- df_base %>%
  filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN", "JUREL")) %>%
  
  # A. Completar el Panel (Todas las especies en todos los meses)
  complete(ANIO, MES, NM_RECURSO) %>%
  
  # B. Tratamiento de NAs
  group_by(NM_RECURSO) %>%
  arrange(ANIO, MES) %>%
  mutate(
    # Si Q es NA, es 0
    Q_Mercado = replace_na(Q_Mercado, 0),
    
    # Si D_Shock es NA, es 0
    D_Shock = replace_na(D_Shock, 0),
    
    # IMPUTACIÓN DE PRECIOS (Interpolación Lineal)
    # na.approx rellena los huecos basándose en el mes anterior y siguiente
    # rule=2 permite rellenar los extremos (inicio/fin) con el valor más cercano
    Precio_Real = na.approx(Precio_Real, rule = 2), 
    
    # Rellenar Exógenas (Hacia abajo y hacia arriba)
    across(c(P_Harina, P_Diesel, SST, Viento), ~na.locf(., na.rm = FALSE)),
    across(c(P_Harina, P_Diesel, SST, Viento), ~na.locf(., fromLast = TRUE))
  ) %>%
  ungroup()

# 3. PREPARACIÓN PARA SYSTEMFIT
df_modelo <- df_completa %>%
  # Calcular H Total (Escala)
  group_by(ANIO, MES) %>%
  mutate(H_Total = sum(Q_Mercado, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # Pivotear a Ancho
  pivot_wider(
    names_from = NM_RECURSO,
    values_from = c(Precio_Real, Q_Mercado, D_Shock)
  ) %>%
  rename_with(~str_replace_all(., " ", "_")) %>%
  
  # Colapsar duplicados
  group_by(ANIO, MES) %>%
  summarise(across(everything(), ~first(.)), .groups="drop") %>%
  
  # Logaritmos
  mutate(
    # Precios
    ln_P_Anchoveta = log(Precio_Real_ANCHOVETA),
    ln_P_Sardina   = log(Precio_Real_SARDINA_COMUN),
    ln_P_Jurel     = log(Precio_Real_JUREL),
    
    # Cantidades (log(Q+1) salva los meses sin pesca)
    ln_h_Anchoveta = log(Q_Mercado_ANCHOVETA + 1),
    ln_h_Sardina   = log(Q_Mercado_SARDINA_COMUN + 1),
    ln_h_Jurel     = log(Q_Mercado_JUREL + 1),
    ln_H           = log(H_Total + 1),
    
    # Exógenas
    ln_P_FishMeal  = log(P_Harina),
    ln_Diesel      = log(P_Diesel),
    ln_SST         = log(SST),
    ln_Viento      = log(Viento),
    D_Shock_Jurel  = D_Shock_JUREL
  ) %>%
  # Filtro final de seguridad
  filter(!is.na(ln_P_Anchoveta) & !is.na(ln_P_Sardina) & !is.na(ln_P_Jurel))

# 4. ESTIMACIÓN 3SLS (IAIDS)
mis_instrumentos <- ~ ln_Diesel + ln_SST + ln_Viento + ln_P_FishMeal + D_Shock_Jurel + ln_H

# Sistema de Ecuaciones
eq_anch  <- ln_P_Anchoveta ~ ln_h_Anchoveta + ln_h_Sardina + ln_h_Jurel + ln_H + ln_P_FishMeal
eq_sard  <- ln_P_Sardina   ~ ln_h_Sardina + ln_h_Anchoveta + ln_h_Jurel + ln_H + ln_P_FishMeal
eq_jurel <- ln_P_Jurel     ~ ln_h_Jurel + ln_h_Anchoveta + ln_h_Sardina + ln_H + ln_P_FishMeal + D_Shock_Jurel

sistema <- list(Anchoveta = eq_anch, Sardina = eq_sard, Jurel = eq_jurel)

print("Estimando Modelo Final...")
fit_3sls <- systemfit(sistema, method = "3SLS", inst = mis_instrumentos, data = df_modelo)