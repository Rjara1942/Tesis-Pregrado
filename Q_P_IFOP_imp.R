library(tidyverse)
library(readr)
library(lubridate)
library(readxl)
# Cargar base de precio original


precios_og <-read_excel("2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx", 
                            sheet = "PRECIO") 
precios_clean <- precios_og %>%

# filtrar para ANIMAL Y MIXTA (INDUSTRIA DE REDUCCIÓN)
# EXCLUIR HUMANO 
  
rename_with(~str_trim(.)) %>%
  mutate(
    NM_RECURSO = str_trim(NM_RECURSO),
    CLASE_INDUSTRIA_II = str_trim(CLASE_INDUSTRIA_II)
  ) %>%
  filter(
    RG %in% c(5, 6, 7, 8, 9, 10, 14, 16),
    
    
    CLASE_INDUSTRIA_II %in% c("ANIMAL", "MIXTA_AH"),
    
    # ELIMINAR SARDINA ESPAÑOLA
    !NM_RECURSO %in% c("SARDINA ESPAÑOLA", "SARDINA ESPANOLA") 
  ) %>%
  mutate(
    year   = as.integer(ANIO),
    month  = as.integer(MES),
    specie = as.character(NM_RECURSO)
  ) %>%
  
  # Cálculo del Precio Promedio Mensual
  group_by(specie, year, month) %>%
  summarize(precio_promedio = mean(PRECIO, na.rm = TRUE), .groups = "drop") 

#---------------------------------------------------------
# union con la base de desembarques mensuales

library(dplyr)
library(tidyr)
library(stringr)

library(tidyverse)
library(stringr)

# Definir un diccionario de meses por si vienen en texto
mapa_meses <- c(
  "enero" = 1, "feb" = 2, "mar" = 3, "abr" = 4, "may" = 5, "jun" = 6,
  "jul" = 7, "ago" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dic" = 12,
  "jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6,
  "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12
)

Q_P_IFOP <- `h_mensual_IFOP2012-2024` %>%
  mutate(
    year = as.integer(year),
    month_clean = str_to_lower(str_trim(as.character(month))),
    month_num = suppressWarnings(as.integer(month_clean)),
    month_final = ifelse(is.na(month_num), 
                         mapa_meses[str_sub(month_clean, 1, 3)], # Usa las primeras 3 letras
                         month_num),
    
    # Aseguramos que la Cantidad (Q) sea numérica
    total_harvest_IFOP_centrosur = as.numeric(total_harvest_IFOP_centrosur)
  ) %>%
  
  # Filtramos si quedó algún mes sin identificar
  filter(!is.na(month_final)) %>%
  
  # Reemplazamos la columna month original
  mutate(month = as.integer(month_final)) %>%
  select(-month_clean, -month_num, -month_final) %>%
  
  # JOIN CON PRECIOS
  left_join(precios_clean, by = c("specie", "year", "month")) %>%
  select(specie, year, month,
         P = precio_promedio,
         Q = total_harvest_IFOP_centrosur) %>%
  mutate(
    specie = tolower(specie) %>% str_replace_all(" ", "_")
  ) %>%
  pivot_wider(
    names_from  = specie,
    values_from = c(P, Q),
    names_sep   = "_"
  ) %>%
  mutate(across(starts_with("Q_"), ~replace_na(., 0))) %>%
  
  arrange(year, month)

saveRDS(Q_P_IFOP,
        file = "/Users/ricardoandejaravalencia/Library/Mobile Documents/com~apple~CloudDocs/tesis_datos/Q_P_IFOP.rds")



library(imputeTS) 
library(tidyverse)

# Aplicar Kalman a los Precios (P)
Q_P_Kal <- Q_P_IFOP %>%
  mutate(
    P_anchoveta_na     = na_kalman(ts(P_anchoveta, frequency=12), model = "StructTS"),
    P_sardina_comun_na = na_kalman(ts(P_sardina_comun, frequency=12), model = "StructTS"),
    P_jurel_na        = na_kalman(ts(P_jurel, frequency=12), model = "StructTS")
  ) %>%
  # Seleccionamos las columnas finales
  select(
    year, month,
    # Precios Imputados
    P_anchoveta = P_anchoveta_na,
    P_sardina   = P_sardina_comun_na,
    P_jurel     = P_jurel_na,
    # Cantidades Originales (con ceros donde no hubo pesca)
    Q_anchoveta,
    Q_sardina   = Q_sardina_comun,
    Q_jurel
  )

saveRDS(Q_P_Kal,
        file = "/Users/ricardoandejaravalencia/Library/Mobile Documents/com~apple~CloudDocs/tesis_datos/Q_P_IFOP_imp.rds")

write_csv(Q_P_Kal, "Q_P_IFOP_imp.csv")




  