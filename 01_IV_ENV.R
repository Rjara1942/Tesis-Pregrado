datos_ambientales <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Investigacion/data_ambiental_por_puerto.RDS")
head(datos_ambientales)

library(dplyr)
library(lubridate)

# Transformación mensual

df_instrumento <- datos_ambientales %>%
  # Crear columnas años y mes
  mutate(
    ANIO = year(date),
    MES = month(date)
  ) %>%
  # sacar promedios mensuales
  # todos los días del mes y todos los puertos de la zona
  
  group_by(ANIO, MES) %>%
  # VARIABLES ESTRATEGICAS // afectan la disponibilidad SPF
  
  summarise(
    #SST costera (0-30km) para sardina y anchoveta que viven 
    # más pegadas a la costa
    SST_Costa = mean(sst_0_30km, na.rm = TRUE),
    
    # SST Oceánica (0-90km) para Jurel que se mueve más lejos
    SST_Oceano = mean(sst_0_90km, na.mr = TRUE),
    
    #Clorofila (0-90km) Alimento en la zona de surgencia
    Clorofila = mean(chl_0_30km, na.mr = TRUE),
    
    # Variable de cierre// afecta la salida de embarcaciones
    
    # viento (0-30k) viento pegado a la costa
    # cierra la bahía e impide que la flota salga
    viento_velocidad = mean(speed_mean_0_30km, na.rm = TRUE),
    
    .groups = "drop"
  )

print(head(df_instrumento))

write.csv(df_instrumento, "variables_instrumentales_v1.csv")

#------------------------------
# PANEL MENSUAL
datos_mensual <- datos_ambientales %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(Puerto, month) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))