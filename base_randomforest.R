

harvest_price_IFOP_wide <- read_csv("harvest_price_IFOP_wide.csv")




#---------------------------------------------
# TRATAMIENTO DE NA /// precio faltante

if(!require(missForest)) install.packages("missForest")
if(!require(zoo)) install.packages("zoo")
if(!require(dplyr)) install.packages("dplyr")
if(!require(readr)) install.packages("readr")

library(missForest)                                  
library(zoo)
library(dplyr)
library(readr)

df <- harvest_price_IFOP_wide
df <- as.data.frame(df)

# tratamiento para el Jurel
# uso de interpolación lineal porque Jurel 
# no esta correlacionado con las otras especies
# rellenar extremos con último dato conocido(??

df$P_jurel_imp <- na.approx(df$P_jurel, na.rm = FALSE, rule = 2) 

# anchoveta y sardina
# consideración variables que aportan información (PRECIO + TIEMPO)

vars_interes <- df %>% select(year, month, P_anchoveta, P_sardina_comun)

set.seed(2024)
imputacion_rf <- missForest(vars_interes, ntree = 100, verbose = TRUE)

# extraer datos rellenos
datos_rellenos <- imputacion_rf$ximp

# juntar ambas imputaciones (jurel y sardina/anchoveta)


df_final <- df %>%
  mutate(
    P_anchoveta = datos_rellenos$P_anchoveta,
    P_sardina_comun = datos_rellenos$P_sardina_comun,
    P_jurel = P_jurel_imp
  ) %>%
  select(-P_jurel_imp) # limpar columnas que ya no sirven

# revisar si quedan NA 
print(colSums(is.na(df_final)))

names(df_final)
unique(df_final$month)
df_final <- df_final %>%
  mutate(
    date = as.Date(sprintf("%d-%02d-01", year, month))
  )





library(ggplot2)
library(dplyr)

df_final %>%
  ggplot(aes(x = date, y = P_anchoveta)) +
  geom_line(color = "steelblue") +
  labs(title = "Precio Anchoveta (mensual)", x = "Fecha", y = "Precio")

saveRDS(df_final,
        file = "/Users/ricardoandejaravalencia/Library/Mobile Documents/com~apple~CloudDocs/tesis_datos/df_final.rds")


