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
  "ene" = 1, "feb" = 2, "mar" = 3, "abr" = 4, "may" = 5, "jun" = 6,
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
        file = "/Users/ricardoandejaravalencia/Library/Mobile Documents/com~apple~CloudDocs/tesis_datos/Q_P_IFO_correg.rds")

write_csv(Q_P_IFO_correg, "Q_P_IFOP_NA.csv")

library(imputeTS)
library(tidyverse)
library(readr)

graficar_demanda <- function(data, x_col, y_col, titulo, color_punto) {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(alpha = 0.6, color = color_punto) + 
    geom_smooth(method = "lm", color = "black", se = TRUE, linetype = "dashed") + 
    geom_smooth(method = "loess", color = "red", se = FALSE) + 
    labs(
      title = titulo,
      x = "Precio Real ($/ton)",
      y = "Cantidad Desembarcada (ton)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

p1 <- graficar_demanda(Q_P_IFO_correg, "P_anchoveta", "Q_anchoveta", "Demanda Anchoveta", "blue")
p2 <- graficar_demanda(Q_P_IFO_correg, "P_sardina_comun", "Q_sardina_comun", "Demanda Sardina", "firebrick")
p3 <- graficar_demanda(Q_P_IFO_correg, "P_jurel", "Q_jurel", "Demanda Jurel", "forestgreen")


grid.arrange(p1, p2, p3, ncol = 3)

#---------------------------------------------------------------------
# Imputación de datos faltantes en precios 
#---------------------------------------------------------------------
# Kalman proyecta la caida de los precios en linea recta dejando precios negativos
# Para evitar lo anterior transformar   LnP e imputar los NA sobre el logaritmo
# trasnformar con exponencial

# Así tener siempre tener número positivo
# Log -> Imputar -> exponencial

# definir función

kalman <- function(x) {
  log_x <- log(x)
  log_x_imp <- na_kalman(ts(log_x, frequency = 12), model = "StructTS")
  return(as.numeric(exp(log_x_imp)))
}

base_imp <- Q_P_IFO_correg %>%
  mutate(
    P_anchoveta = kalman(P_anchoveta),
    P_sardina = kalman(P_sardina_comun),
    P_jurel = kalman(P_jurel)
  ) %>%
  rename(Q_sardina = Q_sardina_comun)

# remover outliers y valores extremos

winsorizar_manual <- function(x, p=0.95) {
  limite <- quantile(x, probs = p, na.rm = TRUE)
  return(pmin(x, limite))
}

Q_P_FINAL <- base_imp %>%
  mutate(
    P_anchoveta = winsorizar_manual(P_anchoveta),
    P_sardina = winsorizar_manual(P_sardina),
    P_jurel = winsorizar_manual(P_jurel)
  )

# Verificación 
print(paste("Precios Negativos Restantes:", sum(Q_P_FINAL$P_sardina < 0)))
print(paste("Total Observaciones:", nrow(Q_P_FINAL)))
print(paste("Total NAs restantes:", sum(is.na(Q_P_FINAL[, c("P_anchoveta","P_sardina","P_jurel")]))))



df_final_lista <- Q_P_FINAL %>%
  select(-any_of("P_sardina_comun")) 

# Verificamos que solo quede la columna "P_sardina" limpia
print(colnames(df_final_lista))

saveRDS(df_final_lista, "Q_P_156.rds")      
write_csv(df_final_lista, "Q_P_156.csv")     
      
      
      