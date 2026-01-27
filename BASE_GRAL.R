# union de base principal y precio FOB (convertido)

library(tidyverse)
library(readr)

pesca_IFOP <- read_csv("Q_P_156.csv") %>%
  mutate(year = as.integer(year), month = as.integer(month))

conteo_ceros <- pesca_IFOP %>%
  select(starts_with("Q")) %>%
  summarise_all(~sum(. == 0, na.rm = TRUE))

print(conteo_ceros)

filas_con_ceros <- pesca_IFOP %>%
  filter(if_any(starts_with("Q"), ~ . == 0)) %>%
  select(year, month, Q_anchoveta, Q_sardina, Q_jurel)


print(filas_con_ceros)

p_internacional <- read_csv("FOB_CLP.csv") %>%
  mutate(year = as.integer(year), month = as.integer(month))

base_IAIDS_1 <- left_join(pesca_IFOP, p_internacional, by = c("year", "month"))

final_IAIDS_1 <- base_IAIDS_1 %>%
  select(
    year, month,
    P_anchoveta, P_sardina, P_jurel,
    Q_anchoveta, Q_sardina, Q_jurel,
    P_harina_FOB_CLP,
    Tipo_Cambio, Precio_USD
  ) %>%
  arrange(year, month) %>%
  mutate( Date = as.Date(paste(year, month, "01", sep = "-")))

print((paste("Total Observaciones:", nrow(final_IAIDS_1))))
print(paste("Rango de Fechas:", min(final_IAIDS_1$year), "-", max(base_IAIDS_1$year)))

# checkeaar si quedaron NAs en FOB
na_fob <- sum(is.na(final_IAIDS_1$P_harina_FOB_CLP))
if(na_fob > 0) {
  warning(paste("¡ALERTA! Tienes", na_fob, "meses sin Precio FOB. Revisa las fechas."))
} else {
  print("ESTADO: PERFECTO. La variable FOB se unió sin vacíos.")
}

saveRDS(final_IAIDS_1, "BASE_156.rds")
write_csv(final_IAIDS_1, "BASE_156.csv")

#-----------------------------------------------------
# DIAGNOSTICO DE DATOS
#-----------------------------------------------------
install.packages("corrplot")
install.packages("stargazer")
install.packages("gridExtra")

library(tidyverse)
library(tseries)
library(corrplot)
library(stargazer)
library(gridExtra)

# variables de interes 

var_interes <- final_IAIDS_1 %>%
  select(P_anchoveta, P_sardina, P_jurel,
         Q_anchoveta, Q_sardina, Q_jurel,
         P_harina_FOB_CLP)

stargazer(as.data.frame(var_interes), type = "text", 
          title = "Estadísticas Descriptivas: Precios y Cantidades (2012-2024)",
          digits = 0, median = TRUE)



# series de tiempo de precios



# Formato Largo
df_plot <- final_IAIDS_1 %>%
  select(Date, P_anchoveta, P_sardina, P_jurel) %>%
  pivot_longer(cols = c(P_anchoveta, P_sardina, P_jurel), 
               names_to = "Especie", 
               values_to = "Precio_CLP") %>%
  mutate(Especie = factor(Especie, 
                          levels = c("P_anchoveta", "P_sardina", "P_jurel"),
                          labels = c("Anchoveta", "Sardina Común", "Jurel")))

# Pantallas separadas
ggplot(df_plot, aes(x = Date, y = Precio_CLP, color = Especie)) +
  geom_line(size = 0.8) +
  # Esto separa los gráficos y deja que cada uno tenga su propia escala Y
  facet_wrap(~ Especie, ncol = 1, scales = "free_y") + 
  labs(title = "Evolución de Precios Ex-Vessel (2012-2024)",
       subtitle = "Precios Nominales en Pesos Chilenos (CLP/ton)",
       y = "Precio ($)", x = "Año") +
  theme_bw() +
  theme(legend.position = "none", 
        strip.background = element_rect(fill = "#EFEFEF"), 
        strip.text = element_text(face = "bold", size = 11)) +
  scale_color_manual(values = c("Anchoveta"="blue", "Sardina Común"="red", "Jurel"="darkgreen"))


