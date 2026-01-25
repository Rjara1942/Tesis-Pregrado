library(tidyverse)
library(readr)
library(lubridate)
library(readxl)

# conversión a CLP

FOB <- read_excel("harina_pescado_FOB.xlsx", skip = 3)

FOB <- read_excel("harina_pescado_FOB.xlsx", skip = 3)
clean_FOB <- FOB%>%
     select(Fecha = 1, Precio_USD = 2) %>%
      drop_na() %>%
      mutate(
          Fecha = ymd(Fecha), # formato AAAA-MM-DD
         year = year(Fecha),
         month = month(Fecha),
        Precio_USD = as.numeric(Precio_USD)
       ) %>%
     select(year, month, Precio_USD)

TC_2012_2024 <- read_excel("TC_2012-2024.xlsx", skip = 3, col_names = FALSE)

clean_TC <- TC_2012_2024 %>%
  select(Fecha = 1, Valor_TC = 2) %>%
  drop_na() %>%
  mutate(
    Fecha = ymd(Fecha),
    year = year(Fecha),
    month = month(Fecha),
    Valor_TC = as.numeric(Valor_TC)
  ) %>%
  group_by(year, month) %>%
  summarise(Tipo_Cambio = mean(Valor_TC, na.rm = TRUE), groups = "drop")

# FÓRMULA DE CONVERSIÓN:
# Precio Harina (CLP) = Precio Harina (USD) * Dólar Observado

base_FOB <- left_join(clean_FOB, clean_TC, by = c("year", "month")) %>%
  mutate(
    P_harina_FOB_CLP = Precio_USD * Tipo_Cambio
  ) %>%
  select(year, month, P_harina_FOB_CLP, Tipo_Cambio, Precio_USD)
write_csv(base_FOB, "FOB_CLP.csv")

