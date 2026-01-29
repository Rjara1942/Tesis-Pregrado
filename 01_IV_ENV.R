datos_ambientales <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Investigacion/data_ambiental_por_puerto.RDS")
head(datos_ambientales)

library(dplyr)
library(lubridate)
library(tidyverse)

mapa_regiones <- tibble(
  Puerto = c(
    "San Antonio",
    "Talcahuano (San Vicente)",
    "Coronel",
    "Calbuco",
    "Lota",
    "Corral",
    "Puerto Montt",
    "Region 7 (puerto)", 
    "Region 9 (puerto)"
  ),
  Region_Num = c(5, 8, 8, 10, 8, 14, 10, 7, 9)
)

regional <- datos_ambientales %>%
  left_join(mapa_regiones, by = "Puerto") %>%
  filter(!is.na(Region_Num)) %>%
  mutate(
    date = ymd(date),
    year = year(date),
    month = month(date)
  ) %>%
  group_by(Region_Num, year, month) %>%
  summarise(
    SST_REGIONAL = mean(sst_0_60km, na.rm = TRUE),
    Chl_Regional = mean(chl_0_60km, na.rm = TRUE),
    Wind_Regional = mean(speed_mean_0_60km, na.rm = TRUE),
    
   
    Wind_max = mean(speed_max_0_60km, na.rm = TRUE),
   
    .groups = "drop"
  )

col_reg <- regional%>%
  pivot_wider(
    names_from = Region_Num,
    values_from = c(SST_REGIONAL, Chl_Regional, Wind_Regional, Wind_max),
    names_prefix = "Reg"
  )

write_csv(col_reg, "env_var_por_region.csv")
