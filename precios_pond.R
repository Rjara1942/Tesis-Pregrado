library(tidyverse)
library(readxl)

df_precio <- read_excel("2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx", 
                      sheet = "PRECIO", guess_max = 10000)
df_proceso <- read_excel("2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx",
                         sheet = "PROCESO", guess_max = 10000)
# unir bases por: año, mes, planta, región, recurso

base_consolidada <- df_proceso %>%
  left_join(df_precio, by =c("ANIO", "MES", "NUI", "RG", "NM_RECURSO"))%>%
  select(
    ANIO,
    REG = RG,
    MES,
    NUI,
    NM_RECURSO,
    MP_TOTAL,
    PRECIO
  )
# PRECIO PONDERADO 
# AGRUPADO POR REGION 

base_p_pond <- base_consolidada %>%
  filter(
    !is.na(PRECIO) & PRECIO > 0,
    !is.na(MP_TOTAL) & MP_TOTAL > 0
  ) %>%
  mutate(MONTO_TRANSACCION = PRECIO * MP_TOTAL) %>%
  group_by(ANIO, REG, MES, NM_RECURSO) %>%
  summarise(
    Precio_ponderado = sum(MONTO_TRANSACCION) / sum(MP_TOTAL),
    Q_total_consolidado = sum(MP_TOTAL),
    N_Plantas_Compradoras = n_distinct(NUI),
    .groups = "drop"
  ) %>%
  arrange(REG, NM_RECURSO, ANIO, MES)

write.csv(base_p_pond, "BASE_PRECIOS_POND_REG.CSV")


library(tidyverse)
library(lubridate)
library(knitr)


df <- base_p_pond

# Creamos una variable de FECHA real para poder graficar en el tiempo
df <- df %>%
  mutate(
    Fecha = make_date(year = ANIO, month = MES, day = 1),
    Region_Label = paste("Región", REG) # Etiqueta para el gráfico
  )


stats_especie <- df %>%
  group_by(NM_RECURSO) %>%
  summarise(
    N_Meses = n(),
    Precio_Promedio = mean(Precio_ponderado, na.rm = TRUE),
    Desv_Estandar = sd(Precio_ponderado, na.rm = TRUE),
    Minimo = min(Precio_ponderado, na.rm = TRUE),
    Maximo = max(Precio_ponderado, na.rm = TRUE),
    CV = (sd(Precio_ponderado)/mean(Precio_ponderado))*100 # Coeficiente de Variación (%)
  )

kable(stats_especie, digits = 0, format.args = list(big.mark = "."))

stats_anual <- df %>%
  group_by(ANIO, NM_RECURSO) %>%
  summarise(
    Precio_promedio = mean(Precio_ponderado, na.rm = TRUE),
    Q_Total = sum(Q_total_consolidado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = NM_RECURSO, values_from = Precio_promedio)
print(head(stats_anual, 10))  #****


g1 <- ggplot(df, aes(x = Fecha, y = Precio_ponderado, color = NM_RECURSO)) +
  geom_line(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") + # Tendencia suave
  facet_wrap(~NM_RECURSO, scales = "free_y", ncol = 1) +
  labs(
    title = "Evolución del Precio (2012-2024)",
    subtitle = "Precios Ponderados (Línea negra punteada indica tendencia suavizada)",
    y = "Precio ($/Ton)",
    x = "Año",
    color = "Especie"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = "."))

print(g1)

# acotado a la zona centro-sur

regiones_centrosur <- c(5, 7, 8, 9, 14, 10)

df_centrosur <- df %>%
  filter(REG %in% regiones_centrosur) %>%
  mutate(
    # Agregamos nombres para que los gráficos se entiendan mejor
    Nombre_Region = case_when(
      REG == 5  ~ "V - Valparaíso",
      REG == 7  ~ "VII - Maule",
      REG == 8  ~ "VIII - Biobío",
      REG == 9  ~ "IX - Araucanía",
      REG == 10 ~ "X - Los Lagos",
      REG == 14 ~ "XIV - Los Ríos",
      TRUE      ~ as.character(REG)
    )
  )
df_centrosur %>%
  count(Nombre_Region) %>%
  print()

# Gráfico de "Presencia de Datos" (Tile Plot)
# Muestra un cuadrado de color si hay dato, y blanco si falta.

ggplot(df_centrosur, aes(x = Fecha, y = Nombre_Region, fill = NM_RECURSO)) +
  geom_tile(height = 0.8, width = 30) + # Cuadraditos
  labs(
    title = "Disponibilidad de Precios por Región y Tiempo",
    subtitle = "Visualizando los vacíos de información (White spaces = Missing)",
    x = "Año",
    y = "Región",
    fill = "Recurso"
  ) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")