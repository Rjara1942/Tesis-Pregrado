library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

# El archivo IPC viene con variaciones mensuales, necesitamos construir el índice.

# Leemos saltando las primeras 3 filas de encabezado basura
ipc_raw <- read_excel("IPC_2012_2024.xlsx", skip = 3, col_names = FALSE) %>%
  select(1, 2) %>% # Solo nos interesan Fecha y Variación
  rename(Fecha_Txt = 1, Var_Mensual = 2) %>%
  filter(!is.na(Fecha_Txt))

# Procesamos para crear el Deflactor
ipc_clean <- ipc_raw %>%
  mutate(
    Fecha = as.Date(Fecha_Txt),
    ANIO = year(Fecha),
    MES = month(Fecha),
    
    # Convertir variación a numérico (ojo si viene como texto)
    Var_Mensual = as.numeric(Var_Mensual),
    
    # CONSTRUCCIÓN DEL ÍNDICE ACUMULADO
    # Asumimos base 100 al inicio y acumulamos las variaciones
    Factor = 1 + (Var_Mensual / 100),
    IPC_Indice_Raw = 100 * cumprod(replace_na(Factor, 1))
  )

# RE-BASE A 2024 (Para tener "Pesos de 2024")
# Calculamos el promedio del índice en 2024
base_2024 <- mean(ipc_clean$IPC_Indice_Raw[ipc_clean$ANIO == 2024], na.rm = TRUE)

# Creamos el Deflactor (Índice / Base 2024)
ipc_final <- ipc_clean %>%
  mutate(Deflactor_2024 = IPC_Indice_Raw / base_2024) %>%
  select(ANIO, MES, Deflactor_2024)

# -----------------------------------------------------------------------------
# 2. CARGAR PRECIOS Y DEFLACTAR
# -----------------------------------------------------------------------------
df_precios <- read_csv("BASE_CENTROSUR_POND.csv")

df_real <- df_precios %>%
  # Unimos con el IPC por Año y Mes
  left_join(ipc_final, by = c("ANIO", "MES")) %>%
  
  mutate(
    # CÁLCULO DEL PRECIO REAL
    PRECIO_REAL = PRECIO_PONDERADO / Deflactor_2024,
    
    # Variables para gráfico
    Fecha = make_date(ANIO, MES, 1),
    Especie = str_to_title(NM_RECURSO),
    Region_Label = paste("Región", REG)
  )

# Guardamos la base lista para econometría (Precios Reales)
write_csv(df_real, "BASE_PRECIOS_POND_REALES_2024.csv")

# -----------------------------------------------------------------------------
# 3. GRÁFICOS DE PRECIOS REALES (SIN INFLACIÓN)
# -----------------------------------------------------------------------------

# GRÁFICO A: Evolución General por Especie (Precio Real)
# Calculamos el promedio ponderado mensual de toda la zona
df_tendencia <- df_real %>%
  group_by(Fecha, Especie) %>%
  summarise(
    Precio_Real_Promedio = weighted.mean(PRECIO_REAL, w = Q_MERCADO_TOTAL, na.rm = TRUE),
    .groups = "drop"
  )

g_real <- ggplot(df_tendencia, aes(x = Fecha, y = Precio_Real_Promedio, color = Especie)) +
  geom_line(linewidth = 1) +
  # Agregamos una línea de tendencia suave para ver ciclos
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "black", alpha = 0.3) +
  
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ".")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  
  labs(
    title = "Evolución del Precio REAL Ex-Vessel (Pesos de 2024)",
    subtitle = "Precios deflactados por IPC. La tendencia muestra el valor real del recurso.",
    y = "Precio Real ($/Ton)",
    x = "Año",
    color = "Especie",
    caption = "Nota: Deflactado base anual 2024. Línea punteada indica tendencia suavizada."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g_real)


head(df_real %>% select(ANIO, MES, Especie, PRECIO_PONDERADO, Deflactor_2024, PRECIO_REAL), 5)

library(tidyverse)
library(lubridate)
library(scales)

#  CARGAR DATOS
df <- read_csv("BASE_PRECIOS_POND_REALES_2024.csv") %>%
  mutate(Fecha = make_date(ANIO, MES, 1))

# ANÁLISIS DE OUTLIERS 
# Filtramos solo Jurel para el análisis
df_jurel <- df %>% filter(NM_RECURSO == "JUREL")

# Calculamos rangos estadísticos
Q1 <- quantile(df_jurel$PRECIO_REAL, 0.25)
Q3 <- quantile(df_jurel$PRECIO_REAL, 0.75)
IQR <- Q3 - Q1
limite_superior <- Q3 + 1.5 * IQR
limite_inferior <- Q1 - 1.5 * IQR

# Identificamos los meses problemáticos
outliers <- df_jurel %>%
  filter(PRECIO_REAL > limite_superior | PRECIO_REAL < limite_inferior) %>%
  select(ANIO, MES, PRECIO_REAL)


print(outliers)




# TRATAMIENTO: CREACIÓN DE DUMMY DE SHOCK
# Definimos el periodo del shock basado en los datos (aprox 2015-2017)

df_tratado <- df %>%
  mutate(
    # 1. Variable Dummy (1 si es Jurel en periodo 2015-2017, 0 si no)
    D_SHOCK_JUREL = if_else(
      NM_RECURSO == "JUREL" & (ANIO >= 2015 & ANIO <= 2017), 
      1, 
      0
    ),
    
    # 2. Variable Winsorizada (Opcional: Recorta los precios extremos)
    PRECIO_REAL_WINSOR = if_else(
      NM_RECURSO == "JUREL" & PRECIO_REAL > limite_superior,
      limite_superior,
      PRECIO_REAL
    )
  )

print(head(df_tratado %>% select(ANIO, NM_RECURSO, PRECIO_REAL, D_SHOCK_JUREL)))



#  GRÁFICO DE DIAGNÓSTICO (ANTES VS DESPUÉS)
# Visualiza dónde están los outliers
ggplot(df_tratado %>% filter(NM_RECURSO == "JUREL"), aes(x = Fecha, y = PRECIO_REAL)) +
  geom_line(color = "grey50") +
  geom_point(aes(color = as.factor(D_SHOCK_JUREL)), size = 2) +
  geom_hline(yintercept = limite_superior, linetype = "dashed", color = "red") +
  
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     labels = c("Normal", "Shock (Dummy=1)")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Tratamiento de Outliers: Serie de Precio Real del Jurel",
    subtitle = "Los puntos rojos indican el periodo donde se activará la variable Dummy",
    y = "Precio Real (2024)",
    color = "Clasificación"
  ) +
  theme_minimal()

# 5. GUARDAR BASE FINAL PARA ECONOMETRÍA
write_csv(df_tratado, "BASE_CON_DUMMIES.csv")

library(tidyverse)
library(scales)

# -----------------------------------------------------------------------------
# GRÁFICO DE EVOLUCIÓN DE PRECIOS POR ESPECIE (CON SHOCK DESTACADO)
# -----------------------------------------------------------------------------

# 1. Preparar datos para el gráfico
# Creamos una etiqueta para la leyenda basada en la Dummy
df_plot <- df_tratado %>%
  mutate(
    # Convertir Año/Mes a fecha para el eje X
    Fecha = make_date(ANIO, MES, 1),
    
    # Etiqueta amigable para el gráfico
    Especie_Label = str_to_title(NM_RECURSO),
    
    # Clasificación para el color (Normal vs Shock)
    # Solo marcamos Shock si es Jurel Y la dummy es 1
    Estado = if_else(D_SHOCK_JUREL == 1, "Shock (Dummy)", "Tendencia Normal")
  ) %>%
  # Filtramos solo las especies principales 
  filter(NM_RECURSO %in% c("JUREL", "ANCHOVETA", "SARDINA COMUN"))

# 2
g_final <- ggplot(df_plot, aes(x = Fecha, y = PRECIO_REAL)) +
  
  # Línea de fondo gris para dar continuidad
  geom_line(color = "grey60", alpha = 0.6) +
  
  # Puntos coloreados según si es Shock o Normal
  geom_point(aes(color = Estado), size = 1.5, alpha = 0.8) +
  
  #  Paneles separados por Especie 
  facet_wrap(~Especie_Label, ncol = 1, scales = "free_y") +
  

  scale_color_manual(values = c("Tendencia Normal" = "#1f78b4", 
                                "Shock (Dummy)"    = "#e31a1c")) +
  
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  labs(
    title = "Evolución del Precio Real Ex-Vessel por Especie",
    subtitle = "Destacando el periodo de cambio estructural en el Jurel (2015-2017)",
    y = "Precio Real (Pesos de 2024 / Tonelada)",
    x = "Año",
    color = "Clasificación del Periodo",
    caption = "Fuente: Elaboración propia en base a datos de Sernapesca e IFOP. \nNota: El periodo en rojo recibe tratamiento de variable dummy en el modelo econométrico."
  ) +
  
  
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"), # Títulos de los paneles más grandes
    plot.title = element_text(face = "bold", size = 14)
  )

print(g_final)

# Guardar en alta calidad 
ggsave("Grafico_Evolucion_Precios_Final.png", g_final, width = 10, height = 8, dpi = 300)

df_anch <- df_tratado %>%
  mutate(Fecha = make_date(ANIO, MES, 1))
 # correlación 
df_analisis <- df_anch %>%
  filter(NM_RECURSO %in% c("ANCHOVETA", "JUREL")) %>%
  select(Fecha, NM_RECURSO, PRECIO_REAL, Q_MERCADO_TOTAL, D_SHOCK_JUREL)

g_diagnostico <- ggplot(df_analisis, aes(x = Fecha)) +
  # Área gris de fondo para Q (Cantidad) invertida
  # (Truco visual: Si la barra gris baja, significa que hay MUCHO pescado. Si sube, hay poco)
  geom_col(aes(y = rescale(Q_MERCADO_TOTAL, to = c(0, max(PRECIO_REAL)*0.8))), 
           fill = "grey80", alpha = 0.5) +
  
  # Línea de Precio
  geom_line(aes(y = PRECIO_REAL, color = NM_RECURSO), size = 1) +
  
  # Destacar el Shock del Jurel
  geom_rect(data = subset(df_analisis, D_SHOCK_JUREL == 1 & NM_RECURSO == "JUREL"),
            aes(xmin = min(Fecha), xmax = max(Fecha), ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.01, inherit.aes = FALSE) +
  
  facet_wrap(~NM_RECURSO, scales = "free_y", ncol = 1) +
  
  scale_y_continuous(labels = dollar_format(), sec.axis = sec_axis(~., name = "Referencia de Volumen (Barras Grises)")) +
  scale_color_manual(values = c("ANCHOVETA" = "#2ca25f", "JUREL" = "#1f78b4")) +
  
  labs(
    title = "Diagnóstico de Mecanismo de Precios: Jurel vs Anchoveta",
    subtitle = "Anchoveta: El precio reacciona perfecto a la escasez (Barras bajas -> Precio alto).\nJurel: En 2016 (Zona Roja), el precio se desacopla de la cantidad (Cambio Estructural).",
    y = "Precio Real ($)",
    x = "Año",
    caption = "Nota: Las barras grises representan el volumen desembarcado (Q)."
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(g_diagnostico)
ggsave("Grafico_Diagnostico_Tratamiento.png", width = 10, height = 8)
