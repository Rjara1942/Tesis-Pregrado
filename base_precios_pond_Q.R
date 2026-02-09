library(tidyverse)
library(readxl)
library(lubridate)
# Esta función lee un rango específico del Excel y lo convierte directo a formato largo
# Solo extrae las regiones de la Zona Centro-Sur (5, 7, 8, 9, 14, 10)



procesar_rango_excel <- function(hoja, rango, especie, flota) {
  
  df <- read_excel(" DESEMBARQUES.xlsx", sheet = hoja, range = rango)
  
  # Renombrar columnas clave (Año y Mes siempre están al inicio)
  # Asumimos que la columna 1 es Año y la 2 es Mes, aunque tengan nombres raros
  colnames(df)[1] <- "ANIO"
  colnames(df)[2] <- "MES_TXT"
  
  # Seleccionar solo columnas de la Zona Centro-Sur
  # Buscamos columnas que se llamen "5", "7", "8", "9", "14", "10"
  regiones_interes <- c("5", "7", "8", "9", "14", "10")
  
  # Verificar cuáles existen en este rango específico
  cols_existentes <- intersect(names(df), regiones_interes)
  
  if(length(cols_existentes) == 0) return(NULL) # Si no hay datos de zona centro sur, saltar
  
  # 4. Filtrar y Pivotar (Wide -> Long)
  df_long <- df %>%
    select(ANIO, MES_TXT, all_of(cols_existentes)) %>%
    pivot_longer(
      cols = all_of(cols_existentes),
      names_to = "REG",
      values_to = "TONELADAS"
    ) %>%
    mutate(
      ESPECIE = especie,
      FLOTA = flota,
      REG = as.numeric(REG),
      TONELADAS = as.numeric(TONELADAS)
    ) %>%
    # Eliminar nulos o ceros que ensucian
    filter(!is.na(TONELADAS) & TONELADAS > 0)
  
  return(df_long)
}


#  EXTRAER DATOS
# -----------------------------------------------------------------------------

lista_datos <- list()

# --- INDUSTRIAL ---
lista_datos[[1]] <- procesar_rango_excel("INDUSTRIAL (nacional)", "A2:L293", "JUREL", "IND")
lista_datos[[2]] <- procesar_rango_excel("INDUSTRIAL (nacional)", "N2:W196", "SARDINA COMUN", "IND")
lista_datos[[3]] <- procesar_rango_excel("INDUSTRIAL (nacional)", "Y2:AJ273", "ANCHOVETA", "IND")

# --- LANCHAS ---
lista_datos[[4]] <- procesar_rango_excel("LANCHAS (CentroSur)", "A2:I143", "JUREL", "ART")
lista_datos[[5]] <- procesar_rango_excel("LANCHAS (CentroSur)", "K2:S170", "SARDINA COMUN", "ART")
lista_datos[[6]] <- procesar_rango_excel("LANCHAS (CentroSur)", "U2:AC169", "ANCHOVETA", "ART")

# --- BOTES ---
lista_datos[[7]] <- procesar_rango_excel("BOTES (CentroSur)", "A2:I146", "JUREL", "ART")
lista_datos[[8]] <- procesar_rango_excel("BOTES (CentroSur)", "K2:S163", "SARDINA COMUN", "ART")
lista_datos[[9]] <- procesar_rango_excel("BOTES (CentroSur)", "U2:AD109", "ANCHOVETA", "ART")

# Unir todo en un solo dataframe bruto
df_desembarques_raw <- bind_rows(lista_datos)

# -----------------------------------------------------------------------------
# PASO 3: LIMPIEZA Y AGREGACIÓN (OFERTA TOTAL)
# -----------------------------------------------------------------------------

df_oferta_total <- df_desembarques_raw %>%
  # 1. Limpiar Meses (Texto -> Número)
  mutate(
    MES_TXT = str_trim(tolower(MES_TXT)),
    MES = case_when(
      MES_TXT %in% c("ene", "enero") ~ 1,
      MES_TXT %in% c("feb", "febrero") ~ 2,
      MES_TXT %in% c("mar", "marzo") ~ 3,
      MES_TXT %in% c("abr", "abril") ~ 4,
      MES_TXT %in% c("may", "mayo") ~ 5,
      MES_TXT %in% c("jun", "junio") ~ 6,
      MES_TXT %in% c("jul", "julio") ~ 7,
      MES_TXT %in% c("ago", "agosto") ~ 8,
      MES_TXT %in% c("sep", "sept", "septiembre") ~ 9,
      MES_TXT %in% c("oct", "octubre") ~ 10,
      MES_TXT %in% c("nov", "noviembre") ~ 11,
      MES_TXT %in% c("dic", "diciembre") ~ 12
    )
  ) %>%
  filter(!is.na(ANIO) & !is.na(MES)) %>%
  
  group_by(ANIO, MES, REG, ESPECIE) %>%
  summarise(
    Q_MERCADO_TOTAL = sum(TONELADAS, na.rm = TRUE),
    .groups = "drop"
  )



# -----------------------------------------------------------------------------
#  CARGAR PRECIOS (BASE ACOTADA CENTRO-SUR) Y UNIR
# -----------------------------------------------------------------------------

# 1. Cargar la base de precios que ya filtraste por región
# Asegúrate de que el archivo 'p_pond_centrosur.csv' esté en tu carpeta de trabajo
df_precios_cs <- read_csv("p_pond_centrosur.csv")

# 2. Estandarización de Nombres para el Cruce
# Es vital que "SARDINA COMÚN" se escriba igual en ambas bases
df_precios_cs <- df_precios_cs %>%
  mutate(
    NM_RECURSO = str_trim(toupper(NM_RECURSO)),
    NM_RECURSO = case_when(
      NM_RECURSO == "SARDINA COMÚN" ~ "SARDINA COMUN", # Quitar tilde si existe
      TRUE ~ NM_RECURSO
    )
  )

df_oferta_total <- df_oferta_total %>%
  mutate(
    ESPECIE = str_trim(toupper(ESPECIE)),
    ESPECIE = case_when(
      ESPECIE == "SARDINA COMÚN" ~ "SARDINA COMUN",
      TRUE ~ ESPECIE
    )
  )

# 3. EL CRUCE FINAL (LEFT JOIN)
# Usamos los PRECIOS como base maestra. Queremos saber cuál fue la oferta total
# del mercado (Q_MERCADO) en el momento en que se pagó ese precio.

df_final <- df_precios_cs %>%
  left_join(df_oferta_total, 
            by = c("ANIO", "MES", "REG", "NM_RECURSO" = "ESPECIE")) %>%
  
  # 4. Tratamiento de NAs en la Oferta
  # Si hay precio pero no hay dato de Sernapesca para ese mes/región, asumimos 0 
  # (o que el desembarque fue despreciable frente a la compra)
  mutate(
    Q_MERCADO_TOTAL = replace_na(Q_MERCADO_TOTAL, 0)
  ) %>%
  
  # 5. Selección y Orden final de variables para la Tesis
  select(
    ANIO, 
    MES, 
    REG, 
    NM_RECURSO,
    PRECIO_PONDERADO = Precio_ponderado,    # Variable Endógena (P)
    Q_MUESTRA_PLANTAS = Q_total_consolidado,# Cantidad de tu muestra (Endógena)
    Q_MERCADO_TOTAL,                        # Cantidad Oficial Sernapesca (Exógena)
    N_PLANTAS = N_Plantas_Compradoras       # Variable de Control
  ) %>%
  arrange(REG, NM_RECURSO, ANIO, MES)

# -----------------------------------------------------------------------------
# PASO 5: GUARDAR Y VALIDAR
# -----------------------------------------------------------------------------

write_csv(df_final, "BASE_CENTROSUR_POND.csv")


print(head(df_final))


summary(df_final$Q_MERCADO_TOTAL)



library(tidyverse)
library(lubridate)
library(scales) # Para formato de pesos ($)


# Asegúrate de que el archivo esté en tu carpeta de trabajo
df <- read_csv("BASE_CENTROSUR_POND.csv")

# Crear variables de fecha y factor regional
df_grafico <- df %>%
  mutate(
    Fecha = make_date(year = ANIO, month = MES, day = 1),
    Region_Label = paste("Región", REG), # Etiqueta legible
    Especie = str_to_title(NM_RECURSO)   # "Jurel" en vez de "JUREL"
  )

# -----------------------------------------------------------------------------
# GRÁFICO 1: EVOLUCIÓN DE PRECIOS POR REGIÓN (Heterogeneidad)
# -----------------------------------------------------------------------------
# Este gráfico es clave para justificar por qué usas un panel regional.
# Muestra si los precios se mueven juntos o si hay regiones "premium".

g1 <- ggplot(df_grafico, aes(x = Fecha, y = PRECIO_PONDERADO, color = Region_Label)) +
  # Líneas y puntos
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.8) +
  
  # Paneles separados por especie (escala libre porque el Jurel es más caro)
  facet_wrap(~Especie, scales = "free_y", ncol = 1) +
  
  # Estética profesional
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Evolución Regional del Precio Real (Macrozona Centro-Sur)",
    subtitle = "Comparativa de precios ponderados ex-vessel por región (2012-2024)",
    x = "Año",
    y = "Precio ($/Ton)",
    color = "Región",
    caption = "Fuente: Elaboración propia en base a datos de plantas y Sernapesca."
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(g1)
# ggsave("Grafico_Precios_Regionales.png", g1, width = 10, height = 10)


# -----------------------------------------------------------------------------
# GRÁFICO 2: TENDENCIA MACROZONAL (Promedio General)
# -----------------------------------------------------------------------------
# Aquí calculamos un precio único para toda la zona centro-sur (promedio ponderado total)
# Ideal para hablar de "El precio del Jurel en Chile" en general.

df_macro <- df_grafico %>%
  group_by(Fecha, Especie) %>%
  summarise(
    # Re-ponderamos usando el Q de mercado total
    Precio_Macro = weighted.mean(PRECIO_PONDERADO, w = Q_MERCADO_TOTAL, na.rm = TRUE),
    .groups = "drop"
  )

g2 <- ggplot(df_macro, aes(x = Fecha, y = Precio_Macro, color = Especie)) +
  geom_line(linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "black", alpha = 0.5) + # Tendencia
  
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ".")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  
  labs(
    title = "Evolución General de Precios Ex-Vessel (Índice Macrozonal)",
    subtitle = "Tendencia agregada ponderada por volumen de desembarque",
    y = "Precio Promedio ($/Ton)",
    x = "Año",
    color = "Especie"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g2)
# ggsave("Grafico_Tendencia_General.png", g2, width = 10, height = 6)




