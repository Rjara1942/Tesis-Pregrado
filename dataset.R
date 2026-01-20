install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("stringr")



library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# ==============================================================================
# CARGA DE BASES DE DATOS 
# ==============================================================================

# 1. Librerías necesarias
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
library(readxl)
library(dplyr)

# 2. Establecer Directorio (Tu carpeta de iCloud)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/tesis_datos")

# 3. Función de Carga Simple
# Solo lee el archivo, salta el título (fila 1) y le pone etiqueta de flota/especie
cargar_base_cruda <- function(archivo, nombre_flota, nombre_especie) {
  
  if(file.exists(archivo)) {
    # skip = 1 para saltar la fila que dice "ANCHOVETA,,,,,"
    df <- suppressMessages(read_excel(archivo, skip = 1))
    
    # Agregamos identificadores para no perder el rastro
    df$Origen_Flota <- nombre_flota
    df$Origen_Especie <- nombre_especie
    
    return(df)
  } else {
    warning(paste("ALERTA: No se encontró el archivo:", archivo))
    return(NULL)
  }
}

# 4. Ejecución de la Carga
print("--- INICIANDO CARGA DE ARCHIVOS ---")

lista_raw <- list(
  cargar_base_cruda("botes_anchoveta.xlsx", "Botes", "Anchoveta"),
  cargar_base_cruda("botes_sardina.xlsx",   "Botes", "Sardina"),
  cargar_base_cruda("botes_jurel.xlsx",     "Botes", "Jurel"),
  
  cargar_base_cruda("lanchas_anchoveta.xlsx", "Lanchas", "Anchoveta"),
  cargar_base_cruda("lanchas_sardina.xlsx",   "Lanchas", "Sardina"),
  cargar_base_cruda("lanchas_jurel.xlsx",     "Lanchas", "Jurel"),
  
  cargar_base_cruda("industrial_anchoveta.xlsx", "Industrial", "Anchoveta"),
  cargar_base_cruda("industrial_sardina.xlsx",   "Industrial", "Sardina"),
  cargar_base_cruda("industrial_jurel.xlsx",     "Industrial", "Jurel")
)

# 5. Unificación en una "Gran Base Cruda"
# bind_rows une todo inteligentemente, rellenando con NA si faltan columnas en alguno
df_total_raw <- bind_rows(lista_raw)

# 6. Verificación
print(paste("Carga completada. Total de filas cargadas:", nrow(df_total_raw)))
print("Primeras filas de la base consolidada:")
print(head(df_total_raw[, c(1, 2, ncol(df_total_raw)-1, ncol(df_total_raw))])) # Muestra Año, Mes, Flota y Especie

# También cargamos Precios por separado
if(file.exists("precio_SPF.xlsx")){
  df_precios_raw <- read_excel("precio_SPF.xlsx")
  print("Archivo de Precios cargado correctamente.")
} else {
  warning("No se encontró precio_SPF.xlsx")
}
print(names(df_total_raw))

str(df_total_raw$`8`)

#  Ver los nombres exactos de las columnas
print("NOMBRES DE COLUMNAS:")
print(names(df_total_raw))

#  Ver si la columna "8" (Biobío) existe y qué tipo de dato es
if("8" %in% names(df_total_raw)) {
  print("Columna '8' encontrada. Tipo de dato:")
  str(df_total_raw[["8"]])
} else {
  print("ALERTA: No se encuentra la columna llamada '8'")
}

# 1. VERIFICACIÓN ESPECÍFICA (SOLO REGIONES 7 Y 9)
library(dplyr)

check_7_9 <- df_total_raw %>%
  # Seleccionamos solo las columnas clave
  select(Origen_Flota, `7`, `9`) %>%
  # Convertimos a número (por si R las leyó como texto)
  mutate(
    Reg_7 = suppressWarnings(as.numeric(`7`)),
    Reg_9 = suppressWarnings(as.numeric(`9`))
  ) %>%
  # Agrupamos por flota para ver el detalle
  group_by(Origen_Flota) %>%
  summarise(
    Total_Maule_7 = sum(Reg_7, na.rm = TRUE),
    Total_Araucania_9 = sum(Reg_9, na.rm = TRUE),
    .groups = "drop"
  )

print(check_7_9)


# ==============================================================================
# PASO 2: LIMPIEZA Y FILTRADO (CÓDIGO SEGURO)
# ==============================================================================

# CARGAR LIBRERÍAS 
library(dplyr)
library(tidyr)
library(stringr)



# Definir Regiones 
cols_region <- c("5", "6", "7", "8", "9", "14", "10", "16")

#  Procesar
df_limpio <- df_total_raw %>%
  #Renombrar columnas 
  rename(
    ANIO = `Años (Fc_Llegada)`,
    MES  = `Meses (Fc_Llegada)`
  ) %>%
  
  #Filtros básicos
  filter(!is.na(ANIO)) %>%
  
  #  SUMA DE CAPTURAS (La parte clave)
  mutate(
    # Convertimos a número solo por seguridad (aunque ya vimos que son numéricas)
    across(any_of(cols_region), ~as.numeric(.)),
    # Sumamos horizontalmente
    Q_CentroSur = rowSums(select(., any_of(cols_region)), na.rm = TRUE)
  ) %>%
  
  #  Limpieza de Fechas
  mutate(
    ANIO = as.numeric(ANIO),
    MES_STR = tolower(trimws(MES)),
    MES_NUM = case_when(
      grepl("ene|jan|1", MES_STR) & !grepl("10|11|12", MES_STR) ~ 1,
      grepl("feb|2", MES_STR) ~ 2,
      grepl("mar|3", MES_STR) ~ 3,
      grepl("abr|apr|4", MES_STR) ~ 4,
      grepl("may|5", MES_STR) ~ 5,
      grepl("jun|6", MES_STR) ~ 6,
      grepl("jul|7", MES_STR) ~ 7,
      grepl("ago|aug|8", MES_STR) ~ 8,
      grepl("sep|9", MES_STR) ~ 9,
      grepl("oct|10", MES_STR) ~ 10,
      grepl("nov|11", MES_STR) ~ 11,
      grepl("dic|dec|12", MES_STR) ~ 12,
      TRUE ~ NA_real_
    )
  ) %>%
  select(ANIO, MES_NUM, Origen_Flota, Origen_Especie, Q = Q_CentroSur) %>%
  filter(!is.na(MES_NUM)) %>%
  filter(Q > 0)

print(paste("Base Limpia lista. Filas:", nrow(df_limpio)))


# ==============================================================================
#  CRUCE CON PRECIOS
# ==============================================================================


regiones_precio_num <- c(5, 6, 7, 8, 9, 14, 10, 16)

# 2. Cargar y Filtrar
if(exists("df_precios_raw")) {
  
  df_precios_clean <- df_precios_raw %>%
    # A. Filtro de Harina
    filter(CLASE_INDUSTRIA_II == "ANIMAL") %>%
    
    # B. FILTRO REGIONAL (La mejora clave)
    # Solo nos quedamos con los precios registrados en la macrozona
    filter(RG %in% regiones_precio_num) %>%
    
    mutate(
      ANIO = as.numeric(ANIO),
      MES_NUM = as.numeric(MES),
      # Mapeo de Especies
      Especie_Join = case_when(
        grepl("SARDINA", toupper(NM_RECURSO)) ~ "Sardina",
        grepl("ANCHOVETA", toupper(NM_RECURSO)) ~ "Anchoveta",
        grepl("JUREL", toupper(NM_RECURSO)) ~ "Jurel",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Especie_Join)) %>%
    
    # C. Promedio Mensual de la ZONA
    # Ahora 'P' es el precio promedio que pagaron las plantas del SUR
    group_by(ANIO, MES_NUM, Especie_Join) %>%
    summarise(P = mean(as.numeric(PRECIO), na.rm = TRUE), .groups = "drop")
  
  # 3. Unir con Capturas (Q ya filtrada en Paso 2)
  df_final <- inner_join(df_limpio, df_precios_clean, 
                         by = c("ANIO", "MES_NUM", "Origen_Especie" = "Especie_Join"))
  
  print(paste("Base Final (Doble Filtro) Lista. Obs:", nrow(df_final)))
  
} else {
  stop("ERROR: Carga df_precios_raw primero (Paso 1).")
}

# ==============================================================================
# PASO 4: VER RESULTADOS MEJORADOS
# ==============================================================================

resultados <- df_final %>%
  group_by(Origen_Flota, Origen_Especie) %>%
  filter(n() > 10) %>%
  summarise(
    Obs = n(),
    Flexibilidad = round(coef(lm(log(P) ~ log(Q + 1)))[2], 4),
    P_Valor = round(summary(lm(log(P) ~ log(Q + 1)))$coefficients[2, 4], 4),
    R2 = round(summary(lm(log(P) ~ log(Q + 1)))$r.squared, 3),
    .groups = "drop"
  )

print(as.data.frame(resultados))


# ==============================================================================
#  AGREGACIÓN FINAL (SUMA DE FLOTAS)
# ==============================================================================



# Agrupar por Año, Mes y Especie (Eliminando la distinción de flota)
df_agregado <- df_final %>%
  group_by(ANIO, MES_NUM, Origen_Especie) %>%
  summarise(
    # Sumamos toda la captura disponible en el mercado ese mes
    Q_Total = sum(Q, na.rm = TRUE),
    # El precio es el mismo (promedio del mes)
    P_Promedio = mean(P, na.rm = TRUE),
    .groups = "drop"
  )

# Regresión Agregada
resultados_agg <- df_agregado %>%
  group_by(Origen_Especie) %>%
  summarise(
    Obs = n(),
    Flexibilidad = round(coef(lm(log(P_Promedio) ~ log(Q_Total + 1)))[2], 4),
    P_Valor = round(summary(lm(log(P_Promedio) ~ log(Q_Total + 1)))$coefficients[2, 4], 4),
    R2 = round(summary(lm(log(P_Promedio) ~ log(Q_Total + 1)))$r.squared, 3)
  )

print(as.data.frame(resultados_agg))

# Gráfico Final 
library(ggplot2)
ggplot(df_agregado, aes(x = log(Q_Total), y = log(P_Promedio), color = Origen_Especie)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Demanda Inversa Agregada (Zona Centro-Sur)",
       subtitle = "Relación Precio-Cantidad Total",
       x = "Log(Captura Total)", y = "Log(Precio Ex-Vessel)") +
  theme_minimal()

install.packages("rmarkdown")
install.packages("knitr")
install.packages("kableExtra") # Para tablas bonitas
 
write.csv(df_final, "Dataset_centrosur_SPF.csv",row.names = FALSE)
          
print(head(df_final))



