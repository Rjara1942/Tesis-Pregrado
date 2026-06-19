# 1. Instalar paquetes si no los tienes
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")

library(dplyr)
library(ggplot2)

# 2. Cargar los datos
# Asegúrate de que el nombre del archivo coincida y de estar en el directorio correcto
ruta_archivo <- here::here("data", "2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx - PRECIO.csv")
datos_precio <- read.csv(ruta_archivo, stringsAsFactors = FALSE)

# 3. Filtrar y preparar los datos
# Quitamos filas donde PRECIO sea NA y seleccionamos lo solicitado
datos_filtrados <- datos_precio %>%
  filter(!is.na(PRECIO)) %>%
  filter(NM_RECURSO %in% c("SARDINA COMUN", "ANCHOVETA"),
         CLASE_INDUSTRIA_II %in% c("ANIMAL", "MIXTA_AH"))

# 4. Calcular métricas de robustez y dispersión
metricas_robustez <- datos_filtrados %>%
  group_by(NM_RECURSO, CLASE_INDUSTRIA_II) %>%
  summarise(
    N_observaciones = n(),
    Media = mean(PRECIO),
    Mediana = median(PRECIO),
    # Desviación Estándar (Sensible a valores extremos)
    Desviacion_Estandar = sd(PRECIO),
    # Coeficiente de Variación (Volatilidad relativa %)
    CV_porcentaje = (sd(PRECIO) / mean(PRECIO)) * 100,
    # Rango Intercuartílico (Robusto: 50% de los datos centrales)
    IQR = IQR(PRECIO),
    # Desviación Absoluta de la Mediana (La medida más robusta frente a atípicos)
    MAD = mad(PRECIO),
    .groups = 'drop'
  )

print("=== MÉTRICAS DE ROBUSTEZ ===")
print(metricas_robustez)

# 5. Visualizaciones para apoyar el análisis de robustez

# Gráfico 1: Boxplot (Diagrama de caja)
# Excelente para ver la concentración de precios (IQR) y detectar valores atípicos (puntos sueltos)
grafico_boxplot <- ggplot(datos_filtrados, aes(x = CLASE_INDUSTRIA_II, y = PRECIO, fill = CLASE_INDUSTRIA_II)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  facet_wrap(~ NM_RECURSO) +
  theme_minimal() +
  labs(
    title = "Comparación de Robustez de Precios (Boxplot)",
    subtitle = "Sardina Común y Anchoveta (Industria Animal vs Mixta)",
    x = "Tipo de Industria",
    y = "Precio",
    fill = "Industria"
  )

print(grafico_boxplot)

# Gráfico 2: Curva de Densidad
# Ayuda a ver si los precios están muy esparcidos (baja robustez) o concentrados en un pico (alta robustez)
grafico_densidad <- ggplot(datos_filtrados, aes(x = PRECIO, fill = CLASE_INDUSTRIA_II, color = CLASE_INDUSTRIA_II)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ NM_RECURSO) +
  theme_minimal() +
  labs(
    title = "Distribución de Precios (Curvas de Densidad)",
    subtitle = "Mayor pico y menor anchura indican mayor concentración y robustez",
    x = "Precio",
    y = "Densidad",
    fill = "Industria",
    color = "Industria"
  )

print(grafico_densidad)