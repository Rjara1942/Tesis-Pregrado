# ==============================================================================
# SCRIPT DE CORRECCIÓN: ELIMINACIÓN DE "ZIG-ZAGS" (KALMAN SMOOTHING)
# ==============================================================================
if(!require(imputeTS)) install.packages("imputeTS") # Paquete especializado en Series de Tiempo
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(forecast)) install.packages("forecast") # Para limpieza de outliers previos

library(imputeTS)
library(tidyverse)
library(forecast)

# 1. Cargar Datos Originales (con NAs)
# ------------------------------------------------------------------------------
df <- read_csv("harvest_price_IFOP_wide.csv")

# 2. Limpieza Previa de Outliers (Opcional pero recomendado)
# Si tus datos originales ya traían picos raros, esto los suaviza antes de imputar.
# "tsclean" usa LOESS para detectar outliers y los marca como NA para ser rellenados.
# ------------------------------------------------------------------------------
df$P_anchoveta_clean <- tsclean(ts(df$P_anchoveta, frequency = 12))
df$P_sardina_clean   <- tsclean(ts(df$P_sardina_comun, frequency = 12))
df$P_jurel_clean     <- tsclean(ts(df$P_jurel, frequency = 12))

# 3. Imputación por KALMAN SMOOTHING (StructTS)
# Modelo: "StructTS" (Structural Time Series) descompone la serie en Nivel + Tendencia + Estacionalidad.
# Es mucho más robusto que ARIMA para huecos largos y elimina el zig-zag.
# ------------------------------------------------------------------------------

print("Aplicando Suavizado de Kalman...")

# Anchoveta
df$P_anchoveta_kalman <- na_kalman(df$P_anchoveta_clean, model = "StructTS")

# Sardina
df$P_sardina_kalman   <- na_kalman(df$P_sardina_clean, model = "StructTS")

# Jurel
df$P_jurel_kalman     <- na_kalman(df$P_jurel_clean, model = "StructTS")

# 4. Comparación Visual (Para tu tranquilidad)
# ------------------------------------------------------------------------------
par(mfrow=c(3,1), mar=c(2,4,2,1))

# Gráfico Anchoveta
plot(df$P_anchoveta_kalman, type="l", col="red", lwd=2, main="Anchoveta: Kalman (Rojo) vs Original (Negro)")
points(df$P_anchoveta, col="black", pch=19, cex=0.5)

# Gráfico Sardina
plot(df$P_sardina_kalman, type="l", col="blue", lwd=2, main="Sardina: Kalman (Azul) vs Original (Negro)")
points(df$P_sardina_comun, col="black", pch=19, cex=0.5)

# Gráfico Jurel
plot(df$P_jurel_kalman, type="l", col="green", lwd=2, main="Jurel: Kalman (Verde) vs Original (Negro)")
points(df$P_jurel, col="black", pch=19, cex=0.5)

# 5. Guardar Archivo Final "Suave"
# ------------------------------------------------------------------------------
df_final_smooth <- df %>%
  select(year, month, 
         P_anchoveta = P_anchoveta_kalman,
         P_sardina_comun = P_sardina_kalman,
         P_jurel = P_jurel_kalman,
         Q_anchoveta, Q_jurel, Q_sardina_comun)

saveRDS(df_final_smooth, "df_final_kalman.rds")
write_csv(df_final_smooth, "df_final_kalman.csv")

print("Proceso terminado. Los precios ahora deben verse orgánicos y sin picos falsos.")