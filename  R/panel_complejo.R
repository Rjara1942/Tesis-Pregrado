# ==============================================================================
# CONSTRUCCIÓN PANEL LIMPIO complejo Anchoveta- Sardina 
# ==============================================================================
#
# Requiere en el mismo directorio:
#   - DESEMBARQUES_SERNAPESCA.xlsx
#   - 2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx
#   - base_integrada3_IV.csv (para FOB e IVs)
#
# ==============================================================================

# ==============================================================================
# 0. LIBRERÍAS
# ==============================================================================

library(tidyverse)
library(readxl)
library(lubridate)

# ==============================================================================
# 1. CONFIGURACIÓN
# ==============================================================================

PATH_SERNAPESCA <- "DESEMBARQUES_SERNAPESCA.xlsx"
PATH_IFOP <- "2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx"
PATH_ENV <- "base_integrada3_IV.csv"

ESPECIES <- c("JUREL", "SARDINA COMUN", "ANCHOVETA")
REGIONES_CENTRO_SUR <- c(5, 6, 7, 8, 9, 10, 14, 16) 
PERIODO <- 2012:2024

MESES_MAP <- c(
  "ene" = 1, "feb" = 2, "mar" = 3, "abr" = 4, "may" = 5, "jun" = 6,
  "jul" = 7, "ago" = 8, "sept" = 9, "sep" = 9, "oct" = 10, "nov" = 11, "dic" = 12
)

# ==============================================================================
# 2. FUNCIÓN PARA PARSEAR SERNAPESCA (Mantenida igual)
# ==============================================================================
parse_sernapesca_sheet <- function(filepath, sheet_name, tipo_flota) {
  df_raw <- read_excel(filepath, sheet = sheet_name, col_names = FALSE, col_types = "text")
  mat <- as.matrix(df_raw)
  row1 <- mat[1, ]
  row2 <- mat[2, ]
  
  bloques <- list()
  current_esp <- NULL
  
  for (i in seq_along(row1)) {
    val <- toupper(trimws(as.character(row1[i])))
    if (!is.na(val) && nchar(val) > 0) {
      if (grepl("JUREL", val)) {
        if (!is.null(current_esp)) bloques[[current_esp]]$end <- i - 1
        current_esp <- "JUREL"
        bloques[["JUREL"]] <- list(start = i, end = NA)
      } else if (grepl("SARDINA", val)) {
        if (!is.null(current_esp)) bloques[[current_esp]]$end <- i - 1
        current_esp <- "SARDINA COMUN"
        bloques[["SARDINA COMUN"]] <- list(start = i, end = NA)
      } else if (grepl("ANCHOVETA", val)) {
        if (!is.null(current_esp)) bloques[[current_esp]]$end <- i - 1
        current_esp <- "ANCHOVETA"
        bloques[["ANCHOVETA"]] <- list(start = i, end = NA)
      }
    }
  }
  if (!is.null(current_esp)) bloques[[current_esp]]$end <- ncol(mat)
  
  all_data <- list()
  for (especie in names(bloques)) {
    start_col <- bloques[[especie]]$start
    end_col <- bloques[[especie]]$end
    subheaders <- row2[start_col:end_col]
    col_map <- list()
    
    for (j in seq_along(subheaders)) {
      sh <- trimws(as.character(subheaders[j]))
      if (!is.na(sh) && grepl("^[0-9]+\\.?[0-9]*$", sh)) {
        region <- as.integer(as.numeric(sh))
        col_map[[as.character(region)]] <- start_col + j - 1
      }
    }
    
    for (idx in 3:nrow(mat)) {
      año_str <- mat[idx, start_col]
      mes_str <- mat[idx, start_col + 1]
      
      if (is.na(año_str)) next
      año_num <- suppressWarnings(as.numeric(año_str))
      if (is.na(año_num) || año_num < 2000) next
      año <- as.integer(año_num)
      
      if (is.na(mes_str)) next
      mes_key <- tolower(trimws(as.character(mes_str)))
      mes <- MESES_MAP[mes_key]
      if (is.na(mes)) next
      
      for (region_str in names(col_map)) {
        col_idx <- col_map[[region_str]]
        valor_str <- mat[idx, col_idx]
        if (!is.na(valor_str)) {
          valor <- suppressWarnings(as.numeric(valor_str))
          if (!is.na(valor) && valor > 0) {
            all_data[[length(all_data) + 1]] <- data.frame(
              ANIO = año, MES = as.integer(mes), NM_RECURSO = especie,
              REGION = as.integer(region_str), DESEMBARQUE = valor,
              TIPO_FLOTA = tipo_flota, stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }
  if (length(all_data) == 0) return(data.frame())
  bind_rows(all_data)
}

# ==============================================================================
# 3. PARSEAR Y AGREGAR SERNAPESCA
# ==============================================================================
df_industrial <- parse_sernapesca_sheet(PATH_SERNAPESCA, "INDUSTRIAL (nacional)", "INDUSTRIAL")
df_lanchas <- parse_sernapesca_sheet(PATH_SERNAPESCA, "LANCHAS (CentroSur)", "ARTESANAL_LANCHA")
df_botes <- parse_sernapesca_sheet(PATH_SERNAPESCA, "BOTES (CentroSur)", "ARTESANAL_BOTE")

df_macro <- bind_rows(df_industrial, df_lanchas, df_botes) %>%
  filter(ANIO %in% PERIODO, REGION %in% REGIONES_CENTRO_SUR) %>%
  group_by(ANIO, MES, NM_RECURSO) %>%
  summarise(Q_MACRO = sum(DESEMBARQUE, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 4. CREAR DESEMBARQUES DEL COMPLEJO Y REZAGOS (¡CORRECCIÓN 2!)
# ==============================================================================
# Extraer series macro
df_complejo <- df_macro %>%
  filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN")) %>%
  group_by(ANIO, MES) %>%
  summarise(h_complejo = sum(Q_MACRO, na.rm = TRUE), .groups = "drop")

df_jurel <- df_macro %>%
  filter(NM_RECURSO == "JUREL") %>%
  select(ANIO, MES, h_jurel = Q_MACRO)

# Crear grilla temporal continua para evitar saltos de meses de veda
meses_completos <- expand_grid(ANIO = PERIODO, MES = 1:12)

# Unir y calcular rezagos en la serie continua
df_macro_ts <- meses_completos %>%
  left_join(df_complejo, by = c("ANIO", "MES")) %>%
  left_join(df_jurel, by = c("ANIO", "MES")) %>%
  arrange(ANIO, MES) %>%
  mutate(
    h_complejo = replace_na(h_complejo, 0),
    h_jurel = replace_na(h_jurel, 0),
    ln_h_complejo = log(h_complejo + 1),
    ln_h_jurel = log(h_jurel + 1),
    
    # Rezagos macrozonales verdaderos
    ln_h_complejo_L1 = dplyr::lag(ln_h_complejo, 1),
    ln_h_complejo_L2 = dplyr::lag(ln_h_complejo, 2),
    ln_h_complejo_L3 = dplyr::lag(ln_h_complejo, 3),
    ln_h_complejo_L4 = dplyr::lag(ln_h_complejo, 4),
    ln_h_complejo_L5 = dplyr::lag(ln_h_complejo, 5),
    ln_h_complejo_L6 = dplyr::lag(ln_h_complejo, 6),
    
    ln_h_jurel_L1 = dplyr::lag(ln_h_jurel, 1),
    ln_h_jurel_L2 = dplyr::lag(ln_h_jurel, 2)
  )

# ==============================================================================
# 5. PARSEAR PRECIOS IFOP Y RECUPERAR PLANTAS MIXTAS (¡CORRECCIÓN 3!)
# ==============================================================================
df_precio <- read_excel(PATH_IFOP, sheet = "PRECIO") %>%
  mutate(NM_RECURSO = str_trim(NM_RECURSO), CLASE_INDUSTRIA_II = str_trim(CLASE_INDUSTRIA_II), NUI = as.character(NUI)) %>%
  filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN"), RG %in% REGIONES_CENTRO_SUR, 
         CLASE_INDUSTRIA_II %in% c("ANIMAL", "MIXTA_AH"), !is.na(PRECIO), PRECIO > 0)

df_proceso <- read_excel(PATH_IFOP, sheet = "PROCESO") %>%
  mutate(NM_RECURSO = str_trim(NM_RECURSO), CLASE_INDUSTRIA = str_trim(CLASE_INDUSTRIA), NUI = as.character(NUI)) %>%
  filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN"), RG %in% REGIONES_CENTRO_SUR, 
         CLASE_INDUSTRIA %in% c("ANIMAL", "MIXTA_AH")) %>%  # <--- SE RECUPERAN LAS PLANTAS MIXTAS
  group_by(ANIO, MES, RG, NUI, NM_RECURSO) %>%
  summarise(MP_TOTAL = sum(MP_TOTAL, na.rm = TRUE), .groups = "drop")

df_P_planta <- inner_join(
  df_precio %>% select(ANIO, MES, RG, NUI, NM_RECURSO, PRECIO),
  df_proceso, by = c("ANIO", "MES", "RG", "NUI", "NM_RECURSO")
) %>%
  group_by(ANIO, MES, NUI) %>%
  summarise(
    P_complejo = weighted.mean(PRECIO, w = MP_TOTAL, na.rm = TRUE),
    h_planta = sum(MP_TOTAL, na.rm = TRUE),
    n_especies = n_distinct(NM_RECURSO),
    RG = first(RG),
    .groups = "drop"
  )

# ==============================================================================
# 6. VARIABLES AMBIENTALES Y FOB
# ==============================================================================
if (file.exists(PATH_ENV)) {
  df_env <- read_csv(PATH_ENV, show_col_types = FALSE)
  df_controles <- df_env %>%
    group_by(ANIO, MES) %>%
    summarise(
      P_FOB_REAL = first(na.omit(P_HARINA_REAL)),
      DEFLACTOR = first(na.omit(DEFLACTOR)),
      SST_MACRO = first(na.omit(SST_MACRO)),
      CHL_A_MACRO = first(na.omit(CHL_A_MACRO)),
      .groups = "drop"
    )
} else {
  df_controles <- data.frame()
}

# ==============================================================================
# 7. INTEGRACIÓN Y TRANSFORMACIONES FINALES
# ==============================================================================
df_panel <- df_P_planta %>%
  left_join(df_macro_ts, by = c("ANIO", "MES")) # Cruza con la serie macro que ya trae los rezagos

if (nrow(df_controles) > 0) {
  df_panel <- df_panel %>% left_join(df_controles, by = c("ANIO", "MES"))
}

df_panel <- df_panel %>%
  mutate(
    FECHA = make_date(ANIO, MES, 1),
    yearmonth = paste0(ANIO, "-", sprintf("%02d", MES)),
    # ¡CORRECCIÓN 4!: Veda principal (enero) añadida
    D_VEDA = ifelse(MES %in% c(1, 8, 9), 1, 0), 
    MES_fact = factor(MES),
    
    ln_h_planta = log(h_planta + 1)
  )

if ("DEFLACTOR" %in% names(df_panel)) {
  df_panel <- df_panel %>%
    mutate(
      P_complejo_real = P_complejo / DEFLACTOR,
      ln_P_complejo = log(P_complejo_real),
      ln_P_FOB = log(P_FOB_REAL)
    )
} else {
  df_panel <- df_panel %>% mutate(ln_P_complejo = log(P_complejo))
}

# ==============================================================================
# 8. GUARDAR
# ==============================================================================
write_csv(df_panel, "panel_complejo_corregido.csv")
cat("\n¡Base generada con éxito y exportada como 'panel_complejo_corregido.csv'!\n")

# ==============================================================================
# ESTADÍSTICAS DESCRIPTIVAS Y ANÁLISIS EXPLORATORIO
# Panel de Precios Complejo Sardina-Anchoveta
# ==============================================================================

library(tidyverse)
library(plm)         # Para análisis de datos de panel
library(stargazer)   # Para exportar tablas formato LaTeX/Word
library(scales)      # Para formatear ejes en gráficos

# ==============================================================================
# 1. CARGA DE DATOS Y PREPARACIÓN
# ==============================================================================



# Convertir variables clave a factores para el análisis
df_panel <- df_panel %>%
  mutate(
    NUI_fact = as.factor(NUI),
    MES_fact = factor(MES, levels = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                                     "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
    D_VEDA_fact = factor(D_VEDA, levels = c(0, 1), labels = c("Operativo", "Veda"))
  )

# Crear objeto pdata.frame para aprovechar las funciones de plm
pdata <- pdata.frame(df_panel, index = c("NUI", "yearmonth"))

# ==============================================================================
# 2. TABLA DE ESTADÍSTICAS RESUMEN (ESTILO PAPER)
# ==============================================================================
cat("\n--- GENERANDO TABLA DE ESTADÍSTICAS RESUMEN ---\n")

# Seleccionar variables continuas de interés
vars_interes <- df_panel %>%
  select(
    `Precio Nominal (CLP/ton)` = P_complejo,
    `Precio Real (CLP/ton)` = P_complejo_real,
    `Precio FOB Real (USD/ton)` = P_FOB_REAL,
    `Desembarque Complejo (ton)` = h_complejo,
    `Desembarque Jurel (ton)` = h_jurel,
    `Procesamiento Planta (ton)` = h_planta,
    `SST Macrozonal (°C)` = SST_MACRO,
    `Clorofila-a Macrozonal` = CHL_A_MACRO
  )

# Generar tabla en consola
stargazer(as.data.frame(vars_interes), 
          type = "text", 
          title = "Estadísticas Descriptivas Principales",
          digits = 2,
          out = "estadisticas_descriptivas.txt") # Guarda un txt en el directorio

# ==============================================================================
# 3. ANÁLISIS DE VARIANZA DEL PANEL (BETWEEN VS WITHIN)
# ==============================================================================
cat("\n--- DESCOMPOSICIÓN DE VARIANZA DEL PRECIO ---\n")

# Esta sección es clave para justificar los Efectos Fijos.
# Evalúa cuánta variación en el precio viene de las diferencias ENTRE plantas (between)
# y cuánta de la variación DENTRO de la misma planta en el tiempo (within).

sum_precio <- summary(pdata$P_complejo_real)
var_decomp <- data.frame(
  Componente = c("Total", "Between (Entre plantas)", "Within (En el tiempo)"),
  Desv_Estandar = c(sd(df_panel$P_complejo_real, na.rm = TRUE),
                    sd(Between(pdata$P_complejo_real, na.rm=TRUE)),
                    sd(Within(pdata$P_complejo_real, na.rm=TRUE)))
)

var_decomp <- var_decomp %>%
  mutate(Participacion = Desv_Estandar^2 / var_decomp$Desv_Estandar[1]^2 * 100)

print(var_decomp)

# ==============================================================================
# 4. CARACTERIZACIÓN DEL DESBALANCE DEL PANEL
# ==============================================================================
cat("\n--- OBSERVACIONES POR PLANTA ---\n")

obs_por_planta <- df_panel %>%
  group_by(NUI) %>%
  summarise(
    Meses_Reportados = n(),
    Primer_Registro = min(yearmonth),
    Ultimo_Registro = max(yearmonth),
    Precio_Promedio = mean(P_complejo_real, na.rm = TRUE)
  ) %>%
  arrange(desc(Meses_Reportados))

print(obs_por_planta)

# ==============================================================================
# 5. GRÁFICOS EXPLORATORIOS
# ==============================================================================

# Tema base para gráficos (limpio y académico)
theme_paper <- theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# --- Gráfico 1: Evolución temporal del Precio Real por Planta ---
p1 <- ggplot(df_panel, aes(x = FECHA, y = P_complejo_real, group = NUI_fact, color = NUI_fact)) +
  geom_line(alpha = 0.6) +
  geom_point(size = 1, alpha = 0.6) +
  geom_smooth(aes(group = 1), method = "loess", color = "black", size = 1.2, se = FALSE) + # Tendencia general
  scale_y_continuous(labels = comma) +
  labs(
    title = "Evolución del Precio Ex-vessel Real (Complejo Sardina-Anchoveta)",
    subtitle = "Líneas de colores representan plantas individuales. Línea negra es la tendencia suavizada.",
    x = "Año",
    y = "Precio Real (CLP/ton)",
    color = "Planta (NUI)"
  ) +
  theme_paper
print(p1)
ggsave("grafico_tendencia_precios.png", plot = p1, width = 10, height = 6, dpi = 300)

# --- Gráfico 2: Estacionalidad de los Desembarques e Impacto de Vedas ---
p2 <- df_panel %>%
  # Colapsar a nivel macrozonal temporal para evitar duplicar desembarques por planta
  distinct(ANIO, MES_fact, D_VEDA_fact, h_complejo) %>%
  ggplot(aes(x = MES_fact, y = h_complejo / 1000, fill = D_VEDA_fact)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Operativo" = "#2c7bb6", "Veda" = "#d7191c")) +
  labs(
    title = "Estacionalidad de los Desembarques Macrozonal",
    subtitle = "Distribución de desembarques mensuales (2012-2024)",
    x = "Mes",
    y = "Desembarque Complejo (Miles de ton)",
    fill = "Estado"
  ) +
  theme_paper
print(p2)
ggsave("grafico_estacionalidad_desembarques.png", plot = p2, width = 8, height = 5, dpi = 300)

# --- Gráfico 3: Dispersión Precio FOB vs Precio Ex-Vessel (Primera visualización de transmisión) ---
p3 <- ggplot(df_panel, aes(x = P_FOB_REAL, y = P_complejo_real)) +
  geom_point(alpha = 0.5, color = "#2b8cbe") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Correlación: Precio Internacional vs. Precio Local Ex-vessel",
    x = "Precio Harina FOB Real (USD/ton)",
    y = "Precio Local Real (CLP/ton)"
  ) +
  theme_paper
print(p3)
ggsave("grafico_dispersion_fob_local.png", plot = p3, width = 7, height = 5, dpi = 300)

cat("\n¡Análisis completo! Se han exportado las tablas y gráficos al directorio de trabajo.\n")




