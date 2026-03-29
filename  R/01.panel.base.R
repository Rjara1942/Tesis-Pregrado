# ==============================================================================
# CONSTRUCCIÓN PANEL BASE PURO: Complejo Anchoveta - Sardina 
# (SIN VARIABLES AMBIENTALES NI MACROECONÓMICAS)
# ==============================================================================
#
# Requiere en el mismo directorio:
#   - DESEMBARQUES_SERNAPESCA.xlsx
#   - 2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx
#
# ==============================================================================
# 0. LIBRERÍAS
# ==============================================================================

library(tidyverse)
library(readxl)
library(lubridate)

cat(strrep("=", 70), "\n")
cat("CONSTRUCCIÓN PANEL BASE PURO (SERNAPESCA + IFOP)\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
# 1. CONFIGURACIÓN
# ==============================================================================

PATH_SERNAPESCA <- "DESEMBARQUES_SERNAPESCA.xlsx"
PATH_IFOP <- "2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx"

ESPECIES <- c("JUREL", "SARDINA COMUN", "ANCHOVETA")
REGIONES_CENTRO_SUR <- c(5, 6, 7, 8, 9, 10, 14, 16) 
PERIODO <- 2012:2024

MESES_MAP <- c(
  "ene" = 1, "feb" = 2, "mar" = 3, "abr" = 4, "may" = 5, "jun" = 6,
  "jul" = 7, "ago" = 8, "sept" = 9, "sep" = 9, "oct" = 10, "nov" = 11, "dic" = 12
)

# ==============================================================================
# 2. FUNCIÓN PARA PARSEAR SERNAPESCA
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
cat("3. Procesando Sernapesca...\n")

df_industrial <- parse_sernapesca_sheet(PATH_SERNAPESCA, "INDUSTRIAL (nacional)", "INDUSTRIAL")
df_lanchas <- parse_sernapesca_sheet(PATH_SERNAPESCA, "LANCHAS (CentroSur)", "ARTESANAL_LANCHA")
df_botes <- parse_sernapesca_sheet(PATH_SERNAPESCA, "BOTES (CentroSur)", "ARTESANAL_BOTE")

df_macro <- bind_rows(df_industrial, df_lanchas, df_botes) %>%
  filter(ANIO %in% PERIODO, REGION %in% REGIONES_CENTRO_SUR) %>%
  group_by(ANIO, MES, NM_RECURSO) %>%
  summarise(Q_MACRO = sum(DESEMBARQUE, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 4. CREAR DESEMBARQUES DEL COMPLEJO Y REZAGOS
# ==============================================================================
cat("4. Generando el complejo pelágico y sus rezagos macro...\n")

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
    
    # Rezagos macrozonales
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
# 5. PARSEAR PRECIOS IFOP Y RECUPERAR PLANTAS MIXTAS
# ==============================================================================
cat("5. Procesando precios IFOP a nivel de planta...\n")

df_precio <- read_excel(PATH_IFOP, sheet = "PRECIO") %>%
  mutate(NM_RECURSO = str_trim(NM_RECURSO), CLASE_INDUSTRIA_II = str_trim(CLASE_INDUSTRIA_II), NUI = as.character(NUI)) %>%
  filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN"), RG %in% REGIONES_CENTRO_SUR, 
         CLASE_INDUSTRIA_II %in% c("ANIMAL", "MIXTA_AH"), !is.na(PRECIO), PRECIO > 0)

df_proceso <- read_excel(PATH_IFOP, sheet = "PROCESO") %>%
  mutate(NM_RECURSO = str_trim(NM_RECURSO), CLASE_INDUSTRIA = str_trim(CLASE_INDUSTRIA), NUI = as.character(NUI)) %>%
  filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN"), RG %in% REGIONES_CENTRO_SUR, 
         CLASE_INDUSTRIA %in% c("ANIMAL", "MIXTA_AH")) %>%  
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
# 6. INTEGRACIÓN Y GENERACIÓN DEL PANEL BASE
# ==============================================================================
cat("6. Integrando panel final...\n")

df_panel <- df_P_planta %>%
  left_join(df_macro_ts, by = c("ANIO", "MES")) %>%
  mutate(
    FECHA = make_date(ANIO, MES, 1),
    yearmonth = paste0(ANIO, "-", sprintf("%02d", MES)),
    D_VEDA = ifelse(MES %in% c(1, 8, 9), 1, 0),
    MES_fact = factor(MES),
    
    ln_h_planta = log(h_planta + 1),
    ln_P_complejo_nominal = log(P_complejo) # Se mantiene nominal hasta que se integre el deflactor externo
  ) %>%
  # Limpieza para asegurar datos válidos
  drop_na(P_complejo) %>%
  arrange(NUI, ANIO, MES)

# ==============================================================================
# 7. GUARDAR
# ==============================================================================
write_csv(df_panel, "panel.base.csv")

cat("\n")
cat(strrep("=", 70), "\n")
cat("¡ÉXITO! Base purificada generada como 'panel.base.csv'\n")
cat("N° Observaciones:", nrow(df_panel), "\n")
cat("N° Plantas únicas:", n_distinct(df_panel$NUI), "\n")
cat(strrep("=", 70), "\n")