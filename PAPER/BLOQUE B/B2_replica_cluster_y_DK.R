# ==============================================================================
# BLOQUE B: cluster por período vs Driscoll-Kraay
# ==============================================================================

library(tidyverse)
library(fixest)     
library(sandwich)    
library(lmtest)      
# ------------------------------------------------------------------------------
# 1. CARGA
# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                 = as.character(NUI),
    ln_P_complejo_real  = log(P_complejo_real),
    period              = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  filter(!is.na(SST_PUERTO_L1))          # dropea las 16 filas con L1 faltante

# Elimina singletons 
df <- df |>
  group_by(NUI) |>
  filter(n() >= 2) |>
  ungroup()

stopifnot(nrow(df) == 418, length(unique(df$NUI)) == 15,
          length(unique(df$period)) == 109)

# ------------------------------------------------------------------------------
# 2. FORMULA COMUN 
# ------------------------------------------------------------------------------
formula_iv <- ln_P_complejo_real ~
  ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
  NUI |
  ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                  ln_biomasa_sardina + ln_TAC_complejo

# ------------------------------------------------------------------------------
# 3. COLUMNA 1 — CLUSTER POR PLANTA 
# ------------------------------------------------------------------------------
m_planta <- feols(formula_iv, data = df, cluster = ~ NUI)

# ------------------------------------------------------------------------------
# 4. COLUMNA 2 — CLUSTER POR PERIODO (mes-año, 109 meses)
# ------------------------------------------------------------------------------
m_periodo <- feols(formula_iv, data = df, cluster = ~ period)

# ------------------------------------------------------------------------------
# 5. COLUMNA 3 — DRISCOLL-KRAAY (HAC sobre residuales agregados por t, bw=4)
# ------------------------------------------------------------------------------
# NW ~ period no funciona en panel porque hay duplicados de tiempo (15 plantas por mes).
# fixest expone Driscoll-Kraay nativo via DK(<lag>) ~ <time>, que agrega los scores
# por periodo y aplica Newey-West sobre la serie agregada 

m_dk <- feols(formula_iv, data = df,
              vcov = DK(4) ~ period)

cat(sprintf("\nDriscoll-Kraay (fixest DK ~ period, bw=4): SE gamma = %.4f\n",
            se(m_dk)["fit_ln_h_complejo"]))

# Se omite  sandwich::vcovPL porque no lee el
# modelo multi-parte de fixest via model.frame()

# ------------------------------------------------------------------------------
# 6. TABLA COMPARATIVA 
# ------------------------------------------------------------------------------
resumen <- function(m, etiqueta) {
  b  <- as.numeric(coef(m)["fit_ln_h_complejo"])
  s  <- as.numeric(se(m)["fit_ln_h_complejo"])
  ci <- confint(m, parm = "fit_ln_h_complejo")
  ci <- as.numeric(unlist(ci))            
  p  <- as.numeric(pvalue(m)["fit_ln_h_complejo"])
  tibble(
    Especificacion = etiqueta,
    gamma          = round(b,     4),
    SE             = round(s,     4),
    IC_inf         = round(ci[1], 4),
    IC_sup         = round(ci[2], 4),
    p_valor        = round(p,     4)
  )
}

tabla <- bind_rows(
  resumen(m_planta,  "Cluster por planta"),
  resumen(m_periodo, "Cluster por periodo (109 meses)"),
  resumen(m_dk,      "Driscoll-Kraay (NW bw=4)")
)

cat("\n==============================================================\n")
cat(" comparativa de matrices de varianza\n")
cat("==============================================================\n")
print(tabla, n = Inf)


dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(tabla,
          here::here("outputs", "reportes_intermedios",
                     "B2_cluster_vs_DK.csv"))

cat("\nGuardado: outputs/reportes_intermedios/B2i_cluster_vs_DK.csv\n")

