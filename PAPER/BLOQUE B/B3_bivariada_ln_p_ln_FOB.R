# ==============================================================================
# BLOQUE B: REGRESION BIVARIADA ln(P_ex-vessel) ~ ln(P_FOB)
# ==============================================================================
#  correr la especificacion mas simple posible entre precio ex-vessel
# y precio internacional FOB de harina, sin efectos fijos, sin controles, sin
# instrumentos. 
# ==============================================================================

library(tidyverse)
library(fixest)
library(sandwich)
library(lmtest)

# ------------------------------------------------------------------------------
# 1. CARGA (usa el panel completo, sin filtros de estimacion IV)
# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  )

# ------------------------------------------------------------------------------
# 2. SERIE AGREGADA MENSUAL
# ------------------------------------------------------------------------------
agg <- df |>
  group_by(period) |>
  summarise(
    ln_P_mean  = mean(ln_P_complejo_real),
    ln_P_wmean = weighted.mean(ln_P_complejo_real, w = h_planta),
    ln_P_FOB   = first(ln_P_FOB),
    N_plantas  = n_distinct(NUI),
    .groups    = "drop"
  )

cat(sprintf("Panel apilado: %d obs. Serie agregada: %d meses.\n",
            nrow(df), nrow(agg)))

# ------------------------------------------------------------------------------
# 3. PANEL APILADO — ln P ~ ln P_FOB SIN EFECTOS FIJOS NI CONTROLES
# ------------------------------------------------------------------------------
m_panel_ols  <- feols(ln_P_complejo_real ~ ln_P_FOB, data = df)
m_panel_cp   <- feols(ln_P_complejo_real ~ ln_P_FOB, data = df,
                      cluster = ~ NUI)
m_panel_ct   <- feols(ln_P_complejo_real ~ ln_P_FOB, data = df,
                      cluster = ~ period)

# ------------------------------------------------------------------------------
# 4. SERIE AGREGADA — Newey-West bw=4
# ------------------------------------------------------------------------------
m_agg_mean   <- feols(ln_P_mean  ~ ln_P_FOB, data = agg,
                      vcov = NW(4) ~ period)
m_agg_wmean  <- feols(ln_P_wmean ~ ln_P_FOB, data = agg,
                      vcov = NW(4) ~ period)

# ------------------------------------------------------------------------------
# 5. TABLA COMPARATIVA
# ------------------------------------------------------------------------------
resumen <- function(m, etiqueta) {
  b  <- as.numeric(coef(m)["ln_P_FOB"])
  s  <- as.numeric(se(m)["ln_P_FOB"])
  ci <- as.numeric(unlist(confint(m, parm = "ln_P_FOB")))
  p  <- as.numeric(pvalue(m)["ln_P_FOB"])
  a  <- as.numeric(coef(m)["(Intercept)"])
  r2 <- as.numeric(r2(m, type = "r2"))
  tibble(
    Especificacion = etiqueta,
    beta_FOB       = round(b,     4),
    SE_beta        = round(s,     4),
    IC_inf         = round(ci[1], 4),
    IC_sup         = round(ci[2], 4),
    p_valor        = signif(p,    4),
    intercepto     = round(a,     4),
    passthrough_impl = round(exp(a), 4),   # exp(a) es el pass-through implicito si beta = 1
    R2             = round(r2,    4),
    N              = nobs(m)
  )
}

tabla <- bind_rows(
  resumen(m_panel_ols,  "Panel apilado, OLS clasico"),
  resumen(m_panel_cp,   "Panel apilado, cluster planta"),
  resumen(m_panel_ct,   "Panel apilado, cluster periodo"),
  resumen(m_agg_mean,   "Agregado, media simple, Newey-West bw=4"),
  resumen(m_agg_wmean,  "Agregado, media ponderada por h_planta, NW bw=4")
)

cat("\n==============================================================\n")
cat("ln P_ex-vessel ~ ln P_FOB\n")
cat("==============================================================\n")
print(tabla, n = Inf, width = Inf)

# ------------------------------------------------------------------------------
# 6. TEST FORMAL: H0 beta = 1 (convencion de traspaso proporcional)
# ------------------------------------------------------------------------------

mods <- list(
  "Panel OLS"                   = m_panel_ols,
  "Panel cluster planta"        = m_panel_cp,
  "Panel cluster periodo"       = m_panel_ct,
  "Agregado media simple NW"    = m_agg_mean,
  "Agregado media ponderada NW" = m_agg_wmean
)
for (nm in names(mods)) {
  m <- mods[[nm]]
  b <- as.numeric(coef(m)["ln_P_FOB"])
  s <- as.numeric(se(m)["ln_P_FOB"])
  z <- (b - 1) / s
  p <- 2 * (1 - pnorm(abs(z)))
  cat(sprintf("  %-32s beta = %.4f  SE = %.4f  z(b=1) = %.2f  p = %.4f\n",
              nm, b, s, z, p))
}

# ------------------------------------------------------------------------------
# 7. RATIO EN NIVELES (contra la convencion 0.12)
# ------------------------------------------------------------------------------
cat("\n--- Ratio en niveles (P_ex-vessel = a + b * P_FOB, real jun-2024) ---\n")
niv <- df |>
  group_by(period) |>
  summarise(P_wmean    = weighted.mean(P_complejo_real, w = h_planta),
            P_FOB_real = first(P_FOB_REAL), .groups = "drop")
r_niv <- lm(P_wmean ~ P_FOB_real, data = niv)
cat(sprintf("  P_ex-vessel = %.0f + %.4f * P_FOB   (R2 = %.4f)\n",
            coef(r_niv)[1], coef(r_niv)[2], summary(r_niv)$r.squared))
cat(sprintf("  Ratio implicito P/FOB = %.4f  (convencion = 0.12)\n",
            coef(r_niv)[2]))

# ------------------------------------------------------------------------------
# 8. GUARDAR
# ------------------------------------------------------------------------------
dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(tabla,
          here::here("outputs", "reportes_intermedios",
                     "B3_lnP_lnFOB.csv"))
cat("\nGuardado: outputs/reportes_intermedios/B3_bivariada_lnP_lnFOB.csv\n")

