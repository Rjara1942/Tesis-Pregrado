# ==============================================================================
# BLOQUE B.2: SERIE AGREGADA MENSUAL CON NEWEY-WEST
# ==============================================================================

library(tidyverse)
library(fixest)
library(sandwich)
library(AER)
library(lmtest)

# ------------------------------------------------------------------------------
# 1. CARGA DEL PANEL Y RESTRICCIONES DE MUESTRA
# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  filter(!is.na(SST_PUERTO_L1)) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup()

stopifnot(nrow(df) == 418, length(unique(df$NUI)) == 15)

# ------------------------------------------------------------------------------
# 2. COLAPSO A NIVEL MENSUAL
# ------------------------------------------------------------------------------
#  Los que varian por puerto (SO_PUERTO, SST_PUERTO) se
# colapsan a nivel mes con promedio ponderado por h_planta.
macro <- df |>
  group_by(period) |>
  summarise(
    ln_h_complejo      = first(ln_h_complejo),
    ln_P_FOB           = first(ln_P_FOB),
    ln_h_jurel         = first(ln_h_jurel),
    SEASON_SIN         = first(SEASON_SIN),
    SEASON_COS         = first(SEASON_COS),
    TENDENCIA          = first(TENDENCIA),
    SST_MACRO          = first(SST_MACRO),
    ln_biomasa_sardina = first(ln_biomasa_sardina),
    ln_TAC_complejo    = first(ln_TAC_complejo),
    SO_agg             = mean(SO_PUERTO,     na.rm = TRUE),
    SST_puerto_agg     = mean(SST_PUERTO,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(period) |>
  mutate(SST_MACRO_L1 = lag(SST_MACRO))

# Precios agregados: media simple y media ponderada por h_planta
precios_agg <- df |>
  group_by(period) |>
  summarise(
    ln_P_mean  = mean(ln_P_complejo_real),
    ln_P_wmean = weighted.mean(ln_P_complejo_real, w = h_planta),
    N_plantas  = n_distinct(NUI),
    .groups    = "drop"
  )

agg <- macro |>
  inner_join(precios_agg, by = "period") |>
  drop_na(SST_MACRO_L1)                     # perdemos el primer mes por el rezago

cat(sprintf("Serie agregada: %d meses (%s a %s)\n",
            nrow(agg),
            format(min(agg$period), "%Y-%m"),
            format(max(agg$period), "%Y-%m")))

# ------------------------------------------------------------------------------
# 3. IV AGREGADO CON NEWEY-WEST 
# ------------------------------------------------------------------------------

m_mean <- feols(
  ln_P_mean ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    ln_h_complejo ~ SST_MACRO_L1 + ln_biomasa_sardina + ln_TAC_complejo,
  data = agg, vcov = NW(4) ~ period
)

m_wmean <- feols(
  ln_P_wmean ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    ln_h_complejo ~ SST_MACRO_L1 + ln_biomasa_sardina + ln_TAC_complejo,
  data = agg, vcov = NW(4) ~ period
)

# Robustez: incluir SO_agg como cuarto instrumento para reportar en el paper
m_mean_4instr <- feols(
  ln_P_mean ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    ln_h_complejo ~ SO_agg + SST_MACRO_L1 + ln_biomasa_sardina + ln_TAC_complejo,
  data = agg, vcov = NW(4) ~ period
)

# ------------------------------------------------------------------------------
# 4. F DE PRIMERA ETAPA 
# ------------------------------------------------------------------------------
first_stage_3 <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                  SST_MACRO_L1 + ln_biomasa_sardina + ln_TAC_complejo,
  data = agg, vcov = NW(4) ~ period
)
F3 <- wald(first_stage_3,
           keep = c("SST_MACRO_L1", "ln_biomasa_sardina", "ln_TAC_complejo"))
cat(sprintf("\nF primera etapa, 3 instrumentos (principal): %.2f\n", F3$stat))

first_stage_4 <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                  SO_agg + SST_MACRO_L1 + ln_biomasa_sardina + ln_TAC_complejo,
  data = agg, vcov = NW(4) ~ period
)
F4 <- wald(first_stage_4,
           keep = c("SO_agg", "SST_MACRO_L1",
                    "ln_biomasa_sardina", "ln_TAC_complejo"))
cat(sprintf("F primera etapa, 4 instrumentos (con SO_agg):  %.2f\n", F4$stat))

# ------------------------------------------------------------------------------
# 5. TABLA COMPARATIVA
# ------------------------------------------------------------------------------
resumen <- function(m, etiqueta) {
  nm <- "ln_h_complejo"    
  if (!nm %in% names(coef(m))) {
    nm <- grep("h_complejo", names(coef(m)), value = TRUE)[1]
  }
  b_g  <- as.numeric(coef(m)[nm])
  s_g  <- as.numeric(se(m)[nm])
  ci_g <- as.numeric(unlist(confint(m, parm = nm)))
  p_g  <- as.numeric(pvalue(m)[nm])
  b_fob <- as.numeric(coef(m)["ln_P_FOB"])
  s_fob <- as.numeric(se(m)["ln_P_FOB"])
  p_fob <- as.numeric(pvalue(m)["ln_P_FOB"])
  tibble(
    Especificacion = etiqueta,
    gamma          = round(b_g,   4),
    SE_gamma       = round(s_g,   4),
    IC_inf         = round(ci_g[1], 4),
    IC_sup         = round(ci_g[2], 4),
    p_gamma        = round(p_g,   4),
    beta_FOB       = round(b_fob, 4),
    SE_FOB         = round(s_fob, 4),
    p_FOB          = round(p_fob, 4),
    N_meses        = nobs(m)
  )
}

tabla <- bind_rows(
  resumen(m_mean,        "Principal: media simple, 3 instrumentos"),
  resumen(m_wmean,       "Principal: media ponderada, 3 instrumentos"),
  resumen(m_mean_4instr, "Robustez: media simple, 4 instrumentos (con SO_agg)")
)

cat("\n==============================================================\n")
cat("BLOQUE B.2.ii — IV agregado mensual con Newey-West (4 instrumentos)\n")
cat("==============================================================\n")
print(tabla, n = Inf, width = Inf)

dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(tabla,
          here::here("outputs", "reportes_intermedios",
                     "B2_agregado_NW.csv"))
cat("\nGuardado: outputs/reportes_intermedios/B2ii_agregado_NW.csv\n")

# ------------------------------------------------------------------------------
# 6. CROSS-CHECK CON AER::ivreg + sandwich::NeweyWest
# ------------------------------------------------------------------------------
cat("\nCross-check AER::ivreg + sandwich::NeweyWest (media simple, 3 instrumentos):\n")
m_ivreg <- ivreg(
  ln_P_mean ~ ln_h_complejo + ln_P_FOB + ln_h_jurel +
              SEASON_SIN + SEASON_COS + TENDENCIA |
              SST_MACRO_L1 + ln_biomasa_sardina + ln_TAC_complejo +
              ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA,
  data = agg
)
nw_vcov <- NeweyWest(m_ivreg, lag = 4, prewhite = FALSE, adjust = FALSE)
ct <- coeftest(m_ivreg, vcov. = nw_vcov)
cat(sprintf("  gamma = %.4f  SE (NW) = %.4f  p = %.4f\n",
            ct["ln_h_complejo", "Estimate"],
            ct["ln_h_complejo", "Std. Error"],
            ct["ln_h_complejo", "Pr(>|t|)"]))

# ------------------------------------------------------------------------------
# 7. INTERVALO ANDERSON-RUBIN (obligatorio con F 1a etapa = 2.35)
# ------------------------------------------------------------------------------


if (!requireNamespace("ivmodel", quietly = TRUE)) install.packages("ivmodel")
library(ivmodel)

# Construir los inputs para ivmodel:
#   Y = dep, D = endogena, Z = instrumentos, X = exogenas incluidas
Y <- agg$ln_P_mean
D <- agg$ln_h_complejo
Z_mat <- as.matrix(agg[, c("SST_MACRO_L1", "ln_biomasa_sardina", "ln_TAC_complejo")])
X_mat <- as.matrix(agg[, c("ln_P_FOB", "ln_h_jurel",
                           "SEASON_SIN", "SEASON_COS", "TENDENCIA")])

iv_obj <- ivmodel(Y = Y, D = D, Z = Z_mat, X = X_mat, heteroSE = TRUE)

cat("\n==============================================================\n")
cat("BLOQUE B.2.ii — Anderson-Rubin (agregado, media simple, 3 instr)\n")
cat("==============================================================\n")

ar_test <- AR.test(iv_obj, alpha = 0.05)
cat(sprintf("AR test H0: gamma = 0\n"))
cat(sprintf("  Fstat = %.4f  df1 = %d  df2 = %d  p = %.4f\n",
            ar_test$Fstat, ar_test$df[1], ar_test$df[2], ar_test$p.value))
cat("Intervalo de confianza AR al 95%:\n")
print(ar_test$ci)

# Repetir para media ponderada
Y_w <- agg$ln_P_wmean
iv_obj_w <- ivmodel(Y = Y_w, D = D, Z = Z_mat, X = X_mat, heteroSE = TRUE)
ar_test_w <- AR.test(iv_obj_w, alpha = 0.05)
cat("\nAgregado, media ponderada por h_planta:\n")
cat(sprintf("  Fstat = %.4f  p = %.4f\n", ar_test_w$Fstat, ar_test_w$p.value))
cat("Intervalo AR al 95%:\n")
print(ar_test_w$ci)

# ------------------------------------------------------------------------------
# 8. VALORES ESPERADOS (referencia rapida)
# ------------------------------------------------------------------------------
# Panel (Bloque B.2.i): F within alto por construccion, IC ~ [-0.57, -0.12]
# Agregado 3 instr    : F = 2.35, IC NW [-0.45, 0.04], AR probablemente mas ancho
# Agregado 4 instr    : F = 6.10, gamma colapsa a -0.06 (SO_agg no aporta)
#
# La lectura correcta: el panel es la especificacion principal; el agregado
# se reporta con AR como sensibilidad honesta bajo instrumentos debiles.
