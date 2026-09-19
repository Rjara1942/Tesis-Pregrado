# ==============================================================================
# BLOQUE C-8 — Hausman del FOB, coeficientes completos y FOB peruano como
# especificacion de robustez para la Tabla 7 (v2: sin wald() ni IV dos-endogenas
# en fixest, para evitar el error terms.formula/Formula sobre ~ anidado).
# ==============================================================================
# Pauta Felipe 14 sep:
#   1) Coeficientes de la propia regresion de control function
#      (con v_FOB adentro): gamma y beta_FOB con su SE y p.
#      Al lado, beta_FOB por MCO en la misma muestra, y R2 primera etapa
#      del FOB peruano.
#   2) FOB peruano como instrumento del FOB chileno como especificacion
#      completa (primera etapa, F, EE cluster planta y DK) para Tabla 7.
#      Verificar signo del gamma que 5.4 declara en 0,338.
# Muestra corregida D (410 obs).
# ==============================================================================

library(tidyverse)
library(fixest)
library(sandwich)
library(lmtest)
library(AER)   # ivreg
library(car)   # linearHypothesis para F cluster

# ------------------------------------------------------------------------------
# 0. CARGA + MUESTRA CORREGIDA
# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  filter(!is.na(SST_PUERTO_L1)) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup()

df_D <- df |> filter(!(NUI == "90073" & ANIO == 2013))
stopifnot(nrow(df_D) == 410, length(unique(df_D$NUI)) == 15)

# Chequeo defensivo de columnas exigidas
req <- c("ln_P_complejo_real","ln_h_complejo","ln_P_FOB","ln_P_FOB_PERU",
         "SO_PUERTO","SST_PUERTO_L1","ln_biomasa_sardina","ln_TAC_complejo",
         "ln_h_jurel","SEASON_SIN","SEASON_COS","TENDENCIA","NUI","period")
falt <- setdiff(req, names(df_D))
if (length(falt) > 0) stop("Faltan columnas: ", paste(falt, collapse = ", "))

cat("Muestra corregida (D):", nrow(df_D), "obs,",
    length(unique(df_D$NUI)), "plantas,",
    length(unique(df_D$period)), "meses.\n\n")

# ==============================================================================
# 1. PRIMERA ETAPA DEL FOB CHILENO CON PERU
# ==============================================================================
cat("==============================================================\n")
cat("1. Primera etapa del FOB: ln_P_FOB ~ ln_P_FOB_PERU + controles\n")
cat("==============================================================\n")

fs_fob <- feols(
  ln_P_FOB ~ ln_P_FOB_PERU + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI,
  data = df_D, cluster = ~ NUI
)

r2_within  <- as.numeric(fitstat(fs_fob, type = "wr2")$wr2)
r2_overall <- as.numeric(fitstat(fs_fob, type = "r2")$r2)

cat(sprintf("coef ln_P_FOB_PERU = %.4f   SE = %.4f   t = %.2f   p = %.4f\n",
            as.numeric(coef(fs_fob)["ln_P_FOB_PERU"]),
            as.numeric(se(fs_fob)["ln_P_FOB_PERU"]),
            as.numeric(coef(fs_fob)["ln_P_FOB_PERU"] /
                       se(fs_fob)["ln_P_FOB_PERU"]),
            as.numeric(pvalue(fs_fob)["ln_P_FOB_PERU"])))
cat(sprintf("R2 within  = %.4f\nR2 overall = %.4f\n", r2_within, r2_overall))

df_D$v_FOB <- residuals(fs_fob)
stopifnot(!anyNA(df_D$v_FOB))

# ==============================================================================
# 2. CONTROL FUNCTION (v_FOB adentro) — 1 endogena, syntax normal de fixest
# ==============================================================================
cat("\n==============================================================\n")
cat("2. Control function (con v_FOB) sobre muestra corregida\n")
cat("==============================================================\n")

# Fixest, cluster planta
m_cf <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA + v_FOB | NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                    ln_biomasa_sardina + ln_TAC_complejo,
  data = df_D, cluster = ~ NUI
)

# Fixest, Driscoll-Kraay bw=4
m_cf_DK <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA + v_FOB | NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                    ln_biomasa_sardina + ln_TAC_complejo,
  data = df_D, vcov = DK(4) ~ period
)

nm_g <- if ("fit_ln_h_complejo" %in% names(coef(m_cf)))
          "fit_ln_h_complejo" else "ln_h_complejo"

extraer <- function(m, k) list(
  b = as.numeric(coef(m)[k]),
  s = as.numeric(se(m)[k]),
  p = as.numeric(pvalue(m)[k])
)

g_cf   <- extraer(m_cf,    nm_g)
g_cfDK <- extraer(m_cf_DK, nm_g)
b_cf   <- extraer(m_cf,    "ln_P_FOB")
b_cfDK <- extraer(m_cf_DK, "ln_P_FOB")
d_cf   <- extraer(m_cf,    "ln_h_jurel")
v_cf   <- extraer(m_cf,    "v_FOB")

cat("\nControl function, cluster planta:\n")
cat(sprintf("  gamma  = %+.4f  SE = %.4f  p = %.4f\n", g_cf$b, g_cf$s, g_cf$p))
cat(sprintf("  beta   = %+.4f  SE = %.4f  p = %.4f\n", b_cf$b, b_cf$s, b_cf$p))
cat(sprintf("  delta  = %+.4f  SE = %.4f  p = %.4f\n", d_cf$b, d_cf$s, d_cf$p))
cat(sprintf("  theta v_FOB (Hausman FOB) = %+.4f  SE = %.4f  p = %.4f\n",
            v_cf$b, v_cf$s, v_cf$p))
cat("\nControl function, Driscoll-Kraay bw=4:\n")
cat(sprintf("  gamma = %+.4f  SE = %.4f  p = %.4f\n",
            g_cfDK$b, g_cfDK$s, g_cfDK$p))
cat(sprintf("  beta  = %+.4f  SE = %.4f  p = %.4f\n",
            b_cfDK$b, b_cfDK$s, b_cfDK$p))

# ==============================================================================
# 3. beta_FOB POR MCO EN LA MISMA MUESTRA CORREGIDA
# ==============================================================================
cat("\n==============================================================\n")
cat("3. beta_FOB por MCO en la muestra corregida\n")
cat("==============================================================\n")

m_ols <- feols(
  ln_P_complejo_real ~ ln_h_complejo + ln_P_FOB + ln_h_jurel +
                       SEASON_SIN + SEASON_COS + TENDENCIA | NUI,
  data = df_D, cluster = ~ NUI
)
b_ols <- extraer(m_ols, "ln_P_FOB")
g_ols <- extraer(m_ols, "ln_h_complejo")
cat(sprintf("gamma (MCO) = %+.4f (SE %.4f)\n", g_ols$b, g_ols$s))
cat(sprintf("beta  (MCO) = %+.4f (SE %.4f)\n", b_ols$b, b_ols$s))

cat("\nComparativa beta_FOB (medida, no estimada a ojo):\n")
cat(sprintf("  MCO                    : %+.4f (SE %.4f)\n", b_ols$b, b_ols$s))
cat(sprintf("  Control function (CP)  : %+.4f (SE %.4f)\n", b_cf$b, b_cf$s))
cat(sprintf("  Control function (DK)  : %+.4f (SE %.4f)\n", b_cfDK$b, b_cfDK$s))
cat(sprintf("  Brecha MCO - CF        : %+.4f puntos\n",   b_ols$b - b_cf$b))

# ==============================================================================
# 4. FOB PERUANO COMO IV COMPLETA (dos endogenas) — via AER::ivreg
# ==============================================================================
# Fixest 0.11 tiene bugs conocidos con formulas IV multi-endogena y con Formula
# anidada; por eso paso a ivreg sobre datos demeaned por planta (equivalente
# a incluir dummies de planta). Las EE cluster planta salen de sandwich::vcovCL
# y las Driscoll-Kraay de vcovPL sobre period.
# ==============================================================================
cat("\n==============================================================\n")
cat("4. FOB peruano como instrumento del FOB chileno — 2SLS con 2 endogenas\n")
cat("==============================================================\n")

demean_por <- function(x, g)
  x - ave(x, g, FUN = function(v) mean(v, na.rm = TRUE))

dd <- df_D
for (v in c("ln_P_complejo_real","ln_h_complejo","ln_P_FOB","ln_P_FOB_PERU",
            "SO_PUERTO","SST_PUERTO_L1","ln_biomasa_sardina",
            "ln_TAC_complejo","ln_h_jurel","SEASON_SIN","SEASON_COS",
            "TENDENCIA")) {
  dd[[paste0(v, "_dm")]] <- demean_por(df_D[[v]], df_D$NUI)
}

# 2SLS con dos endogenas: ln_h_complejo y ln_P_FOB, instrumentos:
#   SO, SST_L1, biomasa, TAC, ln_P_FOB_PERU (5 -> 2, sobreidentificado)
iv_peru <- ivreg(
  ln_P_complejo_real_dm ~ ln_h_complejo_dm + ln_P_FOB_dm +
                           ln_h_jurel_dm + SEASON_SIN_dm + SEASON_COS_dm +
                           TENDENCIA_dm |
                          SO_PUERTO_dm + SST_PUERTO_L1_dm +
                           ln_biomasa_sardina_dm + ln_TAC_complejo_dm +
                           ln_P_FOB_PERU_dm +
                           ln_h_jurel_dm + SEASON_SIN_dm + SEASON_COS_dm +
                           TENDENCIA_dm,
  data = dd
)

# EE cluster planta (df ajustados por G-1)
vCL <- sandwich::vcovCL(iv_peru, cluster = dd$NUI, type = "HC1")
coefCL <- lmtest::coeftest(iv_peru, vcov. = vCL)

# EE Driscoll-Kraay bw=4 (via vcovPL)
vDK <- tryCatch(
  sandwich::vcovPL(iv_peru, cluster = dd$period, lag = 4, adjust = TRUE),
  error = function(e) NULL
)
coefDK <- if (!is.null(vDK)) lmtest::coeftest(iv_peru, vcov. = vDK) else NULL

pretty <- function(cm, nm) {
  b <- cm[nm, "Estimate"]
  s <- cm[nm, "Std. Error"]
  p <- cm[nm, "Pr(>|t|)"]
  list(b = b, s = s, p = p)
}

gpCP <- pretty(coefCL, "ln_h_complejo_dm")
bpCP <- pretty(coefCL, "ln_P_FOB_dm")

cat("\n2SLS con dos endogenas (EE cluster planta):\n")
cat(sprintf("  gamma = %+.4f  SE = %.4f  p = %.4f\n", gpCP$b, gpCP$s, gpCP$p))
cat(sprintf("  beta  = %+.4f  SE = %.4f  p = %.4f\n", bpCP$b, bpCP$s, bpCP$p))

if (!is.null(coefDK)) {
  gpDK <- pretty(coefDK, "ln_h_complejo_dm")
  bpDK <- pretty(coefDK, "ln_P_FOB_dm")
  cat("\n2SLS con dos endogenas (Driscoll-Kraay bw=4):\n")
  cat(sprintf("  gamma = %+.4f  SE = %.4f  p = %.4f\n",
              gpDK$b, gpDK$s, gpDK$p))
  cat(sprintf("  beta  = %+.4f  SE = %.4f  p = %.4f\n",
              bpDK$b, bpDK$s, bpDK$p))
} else {
  gpDK <- list(b = NA, s = NA, p = NA)
  bpDK <- list(b = NA, s = NA, p = NA)
  cat("\n(EE Driscoll-Kraay no calculable con vcovPL sobre este ivreg;\n",
      "  reportar solo cluster planta en la Tabla 7.)\n")
}

# ---- Verificacion del signo del gamma de la seccion 5.4 ---------------------
cat("\n---- Verificacion del signo (5.4 declara 0,338) ----\n")
cat(sprintf("gamma observado (CP): %+.4f\n", gpCP$b))
if (gpCP$b < 0) {
  cat("=> signo NEGATIVO. 5.4 debe reportar '-0,338' (o el numero exacto).\n")
} else {
  cat("=> signo POSITIVO. Averiguar de donde salio el +0,338 antes de publicar.\n")
}

# ---- F de primera etapa por endogena, cluster planta (linearHypothesis) -----
cat("\n---- Primera etapa por endogena, F cluster planta ----\n")

fs_h_lm <- lm(
  ln_h_complejo_dm ~ SO_PUERTO_dm + SST_PUERTO_L1_dm +
                     ln_biomasa_sardina_dm + ln_TAC_complejo_dm +
                     ln_P_FOB_PERU_dm +
                     ln_h_jurel_dm + SEASON_SIN_dm + SEASON_COS_dm +
                     TENDENCIA_dm,
  data = dd
)
fs_p_lm <- lm(
  ln_P_FOB_dm ~ SO_PUERTO_dm + SST_PUERTO_L1_dm +
                ln_biomasa_sardina_dm + ln_TAC_complejo_dm +
                ln_P_FOB_PERU_dm +
                ln_h_jurel_dm + SEASON_SIN_dm + SEASON_COS_dm +
                TENDENCIA_dm,
  data = dd
)

instr <- c("SO_PUERTO_dm","SST_PUERTO_L1_dm",
           "ln_biomasa_sardina_dm","ln_TAC_complejo_dm","ln_P_FOB_PERU_dm")

F_h <- car::linearHypothesis(
  fs_h_lm, instr,
  vcov. = sandwich::vcovCL(fs_h_lm, cluster = dd$NUI, type = "HC1"),
  test = "F"
)$F[2]
F_p <- car::linearHypothesis(
  fs_p_lm, instr,
  vcov. = sandwich::vcovCL(fs_p_lm, cluster = dd$NUI, type = "HC1"),
  test = "F"
)$F[2]

cat(sprintf("F cluster (1a etapa ln_h_complejo): %.3f\n", F_h))
cat(sprintf("F cluster (1a etapa ln_P_FOB)     : %.3f  (dominado por FOB_PERU)\n",
            F_p))

# ==============================================================================
# 5. RESUMEN COMPACTO
# ==============================================================================
resumen <- tribble(
  ~item, ~valor,
  "1a etapa FOB: coef ln_P_FOB_PERU",       round(as.numeric(coef(fs_fob)["ln_P_FOB_PERU"]), 4),
  "1a etapa FOB: R2 within",                round(r2_within, 4),
  "1a etapa FOB: R2 overall",               round(r2_overall, 4),
  "CF cluster planta: gamma",               round(g_cf$b, 4),
  "CF cluster planta: SE gamma",            round(g_cf$s, 4),
  "CF cluster planta: p gamma",             round(g_cf$p, 4),
  "CF cluster planta: beta_FOB",            round(b_cf$b, 4),
  "CF cluster planta: SE beta_FOB",         round(b_cf$s, 4),
  "CF cluster planta: theta (Hausman FOB)", round(v_cf$b, 4),
  "CF cluster planta: p Hausman FOB",       round(v_cf$p, 4),
  "CF DK bw=4: gamma",                      round(g_cfDK$b, 4),
  "CF DK bw=4: SE gamma",                   round(g_cfDK$s, 4),
  "CF DK bw=4: beta_FOB",                   round(b_cfDK$b, 4),
  "CF DK bw=4: SE beta_FOB",                round(b_cfDK$s, 4),
  "MCO: gamma",                             round(g_ols$b, 4),
  "MCO: beta_FOB",                          round(b_ols$b, 4),
  "MCO: SE beta_FOB",                       round(b_ols$s, 4),
  "IV Peru (CP): gamma",                    round(gpCP$b, 4),
  "IV Peru (CP): SE gamma",                 round(gpCP$s, 4),
  "IV Peru (CP): p gamma",                  round(gpCP$p, 4),
  "IV Peru (CP): beta_FOB",                 round(bpCP$b, 4),
  "IV Peru (CP): SE beta_FOB",              round(bpCP$s, 4),
  "IV Peru (DK): gamma",                    round(gpDK$b, 4),
  "IV Peru (DK): SE gamma",                 round(gpDK$s, 4),
  "IV Peru (DK): beta_FOB",                 round(bpDK$b, 4),
  "F cluster 1a etapa ln_h_complejo",       round(F_h, 4),
  "F cluster 1a etapa ln_P_FOB",            round(F_p, 4)
)

dir.create(here::here("outputs","reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(resumen,
          here::here("outputs","reportes_intermedios",
                     "C8_hausman_FOB_completo.csv"))
cat("\nGuardado: outputs/reportes_intermedios/C8_hausman_FOB_completo.csv\n")
print(resumen, n = Inf, width = Inf)
