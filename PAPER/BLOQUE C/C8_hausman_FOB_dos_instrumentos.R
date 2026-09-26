# ==============================================================================
# BLOQUE C-8 v2 — Re-corrida con dos instrumentos anuales y matriz DK
# ==============================================================================
# Muestra: 427 obs, 16 plantas (sin filtro SST_L1, sin 90073/2013).
# Instrumentos: ln_biomasa_sardina + ln_TAC_complejo.
# Matriz: Driscoll Kraay bw=4 (principal); cluster planta al lado como referencia.
# ==============================================================================

library(tidyverse)
library(fixest)
library(sandwich)
library(lmtest)

# ------------------------------------------------------------------------------
# 0. MUESTRA NUEVA
# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup() |>
  filter(!(NUI == "90073" & ANIO == 2013))

cat("Muestra: N =", nrow(df), ", G =", length(unique(df$NUI)),
    ", T =", length(unique(df$period)), "\n")

# ==============================================================================
# 1. PRIMERA ETAPA DEL FOB CHILENO CON FOB PERUANO
# ==============================================================================
cat("\n==============================================================\n")
cat("1. Primera etapa del FOB con ln_P_FOB_PERU\n")
cat("==============================================================\n")

fs_fob <- feols(
  ln_P_FOB ~ ln_P_FOB_PERU + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI,
  data = df, vcov = DK(4) ~ period
)

r2_within  <- as.numeric(fitstat(fs_fob, type = "wr2")$wr2)
r2_overall <- as.numeric(fitstat(fs_fob, type = "r2")$r2)

cat(sprintf("coef ln_P_FOB_PERU = %.4f   SE (DK) = %.4f   t = %.2f\n",
            as.numeric(coef(fs_fob)["ln_P_FOB_PERU"]),
            as.numeric(se(fs_fob)["ln_P_FOB_PERU"]),
            as.numeric(coef(fs_fob)["ln_P_FOB_PERU"] /
                       se(fs_fob)["ln_P_FOB_PERU"])))
cat(sprintf("R2 within = %.4f   R2 overall = %.4f\n", r2_within, r2_overall))

df$v_FOB <- residuals(fs_fob)

# ==============================================================================
# 2. CONTROL FUNCTION CON DOS INSTRUMENTOS Y DK
# ==============================================================================
cat("\n==============================================================\n")
cat("2. Control function (v_FOB adentro), DK bw=4, dos instrumentos\n")
cat("==============================================================\n")

m_cf_DK <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA + v_FOB | NUI |
    ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo,
  data = df, vcov = DK(4) ~ period
)

m_cf_CP <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA + v_FOB | NUI |
    ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo,
  data = df, cluster = ~ NUI
)

nm_g <- if ("fit_ln_h_complejo" %in% names(coef(m_cf_DK)))
          "fit_ln_h_complejo" else "ln_h_complejo"

extraer <- function(m, k) list(
  b = as.numeric(coef(m)[k]),
  s = as.numeric(se(m)[k]),
  p = as.numeric(pvalue(m)[k])
)

g_DK <- extraer(m_cf_DK, nm_g);  b_DK <- extraer(m_cf_DK, "ln_P_FOB")
g_CP <- extraer(m_cf_CP, nm_g);  b_CP <- extraer(m_cf_CP, "ln_P_FOB")
v_DK <- extraer(m_cf_DK, "v_FOB")
v_CP <- extraer(m_cf_CP, "v_FOB")

cat("\nControl function, Driscoll Kraay bw=4:\n")
cat(sprintf("  gamma  = %+.4f  SE = %.4f  p = %.4f\n", g_DK$b, g_DK$s, g_DK$p))
cat(sprintf("  beta   = %+.4f  SE = %.4f  p = %.4f\n", b_DK$b, b_DK$s, b_DK$p))
cat(sprintf("  theta v_FOB (Hausman FOB) = %+.4f  SE = %.4f  p = %.4f\n",
            v_DK$b, v_DK$s, v_DK$p))

cat("\nControl function, cluster planta (referencia):\n")
cat(sprintf("  gamma  = %+.4f  SE = %.4f  p = %.4f\n", g_CP$b, g_CP$s, g_CP$p))
cat(sprintf("  beta   = %+.4f  SE = %.4f  p = %.4f\n", b_CP$b, b_CP$s, b_CP$p))
cat(sprintf("  theta v_FOB = %+.4f  p = %.4f\n", v_CP$b, v_CP$p))

# ==============================================================================
# 3. MCO SOBRE LA MISMA MUESTRA
# ==============================================================================
cat("\n==============================================================\n")
cat("3. beta_FOB por MCO en la misma muestra\n")
cat("==============================================================\n")

m_ols_DK <- feols(
  ln_P_complejo_real ~ ln_h_complejo + ln_P_FOB + ln_h_jurel +
                       SEASON_SIN + SEASON_COS + TENDENCIA | NUI,
  data = df, vcov = DK(4) ~ period
)

b_ols_DK <- extraer(m_ols_DK, "ln_P_FOB")
g_ols_DK <- extraer(m_ols_DK, "ln_h_complejo")

cat(sprintf("MCO (DK): gamma = %+.4f (SE %.4f)   beta_FOB = %+.4f (SE %.4f)\n",
            g_ols_DK$b, g_ols_DK$s, b_ols_DK$b, b_ols_DK$s))

cat("\nComparativa beta_FOB:\n")
cat(sprintf("  MCO (DK)               : %+.4f (SE %.4f, p = %.4f)\n",
            b_ols_DK$b, b_ols_DK$s, b_ols_DK$p))
cat(sprintf("  Control function (DK)  : %+.4f (SE %.4f, p = %.4f)\n",
            b_DK$b, b_DK$s, b_DK$p))
cat(sprintf("  Brecha MCO - CF        : %+.4f puntos\n",
            b_ols_DK$b - b_DK$b))

# ==============================================================================
# 4. RESUMEN
# ==============================================================================
resumen <- tibble::tribble(
  ~item, ~valor,
  "N muestra",                    nrow(df),
  "G plantas",                    length(unique(df$NUI)),
  "T meses",                      length(unique(df$period)),
  "1a etapa FOB: coef Peru",      round(as.numeric(coef(fs_fob)["ln_P_FOB_PERU"]), 4),
  "1a etapa FOB: R2 within",      round(r2_within, 4),
  "CF DK: gamma",                 round(g_DK$b, 4),
  "CF DK: SE gamma",              round(g_DK$s, 4),
  "CF DK: p gamma",               round(g_DK$p, 4),
  "CF DK: beta_FOB",              round(b_DK$b, 4),
  "CF DK: SE beta_FOB",           round(b_DK$s, 4),
  "CF DK: p beta_FOB",            round(b_DK$p, 4),
  "CF DK: theta v_FOB",           round(v_DK$b, 4),
  "CF DK: p Hausman FOB",         round(v_DK$p, 4),
  "CF CP: gamma",                 round(g_CP$b, 4),
  "CF CP: SE gamma",              round(g_CP$s, 4),
  "CF CP: theta v_FOB",           round(v_CP$b, 4),
  "CF CP: p Hausman FOB",         round(v_CP$p, 4),
  "MCO DK: gamma",                round(g_ols_DK$b, 4),
  "MCO DK: beta_FOB",             round(b_ols_DK$b, 4),
  "MCO DK: SE beta_FOB",          round(b_ols_DK$s, 4),
  "MCO DK: p beta_FOB",           round(b_ols_DK$p, 4),
  "Brecha beta MCO menos CF",     round(b_ols_DK$b - b_DK$b, 4)
)

dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(resumen,
          here::here("outputs", "reportes_intermedios",
                     "C8_v2_dos_instrumentos.csv"))
print(resumen, n = Inf, width = Inf)
cat("\nGuardado: outputs/reportes_intermedios/C8_v2_dos_instrumentos.csv\n")
