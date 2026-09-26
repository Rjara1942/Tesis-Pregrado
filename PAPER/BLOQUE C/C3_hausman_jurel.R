# ==============================================================================
# BLOQUE C-3 — Hausman del jurel usando ln_TAC_jurel como instrumento
# ==============================================================================
library(tidyverse)
library(fixest)
library(sandwich)
library(lmtest)

# ------------------------------------------------------------------------------
# 0. MUESTRA
# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup() |>
  filter(!(NUI == "90073" & ANIO == 2013))

if (!"ln_TAC_jurel" %in% names(df)) {
  stop("Falta la columna ln_TAC_jurel en panel_upgrade.csv. ",
       "Construirla en 01_panel_base.R antes de correr este script.")
}

cat("Muestra: N =", nrow(df), ", G =", length(unique(df$NUI)),
    ", T =", length(unique(df$period)), "\n\n")

# ==============================================================================
# 1. PRIMERA ETAPA DEL JUREL — CON Y SIN TENDENCIA
# ==============================================================================
cat("==============================================================\n")
cat("1. Primera etapa: ln_h_jurel ~ ln_TAC_jurel (+ controles)\n")
cat("==============================================================\n")

fs_con <- feols(
  ln_h_jurel ~ ln_TAC_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
               ln_P_FOB | NUI,
  data = df, vcov = DK(4) ~ period
)
fs_sin <- feols(
  ln_h_jurel ~ ln_TAC_jurel + SEASON_SIN + SEASON_COS + ln_P_FOB | NUI,
  data = df, vcov = DK(4) ~ period
)

b_con <- as.numeric(coef(fs_con)["ln_TAC_jurel"])
s_con <- as.numeric(se(fs_con)["ln_TAC_jurel"])
t_con <- b_con / s_con
F_con <- t_con^2  # F de un instrumento = t^2

b_sin <- as.numeric(coef(fs_sin)["ln_TAC_jurel"])
s_sin <- as.numeric(se(fs_sin)["ln_TAC_jurel"])
t_sin <- b_sin / s_sin
F_sin <- t_sin^2

cat("\nCon TENDENCIA (DK bw=4):\n")
cat(sprintf("  coef ln_TAC_jurel = %+.4f  SE = %.4f  t = %.3f  F = %.2f\n",
            b_con, s_con, t_con, F_con))
cat("\nSin TENDENCIA (DK bw=4):\n")
cat(sprintf("  coef ln_TAC_jurel = %+.4f  SE = %.4f  t = %.3f  F = %.2f\n",
            b_sin, s_sin, t_sin, F_sin))

# ==============================================================================
# 2. CONTROL FUNCTION — SI EL F LO PERMITE
# ==============================================================================

cat("\n==============================================================\n")
cat("2. Control function (Hausman jurel) — con y sin tendencia\n")
cat("==============================================================\n")

df$v_jurel_con <- residuals(fs_con)
df$v_jurel_sin <- residuals(fs_sin)

# Ecuacion principal aumentada con v_jurel
m_con <- feols(
  ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                       ln_P_FOB + v_jurel_con | NUI |
    ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo,
  data = df, vcov = DK(4) ~ period
)

m_sin <- feols(
  ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS +
                       ln_P_FOB + v_jurel_sin | NUI |
    ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo,
  data = df, vcov = DK(4) ~ period
)

thV_con <- as.numeric(coef(m_con)["v_jurel_con"])
thS_con <- as.numeric(se(m_con)["v_jurel_con"])
thP_con <- as.numeric(pvalue(m_con)["v_jurel_con"])

thV_sin <- as.numeric(coef(m_sin)["v_jurel_sin"])
thS_sin <- as.numeric(se(m_sin)["v_jurel_sin"])
thP_sin <- as.numeric(pvalue(m_sin)["v_jurel_sin"])

cat("\nHausman jurel, con TENDENCIA:\n")
cat(sprintf("  theta (v_jurel) = %+.4f  SE = %.4f  p = %.4f\n",
            thV_con, thS_con, thP_con))
cat("\nHausman jurel, sin TENDENCIA:\n")
cat(sprintf("  theta (v_jurel) = %+.4f  SE = %.4f  p = %.4f\n",
            thV_sin, thS_sin, thP_sin))

# ==============================================================================
# 3. LECTURA
# ==============================================================================
cat("\n---- Lectura para el memo ----\n")
if (F_con < 10) {
  cat(sprintf("- Con tendencia F = %.2f (debil). El Hausman no tiene potencia:\n",
              F_con))
  cat("  reportar 'no tenemos potencia para testear la exogeneidad del jurel',\n")
  cat("  NO 'el jurel es exogeno'.\n")
} else {
  cat(sprintf("- Con tendencia F = %.2f (adecuado). Hausman informativo.\n", F_con))
}

if (F_sin >= 10) {
  cat(sprintf("- Sin tendencia F = %.2f (fuerte). Aqui si se puede testear.\n",
              F_sin))
  if (thP_sin > 0.10) {
    cat(sprintf("  Hausman no rechaza exogeneidad (p = %.4f).\n", thP_sin))
  } else {
    cat(sprintf("  Hausman rechaza exogeneidad (p = %.4f).\n", thP_sin))
  }
}

# ==============================================================================
# 4. RESUMEN
# ==============================================================================
resumen <- tibble::tribble(
  ~item, ~valor,
  "N muestra",                           nrow(df),
  "1a etapa CON tend: coef TAC_jurel",   round(b_con, 4),
  "1a etapa CON tend: SE",               round(s_con, 4),
  "1a etapa CON tend: F",                round(F_con, 3),
  "1a etapa SIN tend: coef TAC_jurel",   round(b_sin, 4),
  "1a etapa SIN tend: SE",               round(s_sin, 4),
  "1a etapa SIN tend: F",                round(F_sin, 3),
  "Hausman CON tend: theta v_jurel",     round(thV_con, 4),
  "Hausman CON tend: p",                 round(thP_con, 4),
  "Hausman SIN tend: theta v_jurel",     round(thV_sin, 4),
  "Hausman SIN tend: p",                 round(thP_sin, 4)
)

dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(resumen,
          here::here("outputs", "reportes_intermedios",
                     "C3_hausman_jurel.csv"))
print(resumen, n = Inf, width = Inf)
cat("\nGuardado: outputs/reportes_intermedios/C3_hausman_jurel.csv\n")
