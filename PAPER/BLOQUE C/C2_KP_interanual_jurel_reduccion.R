# ==============================================================================
# BLOQUE C.2 
# ==============================================================================

library(tidyverse)
library(fixest)
library(readxl)
library(sandwich)
library(lmtest)

PATH_IFOP  <- here::here("data", "2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx")
PATH_PANEL <- here::here("data", "panel_upgrade.csv")

REGIONES_CENTRO_SUR <- c(5, 6, 7, 8, 9, 10, 14, 16)

# ------------------------------------------------------------------------------
#  CARGA 
# ------------------------------------------------------------------------------
df <- read_csv(PATH_PANEL, show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ANIO_fact          = as.factor(ANIO),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  filter(!is.na(SST_PUERTO_L1)) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup()

stopifnot(nrow(df) == 418, length(unique(df$NUI)) == 15)

# ==============================================================================
# 1. KLEIBERGEN-PAAP rk Wald F
# ==============================================================================


# Estimar el modelo principal 
m_principal <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA | NUI |
                       ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                        ln_biomasa_sardina + ln_TAC_complejo,
  data = df, cluster = ~ NUI
)

# (a) Cragg-Donald bajo iid (lo que aparece en Tabla 5).
m_iid <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA | NUI |
                       ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                        ln_biomasa_sardina + ln_TAC_complejo,
  data = df, vcov = "iid"
)
cd_stat <- fitstat(m_iid, type = "cd", verbose = FALSE)$cd

# (b) Kleibergen-Paap analogo
fs_kp <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                  SO_PUERTO + SST_PUERTO_L1 +
                  ln_biomasa_sardina + ln_TAC_complejo | NUI,
  data = df, cluster = ~ NUI
)
kp_wald <- wald(fs_kp, keep = c("SO_PUERTO", "SST_PUERTO_L1",
                                "ln_biomasa_sardina", "ln_TAC_complejo"),
                print = FALSE)
kp_stat <- kp_wald$stat

cat(sprintf("\nCragg-Donald (iid):                  F = %.4f\n", cd_stat))
cat(sprintf("Kleibergen-Paap rk (cluster planta): F = %.4f\n", kp_stat))
cat(sprintf("Diferencia relativa:                 %.2f %%\n",
            100 * (kp_stat - cd_stat) / cd_stat))
cat("Umbral Stock-Yogo 10 %% para 4 instrumentos y una endogena: 10.27\n")
cat(sprintf("Evaluacion KP: %s\n",
            if (kp_stat < 10.27)
              "F < 10.27 => primera etapa DEBIL bajo errores cluster"
            else
              "F >= 10.27 => primera etapa fuerte bajo errores cluster"))

# ==============================================================================
# 2. IDENTIFICACION INTERANUAL — chequear el rol de biomasa y TAC
# ==============================================================================

# Primera etapa completa (todos los instrumentos)
fs_full <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                  SO_PUERTO + SST_PUERTO_L1 +
                  ln_biomasa_sardina + ln_TAC_complejo | NUI,
  data = df, cluster = ~ NUI
)
F_full <- wald(fs_full, keep = c("SO_PUERTO", "SST_PUERTO_L1",
                                 "ln_biomasa_sardina", "ln_TAC_complejo"))

# Primera etapa sin biomasa ni TAC (solo ambientales)
fs_amb <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                  SO_PUERTO + SST_PUERTO_L1 | NUI,
  data = df, cluster = ~ NUI
)
F_amb <- wald(fs_amb, keep = c("SO_PUERTO", "SST_PUERTO_L1"))

# Primera etapa sin ambientales (solo biomasa y TAC anuales)
fs_anu <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                  ln_biomasa_sardina + ln_TAC_complejo | NUI,
  data = df, cluster = ~ NUI
)
F_anu <- wald(fs_anu, keep = c("ln_biomasa_sardina", "ln_TAC_complejo"))

cat("F conjunto en primera etapa segun subconjunto de instrumentos:\n")
cat(sprintf("  4 instrumentos (completo)                  : %.4f\n", F_full$stat))
cat(sprintf("  Solo ambientales por puerto (SO + SST_L1)  : %.4f\n", F_amb$stat))
cat(sprintf("  Solo anuales (biomasa + TAC)               : %.4f\n", F_anu$stat))

# R2 within de cada primera etapa (proxy del poder explicativo)
r2_full <- r2(fs_full, type = "wr2")
r2_amb  <- r2(fs_amb,  type = "wr2")
r2_anu  <- r2(fs_anu,  type = "wr2")
cat(sprintf("\nR2 within de la primera etapa:\n"))
cat(sprintf("  Completa                                   : %.4f\n", r2_full))
cat(sprintf("  Solo ambientales                           : %.4f\n", r2_amb))
cat(sprintf("  Solo anuales                               : %.4f\n", r2_anu))
cat(sprintf("\nAporte relativo de los instrumentos anuales al R2 within:\n"))
cat(sprintf("  (r2_anu - r2_amb) / (r2_full - r2_amb) = %.2f %%\n",
            100 * (r2_anu - r2_amb) / (r2_full - r2_amb)))



