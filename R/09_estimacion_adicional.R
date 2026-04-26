# =============================================================================
# MEJORAS SUGERIDAS AL MODELO IV
# =============================================================================

library(tidyverse)
library(fixest)

# Cargar datos
panel <- read_csv("panel_con_alternativas.csv")

# -----------------------------------------------------------------------------
# MEJORA 1: Crear anomalías de salinidad (evita colinealidad)
# -----------------------------------------------------------------------------
panel <- panel %>%
  group_by(MES) %>%
  mutate(SO_ANOMALY = SO_PUERTO - mean(SO_PUERTO, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(NUI, ANIO, MES) %>%
  group_by(NUI) %>%
  mutate(SO_ANOMALY_L1 = lag(SO_ANOMALY)) %>%
  ungroup()

# -----------------------------------------------------------------------------
# MEJORA 2: Dummy de cambio estructural post-COVID
# -----------------------------------------------------------------------------
panel <- panel %>%
  mutate(
    POST_2020 = ifelse(ANIO >= 2020, 1, 0),
    ln_h_X_POST = ln_h_complejo * POST_2020
  )

# -----------------------------------------------------------------------------
# MEJORA 3: Interacciones para capturar variación intra-anual
# -----------------------------------------------------------------------------
panel <- panel %>%
  mutate(
    BIOMASA_X_SEASON = ln_biomasa_sardina * SEASON_SIN,
    SO_PUERTO2 = SO_PUERTO^2
  )

# -----------------------------------------------------------------------------
# MODELO A: Muestra completa con cambio estructural
# -----------------------------------------------------------------------------
iv_estructural <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + 
    TENDENCIA + POST_2020 |
    NUI |
    ln_h_complejo + ln_h_X_POST ~ SO_PUERTO + SST_PUERTO_L1 + 
    ln_biomasa_sardina + SO_PUERTO:POST_2020,
  data = panel,
  vcov = ~NUI
)

# -----------------------------------------------------------------------------
# MODELO B: Solo pre-2020 (más robusto)
# -----------------------------------------------------------------------------
iv_pre2020 <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina,
  data = panel %>% filter(ANIO < 2020),
  vcov = ~NUI
)

# -----------------------------------------------------------------------------
# MODELO C: Con anomalías de salinidad
# -----------------------------------------------------------------------------
iv_anomalias <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_ANOMALY + SO_ANOMALY_L1 + SST_PUERTO_L1 + ln_biomasa_sardina,
  data = panel,
  vcov = ~NUI
)

# -----------------------------------------------------------------------------
# COMPARAR MODELOS
# -----------------------------------------------------------------------------
etable(
  iv_pre2020, iv_anomalias, iv_estructural,
  headers = c("Pre-2020", "Anomalías", "Estructural"),
  fitstat = ~ n + ivf + sargan.p
)