# ==============================================================================
# BLOQUE C.2: ROBUSTEZ CON JUREL DESTINADO A REDUCCION
# ==============================================================================

library(tidyverse)
library(fixest)
library(readxl)

PATH_IFOP  <- here::here("data", "2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx")
PATH_PANEL <- here::here("data", "panel_upgrade.csv")

REGIONES_CENTRO_SUR <- c(5, 6, 7, 8, 9, 10, 14, 16)

# ------------------------------------------------------------------------------
# 1. CARGA DEL PANEL DE ESTIMACION
# ------------------------------------------------------------------------------
df <- read_csv(PATH_PANEL, show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  filter(!is.na(SST_PUERTO_L1)) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup()

stopifnot(nrow(df) == 418, length(unique(df$NUI)) == 15)

# ------------------------------------------------------------------------------
# 2. CONSTRUCCION DE ln_h_jurel_reduccion MACROZONAL MENSUAL
# ------------------------------------------------------------------------------
proc <- read_excel(PATH_IFOP, sheet = "PROCESO") |>
  mutate(NM_RECURSO = str_trim(NM_RECURSO),
         NM_LINEA   = str_trim(NM_LINEA))

jur_reduc <- proc |>
  filter(NM_RECURSO == "JUREL",
         RG %in% REGIONES_CENTRO_SUR,
         NM_LINEA == "HARINA") |>
  group_by(ANIO, MES) |>
  summarise(h_jurel_reduc = sum(MP_TOTAL, na.rm = TRUE), .groups = "drop") |>
  mutate(ln_h_jurel_reduc = log(h_jurel_reduc + 1))

cat("Serie mensual de jurel a reduccion (HARINA, macrozona centro-sur):\n")
cat(sprintf("  Meses con datos: %d\n", nrow(jur_reduc)))
cat(sprintf("  h_jurel_reduc: min = %.0f, mediana = %.0f, max = %.0f\n",
            min(jur_reduc$h_jurel_reduc),
            median(jur_reduc$h_jurel_reduc),
            max(jur_reduc$h_jurel_reduc)))

# Ratio jurel_reduc / jurel_total por ano
jur_total_yr <- df |>
  distinct(ANIO, MES, .keep_all = TRUE) |>
  group_by(ANIO) |>
  summarise(h_jurel_total = sum(exp(ln_h_jurel) - 1, na.rm = TRUE))
jur_reduc_yr <- jur_reduc |>
  group_by(ANIO) |>
  summarise(h_jurel_reduc = sum(h_jurel_reduc, na.rm = TRUE))
ratio_yr <- jur_total_yr |>
  inner_join(jur_reduc_yr, by = "ANIO") |>
  mutate(ratio_pct = h_jurel_reduc / h_jurel_total * 100)
cat("\nRatio reduccion / total por ano (esperado 20-54 %):\n")
print(ratio_yr |> mutate(across(where(is.numeric), \(x) round(x, 1))))

# Unir al panel
df_alt <- df |>
  left_join(jur_reduc |> select(ANIO, MES, ln_h_jurel_reduc),
            by = c("ANIO", "MES"))
n_missing <- sum(is.na(df_alt$ln_h_jurel_reduc))
if (n_missing > 0)
  cat(sprintf("\nAtencion: %d obs del panel sin match en jurel_reduc. Se dropean.\n",
              n_missing))
df_alt <- df_alt |> drop_na(ln_h_jurel_reduc)

# ------------------------------------------------------------------------------
# 3. ESTIMACION DEL MODELO PRINCIPAL CON JUREL TOTAL Y CON JUREL REDUCCION
# ------------------------------------------------------------------------------
m_total <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA | NUI |
                       ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                        ln_biomasa_sardina + ln_TAC_complejo,
  data = df, cluster = ~ NUI
)

m_reduc <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel_reduc + SEASON_SIN + SEASON_COS +
                       TENDENCIA | NUI |
                       ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                        ln_biomasa_sardina + ln_TAC_complejo,
  data = df_alt, cluster = ~ NUI
)

# ------------------------------------------------------------------------------
# 4. F DE PRIMERA ETAPA EN AMBAS VERSIONES
# ------------------------------------------------------------------------------
fs_total <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                  SO_PUERTO + SST_PUERTO_L1 +
                  ln_biomasa_sardina + ln_TAC_complejo | NUI,
  data = df, cluster = ~ NUI
)
fs_reduc <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel_reduc + SEASON_SIN + SEASON_COS + TENDENCIA +
                  SO_PUERTO + SST_PUERTO_L1 +
                  ln_biomasa_sardina + ln_TAC_complejo | NUI,
  data = df_alt, cluster = ~ NUI
)
F_total <- wald(fs_total, keep = c("SO_PUERTO", "SST_PUERTO_L1",
                                   "ln_biomasa_sardina", "ln_TAC_complejo"),
                print = FALSE)$stat
F_reduc <- wald(fs_reduc, keep = c("SO_PUERTO", "SST_PUERTO_L1",
                                   "ln_biomasa_sardina", "ln_TAC_complejo"),
                print = FALSE)$stat

# ------------------------------------------------------------------------------
# 5. TABLA COMPARATIVA
# ------------------------------------------------------------------------------
resumen <- function(m, etiqueta, jurel_var, F_1a) {
  nm  <- if ("fit_ln_h_complejo" %in% names(coef(m))) "fit_ln_h_complejo" else "ln_h_complejo"
  b_g <- as.numeric(coef(m)[nm])
  s_g <- as.numeric(se(m)[nm])
  ci  <- as.numeric(unlist(confint(m, parm = nm)))
  p_g <- as.numeric(pvalue(m)[nm])
  b_j <- as.numeric(coef(m)[jurel_var])
  s_j <- as.numeric(se(m)[jurel_var])
  p_j <- as.numeric(pvalue(m)[jurel_var])
  tibble(
    Especificacion = etiqueta,
    gamma          = round(b_g, 4),
    SE_gamma       = round(s_g, 4),
    IC_inf         = round(ci[1], 4),
    IC_sup         = round(ci[2], 4),
    p_gamma        = round(p_g, 4),
    coef_jurel     = round(b_j, 4),
    SE_jurel       = round(s_j, 4),
    p_jurel        = round(p_j, 4),
    F_1a_etapa     = round(F_1a, 2),
    N              = nobs(m)
  )
}

tabla <- bind_rows(
  resumen(m_total, "Principal: ln_h_jurel (desembarque total)", "ln_h_jurel",       F_total),
  resumen(m_reduc, "Robustez: ln_h_jurel_reduc (HARINA)",       "ln_h_jurel_reduc", F_reduc)
)

cat("\n==============================================================\n")
cat("Robustez con jurel destinado a reduccion vs total\n")
cat("==============================================================\n")
print(tabla, n = Inf, width = Inf)

dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(tabla,
          here::here("outputs", "reportes_intermedios",
                     "C2b_jurel_reduccion.csv"))
cat("\nGuardado: outputs/reportes_intermedios/C2b_jurel_reduccion.csv\n")


