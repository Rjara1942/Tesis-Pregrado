# ==============================================================================
# BLOQUE B : AGREGADO IV MENSUAL SOBRE MUESTRA CORREGIDA
# ==============================================================================
# Muestra: 15 plantas, 410 obs planta.mes. Se excluyen las 8 filas de NUI 90073




library(tidyverse)
library(fixest)
library(sandwich)
library(AER)

if (!requireNamespace("ivmodel", quietly = TRUE)) install.packages("ivmodel")
library(ivmodel)

PATH_PANEL <- here::here("data", "panel_upgrade.csv")


df <- read_csv(PATH_PANEL, show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  filter(!is.na(SST_PUERTO_L1)) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup()

stopifnot(nrow(df) == 418, length(unique(df$NUI)) == 15)

# Muestra corregida: quitar NUI 90073 en 2013 (8 filas)
df_D <- df |> filter(!(NUI == "90073" & ANIO == 2013))
cat(sprintf("Muestra corregida (D): %d obs, %d plantas\n",
            nrow(df_D), length(unique(df_D$NUI))))
stopifnot(nrow(df_D) == 410, length(unique(df_D$NUI)) == 15)

# ------------------------------------------------------------------------------
# 1. PANEL IV SOBRE MUESTRA CORREGIDA (referencia)
# ------------------------------------------------------------------------------


m_pan_D <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA | NUI |
                       ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                        ln_biomasa_sardina + ln_TAC_complejo,
  data = df_D, cluster = ~ NUI
)
nm_p <- if ("fit_ln_h_complejo" %in% names(coef(m_pan_D))) "fit_ln_h_complejo" else "ln_h_complejo"
g_pan  <- as.numeric(coef(m_pan_D)[nm_p])
s_pan  <- as.numeric(se(m_pan_D)[nm_p])
ci_pan <- as.numeric(unlist(confint(m_pan_D, parm = nm_p)))
p_pan  <- as.numeric(pvalue(m_pan_D)[nm_p])

# F KP: Wald conjunto de instrumentos en primera etapa
fs_D <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                  SO_PUERTO + SST_PUERTO_L1 +
                  ln_biomasa_sardina + ln_TAC_complejo | NUI,
  data = df_D, cluster = ~ NUI
)
F_kp_pan <- wald(fs_D, keep = c("SO_PUERTO", "SST_PUERTO_L1",
                                "ln_biomasa_sardina", "ln_TAC_complejo"),
                 print = FALSE)$stat

cat(sprintf("gamma = %.4f   SE = %.4f   IC 95%% [%.4f, %.4f]   p = %.4f   F KP = %.2f\n",
            g_pan, s_pan, ci_pan[1], ci_pan[2], p_pan, F_kp_pan))

# ------------------------------------------------------------------------------
# 2. AGREGADO IV MENSUAL CON NEWEY-WEST SOBRE MUESTRA CORREGIDA
# ------------------------------------------------------------------------------


# Colapso mensual. Los regresores macrozonales son invariantes dentro del mes;
# se toman con first(). Precios agregados con media simple y ponderada.
agg_D <- df_D |>
  group_by(period) |>
  summarise(
    ln_P_mean          = mean(ln_P_complejo_real),
    ln_P_wmean         = weighted.mean(ln_P_complejo_real, w = h_planta),
    ln_h_complejo      = first(ln_h_complejo),
    ln_P_FOB           = first(ln_P_FOB),
    ln_h_jurel         = first(ln_h_jurel),
    SEASON_SIN         = first(SEASON_SIN),
    SEASON_COS         = first(SEASON_COS),
    TENDENCIA          = first(TENDENCIA),
    SST_MACRO          = first(SST_MACRO),
    ln_biomasa_sardina = first(ln_biomasa_sardina),
    ln_TAC_complejo    = first(ln_TAC_complejo),
    N_plantas          = n_distinct(NUI),
    .groups = "drop"
  ) |>
  arrange(period) |>
  mutate(SST_MACRO_L1 = lag(SST_MACRO)) |>
  drop_na(SST_MACRO_L1)

cat(sprintf("Serie agregada corregida: %d meses (%s a %s)\n",
            nrow(agg_D),
            format(min(agg_D$period), "%Y-%m"),
            format(max(agg_D$period), "%Y-%m")))

# IV con NW bw=4, instrumentos macrozonales
m_agg_mean <- feols(
  ln_P_mean ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    ln_h_complejo ~ SST_MACRO_L1 + ln_biomasa_sardina + ln_TAC_complejo,
  data = agg_D, vcov = NW(4) ~ period
)
m_agg_wmean <- feols(
  ln_P_wmean ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    ln_h_complejo ~ SST_MACRO_L1 + ln_biomasa_sardina + ln_TAC_complejo,
  data = agg_D, vcov = NW(4) ~ period
)

get_stats <- function(m) {
  nm_a <- if ("fit_ln_h_complejo" %in% names(coef(m))) "fit_ln_h_complejo" else "ln_h_complejo"
  list(
    g  = as.numeric(coef(m)[nm_a]),
    s  = as.numeric(se(m)[nm_a]),
    ci = as.numeric(unlist(confint(m, parm = nm_a))),
    p  = as.numeric(pvalue(m)[nm_a])
  )
}
r_mean  <- get_stats(m_agg_mean)
r_wmean <- get_stats(m_agg_wmean)

# F 1a etapa agregado
fs_agg_D <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                  SST_MACRO_L1 + ln_biomasa_sardina + ln_TAC_complejo,
  data = agg_D, vcov = NW(4) ~ period
)
F_agg_D <- wald(fs_agg_D, keep = c("SST_MACRO_L1", "ln_biomasa_sardina",
                                   "ln_TAC_complejo"),
                print = FALSE)$stat

cat(sprintf("\nMedia simple:    gamma = %.4f   SE = %.4f   IC 95%% [%.4f, %.4f]   p = %.4f\n",
            r_mean$g, r_mean$s, r_mean$ci[1], r_mean$ci[2], r_mean$p))
cat(sprintf("Media ponderada: gamma = %.4f   SE = %.4f   IC 95%% [%.4f, %.4f]   p = %.4f\n",
            r_wmean$g, r_wmean$s, r_wmean$ci[1], r_wmean$ci[2], r_wmean$p))
cat(sprintf("F 1a etapa agregada (Wald NW, 3 instr): %.4f\n", F_agg_D))

# ------------------------------------------------------------------------------
# 3. ANDERSON-RUBIN SOBRE AGREGADO CORREGIDO
# ------------------------------------------------------------------------------


Y     <- agg_D$ln_P_mean
D     <- agg_D$ln_h_complejo
Z_mat <- as.matrix(agg_D[, c("SST_MACRO_L1", "ln_biomasa_sardina", "ln_TAC_complejo")])
X_mat <- as.matrix(agg_D[, c("ln_P_FOB", "ln_h_jurel",
                             "SEASON_SIN", "SEASON_COS", "TENDENCIA")])

iv_obj <- ivmodel(Y = Y, D = D, Z = Z_mat, X = X_mat, heteroSE = TRUE)
ar_res <- AR.test(iv_obj, alpha = 0.05)

cat(sprintf("AR test H0: gamma = 0\n"))
cat(sprintf("  Fstat = %.4f   df1 = %d   df2 = %d   p = %.4f\n",
            ar_res$Fstat, ar_res$df[1], ar_res$df[2], ar_res$p.value))

ar_ci_mat <- ar_res$ci
if (nrow(ar_ci_mat) == 1) {
  ar_ci_txt <- sprintf("[%.4f, %.4f]", ar_ci_mat[1, 1], ar_ci_mat[1, 2])
} else {
  ar_ci_txt <- paste(
    sprintf("[%.4f, %.4f]", ar_ci_mat[, 1], ar_ci_mat[, 2]),
    collapse = " U "
  )
}
cat(sprintf("Intervalo AR 95%%: %s\n", ar_ci_txt))

# Repetir AR para media ponderada como cross-check
Y_w <- agg_D$ln_P_wmean
iv_obj_w <- ivmodel(Y = Y_w, D = D, Z = Z_mat, X = X_mat, heteroSE = TRUE)
ar_res_w <- AR.test(iv_obj_w, alpha = 0.05)
ar_ci_w_mat <- ar_res_w$ci
if (nrow(ar_ci_w_mat) == 1) {
  ar_ci_w_txt <- sprintf("[%.4f, %.4f]", ar_ci_w_mat[1, 1], ar_ci_w_mat[1, 2])
} else {
  ar_ci_w_txt <- paste(
    sprintf("[%.4f, %.4f]", ar_ci_w_mat[, 1], ar_ci_w_mat[, 2]),
    collapse = " U "
  )
}
cat(sprintf("\nMedia ponderada AR: Fstat = %.4f   p = %.4f   IC = %s\n",
            ar_res_w$Fstat, ar_res_w$p.value, ar_ci_w_txt))

# ------------------------------------------------------------------------------
# 4. TABLA COMPARATIVA FINAL (cierra la tabla cruzada de convergencia)
# ------------------------------------------------------------------------------


comp <- tribble(
  ~Item,                                                       ~valor,
  # Panel corregido
  "Panel IV corregido: gamma",                                 as.character(round(g_pan, 4)),
  "Panel IV corregido: SE (cluster planta)",                   as.character(round(s_pan, 4)),
  "Panel IV corregido: IC 95% inf",                            as.character(round(ci_pan[1], 4)),
  "Panel IV corregido: IC 95% sup",                            as.character(round(ci_pan[2], 4)),
  "Panel IV corregido: p",                                     as.character(round(p_pan, 4)),
  "Panel IV corregido: F KP",                                  as.character(round(F_kp_pan, 2)),
  "Panel IV corregido: N",                                     as.character(nobs(m_pan_D)),
  "Panel IV corregido: G",                                     as.character(length(unique(df_D$NUI))),
  # Agregado corregido media simple
  "Agregado IV corregido (media simple): gamma",               as.character(round(r_mean$g, 4)),
  "Agregado IV corregido (media simple): SE (NW)",             as.character(round(r_mean$s, 4)),
  "Agregado IV corregido (media simple): IC 95% NW inf",       as.character(round(r_mean$ci[1], 4)),
  "Agregado IV corregido (media simple): IC 95% NW sup",       as.character(round(r_mean$ci[2], 4)),
  "Agregado IV corregido (media simple): p",                   as.character(round(r_mean$p, 4)),
  # Agregado corregido media ponderada
  "Agregado IV corregido (media ponderada): gamma",            as.character(round(r_wmean$g, 4)),
  "Agregado IV corregido (media ponderada): SE (NW)",          as.character(round(r_wmean$s, 4)),
  "Agregado IV corregido (media ponderada): IC 95% NW inf",    as.character(round(r_wmean$ci[1], 4)),
  "Agregado IV corregido (media ponderada): IC 95% NW sup",    as.character(round(r_wmean$ci[2], 4)),
  "Agregado IV corregido (media ponderada): p",                as.character(round(r_wmean$p, 4)),
  # Diagnosticos agregado
  "Agregado IV corregido: F 1a etapa (NW)",                    as.character(round(F_agg_D, 4)),
  "Agregado IV corregido: AR Fstat (media simple)",            as.character(round(ar_res$Fstat, 4)),
  "Agregado IV corregido: AR p (media simple)",                as.character(round(ar_res$p.value, 4)),
  "Agregado IV corregido: AR IC 95% (media simple)",           ar_ci_txt,
  "Agregado IV corregido: AR IC 95% (media ponderada)",        ar_ci_w_txt,
  # Distancia final panel <-> agregado
  "Distancia panel - agregado (media simple)",                 as.character(round(abs(g_pan - r_mean$g), 4)),
  "Distancia panel - agregado (media ponderada)",              as.character(round(abs(g_pan - r_wmean$g), 4))
)

print(comp, n = Inf, width = Inf)

dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(comp,
          here::here("outputs", "reportes_intermedios",
                     "B2_agregado_muestra_corregida.csv"))
cat("\nGuardado: outputs/reportes_intermedios/B2_agregado_muestra_corregida.csv\n")


