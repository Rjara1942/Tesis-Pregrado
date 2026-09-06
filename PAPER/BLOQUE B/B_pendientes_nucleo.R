# ==============================================================================
# BLOQUE B — Pendientes 
# ==============================================================================


library(tidyverse)
library(fixest)
library(sandwich)
library(lmtest)
library(readxl)
library(AER)          # ivreg + summary(diagnostics=TRUE) para Sargan sobre el nucleo

# ivmodel para Anderson-Rubin
if (!requireNamespace("ivmodel", quietly = TRUE))
  install.packages("ivmodel")
library(ivmodel)

# ------------------------------------------------------------------------------
# CARGA DEL PANEL Y CONSTRUCCION DEL NUCLEO
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

# Nucleo: plantas con al menos 30 observaciones en la muestra estimable
df_nuc <- df |> group_by(NUI) |> filter(n() >= 30) |> ungroup()
cat(sprintf("Nucleo: %d obs, %d plantas.\n",
            nrow(df_nuc), length(unique(df_nuc$NUI))))
stopifnot(length(unique(df_nuc$NUI)) == 6)

# ==============================================================================
# IV PANEL PRINCIPAL SOBRE EL NUCLEO
# ==============================================================================


m_panel_nuc <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA | NUI |
                       ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                        ln_biomasa_sardina + ln_TAC_complejo,
  data = df_nuc, cluster = ~ NUI
)
nm_g <- if ("fit_ln_h_complejo" %in% names(coef(m_panel_nuc)))
          "fit_ln_h_complejo" else "ln_h_complejo"

g_pan <- as.numeric(coef(m_panel_nuc)[nm_g])
s_pan <- as.numeric(se(m_panel_nuc)[nm_g])
ci_pan <- as.numeric(unlist(confint(m_panel_nuc, parm = nm_g)))
p_pan  <- as.numeric(pvalue(m_panel_nuc)[nm_g])
b_fob  <- as.numeric(coef(m_panel_nuc)["ln_P_FOB"])
s_fob  <- as.numeric(se(m_panel_nuc)["ln_P_FOB"])
p_fob  <- as.numeric(pvalue(m_panel_nuc)["ln_P_FOB"])
b_jur  <- as.numeric(coef(m_panel_nuc)["ln_h_jurel"])
s_jur  <- as.numeric(se(m_panel_nuc)["ln_h_jurel"])
p_jur  <- as.numeric(pvalue(m_panel_nuc)["ln_h_jurel"])

# F Kleibergen-Paap sobre el nucleo
fs_nuc <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                  SO_PUERTO + SST_PUERTO_L1 +
                  ln_biomasa_sardina + ln_TAC_complejo | NUI,
  data = df_nuc, cluster = ~ NUI
)
F_kp_nuc <- wald(fs_nuc, keep = c("SO_PUERTO", "SST_PUERTO_L1",
                                  "ln_biomasa_sardina", "ln_TAC_complejo"),
                 print = FALSE)$stat

cat(sprintf("gamma      = %.4f   SE = %.4f   IC 95%% [%.4f, %.4f]   p = %.4f\n",
            g_pan, s_pan, ci_pan[1], ci_pan[2], p_pan))
cat(sprintf("beta FOB   = %.4f   SE = %.4f   p = %.4f\n", b_fob, s_fob, p_fob))
cat(sprintf("delta jur  = %.4f   SE = %.4f   p = %.4f\n", b_jur, s_jur, p_jur))
cat(sprintf("F 1a etapa (KP, cluster planta) = %.4f\n", F_kp_nuc))
cat(sprintf("N = %d,  clusters = %d\n", nobs(m_panel_nuc),
            length(unique(df_nuc$NUI))))


cat("\nPrimera etapa individual sobre el nucleo (coef, t, p):\n")
for (v in c("SO_PUERTO", "SST_PUERTO_L1", "ln_biomasa_sardina", "ln_TAC_complejo")) {
  b_v <- as.numeric(coef(fs_nuc)[v])
  s_v <- as.numeric(se(fs_nuc)[v])
  p_v <- as.numeric(pvalue(fs_nuc)[v])
  cat(sprintf("  %-22s coef = %+.4f   t = %+.2f   p = %.4f\n",
              v, b_v, b_v / s_v, p_v))
}

# ---  Sargan J sobre el nucleo (via AER::ivreg) ---
m_ivreg_nuc <- ivreg(
  ln_P_complejo_real ~ ln_h_complejo + ln_P_FOB + ln_h_jurel +
                       SEASON_SIN + SEASON_COS + TENDENCIA + factor(NUI) |
                       SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                       ln_TAC_complejo + ln_P_FOB + ln_h_jurel +
                       SEASON_SIN + SEASON_COS + TENDENCIA + factor(NUI),
  data = df_nuc
)
diag_nuc <- summary(m_ivreg_nuc, diagnostics = TRUE)$diagnostics
sargan_nuc <- diag_nuc["Sargan", ]
cat(sprintf("\nSargan J (nucleo) = %.4f   df = %d   p = %.4f\n",
            sargan_nuc["statistic"], sargan_nuc["df1"], sargan_nuc["p-value"]))

# ==============================================================================
#  IV AGREGADO CON NEWEY-WEST SOBRE EL NUCLEO
# ==============================================================================


# Colapso mensual sobre el nucleo. Los regresores macro son invariantes dentro
# del mes.
agg_nuc <- df_nuc |>
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
    .groups = "drop"
  ) |>
  arrange(period) |>
  mutate(SST_MACRO_L1 = lag(SST_MACRO)) |>
  drop_na(SST_MACRO_L1)

cat(sprintf("Serie agregada nucleo: %d meses (%s a %s)\n",
            nrow(agg_nuc),
            format(min(agg_nuc$period), "%Y-%m"),
            format(max(agg_nuc$period), "%Y-%m")))

m_agg_nuc <- feols(
  ln_P_mean ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    ln_h_complejo ~ SST_MACRO_L1 + ln_biomasa_sardina + ln_TAC_complejo,
  data = agg_nuc, vcov = NW(4) ~ period
)

nm_a <- if ("fit_ln_h_complejo" %in% names(coef(m_agg_nuc)))
          "fit_ln_h_complejo" else "ln_h_complejo"
g_agg <- as.numeric(coef(m_agg_nuc)[nm_a])
s_agg <- as.numeric(se(m_agg_nuc)[nm_a])
ci_agg <- as.numeric(unlist(confint(m_agg_nuc, parm = nm_a)))
p_agg  <- as.numeric(pvalue(m_agg_nuc)[nm_a])

# F 1a etapa agregado sobre el nucleo
fs_agg_nuc <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                  SST_MACRO_L1 + ln_biomasa_sardina + ln_TAC_complejo,
  data = agg_nuc, vcov = NW(4) ~ period
)
F_agg_nuc <- wald(fs_agg_nuc, keep = c("SST_MACRO_L1", "ln_biomasa_sardina",
                                       "ln_TAC_complejo"),
                  print = FALSE)$stat

cat(sprintf("gamma (agregado IV nucleo) = %.4f   SE (NW) = %.4f\n", g_agg, s_agg))
cat(sprintf("IC 95%% NW: [%.4f, %.4f]   p = %.4f\n", ci_agg[1], ci_agg[2], p_agg))
cat(sprintf("F 1a etapa (Wald NW, 3 instr) = %.4f\n", F_agg_nuc))
cat("ADVERTENCIA: sobre 6 plantas la 1a etapa agregada suele ser aun mas debil\n")
cat("             que sobre 16. Si el F cae por debajo de 3-4, el punto estimado\n")
cat("             no dice mucho por si solo. Reportar AR al lado.\n")

# ==============================================================================
#  ANDERSON-RUBIN SOBRE EL AGREGADO DEL NUCLEO
# ==============================================================================


Y     <- agg_nuc$ln_P_mean
D     <- agg_nuc$ln_h_complejo
Z_mat <- as.matrix(agg_nuc[, c("SST_MACRO_L1",
                               "ln_biomasa_sardina", "ln_TAC_complejo")])
X_mat <- as.matrix(agg_nuc[, c("ln_P_FOB", "ln_h_jurel",
                               "SEASON_SIN", "SEASON_COS", "TENDENCIA")])

iv_obj <- ivmodel(Y = Y, D = D, Z = Z_mat, X = X_mat, heteroSE = TRUE)
ar_res <- AR.test(iv_obj, alpha = 0.05)

cat(sprintf("AR test H0: gamma = 0\n"))
cat(sprintf("  Fstat = %.4f   df1 = %d   df2 = %d   p = %.4f\n",
            ar_res$Fstat, ar_res$df[1], ar_res$df[2], ar_res$p.value))
cat("Intervalo AR 95%%:\n")
print(ar_res$ci)

# ==============================================================================
# WILD CLUSTER BOOTSTRAP SOBRE EL PANEL DEL NUCLEO (6 clusters)
# ==============================================================================


# ---  Modelo bajo H0: gamma = 0 ---

m_rest <- feols(
  ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                       TENDENCIA | NUI,
  data = df_nuc, cluster = ~ NUI
)
u_hat  <- residuals(m_rest)
y_hat0 <- fitted(m_rest)

# Estadistico t observado del modelo IV sin restriccion
t_obs <- as.numeric(coef(m_panel_nuc)[nm_g] / se(m_panel_nuc)[nm_g])

# --- Bootstrap loop ---
webb_w <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
clusters   <- unique(df_nuc$NUI)
G          <- length(clusters)
B          <- 4999   

set.seed(1234)
boot_t <- rep(NA_real_, B)

df_boot <- df_nuc
for (b in seq_len(B)) {
  w_g <- sample(webb_w, G, replace = TRUE)
  names(w_g) <- clusters
  df_boot$y_boot <- y_hat0 + w_g[df_boot$NUI] * u_hat

  m_b <- tryCatch(
    feols(
      y_boot ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA | NUI |
               ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                ln_biomasa_sardina + ln_TAC_complejo,
      data = df_boot, cluster = ~ NUI, notes = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(m_b)) next
  nm_b <- if ("fit_ln_h_complejo" %in% names(coef(m_b))) "fit_ln_h_complejo" else "ln_h_complejo"
  boot_t[b] <- as.numeric(coef(m_b)[nm_b] / se(m_b)[nm_b])
}

boot_t_ok <- boot_t[!is.na(boot_t)]
p_wcb <- mean(abs(boot_t_ok) >= abs(t_obs))

# IC 95 % simetrico por inversion (percentil de |t| bootstrap)
q975 <- quantile(abs(boot_t_ok), 0.95, na.rm = TRUE)
ic_lo <- as.numeric(coef(m_panel_nuc)[nm_g] - q975 * se(m_panel_nuc)[nm_g])
ic_hi <- as.numeric(coef(m_panel_nuc)[nm_g] + q975 * se(m_panel_nuc)[nm_g])

cat(sprintf("Wild cluster bootstrap (Webb, B = %d, %d validos):\n",
            B, length(boot_t_ok)))
cat(sprintf("  t observado           = %.4f\n", t_obs))
cat(sprintf("  p-valor bootstrap     = %.4f\n", p_wcb))
cat(sprintf("  IC 95%% (t-inversion) = [%.4f, %.4f]\n", ic_lo, ic_hi))

# Guardar para el resumen final
boot_res <- list(
  t_stat    = t_obs,
  p_val     = p_wcb,
  conf_int  = c(ic_lo, ic_hi),
  B_valid   = length(boot_t_ok)
)

# ==============================================================================
# RESUMEN 
# ==============================================================================


# Reconstruir el IC AR 
ar_ci_mat <- ar_res$ci
if (nrow(ar_ci_mat) == 1) {
  ar_ci_txt <- sprintf("[%.4f, %.4f]", ar_ci_mat[1, 1], ar_ci_mat[1, 2])
} else {
  ar_ci_txt <- paste(
    sprintf("[%.4f, %.4f]", ar_ci_mat[, 1], ar_ci_mat[, 2]),
    collapse = " U "
  )
}
cat(sprintf("\nIntervalo AR 95%% (agregado nucleo): %s\n", ar_ci_txt))

comp <- tribble(
  ~Item,                                        ~valor,
  # Panel IV nucleo
  "Panel IV nucleo: gamma",                     as.character(round(g_pan, 4)),
  "Panel IV nucleo: SE (cluster planta)",       as.character(round(s_pan, 4)),
  "Panel IV nucleo: p",                         as.character(round(p_pan, 4)),
  "Panel IV nucleo: beta FOB",                  as.character(round(b_fob, 4)),
  "Panel IV nucleo: SE beta FOB",               as.character(round(s_fob, 4)),
  "Panel IV nucleo: p beta FOB",                as.character(round(p_fob, 4)),
  "Panel IV nucleo: delta jurel",               as.character(round(b_jur, 4)),
  "Panel IV nucleo: SE delta jurel",            as.character(round(s_jur, 4)),
  "Panel IV nucleo: p delta jurel",             as.character(round(p_jur, 4)),
  "Panel IV nucleo: F KP",                      as.character(round(F_kp_nuc, 2)),
  "Panel IV nucleo: Sargan J",                  as.character(round(as.numeric(sargan_nuc["statistic"]), 4)),
  "Panel IV nucleo: Sargan p",                  as.character(round(as.numeric(sargan_nuc["p-value"]), 4)),
  "Panel IV nucleo: p wild cluster bootstrap",  as.character(round(boot_res$p_val, 4)),
  "Panel IV nucleo: IC 95% bootstrap inf",      as.character(round(boot_res$conf_int[1], 4)),
  "Panel IV nucleo: IC 95% bootstrap sup",      as.character(round(boot_res$conf_int[2], 4)),
  "Panel IV nucleo: B validos bootstrap",       as.character(boot_res$B_valid),
  # Agregado IV nucleo
  "Agregado IV nucleo: gamma",                  as.character(round(g_agg, 4)),
  "Agregado IV nucleo: SE (NW)",                as.character(round(s_agg, 4)),
  "Agregado IV nucleo: p (NW)",                 as.character(round(p_agg, 4)),
  "Agregado IV nucleo: IC 95% NW inf",          as.character(round(ci_agg[1], 4)),
  "Agregado IV nucleo: IC 95% NW sup",          as.character(round(ci_agg[2], 4)),
  "Agregado IV nucleo: F 1a etapa (NW)",        as.character(round(F_agg_nuc, 4)),
  "Agregado IV nucleo: AR Fstat",               as.character(round(ar_res$Fstat, 4)),
  "Agregado IV nucleo: AR p",                   as.character(round(ar_res$p.value, 4)),
  "Agregado IV nucleo: AR IC 95%",              ar_ci_txt
)

dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(comp,
          here::here("outputs", "reportes_intermedios",
                     "B_pendientes_nucleo.csv"))
cat("\nGuardado: outputs/reportes_intermedios/B_pendientes_nucleo.csv\n")
print(comp, n = Inf, width = Inf)


