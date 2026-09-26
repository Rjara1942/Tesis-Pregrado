# ==============================================================================
# BLOQUE C-4 v2 — Re-corrida con Driscoll Kraay y dos instrumentos anuales
# ==============================================================================

# Cambio estructural:
#   - Principal pasa a DOS instrumentos anuales: ln_biomasa_sardina + ln_TAC_complejo.
#   - Muestra recuperada: 427 obs, 16 plantas (sin filtro NA por SST_PUERTO_L1)
#     y excluyendo 90073/2013.
#   - Los ambientales SO_PUERTO / SST_PUERTO_L1 pasan de identificacion a
#     resultado: F conjunto ~ 0,79 -> "no aportan poder identificador".
#
# ==============================================================================

library(tidyverse)
library(fixest)
library(sandwich)
library(lmtest)
library(car)

# ------------------------------------------------------------------------------
# 0. MUESTRA NUEVA — sin filtro NA por SST_PUERTO_L1
# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  # NO filtramos por SST_PUERTO_L1: recupera 16 obs y una planta
  group_by(NUI) |> filter(n() >= 2) |> ungroup() |>
  filter(!(NUI == "90073" & ANIO == 2013))

cat("Muestra nueva (sin filtro SST_L1, sin 90073/2013):\n")
cat(sprintf("  N = %d, G = %d plantas, T = %d meses\n",
            nrow(df), length(unique(df$NUI)), length(unique(df$period))))

#
if (nrow(df) != 427 || length(unique(df$NUI)) != 16) {
  cat("[nota] Muestra difiere de la esperada (427 obs, 16 plantas).\n")
  cat("       Verificar filtro de contruccion en 01_panel_base.R.\n")
}

# ==============================================================================
# 1. ESPECIFICACION PRINCIPAL: DK, DOS INSTRUMENTOS ANUALES
# ==============================================================================
# Instrumentos: ln_biomasa_sardina + ln_TAC_complejo (los dos anuales).
# Controles exogenos: ln_h_jurel, SEASON_SIN, SEASON_COS, TENDENCIA, ln_P_FOB.


iv_DK <- feols(
  ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                       ln_P_FOB | NUI |
    ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo,
  data = df, vcov = DK(4) ~ period
)

iv_CP <- feols(
  ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                       ln_P_FOB | NUI |
    ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo,
  data = df, cluster = ~ NUI
)

nm_g <- if ("fit_ln_h_complejo" %in% names(coef(iv_DK)))
          "fit_ln_h_complejo" else "ln_h_complejo"

g_DK <- as.numeric(coef(iv_DK)[nm_g])
s_DK <- as.numeric(se(iv_DK)[nm_g])
p_DK <- as.numeric(pvalue(iv_DK)[nm_g])
ic_DK <- g_DK + c(-1, 1) * qnorm(0.975) * s_DK

b_DK <- as.numeric(coef(iv_DK)["ln_P_FOB"])
sb_DK <- as.numeric(se(iv_DK)["ln_P_FOB"])

cat(sprintf("gamma (DK bw=4)   : %+.4f   SE = %.4f   p = %.4f   IC = [%+.3f, %+.3f]\n",
            g_DK, s_DK, p_DK, ic_DK[1], ic_DK[2]))
cat(sprintf("beta_FOB (DK bw=4): %+.4f   SE = %.4f\n", b_DK, sb_DK))

# ==============================================================================
# 2. F EFECTIVO OLEA PFLUEGER CON MATRIZ DK
# ==============================================================================
# Ahora K = 2 instrumentos. 
# ==============================================================================
cat("\n==============================================================\n")
cat("2. F efectivo Olea-Pflueger con matriz DK y dos instrumentos\n")
cat("==============================================================\n")

# Primera etapa con feols para tener acceso a la VCV DK correcta
fs <- feols(
  ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo + ln_h_jurel +
                  SEASON_SIN + SEASON_COS + TENDENCIA + ln_P_FOB | NUI,
  data = df, vcov = DK(4) ~ period
)

zn     <- c("ln_biomasa_sardina", "ln_TAC_complejo")
pi_hat <- as.numeric(coef(fs)[zn])
V_pi   <- as.matrix(vcov(fs))[zn, zn]

# Residualizar Z sobre controles + FE planta (Frisch-Waugh)
demean_por <- function(x, g)
  x - ave(x, g, FUN = function(v) mean(v, na.rm = TRUE))

dm <- df |>
  mutate(across(all_of(c(zn, "ln_h_jurel","SEASON_SIN","SEASON_COS",
                         "TENDENCIA","ln_P_FOB")),
                ~ demean_por(.x, NUI)))

Zresid <- lm(as.formula(paste("cbind(", paste(zn, collapse=","), ") ~ ",
                              "ln_h_jurel + SEASON_SIN + SEASON_COS + ",
                              "TENDENCIA + ln_P_FOB")),
             data = dm)$residuals
ZrZr <- crossprod(Zresid)
K2   <- ncol(Zresid)

num_eff <- as.numeric(t(pi_hat) %*% ZrZr %*% pi_hat)
den_eff <- sum(diag(ZrZr %*% V_pi))
F_eff   <- num_eff / den_eff

# KP con la misma matriz DK
W_KP <- as.numeric(t(pi_hat) %*% solve(V_pi) %*% pi_hat)
F_KP <- W_KP / K2

cat(sprintf("F efectivo Olea-Pflueger (DK bw=4, K=%d) : %.3f\n", K2, F_eff))
cat(sprintf("F Kleibergen-Paap (DK bw=4)              : %.3f\n", F_KP))
cat("Umbral Olea-Pflueger 5%% (TSLS, K=2)     : aprox. 19,7\n")

# ==============================================================================
# 3. INTERVALO ANDERSON RUBIN CON MATRIZ DK
# ==============================================================================

ar_test_DK <- function(g0, df, zn) {
  df$y_g <- df$ln_P_complejo_real - g0 * df$ln_h_complejo
  m <- feols(
    as.formula(paste("y_g ~",
                     paste(c(zn, "ln_h_jurel","SEASON_SIN","SEASON_COS",
                             "TENDENCIA","ln_P_FOB"), collapse=" + "),
                     "| NUI")),
    data = df, vcov = DK(4) ~ period
  )
  # Wald conjunto sobre los coeficientes de Z
  w <- wald(m, keep = zn, print = FALSE)
  c(F = as.numeric(w$stat), p = as.numeric(w$p))
}

# Grilla amplia
grid <- seq(-2, 2, by = 0.005)
ar_out <- t(sapply(grid, function(g0) ar_test_DK(g0, df, zn)))
p_grid <- ar_out[, "p"]

# IC 95%: gamma_0 tal que no rechazamos H0 al 5%
acepta <- p_grid > 0.05
if (any(acepta)) {
  ar_ci <- range(grid[acepta])
  cat(sprintf("IC AR 95%% (DK bw=4, K=%d) : [%+.4f, %+.4f]  (ancho %.3f)\n",
              K2, ar_ci[1], ar_ci[2], diff(ar_ci)))
} else {
  ar_ci <- c(NA, NA)
  cat("IC AR 95%%: vacio. Verificar especificacion.\n")
}

# F AR en gamma = 0
f_ar_0 <- ar_test_DK(0, df, zn)
cat(sprintf("F AR (H0: gamma = 0)      : %.3f   p = %.4f\n",
            f_ar_0["F"], f_ar_0["p"]))

# ==============================================================================
# 4. SARGAN J
# ==============================================================================
cat("\n==============================================================\n")
cat("4. Sargan-Hansen J\n")
cat("==============================================================\n")

sar <- fitstat(iv_DK, type = "sargan")
cat(sprintf("Sargan J = %.3f   p = %.4f\n",
            as.numeric(sar$sargan$stat),
            as.numeric(sar$sargan$p)))

# ==============================================================================
# 5. WALD H0: beta_FOB = 1 (TRANSMISION PERFECTA)
# ==============================================================================
# Correccion Felipe: theta en la tesis es el coef del FOB, no -1/gamma.
# Test: t = (beta_FOB - 1) / SE(beta_FOB) contra normal.
# ==============================================================================
cat("\n==============================================================\n")
cat("5. Wald H0: beta_FOB = 1 (transmision perfecta del FOB)\n")
cat("==============================================================\n")

t_stat <- (b_DK - 1) / sb_DK
p_val  <- 2 * (1 - pnorm(abs(t_stat)))

cat(sprintf("beta_FOB (DK bw=4)        : %+.4f   SE = %.4f\n", b_DK, sb_DK))
cat(sprintf("t Wald (H0: beta_FOB = 1) : %+.4f\n", t_stat))
cat(sprintf("p (bilateral, normal)     : %.4f\n", p_val))
cat("Interpretacion: si p < 0,05 rechazo transmision perfecta del FOB al precio local.\n")

# ==============================================================================
# 6. DIAGNOSTICO SOBRE AMBIENTALES (SO_PUERTO + SST_PUERTO_L1)
# ==============================================================================
# Felipe: "F conjunto de 0,79". Reproducir sobre esta muestra.
# ==============================================================================
cat("\n==============================================================\n")
cat("6. Test de fuerza de los ambientales (SO + SST_L1) — hoy como resultado\n")
cat("==============================================================\n")

if (all(c("SO_PUERTO", "SST_PUERTO_L1") %in% names(df))) {
  df_amb <- df |> filter(!is.na(SST_PUERTO_L1))
  fs_amb <- feols(
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                    ln_TAC_complejo + ln_h_jurel + SEASON_SIN + SEASON_COS +
                    TENDENCIA + ln_P_FOB | NUI,
    data = df_amb, vcov = DK(4) ~ period
  )
  w_amb <- wald(fs_amb, keep = c("SO_PUERTO", "SST_PUERTO_L1"), print = FALSE)
  cat(sprintf("F conjunto ambientales (DK) : %.3f   p = %.4f\n",
              as.numeric(w_amb$stat), as.numeric(w_amb$p)))
  cat("Lectura: si F < 5 aproximadamente, los ambientales no identifican por\n")
  cat("si solos. En el paper pasan de identificacion a resultado.\n")
} else {
  cat("[skip] Faltan columnas SO_PUERTO o SST_PUERTO_L1.\n")
}

# ==============================================================================
# 7. RESUMEN COMPACTO
# ==============================================================================
resumen <- tibble::tribble(
  ~item, ~valor,
  "N muestra",                                    nrow(df),
  "G plantas",                                    length(unique(df$NUI)),
  "T meses",                                      length(unique(df$period)),
  "gamma principal (DK bw=4)",                    round(g_DK, 4),
  "SE gamma (DK)",                                round(s_DK, 4),
  "p gamma (DK)",                                 round(p_DK, 4),
  "IC 95% normal (DK) - bajo",                    round(ic_DK[1], 4),
  "IC 95% normal (DK) - alto",                    round(ic_DK[2], 4),
  "IC AR 95% (DK) - bajo",                        round(ar_ci[1], 4),
  "IC AR 95% (DK) - alto",                        round(ar_ci[2], 4),
  "Ancho IC AR",                                  round(diff(ar_ci), 4),
  "F efectivo Olea-Pflueger (DK, K=2)",           round(F_eff, 4),
  "F Kleibergen-Paap (DK)",                       round(F_KP,  4),
  "F AR (H0: gamma = 0)",                         round(as.numeric(f_ar_0["F"]), 4),
  "p AR",                                         round(as.numeric(f_ar_0["p"]), 4),
  "Sargan J",                                     round(as.numeric(sar$sargan$stat), 4),
  "Sargan p",                                     round(as.numeric(sar$sargan$p), 4),
  "beta_FOB (DK)",                                round(b_DK,  4),
  "SE beta_FOB (DK)",                             round(sb_DK, 4),
  "t Wald (H0: beta_FOB = 1)",                    round(t_stat, 4),
  "p Wald (H0: beta_FOB = 1)",                    round(p_val,  6)
)

dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(resumen,
          here::here("outputs", "reportes_intermedios",
                     "C4_v2_dos_instrumentos_DK.csv"))
cat("\nGuardado: outputs/reportes_intermedios/C4_v2_dos_instrumentos_DK.csv\n")
print(resumen, n = Inf, width = Inf)
