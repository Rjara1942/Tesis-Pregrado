# ==============================================================================
# BLOQUE C-18d — Wild cluster bootstrap TEST CONJUNTO (Anderson-Rubin)
# ==============================================================================
# Los tests individuales (C-18c) sobre cada instrumento no son el test correcto
# para la hipótesis sobre γ. El test AR equivale al test F CONJUNTO de ambos
# instrumentos en la forma reducida. Implementamos WCR sobre ese test F.
#
#   H0: γ = β0  ⇔  H0: β_biomasa = β_TAC = 0 en (y - β0·x_endo)
#   AR(β0) = F conjunto de los dos instrumentos en la forma reducida de y_b
#
# Para β0 = 0 → forma reducida estándar.
# Para construir el intervalo AR-WCB, invertimos el test sobre un grid de β0.
# ==============================================================================

library(tidyverse)
library(fixest)
library(here)

PATH_PANEL <- here::here("data", "panel_upgrade.csv")
DIR_OUT    <- here::here("outputs", "reportes_intermedios")
dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)

df <- read_csv(PATH_PANEL, show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES)),
    ln_P_complejo_real = log(P_complejo_real)
  ) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup() |>
  filter(!(NUI == "90073" & ANIO == 2013))

stopifnot(nrow(df) == 427)
anios   <- df$ANIO
G_anios <- sort(unique(anios))
cat(sprintf("Muestra: N=%d, G_año=%d\n\n", nrow(df), length(G_anios)))

# ------------------------------------------------------------------------------
# Función: test AR-WCR en un β0 dado
#   F_obs: F de significancia conjunta de biomasa+TAC en RF de (y - β0·x_endo)
#   F*_b : bajo H0 (sin biomasa/TAC), por WCR cluster año
#   p    : fracción F*_b >= F_obs
# ------------------------------------------------------------------------------
AR_WCR <- function(beta0, B = 2999, seed = 2025) {
  set.seed(seed + round(beta0 * 1e6))
  df$y_b <- df$ln_P_complejo_real - beta0 * df$ln_h_complejo

  # Modelo completo (con instrumentos)
  m_full <- feols(
    y_b ~ ln_biomasa_sardina + ln_TAC_complejo +
          ln_h_jurel + SEASON_SIN + SEASON_COS +
          TENDENCIA + ln_P_FOB | NUI,
    data = df, cluster = ~ANIO, warn = FALSE, notes = FALSE
  )
  F_obs <- as.numeric(
    wald(m_full, c("ln_biomasa_sardina", "ln_TAC_complejo"),
         print = FALSE)$stat
  )

  # Modelo restringido (sin instrumentos → impone H0)
  m_r <- feols(
    y_b ~ ln_h_jurel + SEASON_SIN + SEASON_COS +
          TENDENCIA + ln_P_FOB | NUI,
    data = df, warn = FALSE, notes = FALSE
  )
  y_hat_r <- predict(m_r)
  u_tilde <- df$y_b - y_hat_r

  # Bootstrap
  F_boot <- numeric(B)
  for (b in seq_len(B)) {
    w_g <- sample(c(-1, 1), length(G_anios), replace = TRUE)
    names(w_g) <- as.character(G_anios)
    w_i <- w_g[as.character(anios)]
    df$y_star <- y_hat_r + w_i * u_tilde

    m_b <- feols(
      y_star ~ ln_biomasa_sardina + ln_TAC_complejo +
               ln_h_jurel + SEASON_SIN + SEASON_COS +
               TENDENCIA + ln_P_FOB | NUI,
      data = df, cluster = ~ANIO, warn = FALSE, notes = FALSE
    )
    F_boot[b] <- as.numeric(
      wald(m_b, c("ln_biomasa_sardina", "ln_TAC_complejo"),
           print = FALSE)$stat
    )
  }

  p <- mean(F_boot >= F_obs)
  list(beta0 = beta0, F_obs = F_obs, p_wcr = p)
}

# ------------------------------------------------------------------------------
# 1. Test puntual en β0 = 0 (equivalente a H0: γ = 0)
# ------------------------------------------------------------------------------
cat("Test AR-WCR en β0 = 0 (H0: γ = 0)...\n")
cat("  B = 2999, ~2-3 minutos\n")
t0 <- Sys.time()
r0 <- AR_WCR(0, B = 2999)
cat(sprintf("  Tiempo: %.1f s\n", as.numeric(Sys.time() - t0, units = "secs")))
cat(sprintf("\n  F_obs = %.3f\n", r0$F_obs))
cat(sprintf("  p_WCR (test conjunto AR) = %.4f\n", r0$p_wcr))

# ------------------------------------------------------------------------------
# 2. Test en β0 = γ_IV baseline = -0.193 (si p es alto → punto estimado dentro IC)
# ------------------------------------------------------------------------------
cat("\nTest AR-WCR en β0 = -0.193 (γ_IV baseline)...\n")
r_bl <- AR_WCR(-0.193, B = 2999)
cat(sprintf("  F_obs = %.3f\n", r_bl$F_obs))
cat(sprintf("  p_WCR = %.4f  (si p>0.05, -0.193 está en el IC AR-WCB)\n",
            r_bl$p_wcr))

# ------------------------------------------------------------------------------
# 3. Barrido de un grid β0 para construir el IC AR-WCR
# ------------------------------------------------------------------------------
cat("\nBarrido grid de β0 para IC AR-WCB (B=1999 por punto, ~8 min)...\n")
grid <- seq(-0.9, 0.3, by = 0.05)
t0 <- Sys.time()
res_grid <- map_dfr(grid, function(b0) {
  r <- AR_WCR(b0, B = 1999)
  cat(sprintf("  β0 = %+.3f  F = %6.3f  p = %.3f\n",
              b0, r$F_obs, r$p_wcr))
  tibble(beta0 = b0, F_obs = r$F_obs, p_wcr = r$p_wcr)
})
cat(sprintf("\n  Tiempo barrido: %.1f min\n",
            as.numeric(Sys.time() - t0, units = "mins")))

# IC AR-WCB: valores de β0 donde p >= 0.05
in_ic <- res_grid |> filter(p_wcr >= 0.05)
if (nrow(in_ic) > 0) {
  ic_lo <- min(in_ic$beta0)
  ic_hi <- max(in_ic$beta0)
  cat(sprintf("\nIC AR-WCB 95%% = [%+.3f, %+.3f]  (resolución del grid: 0.05)\n",
              ic_lo, ic_hi))
} else {
  cat("\n[ADVERTENCIA] Ningún punto del grid con p>=0.05. Ampliar grid.\n")
}

# Guardar
write_csv(res_grid, file.path(DIR_OUT, "C18d_AR_WCB_grid.csv"))

# ------------------------------------------------------------------------------
# Resumen
# ------------------------------------------------------------------------------
cat("\n==============================================================\n")
cat("RESUMEN TESTS CONJUNTOS (AR / Wild Cluster Bootstrap)\n")
cat("==============================================================\n")
cat(sprintf("Test en β0 = 0 (H0: γ=0):    p_WCR = %.4f\n", r0$p_wcr))
cat(sprintf("Test en β0 = -0.193 (baseline): p_WCR = %.4f\n", r_bl$p_wcr))
if (exists("ic_lo")) {
  cat(sprintf("IC AR-WCB 95%% construido por inversión: [%+.3f, %+.3f]\n",
              ic_lo, ic_hi))
}
cat("\nComparar con:\n")
cat("  IC Anderson-Rubin DK(4): [-0.605, -0.090]\n")
cat("  IC Anderson-Rubin cluster año: [-0.480, -0.120]\n")
cat("  γ_IV baseline: -0.193 (SE 0.074 DK, p=0.009)\n")

out <- tibble(
  test = c("β0=0 (H0: γ=0)",
           "β0=-0.193 (γ_IV)",
           "IC_lo", "IC_hi"),
  valor = c(r0$p_wcr, r_bl$p_wcr,
            ifelse(exists("ic_lo"), ic_lo, NA),
            ifelse(exists("ic_hi"), ic_hi, NA))
)
write_csv(out, file.path(DIR_OUT, "C18d_AR_WCB_resumen.csv"))
