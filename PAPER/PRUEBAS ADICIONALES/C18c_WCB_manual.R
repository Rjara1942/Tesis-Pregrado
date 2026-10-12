# ==============================================================================
# BLOQUE C-18c — Wild cluster bootstrap (manual) con pesos Rademacher
# ==============================================================================
# Implementación directa de Cameron, Gelbach & Miller (2008, JAE) sin
# depender de fwildclusterboot. Aplica WCR (wild cluster restricted) sobre
# la forma reducida con cluster = ANIO.
#
# Para cada test individual H0: β_j = 0:
#   1. Estimar modelo restringido (sin β_j·X_j) → residuos restringidos ũ
#   2. Para b = 1..B:
#        w_g ∈ {-1, +1} con prob 0.5 por cluster g (año)
#        y*_i = ŷ_restr_i + w_{g(i)} · ũ_i
#        Estimar modelo completo con y* → β*_j, se*_j
#        t*_b = β*_j / se*_j
#   3. p_WCR = (1/B) Σ 1{|t*_b| ≥ |t_obs|}
#   4. IC bootstrap: percentiles 2.5/97.5 de la distribución bootstrap de t,
#      invertidos vs se_obs.
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
cat(sprintf("Muestra: N=%d, G_NUI=%d, G_año=%d\n\n",
            nrow(df), n_distinct(df$NUI), n_distinct(df$ANIO)))

# ------------------------------------------------------------------------------
# Forma reducida completa
# ------------------------------------------------------------------------------
rf_full <- feols(
  ln_P_complejo_real ~ ln_biomasa_sardina + ln_TAC_complejo +
                        ln_h_jurel + SEASON_SIN + SEASON_COS +
                        TENDENCIA + ln_P_FOB | NUI,
  data = df, cluster = ~ANIO
)

cat("Coeficientes clásicos (cluster año):\n")
print(summary(rf_full))

# ------------------------------------------------------------------------------
# Función WCR para un parámetro
# ------------------------------------------------------------------------------
# Imponemos H0: β_param = 0.
# Modelo restringido = modelo sin la variable param
# ------------------------------------------------------------------------------
WCR <- function(param, B = 9999, seed = 2025) {
  set.seed(seed)

  controles <- c("ln_biomasa_sardina", "ln_TAC_complejo",
                 "ln_h_jurel", "SEASON_SIN", "SEASON_COS",
                 "TENDENCIA", "ln_P_FOB")
  resto <- setdiff(controles, param)

  # Modelo completo
  rhs_full <- paste(controles, collapse = " + ")
  f_full   <- as.formula(sprintf("ln_P_complejo_real ~ %s | NUI", rhs_full))
  m_full   <- feols(f_full, data = df, cluster = ~ANIO, warn = FALSE)
  coef_obs <- as.numeric(coef(m_full)[param])
  se_obs   <- as.numeric(se(m_full)[param])
  t_obs    <- coef_obs / se_obs

  # Modelo restringido (impone β_param = 0)
  rhs_r <- paste(resto, collapse = " + ")
  f_r   <- as.formula(sprintf("ln_P_complejo_real ~ %s | NUI", rhs_r))
  m_r   <- feols(f_r, data = df, warn = FALSE)

  y_hat_r <- predict(m_r)
  u_tilde <- df$ln_P_complejo_real - y_hat_r

  anios      <- df$ANIO
  G_anios    <- sort(unique(anios))

  # Loop bootstrap
  t_boot <- numeric(B)

  for (b in seq_len(B)) {
    # Pesos Rademacher por año
    w_g <- sample(c(-1, 1), length(G_anios), replace = TRUE)
    names(w_g) <- as.character(G_anios)

    # Aplicar a cada obs según su año
    w_i <- w_g[as.character(anios)]

    # Reconstruir y* con residuos reponderados
    df$y_star <- y_hat_r + w_i * u_tilde

    # Estimar modelo completo con y_star
    f_boot <- as.formula(sprintf("y_star ~ %s | NUI", rhs_full))
    m_boot <- feols(f_boot, data = df, cluster = ~ANIO, warn = FALSE,
                    notes = FALSE)
    coef_b <- as.numeric(coef(m_boot)[param])
    se_b   <- as.numeric(se(m_boot)[param])
    t_boot[b] <- coef_b / se_b   # bajo H0, β_j = 0 → test de t = coef/se

    if (b %% 1000 == 0) cat(sprintf("    %s: %d/%d\n", param, b, B))
  }

  # p-valor WCR (two-sided symmetric)
  p_wcr <- mean(abs(t_boot) >= abs(t_obs))

  # IC95% percentile-t
  q <- quantile(t_boot, c(0.025, 0.975), na.rm = TRUE)
  ic_lo <- coef_obs - q[2] * se_obs
  ic_hi <- coef_obs - q[1] * se_obs

  list(param     = param,
       coef_obs  = coef_obs,
       se_obs    = se_obs,
       t_obs     = t_obs,
       p_wcr     = p_wcr,
       ic_lo     = as.numeric(ic_lo),
       ic_hi     = as.numeric(ic_hi),
       t_boot    = t_boot)
}

# ------------------------------------------------------------------------------
# Correr WCR para los dos instrumentos
# ------------------------------------------------------------------------------
cat("\nWild cluster bootstrap (B = 9999, cluster = ANIO)...\n")
cat("  Nota: ~15-30 segundos por parámetro\n\n")

t0 <- Sys.time()
res_tac <- WCR("ln_TAC_complejo",     B = 9999)
res_bio <- WCR("ln_biomasa_sardina",  B = 9999)
cat(sprintf("\nTiempo total: %.1f s\n", as.numeric(Sys.time() - t0, units = "secs")))

# ------------------------------------------------------------------------------
# Tabla resumen
# ------------------------------------------------------------------------------
resumen <- bind_rows(
  tibble(
    parametro = res_tac$param,
    coef      = res_tac$coef_obs,
    se_cy     = res_tac$se_obs,
    t_obs     = res_tac$t_obs,
    p_wcr     = res_tac$p_wcr,
    IC95_lo   = res_tac$ic_lo,
    IC95_hi   = res_tac$ic_hi
  ),
  tibble(
    parametro = res_bio$param,
    coef      = res_bio$coef_obs,
    se_cy     = res_bio$se_obs,
    t_obs     = res_bio$t_obs,
    p_wcr     = res_bio$p_wcr,
    IC95_lo   = res_bio$ic_lo,
    IC95_hi   = res_bio$ic_hi
  )
) |> mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n==============================================================\n")
cat("RESULTADOS WILD CLUSTER BOOTSTRAP (manual, Rademacher)\n")
cat("==============================================================\n")
print(resumen, width = Inf)

write_csv(resumen, file.path(DIR_OUT, "C18c_WCB_manual.csv"))

# ------------------------------------------------------------------------------
# Lectura para el paper
# ------------------------------------------------------------------------------
cat("\n==============================================================\n")
cat("Para la carta de respuesta / Limitaciones (línea 245)\n")
cat("==============================================================\n")
cat(sprintf("Wild cluster bootstrap-null (Cameron, Gelbach y Miller, 2008)\n"))
cat(sprintf("sobre la forma reducida, cluster = año, B = 9999, Rademacher:\n\n"))
cat(sprintf("  β_TAC     = %+.4f   p_WCR = %.4f   IC95%% = [%+.4f, %+.4f]\n",
            res_tac$coef_obs, res_tac$p_wcr,
            res_tac$ic_lo, res_tac$ic_hi))
cat(sprintf("  β_biomasa = %+.4f   p_WCR = %.4f   IC95%% = [%+.4f, %+.4f]\n",
            res_bio$coef_obs, res_bio$p_wcr,
            res_bio$ic_lo, res_bio$ic_hi))
cat("\nComparación con p-valor cluster-robust clásico (asintótico):\n")
print(summary(rf_full)$coeftable[c("ln_TAC_complejo", "ln_biomasa_sardina"), ])
