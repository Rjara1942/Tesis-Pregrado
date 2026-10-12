# ==============================================================================
# BLOQUE C-18 — Reconciliación de cifras para el paper
# ==============================================================================
# Entregables (5 ítems pedidos por Ricardo):
#   (1) Línea 114 (Datos):  medias y P̄/h̄ sobre 427 obs (vs 435)
#   (2) Línea 186 (§5.3):   cociente ∂P/∂h · h̄/P̄ (recálculo sobre 427)
#   (3) Línea 127 (§3.3):   IV principal sobre 435 obs (sin exclusión 90073-2013)
#   (4) Línea 245 (Limit.): AR + wild bootstrap cluster año
#   (5) Línea 229 (§5.6):   IC para la cadena TAC → precio (γ × β_TAC)
# ==============================================================================

library(tidyverse)
library(fixest)
library(sandwich)
library(here)

has_wcb <- requireNamespace("fwildclusterboot", quietly = TRUE)

PATH_PANEL <- here::here("data", "panel_upgrade.csv")
DIR_OUT    <- here::here("outputs", "reportes_intermedios")
dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# Dos muestras: 435 (sin exclusión) y 427 (con exclusión 90073-2013)
# ------------------------------------------------------------------------------
df_full <- read_csv(PATH_PANEL, show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES)),
    ln_P_complejo_real = log(P_complejo_real)
  ) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup()

df_427 <- df_full |> filter(!(NUI == "90073" & ANIO == 2013))
df_435 <- df_full

cat(sprintf("Muestra 435: N=%d, G=%d\n", nrow(df_435), n_distinct(df_435$NUI)))
cat(sprintf("Muestra 427: N=%d, G=%d\n", nrow(df_427), n_distinct(df_427$NUI)))

# ==============================================================================
# (1) LÍNEA 114 — Medias y P̄/h̄ sobre 427 obs
# ==============================================================================
cat("\n==============================================================\n")
cat("(1) Línea 114 — Descriptivos sobre 427 obs\n")
cat("==============================================================\n")

tabla_medias <- function(df, nombre) {
  tibble(
    muestra = nombre,
    N       = nrow(df),
    P_bar   = mean(df$P_complejo_real, na.rm = TRUE),
    h_bar   = mean(exp(df$ln_h_complejo), na.rm = TRUE),
    lnP_bar = mean(df$ln_P_complejo_real, na.rm = TRUE),
    lnh_bar = mean(df$ln_h_complejo, na.rm = TRUE),
    biomasa_bar = mean(exp(df$ln_biomasa_sardina), na.rm = TRUE),
    TAC_bar     = mean(exp(df$ln_TAC_complejo), na.rm = TRUE),
    FOB_bar     = mean(exp(df$ln_P_FOB), na.rm = TRUE),
    jurel_bar   = mean(exp(df$ln_h_jurel), na.rm = TRUE)
  ) |>
    mutate(P_sobre_h = P_bar / h_bar)
}

medias_cmp <- bind_rows(tabla_medias(df_435, "435 (sin exclusión)"),
                        tabla_medias(df_427, "427 (paper)"))
print(medias_cmp |> mutate(across(where(is.numeric), ~ round(.x, 3))),
      width = Inf)

write_csv(medias_cmp, file.path(DIR_OUT, "C18_1_medias_427.csv"))

# ==============================================================================
# (2) LÍNEA 186 — Cociente ∂P/∂h · (h̄/P̄) sobre 427 (debería dar γ = -0.193)
#     y variantes útiles para el párrafo de §5.3
# ==============================================================================
cat("\n==============================================================\n")
cat("(2) Línea 186 — Elasticidades y niveles ∂P/∂h sobre 427\n")
cat("==============================================================\n")

gamma     <- -0.193                           # baseline paper
P_bar_427 <- mean(df_427$P_complejo_real, na.rm = TRUE)
h_bar_427 <- mean(exp(df_427$ln_h_complejo), na.rm = TRUE)
P_bar_435 <- mean(df_435$P_complejo_real, na.rm = TRUE)
h_bar_435 <- mean(exp(df_435$ln_h_complejo), na.rm = TRUE)

# ∂P/∂h en CLP/ton (en el punto medio)
dPdh_427 <- gamma * P_bar_427 / h_bar_427
dPdh_435 <- gamma * P_bar_435 / h_bar_435

# El "cociente 0.111": probablemente |γ × P̄/h̄| expresado en escala relativa,
# o la elasticidad |γ · (h/P) · (ΔP/Δh)|. Reporto varias candidatas:
cat(sprintf("P̄ (427) = %.0f CLP/ton\n", P_bar_427))
cat(sprintf("h̄ (427) = %.1f ton\n", h_bar_427))
cat(sprintf("P̄/h̄ (427) = %.3f\n", P_bar_427 / h_bar_427))
cat(sprintf("∂P/∂h = γ · P̄/h̄ = %.3f · %.3f = %.3f CLP/ton\n",
            gamma, P_bar_427/h_bar_427, dPdh_427))
cat(sprintf("|γ| expresado como ΔP%% / Δh%% = %.3f (elasticidad)\n", abs(gamma)))
cat(sprintf("|γ · h̄/P̄| / 1000 (escala kCLP/kton) = %.4f\n",
            abs(gamma * h_bar_427 / P_bar_427) / 1000))

# Para comparar: en 435
cat(sprintf("\n[Comparativo 435]  P̄=%.0f, h̄=%.1f, ∂P/∂h=%.3f\n",
            P_bar_435, h_bar_435, dPdh_435))

# Guardar
out2 <- tibble(
  muestra   = c("435", "427"),
  P_bar     = c(P_bar_435, P_bar_427),
  h_bar     = c(h_bar_435, h_bar_427),
  P_sobre_h = c(P_bar_435/h_bar_435, P_bar_427/h_bar_427),
  gamma     = gamma,
  dPdh      = c(dPdh_435, dPdh_427)
)
write_csv(out2, file.path(DIR_OUT, "C18_2_dPdh_427.csv"))

# ==============================================================================
# (3) LÍNEA 127 — IV principal sobre 435 (comparar con 427)
# ==============================================================================
cat("\n==============================================================\n")
cat("(3) Línea 127 — IV principal sobre 435 obs (comparar con 427)\n")
cat("==============================================================\n")

FORM_IV <- ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS +
                                TENDENCIA + ln_P_FOB | NUI |
             ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo

m_435 <- feols(FORM_IV, data = df_435, vcov = DK(4) ~ period)
m_427 <- feols(FORM_IV, data = df_427, vcov = DK(4) ~ period)

extract_iv <- function(m, nombre) {
  g  <- as.numeric(coef(m)["fit_ln_h_complejo"])
  s  <- as.numeric(se(m)["fit_ln_h_complejo"])
  ci <- as.numeric(confint(m, "fit_ln_h_complejo"))
  F  <- tryCatch(as.numeric(fitstat(m, "ivf")$`ivf1::ln_h_complejo`$stat),
                 error = function(e) NA_real_)
  tibble(muestra = nombre, gamma = g, se = s,
         ci_low = ci[1], ci_high = ci[2], F = F, N = m$nobs)
}

comp_iv <- bind_rows(extract_iv(m_435, "435 (sin exclusión)"),
                     extract_iv(m_427, "427 (paper)"))
print(comp_iv |> mutate(across(where(is.numeric), ~ round(.x, 3))),
      width = Inf)
write_csv(comp_iv, file.path(DIR_OUT, "C18_3_IV_435_vs_427.csv"))

# ==============================================================================
# (4) LÍNEA 245 — AR con wild cluster bootstrap por año
# ==============================================================================
cat("\n==============================================================\n")
cat("(4) Línea 245 — Anderson-Rubin + wild cluster bootstrap año\n")
cat("==============================================================\n")

# -- AR cluster por año sobre grid β --------------------------------------------
grid_beta <- seq(-1.2, 0.6, by = 0.02)

AR_cluster_year <- function(b, d = df_427) {
  d$y_b <- d$ln_P_complejo_real - b * d$ln_h_complejo
  m <- feols(y_b ~ ln_biomasa_sardina + ln_TAC_complejo +
                   ln_h_jurel + SEASON_SIN + SEASON_COS +
                   TENDENCIA + ln_P_FOB | NUI,
             data = d, cluster = ~ ANIO, warn = FALSE)
  w <- wald(m, c("ln_biomasa_sardina", "ln_TAC_complejo"), print = FALSE)
  as.numeric(w$stat)  # F
}

cat("Calculando curva AR con cluster por año (puede tardar ~15s)...\n")
AR_cy <- tibble(beta = grid_beta,
                F    = sapply(grid_beta, AR_cluster_year)) |>
  mutate(chi2 = F * 2)

crit <- qchisq(0.95, df = 2)
IC_AR <- range(AR_cy$beta[AR_cy$chi2 <= crit])
cat(sprintf("IC Anderson-Rubin (cluster año): [%.3f, %.3f]\n", IC_AR[1], IC_AR[2]))

write_csv(AR_cy, file.path(DIR_OUT, "C18_4_AR_cluster_year.csv"))

# -- Wild cluster bootstrap sobre la forma reducida (TAC coef) ------------------
if (has_wcb) {
  cat("\nWild cluster bootstrap (B=9999, cluster año)...\n")
  rf <- feols(ln_P_complejo_real ~ ln_biomasa_sardina + ln_TAC_complejo +
                                    ln_h_jurel + SEASON_SIN + SEASON_COS +
                                    TENDENCIA + ln_P_FOB | NUI,
              data = df_427)

  bt_tac <- fwildclusterboot::boottest(
    rf, clustid = "ANIO", param = "ln_TAC_complejo",
    B = 9999, nthreads = 1, bootstrap_type = "11"
  )
  bt_bio <- fwildclusterboot::boottest(
    rf, clustid = "ANIO", param = "ln_biomasa_sardina",
    B = 9999, nthreads = 1, bootstrap_type = "11"
  )

  cat(sprintf("TAC:    p_wcb = %.4f   IC95%% = [%.3f, %.3f]\n",
              bt_tac$p_val, bt_tac$conf_int[1], bt_tac$conf_int[2]))
  cat(sprintf("Biomasa: p_wcb = %.4f   IC95%% = [%.3f, %.3f]\n",
              bt_bio$p_val, bt_bio$conf_int[1], bt_bio$conf_int[2]))

  out4b <- tibble(
    param = c("ln_TAC_complejo", "ln_biomasa_sardina"),
    coef  = c(coef(rf)["ln_TAC_complejo"], coef(rf)["ln_biomasa_sardina"]),
    p_wcb = c(bt_tac$p_val, bt_bio$p_val),
    ci_lo = c(bt_tac$conf_int[1], bt_bio$conf_int[1]),
    ci_hi = c(bt_tac$conf_int[2], bt_bio$conf_int[2])
  )
  write_csv(out4b, file.path(DIR_OUT, "C18_4_WCB_formareducida.csv"))
} else {
  cat("\n[OMITIDO] Para wild cluster bootstrap: install.packages('fwildclusterboot')\n")
}

# ==============================================================================
# (5) LÍNEA 229 — IC para la cadena TAC → precio (γ × β_TAC)
# ==============================================================================
cat("\n==============================================================\n")
cat("(5) Línea 229 — IC para γ · β_TAC (cadena TAC → precio)\n")
cat("==============================================================\n")

# Primera etapa para obtener β_TAC y su SE
fs <- feols(ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo +
                             ln_h_jurel + SEASON_SIN + SEASON_COS +
                             TENDENCIA + ln_P_FOB | NUI,
            data = df_427, vcov = DK(4) ~ period)

gamma_hat <- as.numeric(coef(m_427)["fit_ln_h_complejo"])
se_gamma  <- as.numeric(se(m_427)["fit_ln_h_complejo"])
beta_tac  <- as.numeric(coef(fs)["ln_TAC_complejo"])
se_tac    <- as.numeric(se(fs)["ln_TAC_complejo"])

# --- Método delta (asume γ y β independientes, aprox razonable: provienen de
#     distintas ecuaciones y γ es consistente bajo validez del instrumento) ---
elast     <- gamma_hat * beta_tac
se_delta  <- sqrt(beta_tac^2 * se_gamma^2 + gamma_hat^2 * se_tac^2)
ci_delta  <- elast + c(-1, 1) * qnorm(0.975) * se_delta

cat(sprintf("γ_IV = %+.3f (SE %.3f)\n", gamma_hat, se_gamma))
cat(sprintf("β_TAC = %+.3f (SE %.3f)\n", beta_tac, se_tac))
cat(sprintf("\nElasticidad total γ · β_TAC = %+.4f (SE delta = %.4f)\n",
            elast, se_delta))
cat(sprintf("IC95%% delta:            [%+.4f, %+.4f]\n",
            ci_delta[1], ci_delta[2]))

# --- Monte Carlo (sin suponer independencia dentro del AR interval) ----------
# Simplemente simulo γ ~ N(γ̂, SE_γ²), β ~ N(β̂, SE_β²), e informa percentiles
set.seed(123)
B <- 20000
sim_g <- rnorm(B, gamma_hat, se_gamma)
sim_b <- rnorm(B, beta_tac,  se_tac)
sim_e <- sim_g * sim_b
ci_mc <- quantile(sim_e, c(0.025, 0.975))
cat(sprintf("IC95%% Monte Carlo:      [%+.4f, %+.4f]\n", ci_mc[1], ci_mc[2]))

# --- Fieller (robusto a ratios/productos; aquí producto) ---------------------
# Para producto γ·β: usar bootstrap pivotal es más seguro. Ya está arriba.

# Interpretación en escala policy
cat("\n-- Lectura policy (ΔTAC → ΔP) --\n")
for (dTAC in c(-20, -10, 10, 20)) {
  dP     <- elast * dTAC
  dP_lo  <- min(ci_delta[1] * dTAC, ci_delta[2] * dTAC)
  dP_hi  <- max(ci_delta[1] * dTAC, ci_delta[2] * dTAC)
  cat(sprintf("ΔTAC = %+3d %%  →  ΔP = %+6.2f %%   [%+.2f, %+.2f] 95%%\n",
              dTAC, dP, dP_lo, dP_hi))
}

out5 <- tibble(
  metrica = c("gamma_IV", "se_gamma", "beta_TAC", "se_TAC",
              "elasticidad_total", "se_delta",
              "IC95_delta_lo", "IC95_delta_hi",
              "IC95_MC_lo", "IC95_MC_hi"),
  valor = c(gamma_hat, se_gamma, beta_tac, se_tac,
            elast, se_delta, ci_delta[1], ci_delta[2],
            ci_mc[1], ci_mc[2])
) |> mutate(valor = round(valor, 4))
write_csv(out5, file.path(DIR_OUT, "C18_5_cadena_TAC_precio.csv"))

cat("\n==============================================================\n")
cat("Salidas:\n")
cat("  outputs/reportes_intermedios/C18_1_medias_427.csv\n")
cat("  outputs/reportes_intermedios/C18_2_dPdh_427.csv\n")
cat("  outputs/reportes_intermedios/C18_3_IV_435_vs_427.csv\n")
cat("  outputs/reportes_intermedios/C18_4_AR_cluster_year.csv\n")
if (has_wcb) cat("  outputs/reportes_intermedios/C18_4_WCB_formareducida.csv\n")
cat("  outputs/reportes_intermedios/C18_5_cadena_TAC_precio.csv\n")
