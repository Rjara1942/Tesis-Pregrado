# ==============================================================================
# Hansen J robusto bajo matriz de varianzas Driscoll-Kraay
# ==============================================================================

library(tidyverse)
library(fixest)
library(sandwich)


# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup() |>
  filter(!(NUI == "90073" & ANIO == 2013))

stopifnot(nrow(df) == 427, length(unique(df$NUI)) == 16)

cat("Muestra: N =", nrow(df), "G =", length(unique(df$NUI)),
    "T =", length(unique(df$period)), "\n\n")

# ------------------------------------------------------------------------------
# ESTIMAR EL MODELO IV PRINCIPAL (DK bw=4)
# ------------------------------------------------------------------------------
iv_DK <- feols(
  ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                       ln_P_FOB | NUI |
    ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo,
  data = df, vcov = DK(4) ~ period
)

# ------------------------------------------------------------------------------
# SARGAN CLÁSICO 
# ------------------------------------------------------------------------------
sar <- fitstat(iv_DK, type = "sargan")
sargan_stat <- as.numeric(sar$sargan$stat)
sargan_p    <- as.numeric(sar$sargan$p)
sargan_df   <- 1  # K - p = 2 - 1



# ------------------------------------------------------------------------------
# HANSEN J ROBUSTO BAJO DRISCOLL-KRAAY bw=4
# ------------------------------------------------------------------------------

# Residuos del modelo IV
u_hat <- residuals(iv_DK)

# Matriz de instrumentos excluidos
Z_excl <- as.matrix(df[, c("ln_biomasa_sardina", "ln_TAC_complejo")])
K_excl <- ncol(Z_excl)

# Controles exógenos + FE de planta (en forma de dummies)
X_exog <- model.matrix(
  ~ 0 + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA + ln_P_FOB +
    factor(NUI),
  data = df
)

# Residualizar Z sobre controles + FE (Frisch-Waugh)
# Z_resid = Z - X (X'X)^{-1} X'Z
Zr <- Z_excl - X_exog %*% solve(crossprod(X_exog)) %*% crossprod(X_exog, Z_excl)

# Momentos individuales: m_it = Z_it^r · u_it
m_it <- sweep(Zr, 1, u_hat, "*")  # N x K_excl

# Agregar por período (requisito de Driscoll-Kraay)
m_t <- as.matrix(
  as.data.frame(m_it) |>
    mutate(period = df$period) |>
    group_by(period) |>
    summarise(across(everything(), sum), .groups = "drop") |>
    select(-period)
)

T_eff <- nrow(m_t)

# HAC Newey-West con kernel Bartlett y bw = L = 4
L <- 4
S_hat <- crossprod(m_t) / T_eff
for (l in seq_len(L)) {
  w_l <- 1 - l / (L + 1)  # peso Bartlett
  Gl  <- crossprod(m_t[-(1:l), , drop = FALSE],
                   m_t[1:(T_eff - l), , drop = FALSE]) / T_eff
  S_hat <- S_hat + w_l * (Gl + t(Gl))
}

# Media de momentos
g_bar <- colMeans(m_t)

# Estadístico Hansen J
J_hansen <- as.numeric(T_eff * t(g_bar) %*% solve(S_hat) %*% g_bar)
df_J     <- K_excl - 1  # 2 - 1 = 1 grado de libertad
p_hansen <- pchisq(J_hansen, df = df_J, lower.tail = FALSE)

cat(sprintf("Hansen J (DK bw=4) = %.4f  df = %d  p = %.4f\n", J_hansen, df_J, p_hansen))
cat(sprintf("Umbral chi^2(1, 0.05) = %.4f\n", qchisq(0.95, df = df_J)))

if (p_hansen > 0.05) {
  cat("=> No se rechaza la sobreidentificación: los dos instrumentos\n")
  cat("   son mutuamente consistentes bajo la matriz de Driscoll-Kraay.\n\n")
} else {
  cat("=> Rechazo. Los dos instrumentos no son mutuamente consistentes.\n\n")
}

# ------------------------------------------------------------------------------
# COMPARATIVA PARA EL PAPER
# ------------------------------------------------------------------------------


comp <- tibble::tribble(
  ~Estadistico,        ~Valor,          ~p,          ~Supuesto,
  "Sargan clásico (iid)", sargan_stat,   sargan_p,    "errores iid",
  "Hansen J (DK bw=4)",   J_hansen,       p_hansen,    "DK bw=4 (correcto bajo agrupamiento)"
) |>
  mutate(
    Valor = round(Valor, 4),
    p     = round(p, 4)
  )

print(comp, width = Inf)

# ------------------------------------------------------------------------------
# 5. GUARDAR SALIDA
# ------------------------------------------------------------------------------
dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)

resumen <- tibble::tribble(
  ~item, ~valor,
  "N muestra",                nrow(df),
  "T períodos (DK)",          T_eff,
  "K instrumentos excluidos", K_excl,
  "p endógenas",              1L,
  "df Hansen",                df_J,
  "Sargan clásico J",         round(sargan_stat, 4),
  "Sargan clásico p",         round(sargan_p, 4),
  "Hansen J (DK bw=4)",       round(J_hansen, 4),
  "Hansen p",                 round(p_hansen, 4)
)

write_csv(resumen,
          here::here("outputs", "reportes_intermedios",
                     "C10_hansen_robusto.csv"))

cat("\nGuardado: outputs/reportes_intermedios/C10_hansen_robusto.csv\n")

