# ==============================================================================
# BLOQUE D-1 — Contraste pre/post 2020 sobre muestra corregida y set nuevo
# ==============================================================================
# Pauta Felipe 25 sep:
#   Re-correr el contraste pre/post 2020 con la muestra corregida (sin
#   90073/2013) y con el set nuevo de dos instrumentos anuales
#   (ln_biomasa_sardina + ln_TAC_complejo). Es el que puede cambiar de signo
#   porque las 8 filas de 90073 eran de 2013 y caian enteras en la mitad
#   pre-2020.
#
# Numeros previos en conflicto: Felipe -0,327 vs. Ricardo -0,208 sobre la
# muestra vieja. Aca resolvemos.
# ==============================================================================

library(tidyverse)
library(fixest)
library(sandwich)
library(lmtest)

# ------------------------------------------------------------------------------
# 0. MUESTRA
# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES)),
    POST2020           = as.integer(ANIO >= 2020)
  ) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup() |>
  filter(!(NUI == "90073" & ANIO == 2013))

cat("Muestra: N =", nrow(df), ", G =", length(unique(df$NUI)),
    ", T =", length(unique(df$period)), "\n")
cat("Pre 2020: ", sum(df$POST2020 == 0), " obs\n")
cat("Post 2020:", sum(df$POST2020 == 1), " obs\n\n")

# ==============================================================================
# 1. ESTIMACION SEPARADA PRE Y POST 2020
# ==============================================================================
# Especificacion principal: DK bw=4, dos instrumentos anuales.
# ==============================================================================
formula_iv <-
  ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                       ln_P_FOB | NUI |
    ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo

df_pre  <- df |> filter(POST2020 == 0)
df_post <- df |> filter(POST2020 == 1)

cat("==============================================================\n")
cat("1. Pre 2020 (", nrow(df_pre), " obs)\n", sep="")
cat("==============================================================\n")

iv_pre <- feols(formula_iv, data = df_pre, vcov = DK(4) ~ period)
nm_g <- if ("fit_ln_h_complejo" %in% names(coef(iv_pre)))
          "fit_ln_h_complejo" else "ln_h_complejo"

g_pre <- as.numeric(coef(iv_pre)[nm_g])
s_pre <- as.numeric(se(iv_pre)[nm_g])
p_pre <- as.numeric(pvalue(iv_pre)[nm_g])
ic_pre <- g_pre + c(-1, 1) * qnorm(0.975) * s_pre

cat(sprintf("gamma pre  = %+.4f   SE = %.4f   p = %.4f   IC = [%+.3f, %+.3f]\n",
            g_pre, s_pre, p_pre, ic_pre[1], ic_pre[2]))

cat("\n==============================================================\n")
cat("2. Post 2020 (", nrow(df_post), " obs)\n", sep="")
cat("==============================================================\n")

iv_post <- tryCatch(
  feols(formula_iv, data = df_post, vcov = DK(4) ~ period),
  error = function(e) NULL
)

if (!is.null(iv_post)) {
  g_post <- as.numeric(coef(iv_post)[nm_g])
  s_post <- as.numeric(se(iv_post)[nm_g])
  p_post <- as.numeric(pvalue(iv_post)[nm_g])
  ic_post <- g_post + c(-1, 1) * qnorm(0.975) * s_post
  cat(sprintf("gamma post = %+.4f   SE = %.4f   p = %.4f   IC = [%+.3f, %+.3f]\n",
              g_post, s_post, p_post, ic_post[1], ic_post[2]))
} else {
  g_post <- NA_real_; s_post <- NA_real_; p_post <- NA_real_
  ic_post <- c(NA, NA)
  cat("[error] La IV post 2020 no converge. Muestra probablemente muy chica.\n")
}

# ==============================================================================
# 3. TEST FORMAL DE DIFERENCIA — INTERACCION
# ==============================================================================
# Modelo con interaccion POST2020 * ln_h_complejo, instrumentando ambos
# terminos con los dos anuales y sus interacciones con POST2020.
# ==============================================================================
cat("\n==============================================================\n")
cat("3. Test de diferencia pre vs. post: interaccion en modelo unico\n")
cat("==============================================================\n")

iv_int <- tryCatch(
  feols(
    ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                         ln_P_FOB + POST2020 | NUI |
      ln_h_complejo + I(ln_h_complejo * POST2020) ~
        ln_biomasa_sardina + ln_TAC_complejo +
        I(ln_biomasa_sardina * POST2020) + I(ln_TAC_complejo * POST2020),
    data = df, vcov = DK(4) ~ period
  ),
  error = function(e) NULL
)

if (!is.null(iv_int)) {
  # Nombres esperados
  nm_g_int <- grep("^(fit_)?ln_h_complejo$", names(coef(iv_int)), value = TRUE)[1]
  nm_i_int <- grep("POST2020", names(coef(iv_int)), value = TRUE)
  nm_i_int <- nm_i_int[grepl("h_complejo", nm_i_int)][1]

  g_base <- as.numeric(coef(iv_int)[nm_g_int])
  g_diff <- as.numeric(coef(iv_int)[nm_i_int])
  s_diff <- as.numeric(se(iv_int)[nm_i_int])
  p_diff <- as.numeric(pvalue(iv_int)[nm_i_int])

  cat(sprintf("gamma pre (base)              : %+.4f\n", g_base))
  cat(sprintf("gamma diff (interaccion)      : %+.4f   SE = %.4f   p = %.4f\n",
              g_diff, s_diff, p_diff))
  cat(sprintf("gamma post implicito          : %+.4f\n", g_base + g_diff))
} else {
  g_base <- NA_real_; g_diff <- NA_real_; s_diff <- NA_real_; p_diff <- NA_real_
  cat("[error] La IV con interaccion no converge.\n")
}

# ==============================================================================
# 4. RESUMEN
# ==============================================================================
resumen <- tibble::tribble(
  ~item, ~valor,
  "N pre 2020",                     nrow(df_pre),
  "N post 2020",                    nrow(df_post),
  "gamma pre 2020 (DK)",            round(g_pre,  4),
  "SE gamma pre",                   round(s_pre,  4),
  "p gamma pre",                    round(p_pre,  4),
  "IC 95% pre - bajo",              round(ic_pre[1], 4),
  "IC 95% pre - alto",              round(ic_pre[2], 4),
  "gamma post 2020 (DK)",           round(g_post, 4),
  "SE gamma post",                  round(s_post, 4),
  "p gamma post",                   round(p_post, 4),
  "IC 95% post - bajo",             round(ic_post[1], 4),
  "IC 95% post - alto",             round(ic_post[2], 4),
  "gamma diff (interaccion)",       round(g_diff, 4),
  "SE gamma diff",                  round(s_diff, 4),
  "p gamma diff",                   round(p_diff, 4)
)

dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(resumen,
          here::here("outputs", "reportes_intermedios",
                     "D1_pre_post_2020.csv"))
print(resumen, n = Inf, width = Inf)
cat("\nGuardado: outputs/reportes_intermedios/D1_pre_post_2020.csv\n")

cat("\n---- Lectura para el memo ----\n")
cat(sprintf("- Pre 2020 : gamma = %+.4f (SE %.4f, p = %.4f).\n",
            g_pre, s_pre, p_pre))
if (!is.na(g_post)) {
  cat(sprintf("- Post 2020: gamma = %+.4f (SE %.4f, p = %.4f).\n",
              g_post, s_post, p_post))
}
if (!is.na(p_diff)) {
  cat(sprintf("- Diferencia: %+.4f (p = %.4f). %s\n",
              g_diff, p_diff,
              ifelse(p_diff < 0.10,
                     "Cambio estructural pre/post 2020 significativo.",
                     "No hay evidencia de cambio estructural.")))
}
