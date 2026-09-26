# ==============================================================================
# BLOQUE C.0a v2 — Tabla de inferencia sobre nueva muestra y dos instrumentos
# ==============================================================================
# Nueva muestra: 427 obs, 16 plantas, 111 meses.
# Instrumentos: ln_biomasa_sardina + ln_TAC_complejo (anuales).
# Reporto la misma IV con cuatro matrices de varianza:
#   Driscoll Kraay bw=4 (principal), cluster planta, cluster mes, two-way.
# Sortie: outputs/reportes_intermedios/C0a_v2_tabla_inferencia.csv
# ==============================================================================

library(tidyverse)
library(fixest)

# ------------------------------------------------------------------------------
# 0. MUESTRA
# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup() |>
  filter(!(NUI == "90073" & ANIO == 2013))

cat(sprintf("Muestra: N = %d, G = %d plantas, T = %d meses\n",
            nrow(df), length(unique(df$NUI)), length(unique(df$period))))

# ------------------------------------------------------------------------------
# 1. IV BASE + CUATRO VARIANTES DE VCV
# ------------------------------------------------------------------------------
formula_iv <-
  ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                       ln_P_FOB | NUI |
    ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo

m_DK <- feols(formula_iv, data = df, vcov = DK(4) ~ period)
m_CP <- feols(formula_iv, data = df, cluster = ~ NUI)
m_CT <- feols(formula_iv, data = df, cluster = ~ period)
m_2W <- feols(formula_iv, data = df, cluster = ~ NUI + period)

lista <- list(
  "Driscoll-Kraay bw=4" = m_DK,
  "Cluster planta"       = m_CP,
  "Cluster mes"          = m_CT,
  "Two-way (planta+mes)" = m_2W
)

nm_g <- if ("fit_ln_h_complejo" %in% names(coef(m_DK)))
          "fit_ln_h_complejo" else "ln_h_complejo"

extraer_fila <- function(m, etiqueta) {
  b  <- as.numeric(coef(m)[nm_g])
  s  <- as.numeric(se(m)[nm_g])
  z  <- b / s
  p  <- 2 * (1 - pnorm(abs(z)))
  ic <- b + c(-1, 1) * qnorm(0.975) * s
  tibble(
    Especificacion = etiqueta,
    gamma          = round(b, 4),
    SE             = round(s, 4),
    z              = round(z, 3),
    p              = round(p, 4),
    IC_bajo        = round(ic[1], 4),
    IC_alto        = round(ic[2], 4),
    G_planta       = length(unique(df$NUI)),
    T_mes          = length(unique(df$period)),
    N              = nrow(df)
  )
}

tabla <- map2_dfr(lista, names(lista),
                  \(m, nm) extraer_fila(m, nm))
tabla$Es_principal <- tabla$Especificacion == "Driscoll-Kraay bw=4"

print(tabla, width = Inf)

dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(tabla,
          here::here("outputs", "reportes_intermedios",
                     "C0a_v2_tabla_inferencia.csv"))
cat("\nGuardado: outputs/reportes_intermedios/C0a_v2_tabla_inferencia.csv\n")

cat("\n---- Lectura ----\n")
DK <- tabla |> filter(Es_principal)
cat(sprintf("Principal DK bw=4: gamma = %+.4f, SE = %.4f, p = %.4f, IC = [%+.3f, %+.3f]\n",
            DK$gamma, DK$SE, DK$p, DK$IC_bajo, DK$IC_alto))
cat("Robustez con cluster planta / mes / two-way al lado.\n")
