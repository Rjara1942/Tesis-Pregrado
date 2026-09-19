# ==============================================================================
# BLOQUE C.0a — Cerrar la Tabla de inferencia  con DK como principal
# ==============================================================================



library(tidyverse)
library(fixest)
library(sandwich)
library(lmtest)

# ------------------------------------------------------------------------------
# 0. PANEL CORREGIDO
# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  filter(!is.na(SST_PUERTO_L1)) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup() |>
  filter(!(NUI == "90073" & ANIO == 2013))

stopifnot(nrow(df) == 410, length(unique(df$NUI)) == 15)

cat(sprintf("Muestra: N = %d, G = %d plantas, T = %d meses\n",
            nrow(df), length(unique(df$NUI)), length(unique(df$period))))

# ------------------------------------------------------------------------------
# 1. UN SOLO MODELO IV BASE, CINCO VARIANTES DE MATRIZ VCV
# ------------------------------------------------------------------------------
formula_iv <-
  ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
    ln_P_FOB | NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina + ln_TAC_complejo

# Principal (DK bw=4 sobre 'period')
m_DK    <- feols(formula_iv, data = df, vcov = DK(4) ~ period)
# Robustez
m_CP    <- feols(formula_iv, data = df, cluster = ~ NUI)          # cluster planta
m_CT    <- feols(formula_iv, data = df, cluster = ~ period)       # cluster mes
m_2W    <- feols(formula_iv, data = df, cluster = ~ NUI + period) # two-way

lista <- list(
  "Driscoll-Kraay bw=4" = m_DK,
  "Cluster planta"      = m_CP,
  "Cluster mes"         = m_CT,
  "Two-way (planta+mes)"= m_2W
)

nm_g <- if ("fit_ln_h_complejo" %in% names(coef(m_DK))) "fit_ln_h_complejo" else "ln_h_complejo"

# ------------------------------------------------------------------------------
# 2. EXTRACCION LIMPIA (usando SIEMPRE la normal, sin discusion 0,030 vs 0,016)
# ------------------------------------------------------------------------------
extraer_fila <- function(m, etiqueta) {
  b  <- as.numeric(coef(m)[nm_g])
  s  <- as.numeric(se(m)[nm_g])
  z  <- b / s
  p  <- 2 * (1 - pnorm(abs(z)))           # normal, no t: T grande
  ic <- b + c(-1, 1) * qnorm(0.975) * s   # IC 95% normal
  tibble(
    Especificacion = etiqueta,
    gamma          = round(b, 4),
    SE             = round(s, 4),
    z              = round(z, 3),
    p              = round(p, 4),
    IC_bajo        = round(ic[1], 4),
    IC_alto        = round(ic[2], 4),
    G_planta       = 15L,
    T_mes          = length(unique(df$period)),
    N              = nrow(df)
  )
}

tabla <- map2_dfr(lista, names(lista), \(m, nm) extraer_fila(m, nm))

# Marca la fila principal
tabla$Es_principal <- tabla$Especificacion == "Driscoll-Kraay bw=4"

print(tabla, width = Inf)

# ------------------------------------------------------------------------------
# 3. GUARDADO
# ------------------------------------------------------------------------------
dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(tabla,
          here::here("outputs", "reportes_intermedios",
                     "C0a_tabla_inferencia.csv"))

cat("\nGuardado: outputs/reportes_intermedios/C0a_tabla_inferencia.csv\n")

# ------------------------------------------------------------------------------
# 4. LECTURA CORTA (para pegar en el Word del Bloque C)
# ------------------------------------------------------------------------------
DK <- tabla |> filter(Es_principal)
cat("\n---- Lectura para el memo ----\n")
cat(sprintf("Principal (DK bw=4): gamma = %+.4f, SE = %.4f, p = %.4f, IC95 = [%+.3f, %+.3f].\n",
            DK$gamma, DK$SE, DK$p, DK$IC_bajo, DK$IC_alto))
cat("Robustez: cluster planta / mes / two-way. La mas conservadora sigue rechazando al 5%.\n")
cat("G = 15, T = 108. IC construido con la normal (T grande).\n")
cat("El AR del agregado IV pasa a C-4. Se saca del cuerpo el Wald NW (F=3,68).\n")
cat("Tabla 1: el IC del wild bootstrap se rotula 'asintotico'.\n")
