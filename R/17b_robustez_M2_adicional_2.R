###############################################################################
#  17b_robustez_M2_adicional.R
#  ─────────────────────────────────────────────────────────────────────────────
#  ANÁLISIS DE ROBUSTEZ ADICIONAL — Modelo (TAC + biomasa + ambientales)
#
#  Cuatro bloques:
#    A. Prueba Anderson-Rubin (AR) para instrumentos débiles
#    B. Multicolinealidad FOB × Tendencia: estimación sin tendencia + VIF
#    C. Correlación parcial FOB–precio controlando desembarques (serie ag.)
#    D. IC bootstrap y alternativas a fwildclusterboot:
#        
###############################################################################

# ── helpers de instalación silenciosa ────────────────────────────────────────
auto_install <- function(pkgs) {
  nuevos <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(nuevos) > 0) {
    message("Instalando: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, quiet = TRUE)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

auto_install(c("fixest", "sandwich", "clubSandwich",
               "ivreg", "lmtest", "boot", "dplyr", "tidyr",
               "readxl", "stringr"))

# ─────────────────────────────────────────────────────────────────────────────
# 0. VERIFICAR/RECONSTRUIR DATOS Y MODELO M2
# ─────────────────────────────────────────────────────────────────────────────

if (!exists("panel") || !exists("m2")) {
  message("Objetos 'panel' y/o 'm2' no encontrados — re-estimando desde CSV.")

  # Carga mínima del panel (ajustar ruta si es necesario)
  panel <- tryCatch(
    read.csv("panel_con_alternativas.csv"),
    error = function(e) stop("No se encontró panel_con_alternativas.csv: ", e$message)
  )

  # TAC (si el archivo existe; si no, se omite ln_TAC_complejo del instrumento)
  tac_ok <- FALSE
  if (file.exists("TAC_anual.xlsx")) {
    tac_ind_raw <- read_excel("TAC_anual.xlsx", sheet = "industrial")
    names(tac_ind_raw) <- c("year", "recurso", "unidad", "cuota")
    tac_ind <- tac_ind_raw %>%
      mutate(recurso_lower = str_to_lower(str_trim(recurso)),
             unidad = str_trim(unidad))
    tac_ind_complejo <- tac_ind %>%
      filter(recurso_lower %in% c("anchoveta", "sardina común", "sardina comun"),
             str_detect(unidad, regex("V.*X|V-X|V - X|V -X", ignore_case = TRUE))) %>%
      group_by(year) %>%
      summarise(TAC_complejo = sum(cuota, na.rm = TRUE), .groups = "drop") %>%
      mutate(ln_TAC_complejo = log(TAC_complejo))
    panel <- left_join(panel, tac_ind_complejo, by = c("ANIO" = "year"))
    tac_ok <- TRUE
  }

  panel <- panel %>%
    mutate(POST_2020 = ifelse(ANIO >= 2020, 1, 0),
           ln_h_X_POST = ln_h_complejo * POST_2020)

  # Instrumento condicional al TAC
  inst_rhs <- if (tac_ok) {
    ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina + ln_TAC_complejo
  } else {
    ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina
  }

  m2 <- feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA |
      NUI |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo,
    data = panel, vcov = ~NUI
  )
}

# Extraer info básica del modelo
gamma_m2  <- coef(m2)["fit_ln_h_complejo"]
se_cr1_m2 <- se(m2)["fit_ln_h_complejo"]
n_obs     <- nobs(m2)
clusters  <- unique(panel$NUI)
n_clus    <- length(clusters)

cat("\n", strrep("═", 70), "\n")
cat("  ANÁLISIS DE ROBUSTEZ ADICIONAL — M2\n")
cat("  γ =", round(gamma_m2, 4),
    "| SE_CR1 =", round(se_cr1_m2, 4),
    "| N =", n_obs,
    "| G (clusters) =", n_clus, "\n")
cat(strrep("═", 70), "\n\n")


###############################################################################
# A. PRUEBA DE ANDERSON-RUBIN (AR)
#    Complementa el F de primera etapa para el caso de instrumentos débiles.

###############################################################################

cat(strrep("─", 70), "\n")
cat("  A. PRUEBA ANDERSON-RUBIN (AR)\n")
cat(strrep("─", 70), "\n\n")

# ── A.1  Identificar variables del modelo ────────────────────────────────────
# Variables en segunda etapa (sin FE, sin endógena)
ctrl_2s  <- c("ln_P_FOB", "ln_h_jurel", "SEASON_SIN", "SEASON_COS", "TENDENCIA")
# Instrumentos excluidos (los que identifican h_complejo)
instr    <- c("SO_PUERTO", "SST_PUERTO_L1", "ln_biomasa_sardina", "ln_TAC_complejo")
instr    <- instr[instr %in% names(panel)]   # solo los disponibles
n_instr  <- length(instr)

# Datos completos para todas las variables relevantes
vars_req <- c("ln_P_complejo", "ln_h_complejo", ctrl_2s, instr, "NUI")
vars_req <- vars_req[vars_req %in% names(panel)]
dat_ar   <- panel[complete.cases(panel[, vars_req]), ]

# ── A.2  Primera etapa para obtener h_hat ────────────────────────────────────
fe_1st <- feols(
  as.formula(paste("ln_h_complejo ~",
                   paste(c(ctrl_2s, instr), collapse = " + "),
                   "| NUI")),
  data = dat_ar
)

# feols elimina singletons antes de ajustar: obs(fe_1st) devuelve los índices
# (posición en dat_ar, base-1) de las filas efectivamente usadas.
# Hay que restringir dat_ar a esas filas ANTES de calcular los demeaned vectors,
# de lo contrario fitted() tiene menos filas que dat_ar.
used_idx <- obs(fe_1st)                      # enteros: posiciones en dat_ar
dat_ar   <- dat_ar[used_idx, ]               # descartar singletons
dat_ar$h_hat <- fitted(fe_1st)               # ahora las dimensiones coinciden

# ── A.3  Grilla de γ0 y estadístico AR ───────────────────────────────────────
gamma_grid <- seq(-1.2, 0.3, by = 0.005)
ar_stats   <- numeric(length(gamma_grid))

# Eliminar efecto fijo para el cálculo AR (demeaning by NUI)
demean_fe <- function(x, id) {
  x - ave(x, id, FUN = mean)
}

y_dm  <- demean_fe(dat_ar$ln_P_complejo, dat_ar$NUI)
h_dm  <- demean_fe(dat_ar$ln_h_complejo, dat_ar$NUI)

ctrl_dm_list <- lapply(ctrl_2s[ctrl_2s %in% names(dat_ar)], function(v)
  demean_fe(dat_ar[[v]], dat_ar$NUI))
names(ctrl_dm_list) <- ctrl_2s[ctrl_2s %in% names(dat_ar)]
ctrl_mat <- do.call(cbind, ctrl_dm_list)

instr_dm_list <- lapply(instr, function(v)
  demean_fe(dat_ar[[v]], dat_ar$NUI))
names(instr_dm_list) <- instr
instr_mat <- do.call(cbind, instr_dm_list)

n_ar <- nrow(dat_ar)

for (i in seq_along(gamma_grid)) {
  g0     <- gamma_grid[i]
  y_til  <- y_dm - g0 * h_dm            # y̆ = y − γ₀·h (demeaned)

  # Proyectar y_til sobre ctrl, obtener residuos
  if (ncol(ctrl_mat) > 0) {
    fit_ctrl <- lm.fit(cbind(1, ctrl_mat), y_til)
    r_til    <- fit_ctrl$residuals
  } else {
    r_til <- y_til
  }

  # F de instrumentos en la regresión y_tild ~ instr + ctrl
  fit_full <- lm.fit(cbind(1, ctrl_mat, instr_mat), y_til)
  rss_r    <- sum(r_til^2)
  rss_u    <- sum(fit_full$residuals^2)

  ar_stats[i] <- ((rss_r - rss_u) / n_instr) /
                 (rss_u / (n_ar - ncol(ctrl_mat) - n_instr - 1))
}

# ── A.4  Conjunto de confianza AR al 95 % ────────────────────────────────────
chi2_crit_f <- qf(0.95, df1 = n_instr, df2 = n_ar - ncol(ctrl_mat) - n_instr - 1)
chi2_crit   <- qchisq(0.95, df = n_instr)   # versión chi-cuadrado

ar_in_cs    <- gamma_grid[ar_stats <= chi2_crit_f]
ar_ci_lo    <- if (length(ar_in_cs) > 0) min(ar_in_cs) else NA
ar_ci_hi    <- if (length(ar_in_cs) > 0) max(ar_in_cs) else NA

# AR puntual en γ = 0 (test de exclusión global)
ar_at_zero  <- ar_stats[which.min(abs(gamma_grid - 0))]
p_ar_zero   <- pf(ar_at_zero, df1 = n_instr,
                  df2 = n_ar - ncol(ctrl_mat) - n_instr - 1,
                  lower.tail = FALSE)

# AR puntual en γ̂_M2 (debe ser mínimo de la curva)
ar_at_ghat  <- ar_stats[which.min(abs(gamma_grid - gamma_m2))]
p_ar_ghat   <- pf(ar_at_ghat, df1 = n_instr,
                  df2 = n_ar - ncol(ctrl_mat) - n_instr - 1,
                  lower.tail = FALSE)

cat("Instrumentos excluidos usados:", paste(instr, collapse = ", "), "\n")
cat("k (# instrumentos) =", n_instr, "\n\n")
cat("Anderson-Rubin en γ₀ = 0:\n")
cat("  F_AR =", round(ar_at_zero, 3), "| p =", round(p_ar_zero, 4), "\n")
cat("  → Se rechaza H0: γ = 0 al 5%?",
    ifelse(p_ar_zero < 0.05, "SÍ ✓", "NO ✗"), "\n\n")
cat("Anderson-Rubin en γ̂_M2 =", round(gamma_m2, 4), ":\n")
cat("  F_AR =", round(ar_at_ghat, 3), "| p =", round(p_ar_ghat, 4), "\n")
cat("  → γ̂ pertenece al CS AR al 95%?",
    ifelse(p_ar_ghat >= 0.05, "SÍ ✓", "NO (inusual)"), "\n\n")
cat("Conjunto de Confianza AR al 95% (inversión de test F):\n")
cat("  [", round(ar_ci_lo, 4), ",", round(ar_ci_hi, 4), "]\n")
cat("  IC Wald CR1 95% = [",
    round(gamma_m2 - 1.96 * se_cr1_m2, 4), ",",
    round(gamma_m2 + 1.96 * se_cr1_m2, 4), "]\n\n")

cat("Interpretación:\n")
cat("  Si CS_AR aprox. IC_Wald  -> instrumentos fuertes (identificacion robusta).\n")
cat("  Si CS_AR >> IC_Wald      -> instrumentos debiles (usar AR como IC principal).\n")
cat("  Si CS_AR es la recta real -> instrumentos no identifican gamma.\n\n")

# Diagnostico automatico cuantitativo
wald_lo  <- gamma_m2 - 1.96 * se_cr1_m2
wald_hi  <- gamma_m2 + 1.96 * se_cr1_m2
wald_w   <- wald_hi - wald_lo
ar_w     <- if (!is.na(ar_ci_lo)) ar_ci_hi - ar_ci_lo else NA

if (!is.na(ar_w)) {
  ratio_w <- ar_w / wald_w
  cat(sprintf("  Amplitud CS_AR = %.4f | Amplitud IC Wald = %.4f | Ratio = %.2f\n",
              ar_w, wald_w, ratio_w))

  shift_lo <- ar_ci_lo - wald_lo
  shift_hi <- ar_ci_hi - wald_hi
  cat(sprintf("  Desplazamiento extremo inferior: %+.4f  |  superior: %+.4f\n",
              shift_lo, shift_hi))

  if (ratio_w < 1.5) {
    cat("  OK CS_AR y IC Wald muy similares -> identificacion fuerte.\n\n")
  } else if (ratio_w < 3) {
    cat("  AVISO CS_AR algo mas amplio -> identificacion moderada.\n")
    cat("  Reportar AMBOS intervalos (Wald + AR) en el texto.\n\n")
  } else {
    cat("  PROBLEMA CS_AR mucho mas amplio -> evidencia de instrumentos debiles.\n")
    cat("  El IC AR debe ser el resultado PRINCIPAL.\n\n")
  }

  wald_excluye <- wald_lo > 0 | wald_hi < 0
  ar_excluye   <- ar_ci_lo > 0 | ar_ci_hi < 0
  cat(sprintf("  IC Wald excluye 0: %s  |  CS AR excluye 0: %s\n",
              ifelse(wald_excluye, "SI", "NO"),
              ifelse(ar_excluye,   "SI", "NO")))
  if (wald_excluye && ar_excluye)
    cat("  -> Conclusion gamma < 0 (poder de monopsonio) es DOBLEMENTE robusta.\n\n")
}

# ── A.5  F de primera etapa ────────────────────────────────────────────────
cat("F de primera etapa:\n")
tryCatch({
  fstat_iv <- fitstat(m2, "ivf")$ivf$stat
  cat("  F (Cragg-Donald Wald, fixest) =", round(fstat_iv, 2), "\n")
  cat("  Umbral Stock-Yogo 10% sesgo: 10.0 | 5%: 16.38\n")
  if (fstat_iv >= 16.38) {
    cat("  OK F >= 16.38: maximo sesgo IV relativo a OLS < 5%.\n")
  } else if (fstat_iv >= 10.0) {
    cat("  OK F >= 10: convencion minima superada.\n")
    cat("  -> Complementar con CS_AR para inferencia robusta.\n")
  } else {
    cat("  PROBLEMA F < 10: instrumentos debiles. Usar CS_AR como IC principal.\n")
  }
}, error = function(e) cat("  fitstat ivf no disponible\n"))

tryCatch({
  fstat_kp <- fitstat(m2, "kpr")$kpr$stat
  cat("  Kleibergen-Paap rk F =", round(fstat_kp, 2), "\n")
}, error = function(e) {
  cat("  Kleibergen-Paap no disponible via fixest.\n")
  cat("  Alternativa: ivreg::ivreg() + summary(..., diagnostics=TRUE)\n")
  cat("  o ivDiag::ivDiag() que reporta KP, AR y tF en una sola llamada.\n")
})

cat("\n")


###############################################################################
# B. MULTICOLINEALIDAD FOB × TENDENCIA
#    B.1  VIF en primera etapa (reducción)
#    B.2  Estimación M2 sin TENDENCIA → ¿β_FOB se vuelve significativo?
#    B.3  M2 con tendencia cuadrática (alternativa flexible)
#    B.4  M2 con year-FE en lugar de tendencia lineal
###############################################################################

cat(strrep("─", 70), "\n")
cat("  B. MULTICOLINEALIDAD FOB × TENDENCIA\n")
cat(strrep("─", 70), "\n\n")

# ── B.1  Correlación simple FOB–Tendencia ────────────────────────────────────
ts_fob_tend <- panel %>%
  group_by(TENDENCIA) %>%
  summarise(fob = mean(ln_P_FOB, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(fob))

cor_ft <- cor(ts_fob_tend$TENDENCIA, ts_fob_tend$fob, use = "complete.obs")
cat("B.1  Correlación simple ln_P_FOB ~ TENDENCIA (agregado mensual):",
    round(cor_ft, 4), "\n")
cat("     R² =", round(cor_ft^2, 4), "\n")
if (abs(cor_ft) > 0.80)
  cat("     ⚠  Alta correlación (|r| > 0.8). Riesgo de colinealidad.\n\n")

# VIF aproximado: regresión OLS de segunda etapa con FE absorbido
# (usamos residuos del modelo sin esa variable)
ols_plena <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_complejo + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA | NUI,
  data = panel
)

# VIF manual para TENDENCIA y FOB (proyección en el resto de regresores)
calc_vif <- function(var, regresores, datos) {
  datos_c <- datos[complete.cases(datos[, c(var, regresores)]), ]
  fm <- as.formula(paste(var, "~",
                         paste(setdiff(regresores, var), collapse = " + ")))
  r2 <- summary(lm(fm, data = datos_c))$r.squared
  1 / (1 - r2)
}

regresores_vif <- c("ln_P_FOB", "TENDENCIA", "SEASON_SIN", "SEASON_COS",
                    "ln_h_jurel")
regresores_vif <- regresores_vif[regresores_vif %in% names(panel)]

cat("B.1  VIF aproximados (segunda etapa, sin FE):\n")
vif_df <- data.frame(
  Variable = regresores_vif,
  VIF = sapply(regresores_vif, calc_vif,
               regresores = regresores_vif, datos = panel)
)
print(vif_df, digits = 3, row.names = FALSE)
cat("     VIF > 10 indica multicolinealidad severa.\n\n")

# ── B.2  M2 sin TENDENCIA ────────────────────────────────────────────────────
cat("B.2  M2 sin TENDENCIA:\n")
m2_notrend <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                    ln_TAC_complejo,
  data = panel, vcov = ~NUI
)

beta_fob_trend   <- coef(m2)["ln_P_FOB"]
beta_fob_notrend <- coef(m2_notrend)["ln_P_FOB"]
se_fob_trend     <- se(m2)["ln_P_FOB"]
se_fob_notrend   <- se(m2_notrend)["ln_P_FOB"]
gamma_notrend    <- coef(m2_notrend)["fit_ln_h_complejo"]
se_gamma_notrend <- se(m2_notrend)["fit_ln_h_complejo"]

cat(sprintf("  %-20s  β_FOB = %7.4f  (SE = %6.4f)  p = %.4f\n",
            "Con tendencia:",
            beta_fob_trend, se_fob_trend,
            2 * pnorm(-abs(beta_fob_trend / se_fob_trend))))
cat(sprintf("  %-20s  β_FOB = %7.4f  (SE = %6.4f)  p = %.4f\n",
            "Sin tendencia:",
            beta_fob_notrend, se_fob_notrend,
            2 * pnorm(-abs(beta_fob_notrend / se_fob_notrend))))
cat(sprintf("  Cambio β_FOB al eliminar tendencia: Δ = %+.4f\n",
            beta_fob_notrend - beta_fob_trend))
cat(sprintf("  γ sin tendencia = %.4f (vs %.4f con tendencia)\n",
            gamma_notrend, gamma_m2))
cat(sprintf("  F 1ª etapa (sin trend) = %.2f\n\n",
            tryCatch(fitstat(m2_notrend, "ivf")$ivf$stat, error = function(e) NA)))

if (abs(beta_fob_notrend) > abs(beta_fob_trend) &&
    (2 * pnorm(-abs(beta_fob_notrend / se_fob_notrend))) < 0.05) {
  cat("  ✓ β_FOB se vuelve significativo al 5% al eliminar la tendencia.\n")
  cat("  → La tendencia absorbe parte de la variación del FOB. Multicolinealidad confirmada.\n\n")
} else if ((2 * pnorm(-abs(beta_fob_notrend / se_fob_notrend))) >= 0.05) {
  cat("  → β_FOB sigue siendo no significativo sin tendencia.\n")
  cat("  → El resultado β ≈ 0 no se debe a multicolinealidad con la tendencia.\n\n")
}

# ── B.3  M2 con tendencia cuadrática ─────────────────────────────────────────
cat("B.3  M2 con tendencia cuadrática (TENDENCIA + TENDENCIA²):\n")
panel$TENDENCIA2 <- panel$TENDENCIA^2

m2_trend2 <- tryCatch({
  feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA + TENDENCIA2 |
      NUI |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo,
    data = panel, vcov = ~NUI
  )
}, error = function(e) { cat("  Error trend2:", e$message, "\n"); NULL })

if (!is.null(m2_trend2)) {
  gamma_t2   <- coef(m2_trend2)["fit_ln_h_complejo"]
  beta_fob_t2 <- coef(m2_trend2)["ln_P_FOB"]
  cat(sprintf("  γ = %.4f | β_FOB = %.4f | F 1ª etapa = %.2f\n",
              gamma_t2, beta_fob_t2,
              tryCatch(fitstat(m2_trend2, "ivf")$ivf$stat, error = function(e) NA)))
}

# ── B.4  M2 con year-FE en lugar de tendencia lineal ─────────────────────────
cat("\nB.4  M2 con year-FE (absorbe tendencia no paramétrica):\n")
m2_yearfe <- tryCatch({
  feols(
    ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS |
      NUI + ANIO |
      ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                      ln_TAC_complejo,
    data = panel, vcov = ~NUI
  )
}, error = function(e) { cat("  Error year-FE:", e$message, "\n"); NULL })

if (!is.null(m2_yearfe)) {
  gamma_yfe  <- coef(m2_yearfe)["fit_ln_h_complejo"]
  beta_fob_yfe <- tryCatch(coef(m2_yearfe)["ln_P_FOB"], error = function(e) NA)
  cat(sprintf("  γ = %.4f | β_FOB = %.4f (absorbido si NA) | F 1ª et = %.2f\n",
              gamma_yfe,
              ifelse(is.na(beta_fob_yfe), NA, beta_fob_yfe),
              tryCatch(fitstat(m2_yearfe, "ivf")$ivf$stat, error = function(e) NA)))
  cat("  Nota: β_FOB queda absorbido por year-FE si FOB sólo varía entre años.\n")
}

cat("\n")


###############################################################################
# C. CORRELACIÓN PARCIAL FOB–PRECIO (controlando desembarques)
#    Serie de tiempo agregada mensual.
#    Procedimiento:
#      1. Destilar ln_P_FOB y ln_P_complejo del efecto de ln_h_complejo
#         y controles (regresión parcialización, Frisch-Waugh)
#      2. Calcular correlación de los residuos
#      3. Gráfico de dispersión + regresión
###############################################################################

cat(strrep("─", 70), "\n")
cat("  C. CORRELACIÓN PARCIAL FOB–PRECIO (SERIE TIEMPO AGREGADA)\n")
cat(strrep("─", 70), "\n\n")

# ── C.1  Serie de tiempo agregada ────────────────────────────────────────────
ts_data <- panel %>%
  arrange(ANIO, MES) %>%
  group_by(ANIO, MES) %>%
  summarise(
    ln_P_complejo    = mean(ln_P_complejo,    na.rm = TRUE),
    ln_h_complejo    = mean(ln_h_complejo,    na.rm = TRUE),
    ln_P_FOB         = mean(ln_P_FOB,         na.rm = TRUE),
    ln_h_jurel       = first(ln_h_jurel),
    SEASON_SIN       = first(SEASON_SIN),
    SEASON_COS       = first(SEASON_COS),
    TENDENCIA        = first(TENDENCIA),
    .groups = "drop"
  ) %>%
  filter(complete.cases(.))

n_ts <- nrow(ts_data)
cat("Serie agregada: T =", n_ts, "meses\n\n")

# ── C.2  Parcialización Frisch-Waugh ─────────────────────────────────────────
# Controles: ln_h_complejo + estacionalidad + tendencia
ctrl_fw <- c("ln_h_complejo", "SEASON_SIN", "SEASON_COS", "TENDENCIA",
             "ln_h_jurel")
ctrl_fw <- ctrl_fw[ctrl_fw %in% names(ts_data)]

fm_ctrl <- as.formula(paste("~", paste(ctrl_fw, collapse = " + ")))

# Residuos de FOB sobre controles
fob_resid <- lm(update(fm_ctrl, ln_P_FOB ~ .), data = ts_data)$residuals

# Residuos de precio sobre controles
p_resid   <- lm(update(fm_ctrl, ln_P_complejo ~ .), data = ts_data)$residuals

# Correlación parcial
r_partial <- cor(p_resid, fob_resid, use = "complete.obs")
n_r       <- sum(complete.cases(cbind(p_resid, fob_resid)))
t_r       <- r_partial * sqrt((n_r - 2) / (1 - r_partial^2))
p_r       <- 2 * pt(abs(t_r), df = n_r - 2, lower.tail = FALSE)

cat("C.2  Correlación parcial FOB–precio (controlando desembarques y controles):\n")
cat("     r_parcial =", round(r_partial, 4), "\n")
cat("     t =", round(t_r, 3), "| p =", round(p_r, 4),
    "| df =", n_r - 2, "\n")
cat("     R² parcial =", round(r_partial^2, 4), "\n\n")

if (abs(r_partial) < 0.10 || p_r >= 0.10) {
  cat("  → Correlación parcial pequeña y/o no significativa.\n")
  cat("  → El FOB no explica variación adicional en el precio una vez que\n")
  cat("     se controla por los desembarques y la tendencia.\n")
  cat("  → Apoya el resultado β ≈ 0 en el modelo de panel.\n\n")
} else {
  cat("  → Correlación parcial significativa.\n")
  cat("  → El FOB sí tiene contenido informativo independiente.\n")
  cat("  → Revisar especificación del panel o instrumentalizar el FOB.\n\n")
}

# ── C.3  Regresión OLS de precio sobre FOB (serie tiempo) con NW ─────────────
cat("C.3  Regresión IV serie tiempo — β_FOB con Newey-West (HAC):\n")
ts_iv <- tryCatch({
  # Variables de instrumentos a nivel mensual si están disponibles
  inst_ts <- intersect(c("SO_PUERTO", "SST_PUERTO_L1",
                         "ln_biomasa_sardina", "ln_TAC_complejo"),
                       names(ts_data))
  if (length(inst_ts) >= 1) {
    iv_form <- as.formula(paste(
      "ln_P_complejo ~ ln_h_complejo + ln_P_FOB + ln_h_jurel +",
      "SEASON_SIN + SEASON_COS + TENDENCIA |",
      paste(c(inst_ts, "ln_P_FOB", "ln_h_jurel",
              "SEASON_SIN", "SEASON_COS", "TENDENCIA"), collapse = " + ")
    ))
    # Merge de instrumentos si no están en ts_data
    for (v in inst_ts) {
      if (!(v %in% names(ts_data))) {
        ts_data[[v]] <- panel %>%
          group_by(ANIO, MES) %>%
          summarise(tmp = mean(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
          right_join(ts_data[, c("ANIO", "MES")], by = c("ANIO", "MES")) %>%
          pull(tmp)
      }
    }
    iv_ts <- ivreg(iv_form, data = ts_data)
    nw_se <- tryCatch(
      sqrt(diag(sandwich::NeweyWest(iv_ts, lag = floor(n_ts^(1/4))))),
      error = function(e) sqrt(diag(vcov(iv_ts)))
    )
    cat(sprintf("  β_FOB (IV, NW) = %.4f | SE_NW = %.4f | t = %.3f | p = %.4f\n",
                coef(iv_ts)["ln_P_FOB"],
                nw_se["ln_P_FOB"],
                coef(iv_ts)["ln_P_FOB"] / nw_se["ln_P_FOB"],
                2 * pt(abs(coef(iv_ts)["ln_P_FOB"] / nw_se["ln_P_FOB"]),
                       df = n_ts - length(coef(iv_ts)), lower.tail = FALSE)))
    cat(sprintf("  γ   (IV, NW) = %.4f | SE_NW = %.4f\n",
                coef(iv_ts)["ln_h_complejo"],
                nw_se["ln_h_complejo"]))
  } else {
    cat("  Instrumentos no disponibles en serie tiempo. Saltando IV.\n")
  }
  TRUE
}, error = function(e) {
  cat("  ivreg no disponible o error:", e$message, "\n")
  FALSE
})

cat("\n")


###############################################################################
# D. INTERVALOS DE CONFIANZA — ALTERNATIVAS A fwildclusterboot
#
#    D.1  CR2 (Bell-McCaffrey) via clubSandwich — robusto con pocos clusters
#    D.2  Wild bootstrap percentile-t (manual, via boot)
#    D.3  Pairs cluster bootstrap (muestreo de clusters completos, via boot)
#    D.4  Resumen comparativo de todos los SE / IC
###############################################################################

cat(strrep("─", 70), "\n")
cat("  D. IC BOOTSTRAP Y ALTERNATIVAS A fwildclusterboot\n")
cat(strrep("─", 70), "\n\n")
# ── Preparación: modelo base sin fixest para bootstrap ───────────────────────
# Necesitamos un objeto que soporte el bootstrapping manual.
# Usamos ivreg con dummies de planta (within demean no es re-estimable fácil).
# Alternativa: feols y re-estimar en cada réplica.

# Construir dummies de planta (dentro de las disponibles)
n_plantas <- n_clus
cat("G =", n_plantas, "clusters (plantas).\n")
cat("Nota: con G < 20, CR2 > CR1 > asintótico en términos de SE.\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# D.1  CR2 (Bell-McCaffrey) via clubSandwich + ivreg
# ─────────────────────────────────────────────────────────────────────────────
# clubSandwich NO tiene soporte para objetos fixest/feols directamente:
# su metodo interno de proyeccion llama operaciones matriciales sobre
# model.matrix() que fixest no expone en el formato esperado, produciendo
# "Non-numeric argument to mathematical function".
#
# Solucion: re-estimar el mismo modelo con ivreg::ivreg(), que SI tiene
# soporte nativo en clubSandwich. Los efectos fijos de planta se incluyen
# como dummies explicitas (factor(NUI)) para replicar el within-estimator.
# ─────────────────────────────────────────────────────────────────────────────
# ─────────────────────────────────────────────────────────────────────────────
# D.1  CR2 (Bell-McCaffrey) via clubSandwich + ivreg
# ─────────────────────────────────────────────────────────────────────────────
# clubSandwich NO tiene soporte para objetos fixest/feols directamente:
# su metodo interno de proyeccion llama operaciones matriciales sobre
# model.matrix() que fixest no expone en el formato esperado, produciendo
# "Non-numeric argument to mathematical function".
#
# Solucion: re-estimar el mismo modelo con ivreg::ivreg(), que SI tiene
# soporte nativo en clubSandwich. Los efectos fijos de planta se incluyen
# como dummies explicitas (factor(NUI)) para replicar el within-estimator.
# ─────────────────────────────────────────────────────────────────────────────
cat("D.1  CR2 — Bell-McCaffrey (ivreg + clubSandwich)\n")

cr2_result <- tryCatch({
  
  # ── Datos sin el singleton ──────────────────────────────────────────────────
  dat_cr2 <- panel[obs(m2), ]
  
  # ── Within-demeaning manual ─────────────────────────────────────────────────
  dm <- function(x, id) x - ave(x, id, FUN = mean)
  g  <- dat_cr2$NUI
  
  dat_cr2$P_dm    <- dm(dat_cr2$ln_P_complejo,    g)
  dat_cr2$h_dm    <- dm(dat_cr2$ln_h_complejo,    g)
  dat_cr2$fob_dm  <- dm(dat_cr2$ln_P_FOB,         g)
  dat_cr2$jur_dm  <- dm(dat_cr2$ln_h_jurel,       g)
  dat_cr2$sin_dm  <- dm(dat_cr2$SEASON_SIN,       g)
  dat_cr2$cos_dm  <- dm(dat_cr2$SEASON_COS,       g)
  dat_cr2$ten_dm  <- dm(dat_cr2$TENDENCIA,        g)
  dat_cr2$so_dm   <- dm(dat_cr2$SO_PUERTO,        g)
  dat_cr2$sst_dm  <- dm(dat_cr2$SST_PUERTO_L1,    g)
  dat_cr2$bio_dm  <- dm(dat_cr2$ln_biomasa_sardina, g)
  dat_cr2$tac_dm  <- dm(dat_cr2$ln_TAC_complejo,  g)
  
  # ── ivreg sobre variables demeadas ──────────────────────────────────────────
  m2_ivreg <- ivreg::ivreg(
    P_dm ~ h_dm + fob_dm + jur_dm + sin_dm + cos_dm + ten_dm - 1 |
      so_dm + sst_dm + bio_dm + tac_dm +
      fob_dm + jur_dm + sin_dm + cos_dm + ten_dm - 1,
    data = dat_cr2
  )
  
  # ── CORRECCIÓN DE GRADOS DE LIBERTAD (Frisch-Waugh-Lovell) ──────────────────
  # ivreg no sabe que consumimos G grados de libertad al hacer demeaning manual.
  # Ajustamos df.residual para que Satterthwaite calcule bien sus df efectivos.
  G_plantas <- length(unique(dat_cr2$NUI))
  m2_ivreg$df.residual <- m2_ivreg$df.residual - G_plantas
  
  # ── CR2 con clustering por NUI ──────────────────────────────────────────────
  cr2_test <- clubSandwich::coef_test(
    m2_ivreg,
    vcov    = "CR2",
    cluster = dat_cr2$NUI,
    test    = "Satterthwaite"
  )
  
  # Convertir a data.frame puro para evitar problemas de clases ocultas
  df_res <- as.data.frame(cr2_test)
  
  # 1. Encontrar la fila (puede estar en los rownames o en la Columna 1)
  idx_gamma <- which(rownames(df_res) == "h_dm")
  if (length(idx_gamma) == 0) {
    idx_gamma <- which(df_res[[1]] == "h_dm")
  }
  if (length(idx_gamma) == 0) {
    cat("Columnas disponibles:", paste(names(df_res), collapse=", "), "\n")
    stop("Coeficiente h_dm no encontrado.")
  }
  
  row_data <- df_res[idx_gamma, , drop = FALSE]
  
  # 2. Búsqueda inteligente de columnas por nombre (ignorando mayúsculas/minúsculas)
  col_est <- grep("beta|Estimate|est", names(row_data), ignore.case = TRUE)[1]
  col_se  <- grep("SE", names(row_data), ignore.case = TRUE)[1]
  col_df  <- grep("df|d\\.f\\.", names(row_data), ignore.case = TRUE)[1]
  col_t   <- grep("tstat|t-stat", names(row_data), ignore.case = TRUE)[1]
  col_p   <- grep("p_Sat|p-val|p\\.value", names(row_data), ignore.case = TRUE)[1]
  
  # 3. Extracción segura
  gamma_cr2 <- as.numeric(row_data[[col_est]])
  se_cr2    <- as.numeric(row_data[[col_se]])
  df_cr2    <- as.numeric(row_data[[col_df]])
  t_cr2     <- as.numeric(row_data[[col_t]])
  p_cr2     <- as.numeric(row_data[[col_p]])
  
  # Verificación de seguridad para los grados de libertad
  if(is.na(df_cr2) || df_cr2 <= 0) {
    stop("Error: Grados de libertad (df) inválidos o <= 0. No se puede calcular el intervalo.")
  }
  
  # Cálculo del intervalo de confianza
  ci_cr2    <- c(gamma_cr2 - qt(0.975, df_cr2) * se_cr2,
                 gamma_cr2 + qt(0.975, df_cr2) * se_cr2)
  
  cat(sprintf("  γ = %.4f | SE_CR2 = %.4f | df_Sat = %.1f | t = %.3f | p = %.4f\n",
              gamma_cr2, se_cr2, df_cr2, t_cr2, p_cr2))
  cat(sprintf("  IC CR2 95%%: [%.4f, %.4f]\n", ci_cr2[1], ci_cr2[2]))
  
  list(gamma = gamma_cr2, se = se_cr2, df = df_cr2, ci = ci_cr2, p = p_cr2)
  
}, error = function(e) {
  cat("  Error CR2:", e$message, "\n\n")
  NULL
})
  

# ─────────────────────────────────────────────────────────────────────────────
# D.2  Wild Bootstrap percentile-t (manual, sin fwildclusterboot)
#      Implementación via boot::boot con perturbación Rademacher por cluster
# ─────────────────────────────────────────────────────────────────────────────
cat("D.2  Wild Bootstrap percentile-t (Rademacher, B = 999)\n")

set.seed(2024)
B_wild <- 999

# Wild bootstrap IV corregido: perturbacion sobre residuos de la FORMA REDUCIDA
# (primera etapa), no sobre residuos estructurales.
#
# Fundamento (MacKinnon & Webb 2018, p.115):
#   Perturbar y* = yhat + eps_struct * w genera distribuciones degeneradas en IV
#   porque los instrumentos quedan fijos y la endogena no varía.
#   La solucion correcta es perturbar v* = vhat + eps_1st * w (residuos 1a etapa),
#   reconstruir h* = hhat + v*, y luego re-estimar el modelo estructural completo.
#
wild_boot_iv <- function(B, seed = 2024) {
  set.seed(seed)

  # ── Indices y clusters usados en m2 ──────────────────────────────────────
  idx_m2  <- obs(m2)                        # posiciones en panel (longitud 418)
  nui_m2  <- panel$NUI[idx_m2]             # cluster de cada obs
  clus_m2 <- unique(nui_m2)
  G_m2    <- length(clus_m2)

  # ── Primera etapa: residuos de la forma reducida ──────────────────────────
  dat_wb <- panel[idx_m2, ]               # subconjunto sin singletons

  fe_1st_wb <- feols(
    ln_h_complejo ~ ln_P_FOB + ln_h_jurel +
      SEASON_SIN + SEASON_COS + TENDENCIA +
      SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina + ln_TAC_complejo |
      NUI,
    data = dat_wb
  )
  h_hat_wb  <- fitted(fe_1st_wb)          # valores ajustados 1a etapa
  eps_1st   <- residuals(fe_1st_wb)       # residuos forma reducida
  h_obs     <- dat_wb$ln_h_complejo       # desembarque observado

  # Precio observado (para re-estimar segunda etapa)
  p_obs <- dat_wb$ln_P_complejo

  gamma_boot <- numeric(B)
  t_boot     <- numeric(B)

  for (b in seq_len(B)) {
    # Pesos Rademacher por cluster (G_m2 clusters en dat_wb)
    w_g      <- sample(c(-1, 1), G_m2, replace = TRUE)
    names(w_g) <- as.character(clus_m2)
    eps_star <- eps_1st * w_g[as.character(nui_m2)]

    # h* = h_hat + eps_1st*  (residuo perturbado, centrado en ajustado)
    h_star <- h_hat_wb + eps_star

    # Reconstruir panel bootstrap con h* en lugar de h observado
    dat_b <- dat_wb
    dat_b$ln_h_complejo <- h_star

    # Re-estimar modelo estructural completo con h* como endogena
    fit_b <- tryCatch(
      suppressWarnings(feols(
        ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
          SEASON_SIN + SEASON_COS + TENDENCIA |
          NUI |
          ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                          ln_biomasa_sardina + ln_TAC_complejo,
        data = dat_b, vcov = ~NUI
      )),
      error = function(e) NULL
    )

    if (!is.null(fit_b)) {
      gamma_b        <- coef(fit_b)["fit_ln_h_complejo"]
      se_b           <- se(fit_b)["fit_ln_h_complejo"]
      gamma_boot[b]  <- gamma_b
      t_boot[b]      <- (gamma_b - gamma_m2) / se_b   # t centrado en gamma_hat
    } else {
      gamma_boot[b]  <- NA
      t_boot[b]      <- NA
    }
  }

  list(gamma = gamma_boot[!is.na(gamma_boot)],
       t     = t_boot[!is.na(t_boot)])
}

wb_result <- tryCatch({
  wb <- wild_boot_iv(B = B_wild)

  # Percentile-t CI: [γ̂ − t*(0.975)·SE, γ̂ − t*(0.025)·SE]
  t_quant <- quantile(wb$t, probs = c(0.025, 0.975), na.rm = TRUE)
  ci_wbt  <- c(gamma_m2 - t_quant[2] * se_cr1_m2,
               gamma_m2 - t_quant[1] * se_cr1_m2)

  # p-valor: proporción de |t_b| > |t_obs|
  t_obs  <- gamma_m2 / se_cr1_m2
  p_wbt  <- mean(abs(wb$t) >= abs(t_obs), na.rm = TRUE)

  # IC percentile (directo sobre γ)
  ci_pct <- quantile(wb$gamma, probs = c(0.025, 0.975), na.rm = TRUE)

  cat(sprintf("  Réplicas válidas: %d / %d\n", length(wb$gamma), B_wild))
  cat(sprintf("  p-valor wild bootstrap  = %.4f\n", p_wbt))
  cat(sprintf("  IC percentile-t 95%%: [%.4f, %.4f]\n", ci_wbt[1], ci_wbt[2]))
  cat(sprintf("  IC percentile 95%%:   [%.4f, %.4f]\n\n",
              ci_pct[1], ci_pct[2]))

  list(gamma = wb$gamma, t = wb$t,
       ci_wbt = ci_wbt, ci_pct = ci_pct, p = p_wbt)
}, error = function(e) {
  cat("  Error wild bootstrap:", e$message, "\n\n")
  NULL
})

# ─────────────────────────────────────────────────────────────────────────────
# D.3  Pairs Cluster Bootstrap (muestreo de clusters completos)
#      Alternativa no paramétrica, no requiere supuesto de homocedasticidad
#      dentro del cluster (Cameron, Gelbach & Miller 2008)
# ─────────────────────────────────────────────────────────────────────────────
cat("D.3  Pairs Cluster Bootstrap (B = 999)\n")

pairs_cluster_boot <- function(B, seed = 2025) {
  set.seed(seed)
  gamma_b <- numeric(B)

  for (b in seq_len(B)) {
    # Muestrear G clusters con reemplazo
    sampled_clus <- sample(clusters, n_clus, replace = TRUE)

    # Construir panel re-muestreado (asignar nuevos IDs para evitar FE duplicados)
    panel_b <- do.call(rbind, lapply(seq_along(sampled_clus), function(k) {
      df <- panel[panel$NUI == sampled_clus[k], ]
      df$NUI_b <- paste0("C", k)   # ID único para cada "copia"
      df
    }))

    fit_b <- tryCatch(
      suppressWarnings(feols(
        ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
          SEASON_SIN + SEASON_COS + TENDENCIA |
          NUI_b |
          ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                          ln_biomasa_sardina + ln_TAC_complejo,
        data = panel_b, vcov = ~NUI_b
      )),
      error = function(e) NULL
    )

    gamma_b[b] <- if (!is.null(fit_b)) {
      coef(fit_b)["fit_ln_h_complejo"]
    } else NA
  }

  gamma_b[!is.na(gamma_b)]
}

pcb_result <- tryCatch({
  pcb <- pairs_cluster_boot(B = B_wild)

  ci_pcb <- quantile(pcb, probs = c(0.025, 0.975), na.rm = TRUE)
  se_pcb <- sd(pcb, na.rm = TRUE)

  # p-valor simetrizando: proporción |γ_b − γ̂| > |γ̂ − 0|
  p_pcb  <- mean(abs(pcb - gamma_m2) >= abs(gamma_m2), na.rm = TRUE)

  cat(sprintf("  Réplicas válidas: %d / %d\n", length(pcb), B_wild))
  cat(sprintf("  SE_pairs_boot = %.4f\n", se_pcb))
  cat(sprintf("  IC percentile 95%%: [%.4f, %.4f]\n", ci_pcb[1], ci_pcb[2]))
  cat(sprintf("  p-valor (simetrizado) = %.4f\n\n", p_pcb))

  list(gamma = pcb, ci = ci_pcb, se = se_pcb, p = p_pcb)
}, error = function(e) {
  cat("  Error pairs cluster bootstrap:", e$message, "\n\n")
  NULL
})

# ─────────────────────────────────────────────────────────────────────────────
# D.4  Tabla comparativa final de SE e IC para γ
# ─────────────────────────────────────────────────────────────────────────────
cat(strrep("─", 70), "\n")
cat("  D.4  TABLA COMPARATIVA — SE e IC 95% para γ\n")
cat(strrep("─", 70), "\n\n")

# Acumular resultados
ic_tabla <- data.frame(
  Metodo   = character(),
  gamma    = numeric(),
  SE       = numeric(),
  IC_low   = numeric(),
  IC_high  = numeric(),
  p_valor  = numeric(),
  stringsAsFactors = FALSE
)

# CR1 (clustered estándar)
ic_tabla <- rbind(ic_tabla, data.frame(
  Metodo  = "CR1 (cluster estándar)",
  gamma   = gamma_m2,
  SE      = se_cr1_m2,
  IC_low  = gamma_m2 - 1.96 * se_cr1_m2,
  IC_high = gamma_m2 + 1.96 * se_cr1_m2,
  p_valor = 2 * pnorm(-abs(gamma_m2 / se_cr1_m2))
))

# Anderson-Rubin
if (!is.na(ar_ci_lo)) {
  ic_tabla <- rbind(ic_tabla, data.frame(
    Metodo  = "Anderson-Rubin (inv. test)",
    gamma   = gamma_m2,
    SE      = NA,
    IC_low  = ar_ci_lo,
    IC_high = ar_ci_hi,
    p_valor = p_ar_zero
  ))
}

# CR2
if (!is.null(cr2_result)) {
  ic_tabla <- rbind(ic_tabla, data.frame(
    Metodo  = "CR2 Bell-McCaffrey",
    gamma   = cr2_result$gamma,
    SE      = cr2_result$se,
    IC_low  = cr2_result$ci[1],
    IC_high = cr2_result$ci[2],
    p_valor = NA
  ))
}

# Wild bootstrap percentile-t
if (!is.null(wb_result)) {
  ic_tabla <- rbind(ic_tabla, data.frame(
    Metodo  = "Wild bootstrap (perc-t)",
    gamma   = gamma_m2,
    SE      = sd(wb_result$gamma, na.rm = TRUE),
    IC_low  = wb_result$ci_wbt[1],
    IC_high = wb_result$ci_wbt[2],
    p_valor = wb_result$p
  ))
}

# Pairs cluster bootstrap
if (!is.null(pcb_result)) {
  ic_tabla <- rbind(ic_tabla, data.frame(
    Metodo  = "Pairs cluster bootstrap",
    gamma   = gamma_m2,
    SE      = pcb_result$se,
    IC_low  = pcb_result$ci[1],
    IC_high = pcb_result$ci[2],
    p_valor = pcb_result$p
  ))
}

# Imprimir tabla
cat(sprintf("%-30s  %8s  %7s  %8s  %8s  %8s\n",
            "Método", "γ", "SE", "IC 2.5%", "IC 97.5%", "p-valor"))
cat(strrep("─", 75), "\n")
for (i in seq_len(nrow(ic_tabla))) {
  cat(sprintf("%-30s  %8.4f  %7s  %8.4f  %8.4f  %8s\n",
              ic_tabla$Metodo[i],
              ic_tabla$gamma[i],
              ifelse(is.na(ic_tabla$SE[i]), "  —   ",
                     sprintf("%.4f", ic_tabla$SE[i])),
              ic_tabla$IC_low[i],
              ic_tabla$IC_high[i],
              ifelse(is.na(ic_tabla$p_valor[i]), "  —   ",
                     sprintf("%.4f", ic_tabla$p_valor[i]))))
}
cat(strrep("─", 75), "\n\n")

# ── Diagnóstico final ─────────────────────────────────────────────────────────
cat("Diagnóstico integrado:\n\n")

# ¿Todos los IC excluyen 0?
excluye_cero <- all(ic_tabla$IC_low < 0 & ic_tabla$IC_high < 0, na.rm = TRUE)
cat("  ¿Todos los IC excluyen γ = 0?",
    ifelse(excluye_cero, "SÍ ✓ — resultado robusto a múltiples métodos.",
           "NO ✗ — verificar caso por caso."), "\n\n")

# Amplitud relativa de IC (diagnosis de debilidad de instrumentos)
ic_cr1_width <- (gamma_m2 + 1.96 * se_cr1_m2) - (gamma_m2 - 1.96 * se_cr1_m2)
if (!is.na(ar_ci_lo)) {
  ic_ar_width  <- ar_ci_hi - ar_ci_lo
  ratio_width  <- ic_ar_width / ic_cr1_width
  cat("  Amplitud IC AR / Amplitud IC CR1 =", round(ratio_width, 2), "\n")
  if (ratio_width > 2) {
    cat("  ⚠  IC AR mucho más amplio → instrumentos moderadamente débiles.\n")
    cat("     Reportar IC AR como resultado principal junto a CR2.\n")
  } else {
    cat("  ✓  IC AR y Wald son comparables → identificación robusta.\n")
  }
}

cat("\n")
cat("  Recomendación para reporte:\n")
cat("  1. Estadístico principal: γ con SE_CR2 e IC_CR2 (corrección pocos clusters)\n")
cat("  2. Robusto: IC Wild bootstrap percentile-t\n")
cat("  3. Test de instrumentos débiles: IC Anderson-Rubin\n")
cat("  4. Si todos coinciden en excluir 0 → conclusión de poder de monopsonio firme.\n\n")

cat(strrep("═", 70), "\n")
cat("  FIN DE ANÁLISIS DE ROBUSTEZ ADICIONAL\n")
cat(strrep("═", 70), "\n")
