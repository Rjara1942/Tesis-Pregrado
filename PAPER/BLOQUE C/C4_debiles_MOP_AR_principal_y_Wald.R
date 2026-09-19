# ==============================================================================
# BLOQUE C-4 — F efectivo, AR de la principal y Wald 
# ==============================================================================

# install.packages(c("ivDiag","fixest","sandwich","lmtest","car"))
# Nota: AER esta corrupta en esta instalacion (lazy-load). No se usa aqui.
library(tidyverse)
library(ivDiag)
library(fixest)
library(sandwich)
library(lmtest)
library(car)

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

.
demean_por <- function(x, g)
  x - ave(x, g, FUN = function(v) mean(v, na.rm = TRUE))

dm <- df |>
  mutate(
    y   = demean_por(ln_P_complejo_real, NUI),
    D   = demean_por(ln_h_complejo,      NUI),
    z1  = demean_por(SO_PUERTO,          NUI),
    z2  = demean_por(SST_PUERTO_L1,      NUI),
    z3  = demean_por(ln_biomasa_sardina, NUI),
    z4  = demean_por(ln_TAC_complejo,    NUI),
    x1  = demean_por(ln_h_jurel,   NUI),
    x2  = demean_por(SEASON_SIN,   NUI),
    x3  = demean_por(SEASON_COS,   NUI),
    x4  = demean_por(TENDENCIA,    NUI),
    x5  = demean_por(ln_P_FOB,     NUI),
    cl  = NUI,
    per = period
  ) |>
  as.data.frame()

# ==============================================================================
# 1. F EFECTIVO DE MONTIEL OLEA-PFLUEGER (API oficial de ivDiag)
# ==============================================================================


# ivDiag::ivDiag() devuelve F_effective (Olea-Pflueger), F_stat (KP y otras),
# AR, tF, IC AR y IC tF. Firma actual: (data, Y, D, Z, controls, cl, FE, ...)
diag_out <- ivDiag::ivDiag(
  data     = dm,
  Y        = "y",
  D        = "D",
  Z        = c("z1", "z2", "z3", "z4"),
  controls = c("x1", "x2", "x3", "x4", "x5"),
  cl       = "cl",
  bootstrap = FALSE
)

# ---- Diagnostico verboso del layout que devuelve tu ivDiag -----------------

find_stat <- function(obj, patterns) {
  if (is.null(obj)) return(NA_real_)
  # Case 1: escalar numerico o vector sin nombres
  if (is.numeric(obj) && is.null(names(obj)) && is.null(dim(obj))) {
    return(as.numeric(obj)[1])
  }
 
  if (is.numeric(obj) && !is.null(names(obj)) && is.null(dim(obj))) {
    nm <- names(obj)
    for (pat in patterns) {
      hit <- grep(pat, nm, ignore.case = TRUE, value = TRUE)
      if (length(hit) >= 1) return(as.numeric(obj[hit[1]]))
    }
    return(NA_real_)
  }
  
  if (is.matrix(obj) || is.data.frame(obj)) {
    rn <- rownames(obj); cn <- colnames(obj)
    if (is.null(rn)) rn <- character(0)
    if (is.null(cn)) cn <- character(0)
    for (pat in patterns) {
      hit <- grep(pat, rn, ignore.case = TRUE, value = TRUE)
      if (length(hit) >= 1) {
        v <- suppressWarnings(as.numeric(obj[hit[1], 1]))
        if (!is.na(v)) return(v)
      }
      hit <- grep(pat, cn, ignore.case = TRUE, value = TRUE)
      if (length(hit) >= 1) {
        v <- suppressWarnings(as.numeric(obj[1, hit[1]]))
        if (!is.na(v)) return(v)
      }
    }
    return(NA_real_)
  }
  
  if (is.list(obj)) {
    nm <- names(obj); if (is.null(nm)) nm <- character(0)
    for (pat in patterns) {
      hit <- grep(pat, nm, ignore.case = TRUE, value = TRUE)
      if (length(hit) >= 1) {
        v <- suppressWarnings(as.numeric(unlist(obj[[hit[1]]]))[1])
        if (!is.na(v)) return(v)
      }
    }
    
    for (el in obj) {
      v <- find_stat(el, patterns)
      if (!is.na(v)) return(v)
    }
  }
  NA_real_
}

# Patrones ESTRICTOS (evitan que un pattern como "kp" caiga en AR_ci_bajo)
PAT_KP  <- c("^F\\.klbg$", "^F_klbg$", "^F\\.KP$", "F\\.kleibergen",
             "kleibergen")
PAT_EFF <- c("^F\\.effective$", "^F_effective$", "^F\\.eff$", "^F_eff$",
             "olea", "pflueger", "montiel")


F_KP  <- find_stat(diag_out$F_stat, PAT_KP)
F_eff <- find_stat(diag_out$F_stat, PAT_EFF)



fs <- lm(D ~ z1 + z2 + z3 + z4 + x1 + x2 + x3 + x4 + x5, data = dm)
zn <- c("z1","z2","z3","z4")
pi_hat <- coef(fs)[zn]
V_pi   <- sandwich::vcovCL(fs, cluster = dm$cl, type = "HC1")[zn, zn]

# Residualizar Z (columnas z1..z4) sobre X (x1..x5) y constante
Zresid <- lm(cbind(z1,z2,z3,z4) ~ x1 + x2 + x3 + x4 + x5, data = dm)$residuals
ZrZr   <- crossprod(Zresid)
K2     <- ncol(Zresid)

num_eff <- as.numeric(t(pi_hat) %*% ZrZr %*% pi_hat)
den_eff <- sum(diag(ZrZr %*% V_pi))
F_eff_manual <- num_eff / den_eff

# KP a mano: Wald cluster de pi = 0, dividido por K
W_KP <- as.numeric(t(pi_hat) %*% solve(V_pi) %*% pi_hat)
F_KP_manual <- W_KP / K2

cat(sprintf("F efectivo Olea-Pflueger (manual, cluster planta) : %.3f\n",
            F_eff_manual))
cat(sprintf("F Kleibergen-Paap (manual, cluster planta)        : %.3f\n",
            F_KP_manual))

cat(sprintf("Umbral Olea-Pflueger 5%% (K=%d, TSLS)              : ~ 24\n", K2))


if (!is.na(F_eff)) {
  cat(sprintf("[cross-check] F_eff ivDiag = %.3f vs manual = %.3f\n",
              F_eff, F_eff_manual))
} else {
  cat("[nota] ivDiag no expuso F.effective en $F_stat; se usa el manual.\n")
  F_eff <- F_eff_manual
}
if (!is.na(F_KP)) {
  cat(sprintf("[cross-check] F_KP ivDiag = %.3f vs manual = %.3f\n",
              F_KP, F_KP_manual))
} else {
  cat("[nota] ivDiag no expuso F.klbg en $F_stat; se usa el manual.\n")
  F_KP <- F_KP_manual
}



# ==============================================================================
# 2. INTERVALO ANDERSON-RUBIN DE LA ESPECIFICACION PRINCIPAL
# ==============================================================================

as_num1 <- function(x) {
  if (is.null(x)) return(NA_real_)
  v <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  v[is.finite(v)][1]
}
as_num2 <- function(x) {
  if (is.null(x)) return(c(NA_real_, NA_real_))
  v <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  if (length(v) < 2) return(c(NA_real_, NA_real_))
  v[1:2]
}

AR_F   <- as_num1(diag_out$AR$F)
AR_p   <- as_num1(diag_out$AR$pv)
AR_ci  <- as_num2(diag_out$AR$ci)
AR_bnd <- isTRUE(as.logical(diag_out$AR$bounded))

cat("\n[debug] estructura de diag_out$AR:\n")
str(diag_out$AR, max.level = 2)

cat(sprintf("F AR (H0: gamma = 0) : %.3f\n", AR_F))
cat(sprintf("p AR (H0: gamma = 0) : %.4f\n", AR_p))
if (isTRUE(AR_bnd)) {
  cat(sprintf("IC AR 95%%             : [%+.4f, %+.4f]\n",
              AR_ci[1], AR_ci[2]))
} else {
  cat("IC AR 95%%             : no acotado (unbounded)\n")
  cat("                        rango bajo: ", paste(AR_ci, collapse = " "), "\n")
}
cat("Este intervalo no depende del punto estimado; sobrevive a B-1.\n")

# ==============================================================================
# 3. WALD H0: theta = 1 <=> gamma = -1
# ==============================================================================

iv_DK <- feols(
  ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                       ln_P_FOB | NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                    ln_biomasa_sardina + ln_TAC_complejo,
  data = df, vcov = DK(4) ~ period
)
iv_CP <- feols(
  ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                       ln_P_FOB | NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                    ln_biomasa_sardina + ln_TAC_complejo,
  data = df, cluster = ~ NUI
)

nm_g <- if ("fit_ln_h_complejo" %in% names(coef(iv_DK)))
          "fit_ln_h_complejo" else "ln_h_complejo"

# Preferimos DK como principal
if (!is.na(as.numeric(coef(iv_DK)[nm_g]))) {
  g_hat  <- as.numeric(coef(iv_DK)[nm_g])
  g_se   <- as.numeric(se(iv_DK)[nm_g])
  se_lab <- "Driscoll-Kraay bw=4"
} else {
  g_hat  <- as.numeric(coef(iv_CP)[nm_g])
  g_se   <- as.numeric(se(iv_CP)[nm_g])
  se_lab <- "cluster planta"
  cat("(DK no calculable; se reporta cluster planta.)\n")
}

t_stat <- (g_hat - (-1)) / g_se
p_val  <- 2 * (1 - pnorm(abs(t_stat)))

theta_hat <- -1 / g_hat
theta_se  <- g_se / (g_hat^2)                      # delta method
theta_ic  <- theta_hat + c(-1, 1) * qnorm(0.975) * theta_se



# ==============================================================================
# 4. RESUMEN PARA EL MEMO
# ==============================================================================
resumen <- tibble::tribble(
  ~item, ~valor,
  "F efectivo Olea-Pflueger (cluster planta)",  round(F_eff, 4),
  "F Kleibergen-Paap (referencia)",             round(F_KP,  4),
  "AR F (H0: gamma=0)",                         round(AR_F,  4),
  "AR p (H0: gamma=0)",                         round(AR_p,  4),
  "AR IC 95% - bajo",                           round(AR_ci[1], 4),
  "AR IC 95% - alto",                           round(AR_ci[2], 4),
  "AR IC acotado (TRUE/FALSE)",                 as.numeric(isTRUE(AR_bnd)),
  paste0("gamma principal (", se_lab, ")"),     round(g_hat, 4),
  paste0("SE gamma principal (", se_lab, ")"),  round(g_se,  4),
  "t Wald (H0: gamma=-1)",                      round(t_stat, 4),
  "p Wald (H0: gamma=-1)",                      round(p_val,  4),
  "theta implicita (-1/gamma)",                 round(theta_hat, 4),
  "SE theta (delta)",                           round(theta_se,  4),
  "IC 95% theta - bajo",                        round(theta_ic[1], 4),
  "IC 95% theta - alto",                        round(theta_ic[2], 4)
)

dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(resumen,
          here::here("outputs", "reportes_intermedios",
                     "C4_debiles_AR_Wald.csv"))

cat("\nGuardado: outputs/reportes_intermedios/C4_debiles_AR_Wald.csv\n")
print(resumen, n = Inf, width = Inf)

