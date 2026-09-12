# ==============================================================================
# BLOQUE B.1 — Diagnostico de las 8 filas de NUI 90073 en 2013 y comparativa
# de refuerzos del filtro
# ============================================================================== 
#   (0) Reconstruye la muestra base desde el Excel IFOP.
#   (1) Confirma cuantitativamente las 8 filas anomalas.
#   (2) Aplica cuatro variantes de filtro y estima el modelo IV en cada una:
#        A) Baseline actual (CLASE_INDUSTRIA_II en {ANIMAL, MIXTA_AH}).
#        B) Filtro estricto: CLASE_INDUSTRIA = ANIMAL (sin MIXTA_AH).
#        C) Filtro quirurgico por planta-mes: mantener solo (NUI, ANIO, MES)
#           donde en PROCESO la MP a ANIMAL supera la MP a HUMANO.
#        D) Borrado directo de las 8 filas de 90073 en 2013.
#   (3) Comparativa: gamma, SE, p, IC, F Kleibergen-Paap, Sargan, N, G.
# ==============================================================================

library(tidyverse)
library(fixest)
library(readxl)
library(AER)

PATH_IFOP  <- here::here("data", "2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx")
PATH_PANEL <- here::here("data", "panel_upgrade.csv")

REGIONES_CENTRO_SUR <- c(5, 6, 7, 8, 9, 10, 14, 16)

# ------------------------------------------------------------------------------
# 0. CARGAR PANEL YA CONSTRUIDO Y HOJAS CRUDAS DEL IFOP
# ------------------------------------------------------------------------------
df_panel <- read_csv(PATH_PANEL, show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  filter(!is.na(SST_PUERTO_L1)) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup()

stopifnot(nrow(df_panel) == 418, length(unique(df_panel$NUI)) == 15)

pr_raw <- read_excel(PATH_IFOP, sheet = "PRECIO") |>
  mutate(NM_RECURSO = str_trim(NM_RECURSO),
         CLASE_INDUSTRIA_II = str_trim(CLASE_INDUSTRIA_II),
         NUI = as.character(NUI))

pc_raw <- read_excel(PATH_IFOP, sheet = "PROCESO") |>
  mutate(NM_RECURSO = str_trim(NM_RECURSO),
         CLASE_INDUSTRIA = str_trim(CLASE_INDUSTRIA),
         NM_LINEA = str_trim(NM_LINEA),
         NUI = as.character(NUI))

# ------------------------------------------------------------------------------
# 1. DIAGNOSTICO CUANTITATIVO DE LAS 8 FILAS DE 90073
# ------------------------------------------------------------------------------


f90 <- df_panel |> filter(NUI == "90073", ANIO == 2013)
cat(sprintf("Filas en el panel: %d\n", nrow(f90)))
cat("P_complejo_real por mes:\n")
print(f90 |> select(ANIO, MES, P_complejo_real))

# Precio crudo por especie desde PRECIO
p90_raw <- pr_raw |> filter(NUI == "90073", ANIO == 2013)
cat("\nPRECIO crudo IFOP (una fila por especie x mes):\n")
print(p90_raw |> select(ANIO, MES, NM_RECURSO, TIPO_MP, PRECIO, CLASE_INDUSTRIA_II))

# MP procesada por linea desde PROCESO
c90_raw <- pc_raw |> filter(NUI == "90073", ANIO == 2013)
cat("\nPROCESO crudo IFOP, MP por linea de destino (una fila por especie x linea x mes):\n")
print(c90_raw |> select(ANIO, MES, NM_RECURSO, NM_LINEA, MP_TOTAL, CLASE_INDUSTRIA))

# Balance ANIMAL vs HUMANO por mes para 90073 en 2013
bal_90073 <- c90_raw |>
  group_by(ANIO, MES, CLASE_INDUSTRIA) |>
  summarise(MP = sum(MP_TOTAL, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = CLASE_INDUSTRIA, values_from = MP, values_fill = 0) |>
  mutate(razon_ANIMAL_HUMANO = ANIMAL / pmax(HUMANO, 1e-6))
cat("\nBalance ANIMAL vs HUMANO por mes en 90073 (2013):\n")
print(bal_90073)

# ------------------------------------------------------------------------------
# 2. CONSTRUCCION DE LAS CUATRO MUESTRAS
# ------------------------------------------------------------------------------
# Filtro base sobre PRECIO y PROCESO (comun a las cuatro):
build_panel <- function(pr, pc, note = "") {
  df_pr <- pr |>
    filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN"),
           RG %in% REGIONES_CENTRO_SUR,
           !is.na(PRECIO), PRECIO > 0)
  df_pc <- pc |>
    filter(NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN"),
           RG %in% REGIONES_CENTRO_SUR) |>
    group_by(ANIO, MES, RG, NUI, NM_RECURSO) |>
    summarise(MP_TOTAL = sum(MP_TOTAL, na.rm = TRUE), .groups = "drop")
  # Inner join
  df_p_planta <- df_pr |>
    select(ANIO, MES, RG, NUI, NM_RECURSO, PRECIO) |>
    inner_join(df_pc, by = c("ANIO", "MES", "RG", "NUI", "NM_RECURSO")) |>
    group_by(ANIO, MES, NUI) |>
    summarise(
      P_complejo = weighted.mean(PRECIO, w = MP_TOTAL, na.rm = TRUE),
      .groups = "drop"
    )
  cat(sprintf("  %s : %d planta-mes, %d plantas\n",
              note, nrow(df_p_planta), length(unique(df_p_planta$NUI))))
  df_p_planta
}



# (A) Baseline actual: CLASE_INDUSTRIA_II en {ANIMAL, MIXTA_AH} en PRECIO
# y CLASE_INDUSTRIA en {ANIMAL, MIXTA_AH} en PROCESO
pr_A <- pr_raw |> filter(CLASE_INDUSTRIA_II %in% c("ANIMAL", "MIXTA_AH"))
pc_A <- pc_raw |> filter(CLASE_INDUSTRIA %in% c("ANIMAL", "MIXTA_AH"))
key_A <- build_panel(pr_A, pc_A, "(A) Baseline (ANIMAL + MIXTA_AH)")

# (B) Filtro estricto: solo ANIMAL en ambas hojas
pr_B <- pr_raw |> filter(CLASE_INDUSTRIA_II == "ANIMAL")
pc_B <- pc_raw |> filter(CLASE_INDUSTRIA == "ANIMAL")
key_B <- build_panel(pr_B, pc_B, "(B) Filtro estricto ANIMAL")

# (C) Filtro quirurgico por planta-mes: mantener (NUI, ANIO, MES) donde
# MP a ANIMAL > MP a HUMANO en PROCESO (sin restringir por especie).
pm_dominancia <- pc_raw |>
  filter(RG %in% REGIONES_CENTRO_SUR) |>
  group_by(ANIO, MES, NUI, CLASE_INDUSTRIA) |>
  summarise(MP = sum(MP_TOTAL, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = CLASE_INDUSTRIA, values_from = MP, values_fill = 0) |>
  mutate(dominancia_animal = (ANIMAL > HUMANO))
# La muestra base sigue siendo ANIMAL + MIXTA_AH, pero se descartan los
# planta-mes donde HUMANO domina.
key_C_all <- build_panel(pr_A, pc_A, "(C) Base antes de quirurgico")
key_C <- key_C_all |>
  inner_join(pm_dominancia |> filter(dominancia_animal) |>
               select(ANIO, MES, NUI),
             by = c("ANIO", "MES", "NUI"))
cat(sprintf("  (C) Quirurgico (ANIMAL > HUMANO por mes): %d planta-mes, %d plantas\n",
            nrow(key_C), length(unique(key_C$NUI))))

# (D) Base A menos las 8 filas de 90073 en 2013
key_D <- key_A |>
  anti_join(tibble(NUI = "90073", ANIO = 2013),
            by = c("NUI", "ANIO"))
cat(sprintf("  (D) Base menos NUI 90073 en 2013 (8 obs): %d planta-mes, %d plantas\n",
            nrow(key_D), length(unique(key_D$NUI))))

# ------------------------------------------------------------------------------
# 3. RECONSTRUIR EL PANEL DE ESTIMACION EN CADA VARIANTE
# ------------------------------------------------------------------------------
# tomar la key (NUI, ANIO, MES) de cada muestra y hacer inner join
# contra el panel_upgrade que ya trae todas las covariables y el DEFLACTOR.
attach_covars <- function(key) {
  df_panel |>
    inner_join(key |> select(ANIO, MES, NUI), by = c("ANIO", "MES", "NUI")) |>
    filter(!is.na(SST_PUERTO_L1)) |>
    group_by(NUI) |> filter(n() >= 2) |> ungroup()
}

df_A <- attach_covars(key_A)
df_B <- attach_covars(key_B)
df_C <- attach_covars(key_C)
df_D <- attach_covars(key_D)


for (nm in c("A","B","C","D")) {
  d <- get(paste0("df_", nm))
  cat(sprintf("  %s: %d obs, %d plantas\n", nm, nrow(d), length(unique(d$NUI))))
}

# ------------------------------------------------------------------------------
# 4. ESTIMACION DEL MODELO IV EN CADA MUESTRA
# ------------------------------------------------------------------------------
estimar <- function(df, etiqueta) {
  if (nrow(df) < 20 || length(unique(df$NUI)) < 3) {
    return(tibble(Escenario = etiqueta, N = nrow(df),
                  G = length(unique(df$NUI)),
                  gamma = NA, SE = NA, p = NA, IC_inf = NA, IC_sup = NA,
                  F_KP = NA, Sargan_J = NA, Sargan_p = NA))
  }
  m <- feols(
    ln_P_complejo_real ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS +
                         TENDENCIA | NUI |
                         ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 +
                                          ln_biomasa_sardina + ln_TAC_complejo,
    data = df, cluster = ~ NUI
  )
  nm_g <- if ("fit_ln_h_complejo" %in% names(coef(m))) "fit_ln_h_complejo" else "ln_h_complejo"
  g <- as.numeric(coef(m)[nm_g])
  s <- as.numeric(se(m)[nm_g])
  p <- as.numeric(pvalue(m)[nm_g])
  ci <- as.numeric(unlist(confint(m, parm = nm_g)))
  # F KP: Wald conjunto sobre los 4 instrumentos de la 1a etapa
  fs <- feols(
    ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
                    SO_PUERTO + SST_PUERTO_L1 +
                    ln_biomasa_sardina + ln_TAC_complejo | NUI,
    data = df, cluster = ~ NUI
  )
  Fkp <- tryCatch(
    wald(fs, keep = c("SO_PUERTO","SST_PUERTO_L1","ln_biomasa_sardina","ln_TAC_complejo"),
         print = FALSE)$stat,
    error = function(e) NA
  )
  # Sargan J via AER::ivreg
  m_ivreg <- tryCatch(
    ivreg(ln_P_complejo_real ~ ln_h_complejo + ln_P_FOB + ln_h_jurel +
            SEASON_SIN + SEASON_COS + TENDENCIA + factor(NUI) |
            SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
            ln_TAC_complejo + ln_P_FOB + ln_h_jurel +
            SEASON_SIN + SEASON_COS + TENDENCIA + factor(NUI),
          data = df),
    error = function(e) NULL
  )
  if (!is.null(m_ivreg)) {
    diag <- summary(m_ivreg, diagnostics = TRUE)$diagnostics
    if ("Sargan" %in% rownames(diag)) {
      J <- as.numeric(diag["Sargan", "statistic"])
      pJ <- as.numeric(diag["Sargan", "p-value"])
    } else { J <- NA; pJ <- NA }
  } else { J <- NA; pJ <- NA }
  tibble(
    Escenario = etiqueta,
    N         = nrow(df),
    G         = length(unique(df$NUI)),
    gamma     = round(g, 4),
    SE        = round(s, 4),
    p         = round(p, 4),
    IC_inf    = round(ci[1], 4),
    IC_sup    = round(ci[2], 4),
    F_KP      = round(Fkp, 2),
    Sargan_J  = round(J, 4),
    Sargan_p  = round(pJ, 4)
  )
}

cat("\n=================================================================\n")
cat("3. COMPARATIVA IV BAJO CADA FILTRO\n")
cat("=================================================================\n")

tabla <- bind_rows(
  estimar(df_A, "A. Baseline actual (ANIMAL + MIXTA_AH)"),
  estimar(df_B, "B. Estricto: solo ANIMAL"),
  estimar(df_C, "C. Quirurgico por planta-mes (ANIMAL > HUMANO)"),
  estimar(df_D, "D. Base menos NUI 90073 en 2013 (8 obs)")
)
print(tabla, n = Inf, width = Inf)

dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(tabla,
          here::here("outputs", "reportes_intermedios",
                     "B1_comparativa_filtros_90073.csv"))
cat("\nGuardado: outputs/reportes_intermedios/B1_comparativa_filtros_90073.csv\n")

# ------------------------------------------------------------------------------
# 5. EFECTO ESPECIFICO SOBRE 90073
# ------------------------------------------------------------------------------
for (nm in c("A","B","C","D")) {
  d <- get(paste0("df_", nm))
  n90 <- sum(d$NUI == "90073")
  n90_2013 <- sum(d$NUI == "90073" & d$ANIO == 2013)
  cat(sprintf("  %s : NUI 90073 total = %2d obs, en 2013 = %d obs\n",
              nm, n90, n90_2013))
}

