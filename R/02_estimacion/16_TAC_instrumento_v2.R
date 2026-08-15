###############################################################################
#  16_TAC_instrumento_v2.R
#  ─────────────────────────────────────────────────────────────────────────────
#  Incorpora la Cuota Total Admisible (TAC) como instrumento para los
#  desembarques del complejo sardina-anchoveta en el modelo Panel IV-2SLS.
#
#  Opción A: TAC total (industrial + artesanal) → instrumento para ln_h_complejo
#
#  Requisitos:
#    - data/panel_correcto_base_junio_2024.csv  (generado por 06_rebasar_...)
#    - data/TAC_anual.xlsx                      (cuotas por flota, recurso, zona)
#    - Paquetes: readxl, dplyr, stringr, fixest, tidyr
#
#  Salida:
#    - data/panel_upgrade.csv                   (panel jun-2024 + TAC + jurel)
#
#  CORRECCIÓN ENCODING (2025-05):
#    str_to_lower() no elimina tildes → en 2015-2016 el Excel dice "SARDINA COMUN"
#    (sin tilde) y en 2017 "SARDINA COMÚN" (con tilde, mayúsculas). Se usa
#    chartr() para normalizar vocales acentuadas ANTES del filtro.
###############################################################################

library(readxl)
library(dplyr)
library(stringr)
library(fixest)
library(tidyr)

# ═══════════════════════════════════════════════════════════════════════════════
# HELPER: normalizar cadena → minúsculas + sin tildes
# ═══════════════════════════════════════════════════════════════════════════════

normalizar <- function(x) {
  chartr(
    "áéíóúàèìòùäëïöüâêîôûñÁÉÍÓÚÀÈÌÒÙÄËÏÖÜÂÊÎÔÛÑ",
    "aeiouaeiouaeiouaeiounAEIOUAEIOUAEIOUAEIOUN",
    str_to_lower(str_trim(x))
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# 1. LEER Y PROCESAR TAC DESDE EXCEL
# ═══════════════════════════════════════════════════════════════════════════════

# --- 1a. TAC Industrial ---
tac_ind_raw <- read_excel(("TAC_anual.xlsx"),
                          sheet = "industrial")
names(tac_ind_raw) <- c("year", "recurso", "unidad", "cuota")

tac_ind <- tac_ind_raw |>
  mutate(
    recurso       = str_trim(recurso),
    recurso_lower = normalizar(recurso),   # minúsculas + sin tildes
    unidad        = str_trim(unidad)
  )

# Industrial complejo sardina-anchoveta: regiones V-X
tac_ind_complejo <- tac_ind |>
  filter(
    recurso_lower %in% c("anchoveta", "sardina comun", "sardina"),
    str_detect(unidad, regex("V.*X|V-X|V - X|V -X", ignore_case = TRUE))
  ) |>
  group_by(year) |>
  summarise(TAC_ind_complejo = sum(cuota, na.rm = TRUE), .groups = "drop")

# Industrial jurel centro-sur: V-IX + XIV-X
tac_ind_jurel <- tac_ind |>
  filter(
    recurso_lower == "jurel",
    str_detect(unidad, regex("V.*IX|V-IX|V - IX|XIV.*X|XIV-X|XIV - X",
                             ignore_case = TRUE))
  ) |>
  group_by(year) |>
  summarise(TAC_ind_jurel = sum(cuota, na.rm = TRUE), .groups = "drop")


# --- 1b. TAC Artesanal ---
tac_art_raw <- read_excel(("TAC_anual.xlsx"),
                          sheet = "artesanal")
names(tac_art_raw) <- c("year", "recurso", "unidad", "cuota")

tac_art <- tac_art_raw |>
  mutate(
    recurso       = str_trim(recurso),
    recurso_lower = normalizar(recurso),
    unidad        = str_trim(unidad)
  )

# Regiones centro-sur artesanales: V, VI, VII, VIII, IX, XIV, X
regiones_cs <- c(
  "^V[^I]|^V$|V\\s+\\(|ARTESANAL V",
  "^VI[^I]|^VI$|VI\\s+|ARTESANAL VI",
  "^VII|ARTESANAL VII",
  "^VIII|ARTESANAL VIII",
  "^IX|ARTESANAL IX",
  "^XIV|ARTESANAL XIV",
  "^X[^IV]|^X$|X\\s+\\(|ARTESANAL X$",
  "Valparaíso|Higgins|Maule|Biob|Ñuble|Araucanía|Los Rios|Los Lagos"
)
patron_cs <- paste(regiones_cs, collapse = "|")

excluir <- "FAUNA|F\\.A|Investigación|INVESTIGACION|Imprevisto|Consumo|CONSUMO|FUERA|LINEA|Linea|Cesiones|CESIONES"

tac_art_cs <- tac_art |>
  filter(
    str_detect(unidad, regex(patron_cs, ignore_case = TRUE)),
    !str_detect(unidad, regex("^XV|^I$|^II$|^III$|^IV[^I]|^IV$", ignore_case = FALSE)),
    !str_detect(unidad, regex(excluir, ignore_case = TRUE))
  )

# Artesanal complejo sardina-anchoveta
tac_art_complejo <- tac_art_cs |>
  filter(recurso_lower %in% c("anchoveta", "sardina comun", "sardina")) |>
  group_by(year) |>
  summarise(TAC_art_complejo = sum(cuota, na.rm = TRUE), .groups = "drop")

# Artesanal jurel centro-sur
tac_art_jurel <- tac_art_cs |>
  filter(recurso_lower == "jurel") |>
  group_by(year) |>
  summarise(TAC_art_jurel = sum(cuota, na.rm = TRUE), .groups = "drop")


# ═══════════════════════════════════════════════════════════════════════════════
# 2. CONSTRUIR TAC TOTAL (INDUSTRIAL + ARTESANAL)
# ═══════════════════════════════════════════════════════════════════════════════

tac_total <- tac_ind_complejo |>
  full_join(tac_art_complejo, by = "year") |>
  full_join(tac_ind_jurel,    by = "year") |>
  full_join(tac_art_jurel,    by = "year") |>
  mutate(
    across(starts_with("TAC_"), ~ replace_na(.x, 0)),
    TAC_complejo = TAC_ind_complejo + TAC_art_complejo,
    TAC_jurel    = TAC_ind_jurel    + TAC_art_jurel,
    ln_TAC_complejo     = log(TAC_complejo),
    ln_TAC_jurel        = log(TAC_jurel),
    ln_TAC_ind_complejo = log(pmax(TAC_ind_complejo, 1)),
    ln_TAC_art_complejo = log(pmax(TAC_art_complejo, 1))
  )

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  TAC TOTAL CENTRO-SUR (toneladas)\n")
cat("══════════════════════════════════════════════════════════════\n")
print(tac_total |> select(year, TAC_ind_complejo, TAC_art_complejo,
                           TAC_complejo, TAC_jurel), n = 20)


# ═══════════════════════════════════════════════════════════════════════════════
# 3. MERGE CON EL PANEL PRINCIPAL Y GENERAR POST_2020 / INTERACCIÓN
# ═══════════════════════════════════════════════════════════════════════════════

panel_base <- read.csv(here::here("data", "panel_correcto_base_junio_2024.csv"))

panel <- panel_base |>
  select(-any_of(c("TAC_complejo", "TAC_jurel", "ln_TAC_complejo", "ln_TAC_jurel",
                   "TAC_ind_complejo", "TAC_art_complejo",
                   "ln_TAC_ind_complejo", "ln_TAC_art_complejo",
                   "POST_2020", "ln_h_X_POST"))) |>
  left_join(
    tac_total |> select(year, TAC_complejo, TAC_jurel,
                        ln_TAC_complejo, ln_TAC_jurel,
                        TAC_ind_complejo, TAC_art_complejo,
                        ln_TAC_ind_complejo, ln_TAC_art_complejo),
    by = c("ANIO" = "year")
  ) |>
  mutate(
    POST_2020   = ifelse(ANIO >= 2020, 1L, 0L),
    ln_h_X_POST = ln_h_complejo * POST_2020
  )

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  VERIFICACIÓN DEL MERGE\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("Observaciones panel:", nrow(panel), "\n")
cat("NAs en ln_TAC_complejo:", sum(is.na(panel$ln_TAC_complejo)), "\n")
cat("Años con TAC:", paste(sort(unique(panel$ANIO[!is.na(panel$ln_TAC_complejo)])),
                           collapse = ", "), "\n")

panel |>
  group_by(ANIO) |>
  summarise(
    n_obs        = n(),
    TAC_complejo = first(TAC_complejo),
    h_mean       = mean(h_complejo, na.rm = TRUE),
    ratio_h_TAC  = h_mean / first(TAC_complejo),
    .groups = "drop"
  ) |>
  print(n = 20)

# --- Guardar panel definitivo con TAC ---
write.csv(panel,
          here::here("data", "panel_upgrade.csv"),
          row.names = FALSE)
cat("\n✓ Guardado: data/panel_upgrade.csv (",
    nrow(panel), "obs ×", ncol(panel), "vars)\n")


# ═══════════════════════════════════════════════════════════════════════════════
# 4. CORRELACIONES: TAC vs DESEMBARQUES vs BIOMASA
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  CORRELACIONES (nivel observación)\n")
cat("══════════════════════════════════════════════════════════════\n")

cor_vars <- panel |>
  filter(!is.na(ln_TAC_complejo) & !is.na(ln_h_complejo) &
         !is.na(ln_biomasa_sardina)) |>
  select(ln_TAC_complejo, ln_h_complejo, ln_biomasa_sardina,
         ln_biomasa_complejo, ln_TAC_jurel, ln_h_jurel)

cat("\nMatriz de correlaciones:\n")
print(round(cor(cor_vars, use = "complete.obs"), 3))


# ═══════════════════════════════════════════════════════════════════════════════
# 5. MODELO 0: BENCHMARK (sin TAC)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  MODELO 0: BENCHMARK (sin TAC) — Replicación del modelo principal\n")
cat("══════════════════════════════════════════════════════════════\n")

m0 <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina,
  data = panel,
  vcov = ~NUI
)
cat("\n--- Primera etapa M0 ---\n"); print(summary(m0, stage = 1))
cat("\n--- Segunda etapa M0 ---\n"); print(summary(m0))
cat("F primera etapa (M0):", fitstat(m0, "ivf")$ivf$stat, "\n")
cat("Sargan p-valor (M0): ", fitstat(m0, "sargan")$sargan$p, "\n")
cat("Wu-Hausman p-valor:  ", fitstat(m0, "wh")$wh$p, "\n")


# ═══════════════════════════════════════════════════════════════════════════════
# 6. MODELOS CON TAC
# ═══════════════════════════════════════════════════════════════════════════════

# --- Modelo 1: TAC reemplaza biomasa ---
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  MODELO 1: TAC reemplaza biomasa como IV\n")
cat("══════════════════════════════════════════════════════════════\n")

m1 <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_TAC_complejo,
  data = panel,
  vcov = ~NUI
)
cat("\n--- Primera etapa M1 ---\n"); print(summary(m1, stage = 1))
cat("\n--- Segunda etapa M1 ---\n"); print(summary(m1))
cat("F primera etapa (M1):", fitstat(m1, "ivf")$ivf$stat, "\n")
cat("Sargan p-valor (M1): ", fitstat(m1, "sargan")$sargan$p, "\n")


# --- Modelo 2: TAC + biomasa + ambientales (4 IVs) ---
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  MODELO 2: TAC + biomasa + ambientales (4 IVs)\n")
cat("══════════════════════════════════════════════════════════════\n")

m2 <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                    ln_TAC_complejo,
  data = panel,
  vcov = ~NUI
)
cat("\n--- Primera etapa M2 ---\n"); print(summary(m2, stage = 1))
cat("\n--- Segunda etapa M2 ---\n"); print(summary(m2))
cat("F primera etapa (M2):", fitstat(m2, "ivf")$ivf$stat, "\n")
cat("Sargan p-valor (M2): ", fitstat(m2, "sargan")$sargan$p, "\n")


# --- Modelo 3: TAC como único instrumento excluido ---
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  MODELO 3: TAC como único instrumento excluido\n")
cat("══════════════════════════════════════════════════════════════\n")

m3 <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ ln_TAC_complejo,
  data = panel,
  vcov = ~NUI
)
cat("\n--- Primera etapa M3 ---\n"); print(summary(m3, stage = 1))
cat("\n--- Segunda etapa M3 ---\n"); print(summary(m3))
cat("F primera etapa (M3):", fitstat(m3, "ivf")$ivf$stat, "\n")
# Con exacta identificación (1 endógena, 1 IV excluido) no hay test de Sargan


# ═══════════════════════════════════════════════════════════════════════════════
# 7. TABLA COMPARATIVA DE RESULTADOS
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  TABLA COMPARATIVA\n")
cat("══════════════════════════════════════════════════════════════\n")

extraer_resultados <- function(modelo, nombre) {
  coefs <- coef(modelo)
  ses   <- se(modelo)
  f_iv  <- tryCatch(fitstat(modelo, "ivf")$ivf$stat, error = function(e) NA)
  s_p   <- tryCatch(fitstat(modelo, "sargan")$sargan$p, error = function(e) NA)
  data.frame(
    Modelo      = nombre,
    gamma       = round(coefs["fit_ln_h_complejo"], 4),
    SE_gamma    = round(ses["fit_ln_h_complejo"],   4),
    beta_FOB    = round(coefs["ln_P_FOB"],           4),
    delta_jurel = round(coefs["ln_h_jurel"],         4),
    F_1etapa    = round(f_iv, 2),
    Sargan_p    = round(s_p,  3),
    N           = modelo$nobs,
    row.names   = NULL
  )
}

tabla <- bind_rows(
  extraer_resultados(m0, "M0: Benchmark (sin TAC)"),
  extraer_resultados(m1, "M1: TAC reemplaza biomasa"),
  extraer_resultados(m2, "M2: TAC + biomasa + amb."),
  extraer_resultados(m3, "M3: TAC único IV")
)

print(tabla, right = FALSE)


# ═══════════════════════════════════════════════════════════════════════════════
# 8. CONTRIBUCIÓN MARGINAL DEL TAC AL R² WITHIN (primera etapa)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  CONTRIBUCIÓN MARGINAL DEL TAC AL R² WITHIN (primera etapa)\n")
cat("══════════════════════════════════════════════════════════════\n")

fe_base <- feols(ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA | NUI, data = panel, vcov = ~NUI)
fe_amb  <- feols(ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA + SO_PUERTO + SST_PUERTO_L1 | NUI, data = panel, vcov = ~NUI)
fe_bio  <- feols(ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA + SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina | NUI, data = panel, vcov = ~NUI)
fe_tac  <- feols(ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA + SO_PUERTO + SST_PUERTO_L1 + ln_TAC_complejo | NUI, data = panel, vcov = ~NUI)
fe_all  <- feols(ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA + SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina + ln_TAC_complejo | NUI, data = panel, vcov = ~NUI)

r2_tabla <- data.frame(
  Especificacion = c("Base (solo controles)",
                     "+ Ambientales (SO, SST_L1)",
                     "+ Ambientales + Biomasa",
                     "+ Ambientales + TAC",
                     "+ Ambientales + Biomasa + TAC"),
  R2_within = round(c(r2(fe_base, "wr2"), r2(fe_amb, "wr2"), r2(fe_bio, "wr2"),
                      r2(fe_tac,  "wr2"), r2(fe_all, "wr2")), 4)
)
r2_tabla$Incremento <- c(NA, diff(r2_tabla$R2_within))
print(r2_tabla, right = FALSE, row.names = FALSE)


# ═══════════════════════════════════════════════════════════════════════════════
# 9. COLINEALIDAD TAC vs BIOMASA
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  DIAGNÓSTICO: COLINEALIDAD TAC vs BIOMASA\n")
cat("══════════════════════════════════════════════════════════════\n")

anual <- panel |>
  filter(!is.na(ln_TAC_complejo) & !is.na(ln_biomasa_sardina)) |>
  group_by(ANIO) |>
  summarise(
    ln_TAC = first(ln_TAC_complejo),
    ln_bio = first(ln_biomasa_sardina),
    ln_h   = mean(ln_h_complejo, na.rm = TRUE),
    .groups = "drop"
  )

cat("Correlación ln_TAC vs ln_biomasa (anual):", round(cor(anual$ln_TAC, anual$ln_bio), 3), "\n")
cat("Correlación ln_TAC vs ln_h (anual):      ", round(cor(anual$ln_TAC, anual$ln_h),  3), "\n")
cat("Correlación ln_bio vs ln_h (anual):      ", round(cor(anual$ln_bio, anual$ln_h),  3), "\n")

vif_reg <- lm(ln_TAC ~ ln_bio, data = anual)
cat("R² de ln_TAC ~ ln_biomasa:", round(summary(vif_reg)$r.squared, 3), "\n")
cat("VIF implícito:             ", round(1 / (1 - summary(vif_reg)$r.squared), 2), "\n")
cat("  → Si VIF > 5, hay colinealidad relevante entre TAC y biomasa.\n")
cat("  → En ese caso, preferir M1 (TAC reemplaza biomasa) sobre M2 (ambos).\n")


