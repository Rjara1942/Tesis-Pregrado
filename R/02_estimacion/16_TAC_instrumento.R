###############################################################################
#  16_incorporar_TAC_instrumento.R
#  ─────────────────────────────────────────────────────────────────────────────
#  Incorpora la Cuota Total Admisible (TAC) como instrumento para los
#  desembarques del complejo sardina-anchoveta en el modelo Panel IV-2SLS.
#
#   TAC total (industrial + artesanal) instrumento para ln_h_complejo

#    - panel_con_alternativas.csv  
#    - TAC_anual.xlsx             

###############################################################################

library(readxl)
library(dplyr)
library(stringr)
library(fixest)
library(tidyr)

# ═══════════════════════════════════════════════════════════════════════════════
# 1. LEER Y PROCESAR TAC DESDE EXCEL
# ═══════════════════════════════════════════════════════════════════════════════

# --- 1a. TAC Industrial ---
tac_ind_raw <- read_excel(here::here("data", "TAC_anual.xlsx"), sheet = "industrial")
names(tac_ind_raw) <- c("year", "recurso", "unidad", "cuota")

tac_ind <- tac_ind_raw %>%
  mutate(
    recurso = str_trim(recurso),
    recurso_lower = str_to_lower(recurso),
    unidad  = str_trim(unidad)
  )

# Industrial complejo sardina-anchoveta: regiones V-X
tac_ind_complejo <- tac_ind %>%
  filter(
    recurso_lower %in% c("anchoveta", "sardina común", "sardina comun"),
    str_detect(unidad, regex("V.*X|V-X|V - X|V -X", ignore_case = TRUE))
  ) %>%
  group_by(year) %>%
  summarise(TAC_ind_complejo = sum(cuota, na.rm = TRUE), .groups = "drop")

# Industrial jurel centro-sur: V-IX + XIV-X
tac_ind_jurel <- tac_ind %>%
  filter(
    recurso_lower == "jurel",
    str_detect(unidad, regex("V.*IX|V-IX|V - IX|XIV.*X|XIV-X|XIV - X",
                             ignore_case = TRUE))
  ) %>%
  group_by(year) %>%
  summarise(TAC_ind_jurel = sum(cuota, na.rm = TRUE), .groups = "drop")

# --- 1b. TAC Artesanal ---
tac_art_raw <- read_excel(here::here("data", "TAC_anual.xlsx"), sheet = "artesanal")
names(tac_art_raw) <- c("year", "recurso", "unidad", "cuota")

tac_art <- tac_art_raw %>%
  mutate(
    recurso = str_trim(recurso),
    recurso_lower = str_to_lower(recurso),
    unidad  = str_trim(unidad)
  )

# Regiones centro-sur artesanales: V, VI, VII, VIII, IX, XIV, X
# Incluir todas las formas en que aparecen en el archivo
regiones_cs <- c(
  "^V$", "^V ", "^VI$", "^VI ", "^VII", "^VIII", "^IX", "^XIV", "^X$", "^X ",
  "Valparaíso", "Higgins", "Maule", "Biob", "Ñuble", "Araucanía",
  "Los Rios", "Los Lagos",
  "ARTESANAL V$", "ARTESANAL VI$", "ARTESANAL VII", "ARTESANAL VIII",
  "ARTESANAL IX", "ARTESANAL XIV", "ARTESANAL X$"
)
patron_cs <- paste(regiones_cs, collapse = "|")

# Excluir categorías que no son cuotas regionales estándar
excluir <- c("FAUNA", "Fauna", "F\\.A", "Investigación", "INVESTIGACION",
             "Imprevisto", "IMPREVISTO", "Consumo Humano", "CONSUMO HUMANO",
             "FUERA DE UNIDAD", "LINEA DE MANO", "Linea de Mano",
             "Cesiones", "CESIONES")
patron_excluir <- paste(excluir, collapse = "|")

tac_art_cs <- tac_art %>%
  filter(
    str_detect(unidad, regex(patron_cs, ignore_case = FALSE)),
    !str_detect(unidad, regex("^XV|^I$|^II$|^III$|^IV$|^IV ",
                               ignore_case = FALSE)),
    !str_detect(unidad, regex(patron_excluir, ignore_case = FALSE))
  )

# Artesanal complejo sardina-anchoveta
tac_art_complejo <- tac_art_cs %>%
  filter(recurso_lower %in% c("anchoveta", "sardina común", "sardina comun",
                               "sardina", "sardina común")) %>%
  group_by(year) %>%
  summarise(TAC_art_complejo = sum(cuota, na.rm = TRUE), .groups = "drop")

# Artesanal jurel centro-sur
tac_art_jurel <- tac_art_cs %>%
  filter(recurso_lower == "jurel") %>%
  group_by(year) %>%
  summarise(TAC_art_jurel = sum(cuota, na.rm = TRUE), .groups = "drop")


# ═══════════════════════════════════════════════════════════════════════════════
# 2. CONSTRUIR TAC TOTAL (INDUSTRIAL + ARTESANAL)
# ═══════════════════════════════════════════════════════════════════════════════

tac_total <- tac_ind_complejo %>%
  full_join(tac_art_complejo, by = "year") %>%
  full_join(tac_ind_jurel,    by = "year") %>%
  full_join(tac_art_jurel,    by = "year") %>%
  mutate(
    across(starts_with("TAC_"), ~ replace_na(.x, 0)),
    TAC_complejo = TAC_ind_complejo + TAC_art_complejo,
    TAC_jurel    = TAC_ind_jurel    + TAC_art_jurel,
    ln_TAC_complejo     = log(TAC_complejo),
    ln_TAC_jurel        = log(TAC_jurel),
    ln_TAC_ind_complejo = log(pmax(TAC_ind_complejo, 1)),
    ln_TAC_art_complejo = log(pmax(TAC_art_complejo, 1))
  )


print(tac_total %>% select(year, TAC_ind_complejo, TAC_art_complejo,
                            TAC_complejo, TAC_jurel), n = 20)


# ═══════════════════════════════════════════════════════════════════════════════
# 3. MERGE CON EL PANEL PRINCIPAL
# ═══════════════════════════════════════════════════════════════════════════════

panel <- read.csv(here::here("data", "panel_con_alternativas.csv"))

panel <- panel %>%
  left_join(
    tac_total %>% select(year, TAC_complejo, TAC_jurel,
                          ln_TAC_complejo, ln_TAC_jurel,
                          TAC_ind_complejo, TAC_art_complejo,
                          ln_TAC_ind_complejo, ln_TAC_art_complejo),
    by = c("ANIO" = "year")
  )

# Verificar merge

cat("Observaciones panel:", nrow(panel), "\n")
cat("NAs en ln_TAC_complejo:", sum(is.na(panel$ln_TAC_complejo)), "\n")
cat("Años con TAC:", paste(sort(unique(panel$ANIO[!is.na(panel$ln_TAC_complejo)])),
                            collapse = ", "), "\n")

# Resumen por año
panel %>%
  group_by(ANIO) %>%
  summarise(
    n_obs = n(),
    TAC_complejo = first(TAC_complejo),
    h_complejo_mean = mean(h_complejo, na.rm = TRUE),
    ratio_h_TAC = mean(h_complejo, na.rm = TRUE) / first(TAC_complejo),
    .groups = "drop"
  ) %>%
  print(n = 20)


# ═══════════════════════════════════════════════════════════════════════════════
# 4. CORRELACIONES: TAC vs DESEMBARQUES vs BIOMASA
# ═══════════════════════════════════════════════════════════════════════════════



cor_vars <- panel %>%
  filter(!is.na(ln_TAC_complejo) & !is.na(ln_h_complejo) &
         !is.na(ln_biomasa_sardina)) %>%
  select(ln_TAC_complejo, ln_h_complejo, ln_biomasa_sardina,
         ln_biomasa_complejo, ln_TAC_jurel, ln_h_jurel)

cat("\nMatriz de correlaciones:\n")
print(round(cor(cor_vars, use = "complete.obs"), 3))


# ═══════════════════════════════════════════════════════════════════════════════
# 5. ESTIMACIÓN: MODELO PRINCIPAL REPLICADO (SIN TAC, BENCHMARK)
# ═══════════════════════════════════════════════════════════════════════════════



m0 <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina,
  data = panel,
  vcov = ~NUI
)
cat("\n--- Primera etapa M0 ---\n")
summary(m0, stage = 1)
cat("\n--- Segunda etapa M0 ---\n")
summary(m0)
cat("F primera etapa (M0):", fitstat(m0, "ivf")$ivf$stat, "\n")
cat("Sargan p-valor (M0): ", fitstat(m0, "sargan")$sargan$p, "\n")
cat("Wu-Hausman p-valor:  ", fitstat(m0, "wh")$wh$p, "\n")


# ═══════════════════════════════════════════════════════════════════════════════
# 6. ESTIMACIÓN: MODELOS CON TAC
# ═══════════════════════════════════════════════════════════════════════════════

# --- Modelo 1: TAC reemplaza biomasa ---


m1 <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_TAC_complejo,
  data = panel,
  vcov = ~NUI
)
cat("\n--- Primera etapa M1 ---\n")
summary(m1, stage = 1)
cat("\n--- Segunda etapa M1 ---\n")
summary(m1)
cat("F primera etapa (M1):", fitstat(m1, "ivf")$ivf$stat, "\n")
cat("Sargan p-valor (M1): ", fitstat(m1, "sargan")$sargan$p, "\n")


# --- Modelo 2: TAC sumado a los IVs existentes (4 instrumentos) ---


m2 <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina +
                    ln_TAC_complejo,
  data = panel,
  vcov = ~NUI
)
cat("\n--- Primera etapa M2 ---\n")
summary(m2, stage = 1)
cat("\n--- Segunda etapa M2 ---\n")
summary(m2)
cat("F primera etapa (M2):", fitstat(m2, "ivf")$ivf$stat, "\n")
cat("Sargan p-valor (M2): ", fitstat(m2, "sargan")$sargan$p, "\n")


# --- Modelo 3: TAC como único instrumento excluido ---


m3 <- feols(
  ln_P_complejo ~ ln_P_FOB + ln_h_jurel +
    SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI |
    ln_h_complejo ~ ln_TAC_complejo,
  data = panel,
  vcov = ~NUI
)
cat("\n--- Primera etapa M3 ---\n")
summary(m3, stage = 1)
cat("\n--- Segunda etapa M3 ---\n")
summary(m3)
cat("F primera etapa (M3):", fitstat(m3, "ivf")$ivf$stat, "\n")
# Con exacta identificación (1 endógena, 1 IV excluido) no hay Sargan


# ═══════════════════════════════════════════════════════════════════════════════
# 7. TABLA COMPARATIVA DE RESULTADOS
# ═══════════════════════════════════════════════════════════════════════════════


extraer_resultados <- function(modelo, nombre) {
  coefs <- coef(modelo)
  ses   <- se(modelo)
  f_iv  <- tryCatch(fitstat(modelo, "ivf")$ivf$stat, error = function(e) NA)
  s_p   <- tryCatch(fitstat(modelo, "sargan")$sargan$p, error = function(e) NA)
  
  data.frame(
    Modelo      = nombre,
    gamma       = round(coefs["fit_ln_h_complejo"], 4),
    SE_gamma    = round(ses["fit_ln_h_complejo"], 4),
    beta_FOB    = round(coefs["ln_P_FOB"], 4),
    delta_jurel = round(coefs["ln_h_jurel"], 4),
    F_1etapa    = round(f_iv, 2),
    Sargan_p    = round(s_p, 3),
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
# 8. CONTRIBUCIÓN MARGINAL DEL TAC EN PRIMERA ETAPA
# ═══════════════════════════════════════════════════════════════════════════════


fe_base <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA |
    NUI,
  data = panel, vcov = ~NUI
)

fe_amb <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
    SO_PUERTO + SST_PUERTO_L1 |
    NUI,
  data = panel, vcov = ~NUI
)

fe_bio <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
    SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina |
    NUI,
  data = panel, vcov = ~NUI
)

fe_tac <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
    SO_PUERTO + SST_PUERTO_L1 + ln_TAC_complejo |
    NUI,
  data = panel, vcov = ~NUI
)

fe_all <- feols(
  ln_h_complejo ~ ln_P_FOB + ln_h_jurel + SEASON_SIN + SEASON_COS + TENDENCIA +
    SO_PUERTO + SST_PUERTO_L1 + ln_biomasa_sardina + ln_TAC_complejo |
    NUI,
  data = panel, vcov = ~NUI
)

r2_tabla <- data.frame(
  Especificacion = c("Base (solo controles)",
                      "+ Ambientales (SO, SST_L1)",
                      "+ Ambientales + Biomasa",
                      "+ Ambientales + TAC",
                      "+ Ambientales + Biomasa + TAC"),
  R2_within = round(c(r2(fe_base, "wr2"),
                       r2(fe_amb,  "wr2"),
                       r2(fe_bio,  "wr2"),
                       r2(fe_tac,  "wr2"),
                       r2(fe_all,  "wr2")), 4)
)
r2_tabla$Incremento <- c(NA, diff(r2_tabla$R2_within))

print(r2_tabla, right = FALSE, row.names = FALSE)


# ═══════════════════════════════════════════════════════════════════════════════
# 9. COLINEALIDAD TAC vs BIOMASA
# ═══════════════════════════════════════════════════════════════════════════════



anual <- panel %>%
  filter(!is.na(ln_TAC_complejo) & !is.na(ln_biomasa_sardina)) %>%
  group_by(ANIO) %>%
  summarise(
    ln_TAC  = first(ln_TAC_complejo),
    ln_bio  = first(ln_biomasa_sardina),
    ln_h    = mean(ln_h_complejo, na.rm = TRUE),
    .groups = "drop"
  )

cat("Correlación ln_TAC vs ln_biomasa (anual):",
    round(cor(anual$ln_TAC, anual$ln_bio), 3), "\n")
cat("Correlación ln_TAC vs ln_h (anual):",
    round(cor(anual$ln_TAC, anual$ln_h), 3), "\n")
cat("Correlación ln_bio vs ln_h (anual):",
    round(cor(anual$ln_bio, anual$ln_h), 3), "\n")

vif_reg <- lm(ln_TAC ~ ln_bio, data = anual)


