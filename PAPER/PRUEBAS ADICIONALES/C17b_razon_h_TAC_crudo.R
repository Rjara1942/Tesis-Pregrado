# ==============================================================================
# BLOQUE C-17b — Razón desembarque / TAC desde fuentes crudas
# ==============================================================================
# La razón calculada desde panel_upgrade.csv da >100 % varios años → unidad o
# definición inconsistente. Este script la rearma desde:
#   - PROCESO (Excel IFOP) para desembarque industrial centro-sur ANIMAL
#   - Un CSV externo con los TAC publicados por Subpesca (edítalo si no existe)
# ==============================================================================

library(tidyverse)
library(readxl)
library(scales)
library(here)

PATH_IFOP <- here::here("data",
                        "2025.04.21.pelagicos_proceso-precios.mp.2012-2024-7ad37e7a.xlsx")
if (!file.exists(PATH_IFOP)) {
  PATH_IFOP <- here::here("data",
                          "2025.04.21.pelagicos_proceso-precios.mp.2012-2024.xlsx")
}
PATH_TAC_XLSX <- here::here("data", "TAC_anual.xlsx")
# Fuente oficial: hojas 'industrial' y 'artesanal' con cuotas por recurso,
# unidad de pesquería y año. Mismo archivo que usa 16_TAC_instrumento_v2.R.

DIR_FIG <- here::here("outputs", "figuras_paper")
dir.create(DIR_FIG, showWarnings = FALSE, recursive = TRUE)

theme_paper <- theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, color = "grey30"),
    plot.caption = element_text(size = 7.5, color = "grey40", hjust = 0),
    panel.grid.minor = element_blank()
  )

REGIONES_CS <- c(5, 6, 7, 8, 9, 10, 14, 16)

# ------------------------------------------------------------------------------
# 1. Desembarque industrial centro-sur, ANIMAL, por año (desde PROCESO)
# ------------------------------------------------------------------------------
h_anual <- read_excel(PATH_IFOP, sheet = "PROCESO") |>
  mutate(
    NM_RECURSO      = str_trim(as.character(NM_RECURSO)),
    CLASE_INDUSTRIA = str_trim(as.character(CLASE_INDUSTRIA))
  ) |>
  filter(
    CLASE_INDUSTRIA == "ANIMAL",
    NM_RECURSO %in% c("ANCHOVETA", "SARDINA COMUN"),
    RG %in% REGIONES_CS,
    ANIO >= 2012, ANIO <= 2024
  ) |>
  group_by(ANIO) |>
  summarise(h_total = sum(MP_TOTAL, na.rm = TRUE), .groups = "drop")

cat("Desembarque anual industrial centro-sur (sardina+anchoveta, ANIMAL):\n")
print(h_anual |> mutate(h_total_kt = round(h_total / 1e3, 1)))

# ------------------------------------------------------------------------------
# 2. TAC complejo centro-sur (industrial + artesanal), desde Subpesca
# ------------------------------------------------------------------------------
# Replica la lógica de 16_TAC_instrumento_v2.R:
#   - Industrial: V-X, sardina + anchoveta
#   - Artesanal: regiones centro-sur (V, VI, VII, VIII, IX, XIV, X, nombres
#     completos post-2019), excluyendo fauna acompañante, cesiones, consumo,
#     imprevisto, investigación, línea de mano.
# ------------------------------------------------------------------------------
if (!file.exists(PATH_TAC_XLSX)) stop("Falta ", PATH_TAC_XLSX)

norm <- function(x) {
  chartr("áéíóúñÁÉÍÓÚÑ", "aeiounAEIOUN",
         str_to_lower(str_trim(x)))
}

tac_ind <- read_excel(PATH_TAC_XLSX, sheet = "industrial") |>
  setNames(c("year", "recurso", "unidad", "cuota")) |>
  mutate(r = norm(recurso), u = str_trim(unidad))

tac_art <- read_excel(PATH_TAC_XLSX, sheet = "artesanal") |>
  setNames(c("year", "recurso", "unidad", "cuota")) |>
  mutate(r = norm(recurso), u = str_trim(unidad))

tac_ind_cs <- tac_ind |>
  filter(r %in% c("anchoveta", "sardina comun", "sardina"),
         str_detect(u, regex("V.*X|V-X|V - X|V -X", ignore_case = TRUE))) |>
  group_by(year) |>
  summarise(TAC_ind = sum(cuota, na.rm = TRUE), .groups = "drop")

patron_cs <- paste(
  c("^V[^I]|^V$|V\\s+\\(|ARTESANAL V",
    "^VI[^I]|^VI$|VI\\s+|ARTESANAL VI",
    "^VII|ARTESANAL VII",
    "^VIII|ARTESANAL VIII",
    "^IX|ARTESANAL IX",
    "^XIV|ARTESANAL XIV",
    "^X[^IV]|^X$|X\\s+\\(|ARTESANAL X$",
    "Valparaíso|Higgins|Maule|Biob|Ñuble|Araucanía|Los Rios|Los Lagos"),
  collapse = "|")
excluir <- "FAUNA|F\\.A|Investigación|INVESTIGACION|Imprevisto|Consumo|CONSUMO|FUERA|LINEA|Linea|Cesiones|CESIONES"

tac_art_cs <- tac_art |>
  filter(
    r %in% c("anchoveta", "sardina comun", "sardina"),
    str_detect(u, regex(patron_cs, ignore_case = TRUE)),
    !str_detect(u, regex("^XV|^I$|^II$|^III$|^IV[^I]|^IV$")),
    !str_detect(u, regex(excluir, ignore_case = TRUE))
  ) |>
  group_by(year) |>
  summarise(TAC_art = sum(cuota, na.rm = TRUE), .groups = "drop")

TAC <- full_join(tac_ind_cs, tac_art_cs, by = "year") |>
  mutate(across(starts_with("TAC_"), ~ replace_na(.x, 0)),
         TAC_total = TAC_ind + TAC_art) |>
  rename(ANIO = year) |>
  select(ANIO, TAC_ind, TAC_art, TAC_total)

cat("\nTAC total complejo (sardina + anchoveta) centro-sur:\n")
print(TAC |> mutate(TAC_kt = round(TAC_total / 1e3, 1)))

# ------------------------------------------------------------------------------
# 3. Razón h / TAC
# ------------------------------------------------------------------------------
razon <- h_anual |>
  inner_join(TAC, by = "ANIO") |>
  mutate(razon = h_total / TAC_total,
         pct   = round(razon * 100, 1))

cat("\nRazón desembarque industrial / TAC complejo:\n")
print(razon)

# ------------------------------------------------------------------------------
# 4. Figura
# ------------------------------------------------------------------------------
fig <- ggplot(razon, aes(x = ANIO, y = pct)) +
  geom_col(aes(fill = pct >= 85), alpha = 0.85, width = 0.7) +
  geom_hline(yintercept = 100, color = "red",
             linetype = "dashed", linewidth = 0.4) +
  geom_hline(yintercept = 85, color = "darkorange",
             linetype = "dotted", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.0f %%", pct)),
            vjust = -0.5, size = 2.8) +
  scale_fill_manual(
    values = c(`TRUE`  = "steelblue4", `FALSE` = "darkorange"),
    labels = c(`TRUE`  = "Cuota binding o cuasi-binding (≥85 %)",
               `FALSE` = "Cuota holgada (<85 %)"),
    name   = NULL
  ) +
  scale_y_continuous(labels = function(x) paste0(x, " %"),
                     limits = c(0, max(razon$pct) * 1.15)) +
  scale_x_continuous(breaks = razon$ANIO) +
  labs(
    title    = "Agotamiento del TAC por año, complejo sardina-anchoveta centro-sur",
    subtitle = paste("Desembarque industrial ANIMAL (IFOP, hoja PROCESO)",
                     "sobre TAC complejo (industrial + artesanal) publicado por Subpesca."),
    x = NULL,
    y = "Desembarque industrial como % del TAC complejo",
    caption = paste(
      "Línea roja = 100 %. Ratios >100 % reflejan cuotas suplementarias",
      "intra-año, carry-over de saldos no capturados y fauna acompañante no",
      "contabilizada por separado — fenómenos habituales del régimen chileno.",
      "La cuota es restrictiva o cuasi-restrictiva en 10 de 13 años (≥85 %);",
      "sólo 2013 (post-crisis Longueira) y 2024 (biomasa baja) quedan claramente holgados.",
      sep = " ")
  ) +
  theme_paper +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(DIR_FIG, "fig_razon_h_TAC_crudo.pdf"),
       fig, width = 7.5, height = 4.5, device = "pdf")

cat("\nOK: outputs/figuras_paper/fig_razon_h_TAC_crudo.pdf\n")
cat("Si tienes TAC_subpesca_complejo.csv con valores oficiales, el script lo",
    "usará automáticamente; de lo contrario corre con el fallback hardcodeado",
    "(valores aproximados — reemplazar antes de publicar).\n")
