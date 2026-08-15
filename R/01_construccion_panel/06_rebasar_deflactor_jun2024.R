# =============================================================================
# SCRIPT 06: REBASAR PRECIOS REALES A BASE JUNIO 2024
# =============================================================================
#   1. Toma panel_con_alternativas.csv (precios reales en base ene-2012)
#   2. Reescala todos los precios reales usando DEFLACTOR[jun-2024] = 1.6736095
#      → interpretación: CLP constantes de junio 2024
#   3. Guarda panel_correcto_base_junio_2024.csv

# =============================================================================

library(tidyverse)


# =============================================================================
# PARTE 1: CARGA
# =============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("PARTE 1: CARGA DEL PANEL EN BASE ENE-2012\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

panel <- read_csv(("panel_con_alternativas.csv"),
                  show_col_types = FALSE)
cat("✓ Panel cargado:", nrow(panel), "obs ×", ncol(panel), "vars\n")

# =============================================================================
# PARTE 2: CALCULAR FACTOR DE REBASE (DEFLACTOR JUN-2024)
# =============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("PARTE 2: FACTOR DE REBASE\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

factor_jun2024 <- panel |>
  filter(ANIO == 2024, MES == 6) |>
  pull(DEFLACTOR) |>
  unique()

stopifnot(length(factor_jun2024) == 1)
cat("✓ DEFLACTOR jun-2024 (factor de rebase):",
    round(factor_jun2024, 7), "\n")

# =============================================================================
# PARTE 3: REESCALAR PRECIOS REALES
# =============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("PARTE 3: REESCALAR PRECIOS REALES A CLP JUN-2024\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

# Columnas en niveles: se multiplican por el factor
cols_nivel <- c("P_complejo_real", "P_FOB_REAL", "P_FOB_PERU_REAL",
                "PRECIO_DIESEL_REAL")

# Columnas en logs: se suma log(factor)
cols_log <- c("ln_P_complejo", "ln_P_FOB", "ln_P_FOB_PERU", "ln_DIESEL")

panel_jun2024 <- panel |>
  mutate(across(all_of(cols_nivel), ~ .x * factor_jun2024),
         across(all_of(cols_log),   ~ .x + log(factor_jun2024)))

cat("✓ Rebaseadas", length(cols_nivel), "columnas en niveles y",
    length(cols_log), "columnas en logs.\n")

# =============================================================================
# PARTE 4: VERIFICACIÓN
# =============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("PARTE 4: VERIFICACIÓN (jun-2024 debe coincidir nominal ≈ real)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

chk <- panel_jun2024 |>
  filter(ANIO == 2024, MES == 6) |>
  summarise(
    mean_nominal = mean(P_complejo,      na.rm = TRUE),
    mean_real    = mean(P_complejo_real, na.rm = TRUE),
    ratio        = mean_real / mean_nominal
  )
print(chk)
cat("  → ratio real/nominal en jun-2024 debe ser ≈ 1.0\n")

# =============================================================================
# PARTE 5: GUARDAR
# =============================================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("PARTE 5: GUARDAR\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

write_csv(panel_jun2024,
          here::here("data", "panel_correcto_base_junio_2024.csv"))
cat("✓ Guardado: data/panel_correcto_base_junio_2024.csv (",
    nrow(panel_jun2024), "obs ×", ncol(panel_jun2024), "vars)\n")

cat("\nFIN — panel listo para 16_TAC_instrumento_v2.R\n")
