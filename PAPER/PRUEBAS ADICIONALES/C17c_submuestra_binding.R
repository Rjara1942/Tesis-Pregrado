# ==============================================================================
# BLOQUE C-17c — Submuestra con TAC binding (ratio ≥85 %)
# ==============================================================================
# Robustez: excluye 2013 y 2024 (únicos años con razón h/TAC < 85 %) y
# re-estima el IV principal. Si γ se mantiene, el referee no tiene argumento
# en que la cuota sea no restrictiva.
# ==============================================================================

library(tidyverse)
library(fixest)
library(here)

PATH_PANEL <- here::here("data", "panel_upgrade.csv")

df <- read_csv(PATH_PANEL, show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES)),
    ln_P_complejo_real = log(P_complejo_real)
  ) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup() |>
  filter(!(NUI == "90073" & ANIO == 2013))

# Años con cuota binding (ratio h/TAC ≥ 85 %)
anios_binding <- c(2012, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)
anios_holgada <- c(2013, 2024)

cat("Años binding (≥85%):", paste(anios_binding, collapse = ", "), "\n")
cat("Años holgada (<85%):", paste(anios_holgada, collapse = ", "), "\n\n")

df_bind <- filter(df, ANIO %in% anios_binding)
df_hol  <- filter(df, ANIO %in% anios_holgada)

cat(sprintf("Muestra total:  N=%d, G=%d\n", nrow(df), n_distinct(df$NUI)))
cat(sprintf("Muestra binding: N=%d, G=%d, años=%d\n",
            nrow(df_bind), n_distinct(df_bind$NUI), length(anios_binding)))
cat(sprintf("Muestra holgada: N=%d, G=%d, años=%d\n\n",
            nrow(df_hol),  n_distinct(df_hol$NUI),  length(anios_holgada)))

FORM_IV <- ln_P_complejo_real ~ ln_h_jurel + SEASON_SIN + SEASON_COS +
                                TENDENCIA + ln_P_FOB | NUI |
             ln_h_complejo ~ ln_biomasa_sardina + ln_TAC_complejo

estimar <- function(d, nombre) {
  m <- feols(FORM_IV, data = d, vcov = DK(4) ~ period)
  g <- coef(m)["fit_ln_h_complejo"]
  s <- se(m)["fit_ln_h_complejo"]
  ci <- confint(m, "fit_ln_h_complejo")
  F <- as.numeric(fitstat(m, "ivf")$`ivf1::ln_h_complejo`$stat)
  cat(sprintf("%-25s γ = %+.3f  SE = %.3f  IC95%% = [%+.3f, %+.3f]  F = %.2f  N = %d\n",
              nombre, g, s, ci[1], ci[2], F, m$nobs))
  tibble(especificacion = nombre, gamma = g, se = s,
         ci_low = ci[1], ci_high = ci[2], F = F, N = m$nobs)
}

cat("Resultados:\n")
cat(strrep("-", 95), "\n")
res <- bind_rows(
  estimar(df,      "Muestra total (baseline)"),
  estimar(df_bind, "Solo años binding ≥85%"),
  estimar(df_hol,  "Solo años holgada <85%")
)

# Guardar
dir.create(here::here("outputs", "reportes_intermedios"),
           showWarnings = FALSE, recursive = TRUE)
write_csv(res, here::here("outputs", "reportes_intermedios",
                           "C17c_submuestra_binding.csv"))

cat("\n==============================================================\n")
cat("Lectura para el paper\n")
cat("==============================================================\n")
cat(sprintf("- Baseline:  γ = %+.3f (todos los años)\n", res$gamma[1]))
cat(sprintf("- Binding:   γ = %+.3f (10 años, 85%% del panel)\n", res$gamma[2]))
cat(sprintf("- Holgada:   γ = %+.3f (sólo 2013 + 2024)\n", res$gamma[3]))
cat("\nSi γ_binding ≈ γ_baseline, se defiende que el resultado del paper\n")
cat("viene de la variación en años con TAC activo. Si γ_holgada es muy\n")
cat("distinto, se puede justificar reportar el baseline y la submuestra\n")
cat("binding como robustez principal.\n")
