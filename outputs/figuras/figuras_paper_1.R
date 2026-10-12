# ==============================================================================
# Figuras del paper (ggplot2). Corre desde la raiz del repositorio.

# ==============================================================================
library(tidyverse)
library(scales)
library(here)

DIR_OUT <- here::here("outputs", "figuras_paper")
dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# Panel (misma construccion que el script de estimacion)
# ------------------------------------------------------------------------------
df <- read_csv(here::here("data", "panel_upgrade.csv"), show_col_types = FALSE) |>
  mutate(
    NUI                = as.character(NUI),
    ln_P_complejo_real = log(P_complejo_real),
    period             = as.Date(sprintf("%04d-%02d-01", ANIO, MES))
  ) |>
  group_by(NUI) |> filter(n() >= 2) |> ungroup() |>
  filter(!(NUI == "90073" & ANIO == 2013))

stopifnot(nrow(df) == 427, length(unique(df$NUI)) == 16)

cat("Muestra: N =", nrow(df), "G =", length(unique(df$NUI)),
    "T =", length(unique(df$period)), "\n\n")

theme_paper <- theme_minimal(base_size = 9) +
  theme(
    axis.title       = element_text(size = 8.5),
    axis.text        = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, color = "grey88")
  )

azul <- "#0072B2"; naranja <- "#D55E00"
fmt_dec <- label_number(decimal.mark = ",", accuracy = 0.1, style_negative = "minus")

# ------------------------------------------------------------------------------
# FIGURA 1. Precio real y desembarque: promedios anuales
#   Ambos en logaritmos y como desvio respecto de la media de cada planta, de modo
#   que comparten unidad (un solo eje). Circulos huecos: anios con < 20 observaciones.
# ------------------------------------------------------------------------------
anual <- df |>
  mutate(precio = ln_P_complejo_real - ave(ln_P_complejo_real, NUI),
         desemb = ln_h_complejo      - ave(ln_h_complejo, NUI)) |>
  group_by(ANIO) |>
  summarise(precio = mean(precio), desemb = mean(desemb), n = n(), .groups = "drop") |>
  pivot_longer(c(precio, desemb), names_to = "serie", values_to = "valor") |>
  mutate(poca_muestra = n < 20)

cols <- c(precio = azul, desemb = naranja)

fig1 <- ggplot(anual, aes(ANIO, valor, color = serie)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_line(linewidth = 0.6) +
  geom_point(data = filter(anual, !poca_muestra), size = 1.9) +
  geom_point(data = filter(anual, poca_muestra), size = 1.9,
             shape = 21, fill = "white", stroke = 0.6) +
  annotate("text", x = 2014.9, y = 0.25, label = "Precio real",
           color = azul, size = 3.2) +
  annotate("text", x = 2016.5, y = -0.44, label = "Desembarque del complejo",
           color = naranja, size = 3.2) +
  scale_color_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = seq(2012, 2024, 2)) +
  scale_y_continuous(labels = fmt_dec, limits = c(-0.52, 0.45)) +
  labs(x = NULL, y = "Desvío respecto de la media\nde cada planta (log)") +
  theme_paper

ggsave(file.path(DIR_OUT, "fig_interanual.pdf"), fig1,
       width = 6.5, height = 3.0, device = "pdf")

# ------------------------------------------------------------------------------
# FIGURA 2 (opcional). Cobertura del panel: observaciones por planta y anio
#   Entre parentesis, bajo cada anio: plantas con al menos una observacion.
# ------------------------------------------------------------------------------
cob <- df |>
  count(NUI, ANIO) |>
  complete(NUI, ANIO = 2012:2024, fill = list(n = 0L))

orden <- df |>
  group_by(NUI) |>
  summarise(primero = min(ANIO), total = n(), .groups = "drop") |>
  arrange(primero, desc(total)) |>
  pull(NUI)

activas <- cob |> group_by(ANIO) |> summarise(act = sum(n > 0), .groups = "drop")
cat("Plantas activas por anio (muestra de 427):",
    paste(activas$act, collapse = ", "), "\n")

cob <- cob |>
  mutate(NUI = factor(NUI, levels = rev(orden)),
         anio_f = factor(ANIO),
         n_pos = if_else(n > 0, as.numeric(n), NA_real_))

etiq_x <- setNames(paste0(activas$ANIO, "\n(", activas$act, ")"), activas$ANIO)

fig2 <- ggplot(cob, aes(anio_f, NUI)) +
  geom_tile(aes(fill = n_pos), color = "white", linewidth = 0.6) +
  geom_text(data = filter(cob, n > 0),
            aes(label = n, color = n > 6), size = 2.5) +
  scale_fill_gradient(low = "#DCE9F5", high = "#08519C", na.value = "white",
                      limits = c(0, 12), guide = "none") +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey15"), guide = "none") +
  scale_x_discrete(labels = etiq_x, position = "bottom") +
  labs(x = NULL, y = "Planta (NUI)") +
  theme_paper +
  theme(panel.grid = element_blank(), axis.ticks = element_blank())

ggsave(file.path(DIR_OUT, "fig_cobertura.pdf"), fig2,
       width = 6.5, height = 3.7, device = "pdf")


