library(tidyverse)
library(readxl)

base <- read_csv("BASE_156.csv")

ipc_data <- read_excel("IPC_2012_2024.xlsx", skip = 2, col_names = FALSE)

ipc_clean <- ipc_data %>%
  select(1, 2) %>%
  rename(Fecha_Excel = 1, Var_mensual = 2) %>%
  mutate(
    Date = as.Date(as.numeric(Fecha_Excel), origin = "1899-12-30"),
    year = year(Date),
    month = month(Date),
    Var_mensual = as.numeric(Var_mensual)
  ) %>%
  arrange(Date) %>%
  drop_na(Date)

ipc_clean$IPC_index <- 100 * cumprod(1 + ipc_clean$Var_mensual/100)
print(head(ipc_clean))  

base_deflactada <- base %>%
  left_join(ipc_clean %>% select(year, month, IPC_index), by = c("year", "month")) %>%
  mutate(
    P_anchoveta_real = (P_anchoveta / IPC_index) * 100,
    P_sardina_real = (P_sardina / IPC_index) * 100,
    P_jurel_real = (P_jurel / IPC_index ) * 100,
    P_FOB_real = (P_harina_FOB_CLP / IPC_index) * 100
  )

write_csv(base_deflactada, "BASE_156_DFTDA.csv")


library(tidyverse)
library(ggplot2)
library(gridExtra)

base_deflactada <- BASE_156_DFTDA


graficar_demanda <- function(data, x_col, y_col, titulo, color_punto) {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(alpha = 0.6, color = color_punto) + 
    geom_smooth(method = "lm", color = "black", se = TRUE, linetype = "dashed") + 
    geom_smooth(method = "loess", color = "red", se = FALSE) + 
    labs(
      title = titulo,
      x = "Precio Real ($/ton)",
      y = "Cantidad Desembarcada (ton)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}



p1 <- graficar_demanda(base_deflactada, "P_anchoveta_real", "Q_anchoveta", "Demanda Anchoveta", "blue")
p2 <- graficar_demanda(base_deflactada, "P_sardina_real", "Q_sardina", "Demanda Sardina", "firebrick")
p3 <- graficar_demanda(base_deflactada, "P_jurel_real", "Q_jurel", "Demanda Jurel", "forestgreen")


grid.arrange(p1, p2, p3, ncol = 3)

# -----------------------------------------------------------------------------
#  Escala Logarítmica 


graficar_log_log <- function(data, x_col, y_col, titulo, color_punto) {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(alpha = 0.5, color = color_punto) +
    geom_smooth(method = "lm", color = "black", linetype = "dashed") +
    scale_x_log10() + 
    scale_y_log10() + 
    labs(
      title = paste(titulo, "(Log-Log)"),
      x = "Log(Precio Real)",
      y = "Log(Cantidad)"
    ) +
    theme_bw()
}

pl1 <- graficar_log_log(base_deflactada, "P_anchoveta_real", "Q_anchoveta", "Anchoveta", "blue")
pl2 <- graficar_log_log(base_deflactada, "P_sardina_real", "Q_sardina", "Sardina", "firebrick")
pl3 <- graficar_log_log(base_deflactada, "P_jurel_real", "Q_jurel", "Jurel", "forestgreen")

grid.arrange(pl1, pl2, pl3, ncol = 3)

# Elasticidades Simples (Log-Log)
coef(lm(log(Q_anchoveta) ~ log(P_anchoveta_real), data = base_deflactada))[2]
coef(lm(log(Q_sardina) ~ log(P_sardina_real), data = base_deflactada))[2]
coef(lm(log(Q_jurel) ~ log(P_jurel_real), data = base_deflactada))[2]
# + 1 cantidades 0
coef(lm(log(Q_jurel + 1) ~ log(P_jurel_real), data = base_deflactada))[2]
