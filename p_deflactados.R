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


