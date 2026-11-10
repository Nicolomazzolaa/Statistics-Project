library(tidyverse)
library(janitor)
library(tidyverse)
library(janitor)

# 1. Carica i dati usando read_csv
df_mortalita <- read_csv("cause.csv") %>% clean_names()
df_pil <- read_csv("pil.csv") %>% clean_names()
df_popolazione <- read_csv("popolazione.csv") %>% clean_names()

# 2. Unisci a catena i tre dataset tramite inner_join
dataset_completo <- df_mortalita %>%
  inner_join(df_pil, by = c("territorio")) %>%
  inner_join(df_popolazione, by = c("territorio"))


# 3. Esporta il file definitivo per il team
write_csv(dataset_master, "dataset_master_italia.csv")

dataset_master <- dataset_completo %>%
  mutate(
    pil_pro_capite = osservazione.y / osservazione,
    tasso_mortalita = (as.numeric(osservazione.x) / osservazione) * 10000
  )

write_csv(dataset_master, "dataset_master_italia.csv")

