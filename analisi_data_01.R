data <- read.csv("dataset_master_italia.csv")



View(data)

# Installa il pacchetto se non lo hai già fatto: install.packages("dplyr")
library(dplyr)

# Vediamo quali cause sono disponibili per sceglierne una
unique(data$causa_iniziale_di_morte_european_short_list)

# Creiamo un sotto-dataset per una causa specifica (es. malattie del sistema circolatorio)
# Sostituisci il testo con la causa esatta che vuoi analizzare
data_malattie_sistema_circolatorio <- data %>%
  filter(causa_iniziale_di_morte_european_short_list == "Malattie del sistema circolatorio")

View(data_malattie_sistema_circolatorio)


library(dplyr)

data_malattie_sistema_circolatorio <- data_malattie_sistema_circolatorio %>%
  # 1. Raggruppa i dati per territorio (provincia)
  group_by(territorio) %>%
  # 2. Assicurati che R legga la colonna come numero, non come testo
  mutate(osservazione.x = as.numeric(osservazione.x)) %>%
  # 3. Mantieni solo la riga con il valore massimo (il valore assoluto)
  slice_max(order_by = osservazione.x, n = 1, with_ties = FALSE) %>%
  # 4. Rimuovi il raggruppamento per non avere problemi con le analisi successive
  ungroup()

# Apri la tabella per verificare che le 214 righe siano diventate 107
View(data_malattie_sistema_circolatorio)

data_malattie_sistema_circolatorio$tasso_mortalita <- as.numeric(data_malattie_sistema_circolatorio$tasso_mortalita)



# Installa il pacchetto se non lo hai già fatto: install.packages("ggplot2")
library(ggplot2)

ggplot(data_malattie_sistema_circolatorio, aes(x = pil_pro_capite, y = tasso_mortalita)) +
  geom_point(alpha = 0.6, color = "blue") +      # Punti del grafico
  geom_smooth(method = "lm", color = "red") +    # Linea di tendenza lineare
  theme_minimal() +
  labs(
    title = "Relazione tra PIL pro capite e Tasso di Mortalità",
    subtitle = "Causa: Malattie infettive e parassitarie",
    x = "PIL pro capite (Determinante Socio-Economico)",
    y = "Tasso di Mortalità"
  )



# 1. Test di Correlazione (misura la forza dell'associazione)
cor.test(data_malattie_sistema_circolatorio$pil_pro_capite, data_malattie_sistema_circolatorio$tasso_mortalita)

# 2. Modello di Regressione (misura l'impatto)
modello_circ <- lm(tasso_mortalita ~ pil_pro_capite, data = data_malattie_sistema_circolatorio)
summary(modello_circ)


# 1. Installa i pacchetti necessari (esegui questa riga solo una volta)
install.packages(c("dplyr", "tidyr", "purrr", "broom"))

# 2. Carica le librerie
library(dplyr)
library(tidyr)
library(purrr)
library(broom)

# 3. Pulizia globale dei dati (rimuove i tassi ISTAT duplicati per TUTTE le cause)
dati_completi_puliti <- data %>%
  mutate(
    osservazione.x = as.numeric(osservazione.x),
    tasso_mortalita = as.numeric(tasso_mortalita)
  ) %>%
  group_by(territorio, causa_iniziale_di_morte_european_short_list) %>%
  slice_max(order_by = osservazione.x, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # Escludiamo eventuali valori mancanti per evitare errori matematici
  filter(!is.na(tasso_mortalita) & !is.na(pil_pro_capite))

# 4. Calcolo automatico di tutte le regressioni
tabella_riassuntiva <- dati_completi_puliti %>%
  # Raggruppa i dati separando ogni singola causa di morte
  nest(dati = -causa_iniziale_di_morte_european_short_list) %>%
  mutate(
    # Esegue il modello lineare (lm) su ciascun gruppo
    modello = map(dati, ~ lm(tasso_mortalita ~ pil_pro_capite, data = .x)),
    # Estrae Estimate e P-value
    risultati = map(modello, tidy),
    # Estrae il Multiple R-squared
    statistiche = map(modello, glance)
  ) %>%
  # Srotola i risultati in formato tabella
  unnest(risultati) %>%
  # Ci interessa solo la riga dell'impatto del PIL (escludendo l'intercetta)
  filter(term == "pil_pro_capite") %>%
  unnest(statistiche, names_sep = "_") %>%
  # Seleziona, rinomina e ordina le colonne finali
  select(
    Causa = causa_iniziale_di_morte_european_short_list,
    Impatto_Estimate = estimate,
    P_Value = p.value,
    R_Quadrato = statistiche_r.squared
  ) %>%
  # Ordina la tabella mettendo in cima le relazioni statisticamente più significative
  arrange(P_Value)

# 5. Visualizza il risultato finale
View(tabella_riassuntiva)


# 1. Spegne la notazione scientifica in tutto RStudio
options(scipen = 999)

# 2. Crea una versione leggibile della tua tabella arrotondando i numeri
tabella_riassuntiva_leggibile <- tabella_riassuntiva %>%
  mutate(
    # Arrotonda l'impatto a 2 decimali
    Impatto_Estimate = round(Impatto_Estimate, 2),
    
    # Arrotonda il P-value a 5 decimali (così vedi chiaramente gli 0.00000)
    P_Value = round(P_Value, 5),
    
    # Moltiplica l'R-quadrato per 100 per renderlo una percentuale e tiene 2 decimali
    R_Quadrato_Perc = round(R_Quadrato * 100, 2)
  ) %>%
  # Rimuove la vecchia colonna R_quadrato non in percentuale
  select(-R_Quadrato)

# 3. Visualizza la tabella pulita
View(tabella_riassuntiva_leggibile)

library(tidyverse)
library(janitor)
library(tidyverse)
library(janitor)


write_csv(tabella_riassuntiva_leggibile, "tabella_riassuntiva_leggibile.csv")


write_csv(dati_completi_puliti, "dati_completi_puliti.csv")

