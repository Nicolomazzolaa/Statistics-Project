library(tidyverse)
library(broom)

# 1. Caricamento dati e riscalamento del PIL
# Moltiplichiamo per 1000 in modo che 1 unità = 1.000 Euro, 
# rendendo i coefficienti di regressione facili da interpretare.
df <- read_csv("dati_per_analisi.csv") %>%
  mutate(pil_pro_capite = pil_pro_capite * 1000)

View(df)

# 2. Esplorazione Visiva (Scatterplot multipannello)
ggplot(df, aes(x = pil_pro_capite, y = `tasso_mortalita (10k abitanti)`)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) + # Aggiunge la linea di tendenza lineare
  facet_wrap(~ causa_iniziale_di_morte_european_short_list, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Relazione tra PIL pro capite e Mortalità per Causa",
    x = "PIL Pro Capite (Migliaia di Euro)",
    y = "Tasso di Mortalità (per 10k abitanti)"
  )

# 3. Calcolo della correlazione
correlazioni <- df %>%
  group_by(causa_iniziale_di_morte_european_short_list) %>%
  summarise(
    # Usiamo Spearman perché i test preliminari indicano che i tassi di 
    # mortalità per alcune cause non sono distribuiti in modo perfettamente normale
    correlazione = cor(pil_pro_capite, `tasso_mortalita (10k abitanti)`, method = "spearman")
  ) %>%
  arrange(correlazione)

print(correlazioni)

# 4. Regressione lineare per ogni macromalattia
risultati_regressione <- df %>%
  group_by(causa_iniziale_di_morte_european_short_list) %>%
  # Modello lineare: Mortalità in funzione del PIL pro capite
  do(tidy(lm(`tasso_mortalita (10k abitanti)` ~ pil_pro_capite, data = .))) %>%
  filter(term != "(Intercept)") %>% # Filtriamo l'intercetta, ci interessa solo l'effetto del PIL
  select(causa_iniziale_di_morte_european_short_list, estimate, p.value) %>%
  mutate(
    # Creiamo una colonna per leggere al volo la significatività
    significativo = ifelse(p.value < 0.05, "Sì", "No") 
  ) %>%
  arrange(estimate)

print(risultati_regressione)


#boxplot delle malattie

# Caricamento delle librerie
library(tidyverse)

# 1. Caricamento dei dati
dati <- read_csv("dati_per_analisi.csv")

# 2. Calcolo della statistica descrittiva per ciascuna malattia
tabella_statistiche <- dati %>%
  group_by(causa_iniziale_di_morte_european_short_list) %>%
  summarise(
    Media = mean(`tasso_mortalita (10k abitanti)`, na.rm = TRUE),
    Mediana = median(`tasso_mortalita (10k abitanti)`, na.rm = TRUE),
    Dev_Standard = sd(`tasso_mortalita (10k abitanti)`, na.rm = TRUE),
    Varianza = var(`tasso_mortalita (10k abitanti)`, na.rm = TRUE)
  ) %>%
  arrange(desc(Media))

# Stampa la tabella riassuntiva in console
print(tabella_statistiche)


# 3. Creazione del Boxplot (con le cause ordinate per media decrescente)
ggplot(dati, aes(x = reorder(causa_iniziale_di_morte_european_short_list, `tasso_mortalita (10k abitanti)`, FUN = median), 
                 y = `tasso_mortalita (10k abitanti)`)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.colour = "red", outlier.size = 1.5) +
  coord_flip() + # Gira il grafico in orizzontale per leggere i nomi delle malattie
  theme_minimal() +
  labs(
    title = "Distribuzione dei Tassi di Mortalità per Causa",
    subtitle = "Confronto tra le province italiane (Boxplot)",
    x = "Causa di Morte",
    y = "Tasso di Mortalità (per 10.000 abitanti)"
  ) +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 12)
  )



library(tidyverse)

dati <- read_csv("dati_per_analisi.csv")

# Statistiche descrittive del tasso di mortalità per ciascuna causa
statistiche_mortalita <- dati %>%
  group_by(causa_iniziale_di_morte_european_short_list) %>%
  summarise(
    Media = mean(`tasso_mortalita (10k abitanti)`, na.rm = TRUE),
    Mediana = median(`tasso_mortalita (10k abitanti)`, na.rm = TRUE),
    Dev_Standard = sd(`tasso_mortalita (10k abitanti)`, na.rm = TRUE),
    Varianza = var(`tasso_mortalita (10k abitanti)`, na.rm = TRUE)
  ) %>%
  arrange(desc(Media))

print(statistiche_mortalita)

# 2. Creazione del grafico per la Statistica 
ggplot(dati, aes(x = reorder(causa_iniziale_di_morte_european_short_list, `tasso_mortalita (10k abitanti)`, FUN = median), 
                 y = `tasso_mortalita (10k abitanti)`)) +
  # Boxplot di fondo (mostra mediana e quartili)
  geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.shape = NA) +
  # Punti reali di ciascuna delle 107 province sovrapposti
  geom_jitter(color = "darkblue", size = 1.3, width = 0.2, alpha = 0.6) +
  # Gira il grafico in orizzontale per leggere perfettamente i nomi delle malattie
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribuzione dei Tassi di Mortalità per Causa",
    subtitle = "Ogni punto corrisponde a una provincia italiana (107 osservazioni)",
    x = "Causa di Morte",
    y = "Tasso di Mortalità (per 10.000 abitanti)"
  ) +
  theme(
    axis.text.y = element_text(size = 9, face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank()
  )
