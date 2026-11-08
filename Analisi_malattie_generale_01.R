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
