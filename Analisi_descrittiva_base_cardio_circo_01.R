library(tidyverse)

# 1. Carica il file gestendo "/" come dato mancante
dati <- read_csv("dati_grezzi_puliti.csv", na = c("", "NA", "/"))

# 2. Converte forzatamente in formato numerico
dati <- dati %>%
  mutate(
    tasso_mortalita = as.numeric(as.character(tasso_mortalita)),
    pil_pro_capite = as.numeric(as.character(pil_pro_capite))
  )

# 3. Calcola il quadro statistico completo per provincia
stats_per_provincia <- dati %>%
  group_by(territorio) %>%
  summarise(
    tasso_mortalità_medio = mean(tasso_mortalita, na.rm = TRUE),
    tasso_mortalità_mediana = median(tasso_mortalita, na.rm = TRUE),
    tasso_mortalità_sd = sd(tasso_mortalita, na.rm = TRUE),
    pil_medio = mean(pil_pro_capite, na.rm = TRUE)
  )

# 4. Visualizza la tabella finale

View(stats_per_provincia)


library(tidyverse)

# 5. Calcola la tabella riassuntiva pulita per provincia
stats_per_provincia <- dati %>%
  group_by(territorio) %>%
  summarise(
    pil_medio = mean(pil_pro_capite, na.rm = TRUE),
    tasso_medio = mean(tasso_mortalita, na.rm = TRUE)
  )

# 6. Crea un grafico semplice ma chiarissimo con i nomi delle città
plot(stats_per_provincia$pil_medio, stats_per_provincia$tasso_medio, 
     pch = 19, col = "steelblue", cex = 1.2,
     main = "PIL pro-capite vs Tasso di Mortalità medio per Provincia",
     xlab = "PIL pro-capite medio", ylab = "Tasso di Mortalità medio")

# 7. Aggiunge i nomi delle province vicino ai punti
text(stats_per_provincia$pil_medio, stats_per_provincia$tasso_medio, 
     labels = stats_per_provincia$territorio, cex = 0.7, pos = 3, col = "darkblue")

# 8. Aggiunge la retta di tendenza lineare
abline(lm(tasso_medio ~ pil_medio, data = stats_per_provincia), col = "darkred", lwd = 2, lty = 2)
