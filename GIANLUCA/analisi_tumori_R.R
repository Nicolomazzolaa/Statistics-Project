install.packages("ggplot2")
install.packages("dplyr")

dati <- read.csv(
  file.choose(),
  header = TRUE,
  sep = ",",
  dec = "."
)
View(dati)
install.packages("dplyr")
tumori <- dati[
  dati[["causa_iniziale_di_morte_european_short_list"]] == "Tumori",
]
View(tumori)
tasso <- as.numeric(tumori[[7]])

media <- mean(tasso, na.rm = TRUE)

mediana <- median(tasso, na.rm = TRUE)

dev_std <- sd(tasso, na.rm = TRUE)

# CARICAMENTO PACCHETTI
library(ggplot2)

# CALCOLO DELLE STATISTICHE
tasso <- as.numeric(tumori[[7]])

media <- mean(tasso, na.rm = TRUE)
mediana <- median(tasso, na.rm = TRUE)
dev_std <- sd(tasso, na.rm = TRUE)

# CREAZIONE DEL BAR PLOT
grafico_barre <- ggplot(
  tumori,
  aes(
    x = reorder(territorio, tasso),
    y = tasso
  )
) +
  
  # BARRE VERTICALI
  geom_col(
    fill = "steelblue",
    width = 0.85
  ) +
  
  # LINEA DELLA MEDIA
  geom_hline(
    yintercept = media,
    color = "red",
    linewidth = 1
  ) +
  
  # LINEA DELLA MEDIANA
  geom_hline(
    yintercept = mediana,
    color = "darkgreen",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  # LINEE DELLA DEVIAZIONE STANDARD
  geom_hline(
    yintercept = media + dev_std,
    color = "orange",
    linetype = "dotted",
    linewidth = 1
  ) +
  
  geom_hline(
    yintercept = media - dev_std,
    color = "orange",
    linetype = "dotted",
    linewidth = 1
  ) +
  
  # TITOLI ED ETICHETTE
  labs(
    title = "Tasso di mortalità per tumore nelle province italiane",
    subtitle = "Decessi per 10.000 abitanti",
    x = "Province e territori",
    y = "Tasso di mortalità",
    caption = paste0(
      "Media = ", round(media, 2),
      " | Mediana = ", round(mediana, 2),
      " | Deviazione standard = ", round(dev_std, 2),
      "\nRosso = Media | Verde tratteggiato = Mediana",
      " | Arancione = Media ± 1 DS"
    )
  ) +
  
  # GRAFICA E LEGGIBILITÀ
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(size = 12),
    
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 8
    ),
    
    axis.text.y = element_text(size = 10),
    
    axis.title = element_text(size = 12),
    
    plot.caption = element_text(size = 10),
    
    panel.grid.major.x = element_blank()
  )

# VISUALIZZAZIONE DEL GRAFICO
print(grafico_barre)

# SALVATAGGIO DEL GRAFICO IN PNG
ggsave(
  "barplot_tumori.png",
  plot = grafico_barre,
  width = 28,
  height = 14,
  units = "in",
  dpi = 300
)

# SALVATAGGIO DEL GRAFICO IN PDF
ggsave(
  "barplot_tumori.pdf",
  plot = grafico_barre,
  width = 28,
  height = 14,
  units = "in"
)
panel.grid.major.x = element_blank()
  )
getwd()
ggsave(
  "barplot_tumori.png",
  plot = grafico_barre,
  width = 28,
  height = 14,
  units = "in",
  dpi = 300
)





