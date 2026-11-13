# ==============================================================================
# SCRIPT DEFINITIVO: 12 CAUSE - BOXPLOT CON TUTTE LE PROVINCE + 3 VARIANTI DI ISTOGRAMMI
# Working Directory: C:\Users\giuli\OneDrive - Università degli Studi di Milano\Desktop\STATISTICS\STATISTICS PROJECT\PROJECT\REPO\Statistics-Project\GIULIA
# ==============================================================================

# 1. Imposta la working directory
setwd("C:/Users/giuli/OneDrive - Università degli Studi di Milano/Desktop/STATISTICS/STATISTICS PROJECT/PROJECT/REPO/Statistics-Project/GIULIA")

# 2. Installa e carica automaticamente i pacchetti necessari (incluso ggrepel per le etichette pulite)
packages <- c("ggplot2", "dplyr", "patchwork", "ggrepel")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 3. PULIZIA TOTALE: Cancella tutti i vecchi file nella cartella risultati per evitare duplicati
if (dir.exists("risultati")) {
  file.remove(list.files("risultati", full.names = TRUE))
}
dir.create("risultati", showWarnings = FALSE)

# 4. Carica i dati e prepara le cause
dati <- read.csv("dati_per_analisi.csv", check.names = FALSE)
colnames(dati) <- c("provincia", "causa", "decessi", "pil_tot", "popolazione", "pil_pc", "tasso")

# Assegna i nomi brevi alle 12 cause esattamente come nel tuo script iniziale
dati$causa_breve <- dati$causa
dati$causa_breve[dati$causa == "Alcune malattie infettive e parassitarie"] <- "Infettive"
dati$causa_breve[dati$causa == "Tumori"] <- "Tumori"
dati$causa_breve[dati$causa == "Malattie del sangue e degli organi ematopoietici ed alcuni disturbi del sistema immunitario"] <- "Sangue/immunitario"
dati$causa_breve[dati$causa == "Disturbi psichici e comportamentali"] <- "Psichici"
dati$causa_breve[dati$causa == "Malattie del sistema nervoso e degli organi di senso"] <- "Sistema nervoso"
dati$causa_breve[dati$causa == "Malattie del sistema circolatorio"] <- "Circolatorio"
dati$causa_breve[dati$causa == "Malattie del sistema respiratorio"] <- "Respiratorio"
dati$causa_breve[dati$causa == "Malattie del sistema osteomuscolare e del tessuto connettivo"] <- "Osteomuscolare"
dati$causa_breve[dati$causa == "Alcune condizioni morbose che hanno origine nel periodo perinatale"] <- "Perinatali"
dati$causa_breve[dati$causa == "Malformazioni congenite ed anomalie cromosomiche"] <- "Malformazioni"
dati$causa_breve[dati$causa == "Covid-19"] <- "Covid-19"
dati$causa_breve[dati$causa == "Cause esterne di traumatismo e avvelenamento"] <- "Cause esterne"

cause <- unique(dati$causa_breve)

# 5. Funzione per generare il report PDF a 2 pagine per ciascuna causa
genera_report_causa <- function(dati_completi, nome_causa) {
  
  df_causa <- dati_completi %>% filter(causa_breve == nome_causa)
  
  m_mean <- mean(df_causa$tasso, na.rm = TRUE)
  m_median <- median(df_causa$tasso, na.rm = TRUE)
  
  # Identificazione outlier per etichettarli
  stats_box <- boxplot.stats(df_causa$tasso)$stats
  lower_bound <- stats_box[1]
  upper_bound <- stats_box[5]
  
  df_causa <- df_causa %>%
    mutate(
      is_outlier = tasso < lower_bound | tasso > upper_bound,
      label_prov = ifelse(is_outlier, as.character(provincia), "")
    )
  
  nome_file_safe <- gsub("/", "_", nome_causa)
  
  # A. BOXPLOT ESTETICO: più stretto e con TUTTE le province rappresentate come punti
  p_box <- ggplot(df_causa, aes(x = "", y = tasso)) +
    geom_boxplot(fill = "lightblue", color = "darkblue", width = 0.35, 
                 outlier.shape = NA, alpha = 0.6) +
    geom_jitter(aes(color = is_outlier), width = 0.12, size = 2, alpha = 0.8) +
    scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "firebrick"), guide = "none") +
    geom_text_repel(aes(label = label_prov), size = 3, color = "darkred", max.overlaps = 20) +
    labs(
      title = paste("Boxplot -", nome_causa),
      x = "",
      y = "Deaths per 10,000 inhabitants"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  # B. ISTOGRAMMA CON BINS STRETTI (Pagina 1)
  p_hist_stretti <- ggplot(df_causa, aes(x = tasso)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.85) +
    geom_vline(aes(xintercept = m_mean, color = "mean"), linetype = "solid", linewidth = 1) +
    geom_vline(aes(xintercept = m_median, color = "median"), linetype = "dashed", linewidth = 1) +
    scale_color_manual(
      name = "", 
      values = c("mean" = "firebrick", "median" = "black"),
      labels = c(paste0("mean = ", round(m_mean, 1)), 
                 paste0("median = ", round(m_median, 1)))
    ) +
    labs(
      title = paste0("Histogram - ", nome_causa, " (Bins Stretti)"),
      x = "Deaths per 10,000 inhabitants",
      y = "Number of provinces"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = c(0.78, 0.82),
      legend.background = element_rect(fill = alpha("white", 0.9), color = NA)
    )
  
  # C. ALTRI DUE ISTOGRAMMI CON BINS DIVERSI: Bins = 10 e Bins = 20 (Pagina 2)
  p_hist_10 <- ggplot(df_causa, aes(x = tasso)) +
    geom_histogram(bins = 10, fill = "cornflowerblue", color = "white", alpha = 0.85) +
    geom_vline(xintercept = m_mean, color = "firebrick", linetype = "solid", linewidth = 1) +
    geom_vline(xintercept = m_median, color = "black", linetype = "dashed", linewidth = 1) +
    labs(title = paste0(nome_causa, " - Bins = 10"), x = "Deaths per 10,000 inhabitants", y = "Count") +
    theme_minimal() + theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5))
  
  p_hist_20 <- ggplot(df_causa, aes(x = tasso)) +
    geom_histogram(bins = 20, fill = "dodgerblue4", color = "white", alpha = 0.85) +
    geom_vline(xintercept = m_mean, color = "firebrick", linetype = "solid", linewidth = 1) +
    geom_vline(xintercept = m_median, color = "black", linetype = "dashed", linewidth = 1) +
    labs(title = paste0(nome_causa, " - Bins = 20"), x = "Deaths per 10,000 inhabitants", y = "Count") +
    theme_minimal() + theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5))
  
  # Creazione file PDF a 2 pagine nella cartella risultati
  pdf_path <- paste0("risultati/report_", nome_file_safe, ".pdf")
  pdf(pdf_path, width = 11, height = 6.5)
  
  # PAGINA 1: Boxplot con tutte le province + Istogramma a bins stretti
  pag1 <- (p_box | p_hist_stretti) + plot_layout(widths = c(0.9, 1.3))
  print(pag1)
  
  # PAGINA 2: Gli altri due istogrammi (bins 10 e bins 20)
  pag2 <- (p_hist_10 / p_hist_20)
  print(pag2)
  
  dev.off()
}

# 6. Esecuzione del ciclo per tutte le 12 cause
for (k in cause) {
  message(paste("Generazione report PDF per:", k))
  genera_report_causa(dati, k)
}

print("Fatto! Vecchi file eliminati. Generati con successo i report PDF per tutte le 12 cause.")