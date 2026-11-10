getwd()

dati <- read.csv("dati_per_analisi.csv", check.names = FALSE)

#rinomina colonne per comodità 
colnames(dati) <- c("provincia", "causa", "decessi", "pil_tot", "popolazione",
                    "pil_pc", "tasso")

#controlli di base 
str(dati)
head(dati)
nrow(dati)
sum(is.na(dati))

#prep. variabili 
dati$pil_pc_k <- dati$pil_pc * 1000 #PIL in migiaia più leggibile
#nomi delle colonne più brevi e leggibili
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

# tabella con una sola riga per provincia, per le analisi sul PIL
province <- dati[!duplicated(dati$provincia), c("provincia", "popolazione", "pil_tot", "pil_pc_k")]
nrow(province)

dir.create("risultati", showWarnings = FALSE)

#tabella descrittiva per ogni causa
#per ogni causa --> n, media, mediana, deviazione standard, varianza, minimo e massimo e coefficiente di variazione
riassunto <- function(x) {
  c(n        = length(x),
    media    = mean(x),
    mediana  = median(x),
    dev_std  = sd(x),
    varianza = var(x),
    minimo   = min(x),
    massimo  = max(x),
    cv       = sd(x) / mean(x))   # coefficiente di variazione
}

#la funzione tapply(X, G, f) applica la funzione f a X separatamente per ogni gruppo G
lista <- tapply(dati$tasso, dati$causa_breve, riassunto)
tabella_cause <- do.call(rbind, lista) #unisce in una tabella 
tabella_cause <- round(tabella_cause, 2) #arrotonda a due decimali
tabella_cause <- tabella_cause[order(-tabella_cause[, "media"]), ] #dalla più frequente 
print(tabella_cause)
write.csv(tabella_cause, "risultati/tabella_cause.csv")

print(round(riassunto(province$pil_pc_k),2))
summary(province$pil_pc_k)

#GRAFICO A BARRE PER CUASA DI MORTE DI TUTTE LE PRONVINCE
cause <- unique(dati$causa_breve)

pdf("risultati/province_per_causa.pdf", width = 8, height = 18)
for (k in cause) {
  s <- dati[dati$causa_breve == k, ]     # solo le righe di questa causa
  s <- s[order(s$tasso), ]               # ordina dal tasso più basso al più alto
  
  par(mar = c(4, 9, 3, 1))               # più spazio a sinistra per i nomi
  barplot(s$tasso, names.arg = s$provincia,
          horiz = TRUE,                  # barre orizzontali
          las = 1,                       # nomi scritti in orizzontale
          cex.names = 0.5,               # nomi piccoli, per farli stare tutti
          col = "steelblue", border = NA,
          main = paste("Mortalità per", k, "- confronto tra province"),
          xlab = "Decessi ogni 10.000 abitanti")
  abline(v = mean(s$tasso), col = "red", lwd = 2, lty = 2)   # linea della media
}
dev.off()

#ESTREMI PER I TUMORI (prime dieci provincie e ultime dieci province)
s <- dati[dati$causa_breve == "Tumori", ]
s <- s[order(s$tasso), ]
s <- rbind(head(s, 10), tail(s, 10))     # 10 più basse + 10 più alte

png("risultati/tumori_estremi.png", width = 900, height = 700)
par(mar = c(4, 10, 3, 1))
barplot(s$tasso, names.arg = s$provincia, horiz = TRUE, las = 1,
        col = c(rep("palegreen3", 10), rep("tomato", 10)), border = NA,
        main = "Tumori: province con mortalità più bassa e più alta",
        xlab = "Decessi ogni 10.000 abitanti")
dev.off()

#BOXPLOT
pdf("risultati/boxplot_province_per_causa.pdf", width = 8, height = 5)
for (k in cause) {
  s <- dati[dati$causa_breve == k, ]
  
  b <- boxplot(s$tasso, horizontal = TRUE, col = "lightblue",
               main = paste("Boxplot -", k),
               xlab = "Decessi ogni 10.000 abitanti")
  
  # un punto per ogni provincia, spostati un po' in verticale per non sovrapporsi
  stripchart(s$tasso, method = "jitter", add = TRUE,
             pch = 19, col = rgb(0, 0, 0, 0.3), cex = 0.7)
  
  # nomi delle province outlier (b$out contiene i valori degli outlier)
  out <- s[s$tasso %in% b$out, ]
  if (nrow(out) > 0) {
    text(out$tasso, 1.1, labels = out$provincia, pos = 3, cex = 0.7)
  }
}
dev.off()

#UN SOLO BOXPLOT CON TUTTE LE CAUSE 
#usiamo una scala logaritmica sull'asse delle y 
png("GIULIA/risultati/boxplot_tutte_le_cause.png", width = 1100, height = 650)
par(mar = c(9, 5, 3, 1))     # più spazio in basso per le etichette
boxplot(tasso ~ causa_breve, data = dati, las = 2, log = "y", col = "lightblue",
        ylab = "Decessi ogni 10.000 abitanti (scala logaritmica)", xlab = "",
        main = "Mortalità per causa: confronto tra le province (2023)")
dev.off()


#PROVINCE CON I VALORI PIù ALTI E PIù BASSI 
#per ogni causa --> le 3 province con il tasso più basso e le pre con il taso più alto
estremi <- data.frame()
for (k in cause) {
  s <- dati[dati$causa_breve == k, c("provincia", "tasso")]
  s <- s[order(s$tasso), ]
  estremi <- rbind(estremi,
                   data.frame(causa = k, tipo = "più bassi",
                              province = paste(head(s$provincia, 3), collapse = ", ")),
                   data.frame(causa = k, tipo = "più alti",
                              province = paste(tail(s$provincia, 3), collapse = ", ")))
}
print(estremi)
write.csv(estremi, "GIULIA/risultati/province_estreme.csv", row.names = FALSE)

#PIL PRO CAPITE: ISTOGRAMMA E BOXPLOT
png("GIULIA/risultati/pil_pro_capite.png", width = 1000, height = 450)
par(mfrow = c(1, 2))
hist(province$pil_pc_k, breaks = 15, col = "lightgreen", border = "white",
     main = "PIL pro capite delle province (2023)",
     xlab = "Migliaia di euro per abitante", ylab = "Numero di province")
boxplot(province$pil_pc_k, col = "lightgreen",
        main = "PIL pro capite", ylab = "Migliaia di euro per abitante")
dev.off()

# Quali province sono outlier per il PIL pro capite?
boxplot.stats(province$pil_pc_k)$out
province$provincia[province$pil_pc_k %in% boxplot.stats(province$pil_pc_k)$out]

#MORTALITà VS PIL PRO CAPITE (scatterplot e correlazione)
#per ogni causa: nuvola di punti (una per provincia) + retta + correlazione
# cor() calcola il coefficiente di Pearson (r): associazione LINEARE 
# method = "spearman" lavora sui ranghi: associazione monotona, meno sensibile agli outlier

pdf("GIULIA/risultati/scatter_pil_vs_cause.pdf", width = 7, height = 5)
corr <- data.frame()
for (k in cause) {
  s <- dati[dati$causa_breve == k, ]
  r   <- cor(s$pil_pc_k, s$tasso)
  rho <- cor(s$pil_pc_k, s$tasso, method = "spearman")
  corr <- rbind(corr, data.frame(causa = k, n = nrow(s),
                                 pearson = round(r, 2), spearman = round(rho, 2)))
  plot(s$pil_pc_k, s$tasso, pch = 19, col = rgb(0, 0, 1, 0.5),
       xlab = "PIL pro capite (migliaia di euro)",
       ylab = "Decessi ogni 10.000 abitanti",
       main = paste0(k, "  (r = ", round(r, 2), ")"))
  abline(lm(tasso ~ pil_pc_k, data = s), col = "red", lwd = 2)  # retta di riferimento
}
dev.off()

corr <- corr[order(-abs(corr$pearson)), ]
print(corr)
write.csv(corr, "GIULIA/risultati/correlazioni.csv", row.names = FALSE)

#MORTALITà TOTALE 
#sommiamo i decessi delle cause per provincia e dividiamo per la popolazione 
totale <- aggregate(decessi ~ provincia, data = dati, FUN = sum)
totale <- merge(totale, province, by = "provincia")
totale$tasso_tot <- totale$decessi / totale$popolazione * 10000

png("GIULIA/risultati/scatter_mortalita_totale.png", width = 700, height = 500)
plot(totale$pil_pc_k, totale$tasso_tot, pch = 19, col = rgb(0, 0, 1, 0.5),
     xlab = "PIL pro capite (migliaia di euro)",
     ylab = "Decessi ogni 10.000 abitanti (12 cause)",
     main = "Mortalità totale e PIL pro capite")
abline(lm(tasso_tot ~ pil_pc_k, data = totale), col = "red", lwd = 2)
dev.off()
cor(totale$pil_pc_k, totale$tasso_tot)

#CONFRONTO TRA FASCE DI PIL 
# dividiamo le province in 3 gruppi di numerosità simile usando i terzili 
#poi confrontiamo la mortalità per tumori e per malattie circolatorie 
tagli <- quantile(province$pil_pc_k, probs = c(0, 1/3, 2/3, 1))
dati$fascia_pil <- cut(dati$pil_pc_k, breaks = tagli, include.lowest = TRUE,
                       labels = c("PIL basso", "PIL medio", "PIL alto"))

pdf("GIULIA/risultati/boxplot_fasciaPIL.pdf", width = 7, height = 5)
par(mfrow = c(1, 2))
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Infettive", ],
        col = c("tomato", "gold", "palegreen3"), main = "Malattie infettive",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Tumori", ],
        col = c("tomato", "gold", "palegreen3"), main = "Tumori",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Sangue/immunitario", ],
        col = c("tomato", "gold", "palegreen3"), main = "Malattie del sangue e del sistema immunitario",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Psichici", ],
        col = c("tomato", "gold", "palegreen3"), main = "Disturbi psichici",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Sistema nervoso", ],
        col = c("tomato", "gold", "palegreen3"), main = "Malattie del sistema nervoso",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Circolatorio", ],
        col = c("tomato", "gold", "palegreen3"), main = "Malattie circolatorie",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Respiratorio", ],
        col = c("tomato", "gold", "palegreen3"), main = "Malattie del sistema respiratorio",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Osteomuscolare", ],
        col = c("tomato", "gold", "palegreen3"), main = "Malattie del sistema osteomuscolare",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Perinatali", ],
        col = c("tomato", "gold", "palegreen3"), main = "Condizioni del periodo perinatale",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Malformazioni", ],
        col = c("tomato", "gold", "palegreen3"), main = "Malformazioni e anomalie cromosomiche",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Covid-19", ],
        col = c("tomato", "gold", "palegreen3"), main = "Covid-19",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
boxplot(tasso ~ fascia_pil, data = dati[dati$causa_breve == "Cause esterne", ],
        col = c("tomato", "gold", "palegreen3"), main = "Cause esterne",
        xlab = "", ylab = "Decessi ogni 10.000 abitanti")
dev.off()

print(round(tapply(dati$tasso, list(dati$causa_breve, dati$fascia_pil), mean), 2))

