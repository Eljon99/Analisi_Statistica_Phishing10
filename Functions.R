#Funzione che calcola il CV
cv <- function (data){
  sd(data)/abs(mean(data))
}


# Funzione che calcola la moda
f_moda = function(x) {
  # Calcola le frequenze dei valori unici
  freq = table(x)
  
  # Trova il valore con frequenza massima
  moda = as.numeric(names(freq[freq == max(freq)]))
  
  return(moda)
}


# Funzione per il calcolo delle frequenze
f_calcola_frequenze <- function(intervalli) {
  
  # Calcola le frequenze assolute
  freq_assoluta <- table(intervalli)
  

  freq_relativa <- prop.table(freq_assoluta)
  

  freq_cumulata <- cumsum(freq_relativa)
  
  # Crea un dataframe per una visualizzazione ordinata
  df_frequenze <- data.frame(
    Intervalli = names(freq_assoluta),
    Frequenza_Assoluta = as.vector(freq_assoluta),
    Frequenza_Relativa = round(as.vector(freq_relativa), 4),
    Frequenza_Cumulata = round(as.vector(freq_cumulata), 4)
  )
  

  print(df_frequenze)

}


# Funzione che crea 2 grafici (URLLength e NoOfObfuscatedChar) con i boxplot di 5 anni consecutivi
f_quantili = function (data, name){ # name = titolo del grafico, data = dataset inviato alla funzione
    # Creazione del boxplot
    boxplot(data, 
            main=paste(name), 
            names=c(""),
            col=c("green"),
            outline=FALSE)
}


f_calcola_quartili = function(data, feature) {
  # data: dataset contenente la variabile di interesse
  # feature: nome della colonna da analizzare
  
  valori = data[[feature]]  # Estrai la variabile numerica
  quartili = quantile(valori, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)  # Calcola i quartili
  
  # Definizione dei range basati sui quartili
  range_labels = c("Q1", "Q2", "Q3", "Q4")
  data$quartile_group = cut(valori, breaks = quartili, include.lowest = TRUE, labels = range_labels)
  
  # Creazione della tabella con rbind()
  summary_table = rbind(
    c("Intervallo", "Minimo", "Massimo", "Numero Elementi"),
    c(range_labels[1], 
      min(valori[data$quartile_group == "Q1"], na.rm = TRUE), 
      max(valori[data$quartile_group == "Q1"], na.rm = TRUE), 
      sum(data$quartile_group == "Q1", na.rm = TRUE)),
    c(range_labels[2], 
      min(valori[data$quartile_group == "Q2"], na.rm = TRUE), 
      max(valori[data$quartile_group == "Q2"], na.rm = TRUE), 
      sum(data$quartile_group == "Q2", na.rm = TRUE)),
    c(range_labels[3], 
      min(valori[data$quartile_group == "Q3"], na.rm = TRUE), 
      max(valori[data$quartile_group == "Q3"], na.rm = TRUE), 
      sum(data$quartile_group == "Q3", na.rm = TRUE)),
    c(range_labels[4], 
      min(valori[data$quartile_group == "Q4"], na.rm = TRUE), 
      max(valori[data$quartile_group == "Q4"], na.rm = TRUE), 
      sum(data$quartile_group == "Q4", na.rm = TRUE))
  )
  
  # Stampa della tabella in formato leggibile
  apply(summary_table, 1, function(row) cat(paste(row, collapse = " | "), "\n"))
}





# Funzione che calcola la FdDC
f_FdDC <- function(dataset){
  
  #URLLength
  urls <- c(dataset$URLLength)
  freqRel <- table(urls)/length(urls)
  freqRel
  round(freqRel,3)
  
  m <- length(freqRel)
  classi_url <- quantile(urls, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  intervalli <- cut(urls, breaks=classi_url, include.lowest = TRUE, right=FALSE)
  freqRelClassi <- table(intervalli)/length(urls)
  Fcum <- cumsum(freqRelClassi)
  Fcum[4] <- Fcum[4] + freqRel[m]
  freqAss <- table(intervalli)
  
  f_calcola_frequenze(intervalli)
  
  ascisse <- c(0,14,24,28,34,50)
  ordinate <- c(0,Fcum[1:4],1)
  
  plot(ascisse, ordinate, type="b", 
       main="Funzione di distribuzione continua",
       col="red", ylim=c(0,1), 
       xlab="URLLength", ylab="F(x)", axes=FALSE)
  
  axis(1, ascisse)  # Asse x
  axis(2, seq(0, 1, by=0.4))  
  box()
  
  
  #NoOfExternalRef
  extref <- c(dataset$NoOfExternalRef)
  freqRel2 <- table(extref)/length(extref)
  freqRel2
  round(freqRel2,3)
  
  m2 <- length(freqRel2)
  classi_extref <- quantile(extref, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  intervalli2 <- cut(extref, breaks=classi_extref, right=FALSE)
  freqRelClassi2 <- table(intervalli2)/length(extref)
  Fcum2 <- cumsum(freqRelClassi2)
  Fcum2[4] <- Fcum2[4] + freqRel2[m2]
  freqAss2 <- table(intervalli2)
  
  f_calcola_frequenze(intervalli2)
  
  
  ascisse2 <- c(0,14,24,34,56,60)
  ordinate2 <- c(0,Fcum2[1:4],1)
  
  plot(ascisse2, ordinate2, type="b", 
       main="Funzione di distribuzione continua",
       col="red", ylim=c(0,1), 
       xlab="NoOfExternalRef", ylab="F(x)", axes=FALSE)
  
  axis(1, ascisse2)  # Asse x
  axis(2, seq(0, 1, by=0.4))  
  box()
  
  # Stampa i valori della FdDC per URLLength
  print(data.frame(URLLength = ascisse, FdDC = ordinate))
  
  # Stampa i valori della FdDC per NoOfExternalRef
  print(data.frame(NoOfExternalRef = ascisse2, FdDC = ordinate2))
  
}



# Funzione per clustering NON gerarchico
f_kMeansClustering <- function(variabile, n_clust, n_start=25, iter_max=500) {

  best_km <- NULL
  best_ratio <- -Inf  
  
  for (i in 1:10) {  
    km <- kmeans(variabile, centers = n_clust, nstart = n_start, iter.max = iter_max)
    ratio <- km$betweenss / sum(km$withinss)  
    
    if (ratio > best_ratio) {  
      best_km <- km
      best_ratio <- ratio
    }
  }
  return(best_km)
  
}

# Funzione per eseguire il clustering K-Means su una variabile specifica e stampare i plot
f_plotsClustering <- function(data, var_name, k_range = 2:10, best_k = "auto") {
  
  # Normalizza la variabile scelta
  data_cluster <- scale(as.matrix(data[[var_name]]))
  
  # Esegui K-Means per diversi valori di k con ottimizzazione
  set.seed(123)
  k_models <- lapply(k_range, function(k) f_kMeansClustering(data_cluster, k))
  
  # Calcola le metriche per ogni k
  wcss_values <- sapply(k_models, function(km) sum(km$withinss))
  bcss_values <- sapply(k_models, function(km) km$betweenss)
  silhouette_scores <- sapply(k_models, function(km) {
    silhouette_res <- silhouette(km$cluster, dist(data_cluster))
    mean(silhouette_res[, 3])  
  })
  ch_index_values <- sapply(seq_along(k_models), function(i) {
    k <- k_range[i]
    (bcss_values[i] / (k - 1)) / (wcss_values[i] / (nrow(data) - k))
  })
  
  # Creazione DataFrame con i risultati
  df_results <- data.frame(
    K = k_range,
    WCSS = wcss_values,
    BCSS = bcss_values,
    Silhouette = silhouette_scores,
    Calinski_Harabasz = ch_index_values
  )
  
  print(df_results)
  
  # Se best_k = "auto", scegli il miglior k in base al rapporto BCSS/WCSS
  if (best_k == "auto") {
    best_k <- k_range[which.max(bcss_values / wcss_values)]
  }
  
  # Grafico Elbow Method
  elbow_plot <- ggplot(df_results, aes(x = K, y = WCSS)) +
    geom_point(color = "red", size = 3) +
    geom_line(color = "blue", size = 1) +
    labs(title = paste("Elbow Method per", var_name, "(con kmeans)"),
         x = "Numero di Cluster",
         y = "Within-cluster Sum of Squares (WCSS)") +
    theme_minimal()
  
  print(elbow_plot)
  
  # Grafico Silhouette Method
  silhouette_plot <- ggplot(df_results, aes(x = K, y = Silhouette)) +
    geom_point(color = "red", size = 3) +
    geom_line(color = "blue", size = 1) +
    labs(title = paste("Silhouette Method per", var_name, "(con kmeans)"),
         x = "Numero di Cluster",
         y = "Coefficiente di Silhouette") +
    theme_minimal()
  
  print(silhouette_plot)
  
  # Analisi per best_k
  best_km <- k_models[[which(k_range == best_k)]]
  
  if ("data.table" %in% class(data)) {
    data[, Cluster := as.factor(best_km$cluster)]
  } else {
    data$Cluster <- as.factor(best_km$cluster)
  }
  
  # Distribuzione della variabile label nei cluster
  if ("data.table" %in% class(data)) {
    label_distribution <- data[, .N, by = .(Cluster, label)]
  } else {
    label_distribution <- as.data.frame(table(data$Cluster, data$label))
    colnames(label_distribution) <- c("Cluster", "label", "N")
  }
  
  bar_plot <- ggplot(label_distribution, aes(x = Cluster, y = N, fill = as.factor(label))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = N), vjust = -0.5, size = 5) +
    scale_fill_manual(values = c("red", "plum"), labels = c("Phishing (0)", "Non-Phishing (1)")) +
    labs(title = paste("Numero di label nei cluster di", var_name, "(K =", best_k, ")"),
         x = "Cluster",
         y = "Numero di istanze",
         fill = "Label") +
    theme_minimal()
  
  print(bar_plot)
  
  
  # Creazione del dataframe dei centroidi e trasformazione alla scala originale
  centroids <- data.frame(
    Cluster = factor(1:nrow(best_km$centers)), 
    x = best_km$centers[, 1] * attr(data_cluster, "scaled:scale")[1] + attr(data_cluster, "scaled:center")[1]
  )
  

  scatter_plot <- ggplot(data, aes_string(x = var_name, y = var_name, color = "Cluster")) +
    geom_point(alpha = 0.6) +
    # Centroidi riportati alla scala originale
    geom_point(data = centroids, 
               aes(x = x, y = x), 
               color = "black", shape = 8, size = 4) +  # Centroidi come stelline
    labs(title = paste("Clusterizzazione di", var_name, "per K =", best_k),
         x = var_name,
         y = var_name,
         color = "Cluster") +
    theme_minimal()
  
  print(scatter_plot)
  
}
