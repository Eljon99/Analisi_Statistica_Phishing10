library(data.table)
library(ggplot2)
library(cluster)
library(factoextra)

# Carica il file CSV
dati <- fread("Phishing_URL_Dataset_10.csv")

# Normalizza la colonna NoOfExternalRef
data_cluster <- scale(as.matrix(dati[, .(NoOfExternalRef)]))

# Funzione per ottimizzare K-Means (minimizza within e massimizza between)
kMeansOptimized <- function(data, n_clust, n_start = 25, iter_max = 500) {
  best_km <- NULL
  best_ratio <- -Inf  

  for (i in 1:10) {  
    km <- kmeans(data, centers = n_clust, nstart = n_start, iter.max = iter_max)
    ratio <- km$betweenss / sum(km$withinss)  
    
    if (ratio > best_ratio) {  
      best_km <- km
      best_ratio <- ratio
    }
  }
  return(best_km)
}

# Testiamo diversi valori di k (da 2 a 10)
set.seed(123)
k_values <- 2:10
k_models <- lapply(k_values, function(k) kMeansOptimized(data_cluster, k))

# Calcoliamo WCSS, BCSS, Silhouette e CH index per ogni k
wcss_values <- sapply(k_models, function(km) sum(km$withinss))
bcss_values <- sapply(k_models, function(km) km$betweenss)
silhouette_scores <- sapply(k_models, function(km) {
  silhouette_res <- silhouette(km$cluster, dist(data_cluster))
  mean(silhouette_res[, 3])  
})
ch_index_values <- sapply(seq_along(k_models), function(i) {
  k <- k_values[i]
  (bcss_values[i] / (k - 1)) / (wcss_values[i] / (nrow(dati) - k))
})

# Creazione DataFrame con tutti i risultati
df_results <- data.frame(
  K = k_values,
  WCSS = wcss_values,
  BCSS = bcss_values,
  Silhouette = silhouette_scores,
  Calinski_Harabasz = ch_index_values
)

# Stampiamo i risultati in forma tabellare
print(df_results)

# Grafico Elbow Method
elbow_plot <- ggplot(df_results, aes(x = K, y = WCSS)) +
  geom_point(color = "red", size = 3) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Elbow Method per NoOfExternalRef (con kmeans)",
       x = "Numero di Cluster",
       y = "Within-cluster Sum of Squares (WCSS)") +
  theme_minimal()

# Grafico Silhouette Method
silhouette_plot <- ggplot(df_results, aes(x = K, y = Silhouette)) +
  geom_point(color = "red", size = 3) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Silhouette Method per NoOfExternalRef (con kmeans)",
       x = "Numero di Cluster",
       y = "Coefficiente di Silhouette") +
  theme_minimal()

# Salviamo i grafici
ggsave(filename = "Elbow_Method_NoOfExternalRef.png", plot = elbow_plot, width = 6, height = 5, dpi = 300, bg = "white")
ggsave(filename = "Silhouette_Method_NoOfExternalRef.png", plot = silhouette_plot, width = 6, height = 5, dpi = 300, bg = "white")

# Stampiamo i grafici
print(elbow_plot)
print(silhouette_plot)

cat("Grafici e metriche salvati correttamente!\n")

# Analisi per K = 3
best_k3 <- k_models[[which(k_values == 3)]]
dati[, Cluster := as.factor(best_k3$cluster)]  # Assegniamo i cluster

# Estrai le righe dei cluster 1 e 3
cluster_1_data <- dati[Cluster == 1]
cluster_3_data <- dati[Cluster == 3]

# Visualizza le etichette dei valori nei cluster 1 e 3
cat("Etichette nel Cluster 1:\n")
print(cluster_1_data[, .(URLLength, NoOfExternalRef, label)])

cat("\nEtichette nel Cluster 3:\n")
print(cluster_3_data[, .(URLLength, NoOfExternalRef, label)])

# Contiamo la distribuzione della variabile label nei cluster
label_distribution <- dati[, .N, by = .(Cluster, label)]

# Grafico a barre della distribuzione della variabile label nei cluster
bar_plot <- ggplot(label_distribution, aes(x = Cluster, y = N, fill = as.factor(label))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = N), vjust = -0.5, size = 5) +  # Etichette con i numeri
  scale_fill_manual(values = c("red", "plum"), labels = c("Phishing (0)", "Non-Phishing (1)")) +
  labs(title = "Numero di label nei cluster di NoOfExternalRef (K = 3)",
       x = "Cluster",
       y = "Numero di istanze",
       fill = "Label") +
  theme_minimal()

# Salviamo il grafico
ggsave(filename = "Label_Distribution_NoOfExternalRef_K3.png", plot = bar_plot, width = 6, height = 5, dpi = 300, bg = "white")

# Stampiamo il grafico
print(bar_plot)

cat("Grafico della distribuzione della label salvato correttamente!\n")

# Scatter plot di NoOfExternalRef rispetto a se stesso con i centroidi
scatter_plot <- ggplot(dati, aes(x = NoOfExternalRef, y = NoOfExternalRef, color = Cluster)) +
  geom_point(alpha = 0.6) +
  # Centroidi separati e riportati alla scala originale
  geom_point(data = as.data.table(best_k3$centers), 
             aes(x = NoOfExternalRef * attr(data_cluster, "scaled:scale")[1] + attr(data_cluster, "scaled:center")[1], 
                 y = NoOfExternalRef * attr(data_cluster, "scaled:scale")[1] + attr(data_cluster, "scaled:center")[1]), 
             color = "black", shape = 8, size = 4) +  # Centroidi come stelline
  labs(title = "Clusterizzazione di NoOfExternalRef per K = 3",
       x = "NoOfExternalRef",
       y = "NoOfExternalRef",
       color = "Cluster") +
  theme_minimal()

# Salviamo il grafico
ggsave(filename = "Scatter_NoOfExternalRef_K3.png", plot = scatter_plot, width = 6, height = 5, dpi = 300, bg = "white")

# Stampiamo il grafico
print(scatter_plot)

cat("Grafico di NoOfExternalRef rispetto a se stesso con centroidi separati e riportati alla scala originale salvato correttamente!\n")
