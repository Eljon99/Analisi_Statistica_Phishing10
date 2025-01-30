# Carica il dataset
phishing_data <- read.csv("DatasetStatistica - Phishing_URL_Dataset_10.csv")

# Calcolo delle frequenze per isHTTPS nei due casi di label
library(dplyr)
# Creazione dei barplot
library(ggplot2)

frequenze_phishing <- phishing_data %>%
  filter(label == 0) %>%
  group_by(isHTTPS) %>%
  summarise(frequenza_assoluta = n()) %>%
  mutate(frequenza_relativa = frequenza_assoluta / sum(frequenza_assoluta))

frequenze_legittimo <- phishing_data %>%
  filter(label == 1) %>%
  group_by(isHTTPS) %>%
  summarise(frequenza_assoluta = n()) %>%
  mutate(frequenza_relativa = frequenza_assoluta / sum(frequenza_assoluta))



# Barplot per i siti di phishing
plot_phishing <- ggplot(frequenze_phishing, aes(x = as.factor(isHTTPS), y = frequenza_relativa, fill = as.factor(isHTTPS))) +
  geom_bar(stat = "identity") +
  labs(title = "Distribuzione di isHTTPS nei siti di phishing", x = "isHTTPS", y = "Frequenza Relativa") +
  scale_fill_discrete(name = "isHTTPS") +
  theme_minimal()

# Barplot per i siti legittimi
plot_legittimo <- ggplot(frequenze_legittimo, aes(x = as.factor(isHTTPS), y = frequenza_relativa, fill = as.factor(isHTTPS))) +
  geom_bar(stat = "identity") +
  labs(title = "Distribuzione di isHTTPS nei siti legittimi", x = "isHTTPS", y = "Frequenza Relativa") +
  scale_fill_discrete(name = "isHTTPS") +
  theme_minimal()

# Mostra i grafici
print(plot_phishing)
print(plot_legittimo)

