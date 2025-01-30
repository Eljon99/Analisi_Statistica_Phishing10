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


# Funzione che calcola la FdDC basata su URLLength
f_FdDC <- function(dataset){
  # QUESTA FUNZIONE DOVREBBE ESSERE FATTA PER I VARI CASI, SIA PHISHING CHE NON E PER OGNI VARIABILE ??????
  #URLLength
  urls <- c(dataset$URLLength)
  freqRel <- table(urls)/length(urls)
  freqRel
  round(freqRel,3)
  
  m <- length(freqRel)
  classi_url <- c(14,24,28,34,50)
  intervalli <- cut(urls, breaks=classi_url, right=FALSE)
  freqRelClassi <- table(intervalli)/length(urls)
  Fcum <- cumsum(freqRelClassi)
  Fcum[4] <- Fcum[4] + freqRel[m]
  
  ascisse <- c(0,14,24,28,34,50)
  ordinate <- c(0,Fcum[1:4],1)
  
  plot(ascisse, ordinate, type="b", 
       main="Funzione di distribuzione continua",
       col="red", ylim=c(0,1), 
       xlab="URLLength", ylab="F(x)", axes=FALSE)
  
  axis(1, ascisse)  # Asse x
  axis(2, seq(0, 1, by=0.4))  
  box()
  
  
  #NoOfObfuscatedChar
  extref <- c(dataset$NoOfExternalRef)
  freqRel2 <- table(extref)/length(extref)
  freqRel2
  round(freqRel2,3)
  
  m2 <- length(freqRel2)
  classi_extref <- c(14,24,28,34,50)
  intervalli2 <- cut(extref, breaks=classi_extref, right=FALSE)
  freqRelClassi2 <- table(intervalli2)/length(extref)
  Fcum2 <- cumsum(freqRelClassi2)
  Fcum2[4] <- Fcum2[4] + freqRel2[m2]
  
  ascisse2 <- c(0,14,24,34,56,60)
  ordinate2 <- c(0,Fcum2[1:4],1)
  
  plot(ascisse2, ordinate2, type="b", 
       main="Funzione di distribuzione continua",
       col="red", ylim=c(0,1), 
       xlab="NoOfExternalRef", ylab="F(x)", axes=FALSE)
  
  axis(1, ascisse2)  # Asse x
  axis(2, seq(0, 1, by=0.4))  
  box()
}


#Funzione per i barplot di DPM
#Per VSL non ha senso farlo, i valori sono uguali per ogni fascia di età
f_barre_sovrapp_per_eta = function(country, data_15, dataComp, data64, tipo){
  anni=rep(c(1995:2018),each=3)
  classi=rep(c("15 -","15 + & 64 -","64+"),24)
  
  values=c(t(cbind(data_15[country,], dataComp[country,], data64[country,])))
  df=data.frame(anni, classi, values)
  ggplot(df, aes(fill=classi, y=values, x=anni)) + 
    geom_bar(position="dodge", stat="identity", width=0.8,
             alpha=0.7, colour="black") + 
    ggtitle(paste(tipo," per Classi di Età (1995 - 2018) - ",country, sep="")) + 
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
}


f_regressione_lineare = function(d1,d2,paese,offset){
  c_cor=cor(d1,d2)
  
  model <- lm(d2~d1)
  
  if(summary(model)$r.squared > offset && (c_cor<(-0.85) || c_cor>0.85)) {
    plot(d1,d2, col="red", main =paste("Scatterplot e Curva Stimata",paese,sep=" - "))
    abline(model, col="blue")
    stime <- fitted (model)
    segments (d1, stime, d1, d2, col="magenta")
    
    summary <- summary(model)
    # print(paese)
    # print(summary$r.squared)
    # print(c_cor)
    
    residui <- resid(model)
    plot (d2, residui, main = paste("Diagramma dei residui",paese,sep=" - "),
          xlab = "VSL" , ylab ="Residui " , pch =9 , col =" red " )
    abline ( h =0 , col =" blue " , lty =2)
    # print(residui)
    
    return(list(c_cor=c_cor, r_squared=summary$r.squared, summary=summary, resid = residui))
  } else {
    return(paese)
  }
}


best_model_function <- function(d1, d2, paese) {
  
  # Definizione modelli di regressione studiati
  models <- list(
    lin = lm(d2 ~ d1),
    quad = lm(d2 ~ d1 + I(d1^2)),
    exp = lm(d2 ~ I(exp(d1))),
    semilog = lm(I(log(d2)) ~ d1),
    log = lm(I(log(d2)) ~ I(log(d1)))
  )
  
  best_model <- NULL
  best_r_squared <- -Inf
  
  temp_r_squared <- numeric(length(models))
  
  c_cor <- cor(d1, d2)
  
  for (model_name in names(models)) {
    model <- models[[model_name]]
    
    r_squared <- summary(model)$r.squared
    
    temp_r_squared[which(names(models) == model_name)] <- r_squared
    
    if (r_squared > best_r_squared) {
      best_r_squared <- r_squared
      best_model <- model
      best_model_name <- model_name
    }
  }
  
  
  # Plot dei dati e della curva stimata
  plot(d1, d2, col="red", main=paste("Scatterplot e Curva Stimata -", paese, "(", best_model_name, ")", "-", r_squared, sep=" "))
  switch(
    best_model_name,
    "lin" = abline(best_model, col="blue"),
    "quad" = curve(best_model$coefficients[[1]] + best_model$coefficients[[2]] * x + best_model$coefficients[[3]] * x^2, add=TRUE, col="blue"),
    "exp" = curve(best_model$coefficients[[1]] * exp(best_model$coefficients[[2]] * x), add=TRUE, col="blue"),
    "semilog" = curve(exp(best_model$coefficients[[1]] + best_model$coefficients[[2]] * x), add=TRUE, col="blue"),
    "log" = curve(exp(best_model$coefficients[[1]] + best_model$coefficients[[2]] * log(x)), add=TRUE, col="blue")
  )
  
  switch(
    best_model_name,
    "lin" = segments(d1, fitted(best_model), d1, d2, col="magenta"),
    "quad" = segments(d1, best_model$coefficients[[1]] + best_model$coefficients[[2]] * d1 + best_model$coefficients[[3]] * (d1)^2, d1, d2, col="magenta"),
    "exp" = segments(d1, best_model$coefficients[[1]] + best_model$coefficients[[2]] * exp (d1) , d1, d2, col="magenta"),
    "semilog" = segments(d1, exp(best_model$coefficients[[1]] + best_model$coefficients[[2]] * d1), d1, d2, col="magenta"),
    "log" = stime <- segments(d1, best_model$coefficients[[1]]*((d1)^ best_model$coefficients[[2]] ), d1, d2, col="magenta")
  )
  
  # Plot dei residui
  residui <- resid(best_model)
  plot(d2, residui, main=paste("Diagramma dei residui -", paese), xlab="Valori previsti", ylab="Residui", pch=19, col="red")
  abline(h=0, col="blue", lty=2)
  
  # Restituzione dei risultati
  return(list(
    model = best_model_name,
    r_squared = best_r_squared,
    # summary = summary(best_model),
    resid = residui,
    summary = data.frame(best_model=best_model_name, c_cor=c_cor, lin = temp_r_squared[1], quad = temp_r_squared[2], exp = temp_r_squared[3], semilog = temp_r_squared[4], log = temp_r_squared[5])
  ))
}


# Funzione per clustering gerarchico
hierarchClustering <- function(hls, n_clust, data, metodo, trHI, variabile) {
  taglio_hls <- cutree(hls, k = n_clust, h = NULL)
  
  table_hls <- table(taglio_hls)
  taglio_list_hls <- list(taglio_hls)
  
  # Calcolo misure di non omogeneità statistiche
  agvar_hls <- aggregate(data, taglio_list_hls, var)[, -1]
  
  trH_values <- numeric(n_clust)
  for(i in 1:n_clust) {
    trH_values[i] <- (table_hls[[i]] - 1) * sum(agvar_hls[i, ])
    if(is.na(trH_values[i])) trH_values[i] <- 0
  }
  
  trH_within <- sum(trH_values)
  trH_between <- trHI - trH_within
  
  
  plot(hls, hang=-1, xlab=paste(variabile,"- Metodo gerarchico agglomerativo"), sub=metodo)
  axis(side=4, at=round(c(0, hls$height), 2))
  
  rect.hclust(hls, k=n_clust, border=rainbow(n_clust))
  
  return(list(trH_within = trH_within, trH_between = trH_between))
}


# Funzione per clustering NON gerarchico
kMeansClustering <- function(data, n_clust, n_start = 5, iter_max, variabile) {
  km <- kmeans(data, centers = n_clust, nstart = n_start, iter.max = iter_max)
  
  # Calcola misure di non omogeneità
  trH_within <- sum(km$withinss)
  trH_between <- km$betweenss
  
  plot(data, col = km$cluster, xlab = variabile, ylab = variabile)
  points(km$centers, col = 1:n_clust, pch = 8, cex = 2)
  
  return(list(trH_within = trH_within, trH_between = trH_between, clusters = km$cluster))
}