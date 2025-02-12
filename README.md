# Analisi_Statistica_Phishing10

## Overview
Questo progetto è stato sviluppato per l'esame di Statistica ed Analisi dei Dati (UNISA). L'obiettivo principale di questo studio è esplorare il ruolo di specifiche caratteristiche degli URL nell'identificazione di siti di phishing rispetto a quelli legittimi. Attraverso l'applicazione di metodologie analitiche, verrà valutato in che misura tali caratteristiche influenzano la probabilità che un sito appartenga a una delle due categorie. Questo approccio permetterà di comprendere meglio i fattori chiave che contribuiscono alla rilevazione automatizzata delle minacce online, fornendo basi empiriche per lo sviluppo di strumenti di sicurezza più efficaci.

## Autori
<a href="https://github.com/gCascone01/Analisi_Statistica_DPDFP/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=gCascone01/Analisi_Statistica_DPDFP" />
</a>

* [Eljon Hida](https://github.com/Eljon99)
* [Francesco Ferrara](https://github.com/Rokuoganz)

## Obiettivi
- Determinare se la lunghezza degli URL impatta sulla distinzione di un sito di phishing da un legittimo.
- Determinare se il numero di riferimenti esterni impatta sulla distinzione di un sito di phishing da un legittimo.
- Comparare il dataset reale con uno sintetico generato da ChatGPT 4o e discutere delle differenze.

## Dataset
Il dataset usato in questo studio cataloga vari URL in: URL di phishing e URL legittimi. Questa catalogazione è fatta attraverso la variabile label che, quando vale 0 indica un sito di phishing e 1 nel caso di sito legittimo. Il dataset presenta altre variabili che potrebbero permettere un'identificazione prematura di questi URL come la lunghezza degli stessi, il numero di riferimenti esterni/interni, il numero di caratteri speciali ecc.

## Strumenti e Tecnologie
- R: per l'analisi statistica dei dati.
- RStudio: IDE.
- R Markdown: per la presentazione dei risultati.

## Installazione e Uso
1. Installare R e RStudio.
2. Clonare la repository GitHub.
3. Aprire il progetto in RStudio.
4. Esegui il codice presente nel file `docs.Rmd` per generare l'analisi e visualizzare i risultati.

## Struttura del Progetto
- `docs.Rmd`: Documento R Markdown con l'analisi completa.
- `functions.R`: Funzioni R utilizzate per l'analisi dei dati.
- `Datasets/`: Directory contenente il dataset reale e quello sintetico.
