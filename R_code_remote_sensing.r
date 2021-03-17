# il mio primo codice per il telerilevamento 

# codice per installazione pacchetti aggiuntivi raster 
install.packages("raster")

# funzione library per richiamare il pacchetto raster 
library(raster)

#indicare la cartella da cui estrarre i dati 
setwd("C:/lab/")

# funzione brick per importare sul file 
p224r63_2011 <- brick("p224r63_2011_masked.grd")

# funzione per avere le info sul file 
p224r63_2011

# funzione plot immagini per visualizzare le varie bande 
plot(p224r63_2011)

# per sviluppare palette cromatiche sotto forma di vettore (C) e livelli (100)
cl <- colorRampPalette(c("black","grey","light grey")) (100)

# per plottare immagine con colore selezionato dalla nostra legenda personale
plot(p224r63_2011, col=cl)

# per cambiare colori aggiungine altri al punto 22
cl <- colorRampPalette(c("black","grey","light grey","green","pink","yellow","violet")) (100)
