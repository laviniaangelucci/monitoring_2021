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

### DAY 2

# per plottare immagine con colore selezionato dalla nostra legenda personale
plot(p224r63_2011, col=cl)

# per cambiare colori aggiungine altri al punto 22
cl <- colorRampPalette(c("black","blue","green","pink","yellow","violet")) (100)

### DAY 3

#carico pacchetto di dati raster in R
library(raster)

# seleziono la cartella nel mio sistema operativo da cui il software recupera i dati (cartella lab in :C)
setwd("C:/lab/")

# localizzo i dati
p224r63_2011 <- brick("p224r63_2011_masked.grd")

# utilizziamo la colorrapmpalette
cls <- colorRampPalette(c("red","pink","orange","purple")) (200)
plot(p224r63_2011, col=cls)

# interroghiamo l'immagine per vedere tutte le bande di Landsat che la compongono 
p224r63_2011

# B1 banda del blu 
# B2 banda del verde
# B3 banda del rosso
# B4 infrarosso vicino 
# B5 infrarosso medio
# B6 infrarosso termico 
# B7 infrarosso medio

# per eliminare la grafica precedente
dev.off()

# plot band 1 with a predefined colut ramp palette
plot(p224r63_2011$B1_sre, col=cls)

# funzione PAR per banda B1 e B2
par(mfrow=c(1,2))
plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)

par(mfrow=c(2,1))
plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)

# plot the first four bands of Landsat
par(mfrow=c(4,1))
plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)
plot(p224r63_2011$B3_sre)
plot(p224r63_2011$B4_sre)

# a quadrat of bands
par(mfrow=c(2,2))
plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)
plot(p224r63_2011$B3_sre)
plot(p224r63_2011$B4_sre)

# color ramp palette del blu per B1
par(mfrow=c(2,2))
clb <- colorRampPalette(c("dark blue","blue","light blue")) (100)
plot(p224r63_2011$B1_sre, col=clb)

# color ramp palette del blu per B2
clg <- colorRampPalette(c("dark green","green","light green")) (100)
plot(p224r63_2011$B2_sre, col=clg)

# color ramp palette del rosso per B3
clr <- colorRampPalette(c("dark red","red","pink")) (100)
plot(p224r63_2011$B3_sre, col=clr)
     
# color ramp palette dell'infrarosso per B4 
clnir <- colorRampPalette(c("red","orange","yellow")) (100)
plot(p224r63_2011$B4_sre, col=clnir)



