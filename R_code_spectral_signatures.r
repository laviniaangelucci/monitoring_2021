# R_code_spectral_signatures.r

# settiamo la working directory
library(raster)
setwd("C:/lab/")
library(rgdal)
library(ggplot2)

# inseriamo in R un dato proveninte da cartella lab attraverso la funzione brick
defor2 <- brick("defor2.png")
# plottiamo l'immagine attraverso la funzione plot con bande RGB (red 1, green 2 , blue 3) con stretch lineare 
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")

# plottiamo con stretch hist 
plotRGB(defor2, r=1, g=2, b=3, stretch="hist")

# inseriamo in working directory la libreria rgdal
# utiliziamo la funzione click per riuscire a cliccare nella mappa e ricavare informazioni quali: ID, informazioni spaziali xy, numero della cella, info sulla mappa es. riflettanza
click(defor2, id=T, xy=T, cell=T, type="p", pch=16, cex=4, col="yellow")
# risultati: 
# x     y   cell defor2.1 defor2.2 defor2.3
# 1 292.5 220.5 184562      197      115      117
#    x     y   cell defor2.1 defor2.2 defor2.3
# 1 152.5 264.5 152874      181       14       21

# definiamo le bande del sistema dataset attraverso la creazione di colonne che equivalgono al valore di riflettanza

# usiamo la funzione dataframe per raggruppare le bande precedentemente inserite

# utilizziamo la libreria ggplot2 per fare un plot dell'immagine

# usiamo la funzione di ggplot2 e inseriamo le geometrie di interesse (es.linee) attraverso la funzione geom_line
# vegetazione: ggplot(spectrals, aes(x=band)) +
geom_line(aes(y=forest), color="green")

# acqua: ggplot(spectrals, aes(x=band)) +
 geom_line(aes(y=forest), color="green") +
 geom_line(aes(y=water), color="blue") 
 
 # utilizziamo l'immagine defert1
 defor1 <- brick("defor1.png") 
 plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
 
 # creiamo una mappa con click*guarda funzione click*
 click(defor1, id=T, xy=T, cell=T, type="p", pch=16, cex=4, col="yellow")
