# R_code_land_cover.r
library(raster)
library(RStoolbox)
library(ggplot2)

# inseriamo la workingdirectory
setwd("C:/lab/")

# richiamiamo le librerie dati "raster" e "RStoolbox" per fare la classificazione 
library(raster)
library(RStoolbox)

# installiamo il pacchetto ggplot e richiamiamo un altra libreria "ggplot"
install.packages("ggplot2")
library(ggplot2)

# richiamiamo il dataset defort1 attraverso la funzione "brick"  
defor1 <- brick("defor1.png")

# plottiamo con la funzione RGB NIR 1 RED 2 GREEN 3
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")

# carichiamo il dataset defort2 attraverso la funzione "brick"
defor2 <- brick("defor2.png")

# plottiamo con la funzione RGB NIR 1 RED 2 GREEN 3 
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")

# utiliziamo la funzione ggplot per plottare con funzione RGB a partire da 3 "rasterlayer" con stretch lineare
ggRGB(defor2, r=1, g=2, b=3, stretch="lin")

# usiamo la funzione "par" per plottare defort1 e defort2 insieme attraverso il plot con funzione "plotRGB" 
par(mfrow=c(1,2))
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")

# installiamo "GreenExtra" che permette di utilizzare ggplot per dati raster
install.packages("gridExtra")
library(gridExtra)

# facciamo un multiframe per visualizzare defort1 e defort2 insieme attraverso la funzione "ggplot" e "grid.arrange" (mette insieme diverse sezioni all'interno del grafico in R)
p1 <- ggRGB(defor1, r=1, g=2, b=3, stretch="lin")
p2 <- ggRGB(defor2, r=1, g=2, b=3, stretch="lin")
grid.arrange(p1, p2, nrow=2)

