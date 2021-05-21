# R_code_variability_temp.r

# settiamo la working directory e le library
library(raster)
library(RStoolbox)
# install.packages("RStoolbox")
setwd("C:/lab/")

# portiamo in R l'immagine "sentinel.png" attraverso la funzione brik
sent <- brick("sentinel.png")
 
# plottiamo l'immagine attraverso la funzione RGB
# NIR=1 RED=2 BLU=3 
#con r=1 b=2 g=3 e stretch="lin"
plotRGB(sent, stretch="lin")

# calcoliamo il layer NDVI (ovvero la differenza tra NIR infrared e red fratto la loro differenza)
# associamo le due bande NIR e RED alle due immagini 
nir <- sent$sentinel.1
red <- sent$sentinel.2

# costruiamo quindi la funzione NDVI e plottiamo l'immagine 
ndvi <- (nir-red) / (nir+red)
plot(ndvi)

# cambiamo la colorramppalette 
cl <- colorRampPalette(c('black','white','red','magenta','green'))(100)  
plot(ndvi,col=cl)

# calcoliamo la deviazione standard utilizzando la funzione "focal" e la funzione "window" (w=) 
ndvisd3 <- focal(ndvi, w=matrix(1/9, nrow=3, ncol=3), fun=sd)

# plottiamo l'immagine 
plot(ndvisd3)

# utilizziamo una "colorrampopalette" differente per apprezzare meglio la variazione della deviazione standard
clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) 
plot(ndvisd3, col=clsd)
 
 # facciamo la media di ndvi con 3x3 pixel utilizzando la funzione "focal"
ndvimean3 <- focal(ndvi, w=matrix(1/9, nrow=3, ncol=3), fun=mean)

# plottiamo l'immagine con la nostra colorampopalette
clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) # 
plot(ndvimean3, col=clsd)

# facciamo la media ndvi con 13x13 pixel con la funzione "focal" e plottiamo 
ndvimean13 <- focal(ndvi, w=matrix(1/269, nrow=13, ncol=13), fun=mean)
plot(ndvimean13, col=clsd)

# cambiamo la grandezza della "moving window" con deviazione standard 5x5 pixel e plottiamo
ndvimean5 <- focal(ndvi, w=matrix(1/25, nrow=5, ncol=5), fun=mean)
plot(ndvimean5, col=clsd)

# usiamo la funzione PCA per fare analisi multivariata 
sentpca <- rasterPCA(sent)

# plottiamo l'immagine 
plot(sentpca$map)

# facciamo un "summary" del modello per vedere la proporzione di variabilità spiegata da ogni singola componente
summary(sentpca$model) # la prima componente spiega il 6736804 % della variabilità totale

## DAY 2 

# settiamo la working directory e le library
library(raster)
library(RStoolbox)

# install.packages("RStoolbox")
setwd("C:/lab/")

# settiamo la library ggplot
library(ggplot2)

# libreria per plottare ggplot insieme 
library(gridExtra)

# installiamo la libreria per plottare colorato automaticamente 
install.packages("viridis")
library (viridis)

# portiamo in R l'immagine "sentinel.png" attraverso la funzione brik
sent <- brick("sentinel.png")

# usiamo la funzione PCA e plottiamo l'immagine per fare l'analisi multivariata
sentpca <- rasterPCA(sent)
plot(sentpca$map)

# facciamo un "summary" del modello per vedere la proporzione di variabilità spiegata da ogni singola componente
sentpca
summary(sentpca$model)

# utilizziamo la funzione focal 3x3 per visualizzare la deviazione standard utilizzando come "oggetto" PC1
pc1 <- sentpca$map$PC1
pc1sd3 <- focal (pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)

# utilizziamo la funzione focal 3x3 per visualizzare la deviazione standard utilizzando come "oggetto" PC1 
pc1sd5 <- focal(pc1, w=matrix(1/25, nrow=5, ncol=5), fun=sd)

# utiliziamo una color ramppalette e plottiamo l'immagine 
clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) 
plot(pc1sd3, col=clsd)

# prepariamo il codice salviamolo nel server e usiamo la funzione "source" per recuperare facilmente il file su R
# pc1sd5 <- focal(pc1, w=matrix(1/25, nrow=5, ncol=5), fun=sd)
# clsd <- colorRampPalette(c('blue','green','pink','magenta','orange','brown','red','yellow'))(100) # 
# plot(pc1sd5, col=clsd)
source("source_test_lezione.r")

# prepariamo un altro codice e portiamolo in r con la funzione "source"
source("source_ggplot.r")

# attraverso questa funzione individuiamo bene la discontinuità urbana, a livello geologico le diversità geomorfologiche e a livello ecologico indiviuiamo gli ecotoni (passaggi da un ambiente all'altro es bosco, prateria)
# aggiungiamo a ggplot() con + a una geometria (punti, linee, poligoni, pixel raster ecc..) in questo caso usiamo la funzione geom_raster con oggetto la mappa PCA
# definiamo le "estetiche" (aes) ossia il layer che vogliamo mappare attraverso la funzione "mapping"
# le aes avrà coordinate geografiche x y e riempimento: x= x y=y riempimento = layer)
ggplot() +
geom_raster(pc1sd3, mapping = aes(x = x, y = y, fill = layer))

# usiamo una delle legende di viridis per dichiarare una colorappalette senza citarla nel codice
scale_fill_viridis()
ggtitle("Standard deviation of PC1 by viridis colour scale")

# utiliziamo "magma" come colour scale attraverso la funzione "option = magma"
ggplot() +
geom_raster(pc1sd3, mapping = aes(x = x, y = y, fill = layer)) +
scale_fill_viridis(option = "magma") +
ggtitle("Standard deviation of PC1 by magma colour scale")

# inseriamo più grafici in una pagina attraverso funzione "grid.arrange" e la library "library(gridExtra)"
grid.arrange(p1, p2, p3, nrow = 1)

 




