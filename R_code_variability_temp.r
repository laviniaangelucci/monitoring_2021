R_code_variability_temp.r
# settiamo la working directory
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
