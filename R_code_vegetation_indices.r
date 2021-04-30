# DAY 1 missing 

# DAY 2 

# carichiamo i pacchetti raster di nostro interesse
library(raster)
# for vegetation indices calculation
library(RStoolbox) 

# set della workingdirectory 
setwd("C:/lab/") 

# lavoriamo con immagini della foresta amazonica di NASA portiamo all'interno di R le immagini con funzione "brick"
defor1 <- brick("defor1.jpg")
defor2 <- brick("defor2.jpg")

# plottiamo le immagini con funzione "par" per visualizzarle insieme e RGB con stretch lineare
par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")

# b1 = NIR b2 = red b3 = green
 
 # usiamo la "different vegetation index" dell'immagine 1 div1 per ottenere un immagine e visualizzare differenza dell'infrarosso e il rosso 
 dvi1 <- defor1$defor1.1 - defor1$defor1.2

# plottiamo l'immagine dvi1 con la funzione "plot" 
plot(dvi1) 

#  cambiamo la colorramppalette 
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100)

# plottiamo l'immagine dvi1 con funzione "plot"
plot(dvi1, col=cl)
plot(dvi1, col=cl, main="DVI at time 1")

# usiamo la "different vegetation index" dell'immagine dvi2 e plottiamo
dvi2 <- defor2$defor2.1 - defor2$defor2.2
plot(dvi2, col=cl, main="DVI at time 2")

# analisi multitemporale usando la funzione "par" per visualizzare dvi1 e dvi2 insieme 
par(mfrow=c(2,1))
plot(dvi1, col=cl, main="DVI at time 1")
plot(dvi2, col=cl, main="DVI at time 2")

usiamo la funzione "div" per fare la sottrazione tra dvi1 e dvi2 ottentendo una differenza temporale tra le due mappe 
difdvi <- dvi1 - dvi2

# plottiamo con una nuova colorramppalette notiamo in rosso le zone più antropizzate e sottoposte a deforestazione nel tempo 
cld <- colorRampPalette(c('blue','white','red'))(100)
plot(difdvi, col=cld)

# con la funzione "Ndvi1" possiamo visualizzare immagini con funzione radiometrica diversa con range possibile da 1 a -1
# la funzione ndvi si calcola = (NIR - RED) / (NIR + RED)
ndvi1 <- (defor1$defor1.1 - defor1$defor1.2) / (defor1$defor1.1 + defor1$defor1.2)

# plottiamo con colorramppalette 
cl <- colorRampPalette(c('darkblue','yellow','red','black'))(100)
plot(ndvi1, col=cl)

# plottiamo l'immagine con la funzione "Ndvi2"
ndvi2 <- (defor2$defor2.1 - defor2$defor2.2) / (defor2$defor2.1 + defor2$defor2.2)
plot(ndvi2, col=cl)

# carichiamo il pacchetto RStoolbox
library(RStoolbox) 

# usiamo la funzione "spectral indices" con "vi1" e plottiamo con la colorramppalette "col"
vi <- spectralIndices(defor1, green = 3, red = 2, nir = 1)
plot(vi, col=cl)

# usciamo la funzioe "spectral indices" con "vi2" e plottiamo
vi2 <- spectralIndices(defor2, green = 3, red = 2, nir = 1)
plot(vi2, col=cl)

plottiamo con la funzione ndvi
difndvi <- ndvi1 - ndvi2
cld <- colorRampPalette(c('blue','white','red'))(100) 
plot(difndvi, col=cld)

