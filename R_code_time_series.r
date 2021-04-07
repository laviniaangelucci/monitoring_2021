# time series analysis 
# greenland increase of temperature 
# data and code from Emanuela Cosma 

install.packages("raster")

library(raster)

setwd("C:/lab/greenland")

# we are using the RASTER function 

lst_2000 <- raster("lst_2000.tif")
plot(lst_2000)

lst_2005 <- raster("lst_2005.tif")
plot(lst_2005)

lst_2010 <- raster("lst_2010.tif")
plot(lst_2010)

lst_2015 <- raster("lst_2015.tif")
plot(lst_2015)

# funzione PAR per plottare matrice di immagini 
par(mfrow=c(2,2))
lst_2000 <- raster("lst_2000.tif")
plot(lst_2000)
lst_2005 <- raster("lst_2005.tif")
plot(lst_2005)
lst_2010 <- raster("lst_2010.tif")
plot(lst_2010)
lst_2015 <- raster("lst_2015.tif")
plot(lst_2015)

# using LIST and PATTERN functions per fare una lista di files raster 
rlist <- list.files(pattern="lst")

# LAPPLY function using list e plottiamo l'immagine
import <- lapply(rlist,raster)

TGr <- stack(import)
plot(TGr)
