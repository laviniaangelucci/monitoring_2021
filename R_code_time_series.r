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

# DAY 2 

library(rasterVis)
library(raster)
setwd("C:/lab/greenland") 

# codice per importare immagini 
rlist <- list.files(pattern="lst")
rlist
import <- lapply(rlist,raster)
import
TGr <- stack(import)
TGr

# funzione level plot 
levelplot(TGr)

# plottiamo una singola immagine attraverso il pacchetto "rgdal"
install.packages("rgdal")
levelplot(TGr$lst_2000)

# usiamo la colorramppalette per riplottare l'immagine
cl <- colorRampPalette(c("blue","light blue","pink","red"))(100)
levelplot(TGr, col.regions=cl)

# utiliziamo la funzione names.attr e la funzione main 
levelplot(TGr,col.regions=cl, names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))
levelplot(TGr,col.regions=cl, main="LST variation in time",
          names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))

# utiliziamo i dati MELT ricavati attraverso il satellite a microwave
meltlist <- list.files(pattern="melt")
melt_import <- lapply(meltlist,raster)
melt <- stack(melt_import)
melt

# facciamo il levelplot del meltlist 
levelplot(melt)

# sottraiamo il primo dato al secondo associando un nome "melttime"
melt_amount <- melt$X2007annual_melt - melt$X1979annual_melt

# plottiamo attraverso colorramppalette 
clb <- colorRampPalette(c("blue","white","red"))(100)
plot(melt_amount, col=clb)

# facciamo un levelplot dell'argomento (melt_amount) 
levelplot(melt_amount, col.regions=clb)
 
