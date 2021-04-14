# R_code_copernicus.r
# visualizing Copernicus data

# library(raster)

# installiamo il nuovo pacchetto in R 
install.packages("ncdf4")

# Carico il nuovo pacchetto in R 
library(ncdf4)

# selezioniamo la cartella lab 
setwd("C:/lab/")

# carichiamo il file scaricato da Copernicus in R 
albedo <- raster("c_gls_SWI_202104111200_GLOBE_ASCAT_V3.1.1.nc")

# plottiamo attraverso ColorRampPalette
cl <- colorRampPalette(c('light blue','green','red','yellow'))(100)
plot(albedo, col=cl)

ricampionamento della variabile albedo attraverso un fattore "fact" per aggregare dati del file in uscita
# albedores <- aggregate(albedo, fact=100)

# plottiamo la nuova variabile albedores 
plot(albedores, col=cl)
