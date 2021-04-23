# R_code_classification.r

# DAY 1 SOLAR ORBITER

# settiamo la working directory che stiamo utilizzando 
setwd("C:/lab/")
library(raster)

# usiamo la funzione brick per importare su R i dati del pacchetto raster
so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
so

# visualiziamo i tre livelli RGB dell'immagine attraverso la funzione PLOT con stretch lineare
plotRGB(so, 1, 2, 3, stretch="lin")

# installiamo RStoolbox 
install.packages("RStoolbox")
library(RStoolbox)

# "unsupervised classification" dell'immagine con n.3 classi
soc <- unsuperClass(so, nClasses=3)

# plottiamo l'immagine 
plot(soc$map)

# "unsupervised classification" dell'immagine con n.20 classi
soc20 <- unsuperClass(so, nClasses=20)
plot(soc20$map)

# scarichiamo immagine dal programma "SolarOrbiter"
https://www.esa.int/ESA_Multimedia/Missions/Solar_Orbiter/(result_type)/images

# portiamo l'immagine all'interno di R e plottiamo l'immagine con n.20 classi
sun <- brick("sun.png") 
sunc <- unsuperClass(sun, nClasses=20)
plot(sun.png$map)

# DAY 2 GRAND CANYON

# settiamo la working directory 
 setwd("C:/lab/")
library(raster)

# portiamo l'immagine del Grand Canyon all'interno di R attraverso la funzione "brick" 
gc <- brick("dolansprings_oli_2013088_canyon_lrg.jpg")

# plottiamo l'immagine attraverso funzione "RGB" con stretch lineare e stretch "hist" che presenta uno "slope" più "ripido" rispetto allo stretching lineare
plotRGB(gc, r=1, g=2, b=3, stretch="lin")
plotRGB(gc, r=1, g=2, b=3, stretch="hist")

# installo RStoolbox 
install.packages("RStoolbox")
library(RStoolbox)

# utiliziamo la funzione "unsuperClass" con n. 2 di classi per classificare l'immagine e plottiamo successivamente
gcc2 <- unsuperClass(gc, nClasses=2)
gcc2
plot(gcc2$map)

# utiliziamo la funzione di classificazione con n.4 di classi e plottiamo succesivamente 
gcc4 <- unsuperClass(gc, nClasses=4)
plot(gcc4$map)


