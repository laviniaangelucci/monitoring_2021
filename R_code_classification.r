# R_code_classification.r

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
