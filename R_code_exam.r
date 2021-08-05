# R_code_exam_LaviniaAngelucci


#PAYPAL
#paolo.patrizio7@gmail.com

https://coastobs.eu/news/32/covid19-reveals-human-impact-on-environment-example-of-venice-water-transparency-seen-by-copernicus


INVESTIGHIAMO UN OGGETTO PRIMA DI INIZIARE UN PROGETTO 
# PROVA NUMERO 1 --------------------------------------------------------------------
#settiamo la working directory
#library(raster)
#setwd("C:/pt/")

# usiamo la funzione brick per importare l'immagine 043 laguna di venezia in pt su :C
#laguna_13a_02g <- brick("043.jpg")

# names      : X043.1, X043.2, X043.3  queste sono le bande 
# crs        : NA NON è GEOREFERENZIATA!!!!!

#plottiamo l'immagine
#plot(laguna_13a_02g)
# si evidenzia una differenza a livello dell'acqua, potrebbe essere torbidità? lo andiamo a scoprire dopo 
# può essere che la torbidità rifletta di più in una di quelle tre bande x043.1 x043.2 x043.3

# conviene utilizzare la funzione RGB in quanto partiamo da una immagine "reale" 
# nota che in questo caso la banda near infrared non è possibile inserirla (perchè banda 4)
#plotRGB(laguna_13a_02g, r=3, g=2, b=1)
#plotrgb
#plotRGB(laguna_13a_02g)
#----------------------------------------------------------------------------------------
#installa pacchetto
install.packages("viridis")
library (viridis)
library(raster)
setwd("C:/pt/")
library(rgdal)
library(gridExtra)
library(RStoolbox)
library(ggplot2)

# CARICO PACCHETTI IMMAGINI 
fg2a_feb20<- brick("fig2a_wgs84.tif")
fg2a_marz11<- brick("fig2b_wgs84.tif")
fg2a_marz19<- brick("fig2c_wgs84.tif")

readOGR("files lavinia angelucci.shp")
lagoon_shp <- readOGR ("files lavinia angelucci.shp")


# plotto le immagini con RGB 
plotRGB(fg2a_feb20, r=3, g=2, b=1, stretch="hist")
plotRGB(fg2a_marz11, r=3, g=2, b=1, stretch="hist")
plotRGB(fg2a_marz19, r=3, g=2, b=1, stretch="hist")

# plotto con ggRGB
ggRGB(fg2a_feb20,1,2,3, stretch="hist")
ggRGB(fg2a_marz11,1,2,3, stretch="hist")
ggRGB(fg2a_marz19,1,3,2, stretch="hist")
p1 <- ggRGB(fg2a_feb20,1,2,3, stretch="hist")
p2 <-  ggRGB(fg2a_marz11,1,2,3, stretch="hist")
p3 <-  ggRGB(fg2a_marz19,1,2,3, stretch="hist")

#faccio grid.arrange
grid.arrange(p1, p2, p3, nrow=1)

# rinomino le immagini croppate
p1_cropped <- ggRGB(crop(fg2a_feb20,lagoon_shp),1,2,3, stretch="hist")
p2_cropped <- ggRGB(crop(fg2a_marz11,lagoon_shp),1,2,3, stretch="hist")
p3_cropped <- ggRGB(crop(fg2a_marz19,lagoon_shp),1,2,3, stretch="hist")

# plotto le immagini croppate con grid.arrange
grid.arrange(p1_cropped, p2_cropped, p3_cropped, nrow=3)



#PCA, analisi multivariata
#you take a dataset with many variables, and you simplify that dataset by turning your original variables into a smaller number of "Principal Components".
set.seed(25)
feb20_pca <- rasterPCA(fg2a_feb20)
marz11_pca <- rasterPCA(fg2a_marz11)
marz19_pca <- rasterPCA(fg2a_marz19)

#par(mfrow=c(1,3))
#plot(feb20_pca$map)
#plot(marz11_pca$map)
#plot(marz19_pca$map)


ggRGB(feb20_pca$map,1,2,3, stretch="lin", q=0)
ggRGB(marz19_pca$map,1,2,3, stretch="lin", q=0)
ggRGB(marz11_pca$map,1,2,3, stretch="lin", q=0)


x <- list(1:3)


plot20 <- lapply(1:3, function[x] ggRGB(feb20_pca$map,1,2,3, stretch="lin", q=0), x, geom_raster = TRUE)
plot11 <- lapply(1:3, function(x) ggRGB(marz11_pca$map,1,2,3, stretch="lin", q=0), x, geom_raster = TRUE)
plot19 <- lapply(1:3, function(x) ggRGB(marz19_pca$map,1,2,3, stretch="lin", q=0), x, geom_raster = TRUE)

 grid.arrange(plot20[[1]],plot20[[2]], plot20[[3]], ncol=2)
 grid.arrange(plot11[[1]],plot11[[2]], plot11[[3]], ncol=2)
 grid.arrange(plot19[[1]],plot19[[2]], plot19[[3]], ncol=2)
 
 #grid.arrange delle 3 immagini 
 grid.arrange(plot20[[1]],plot11[[1]], plot19[[1]], ncol=2)
 
# crop delle immagini PCA
crop(feb20_pca$map,lagoon_shp)
crop(marz11_pca$map,lagoon_shp)
crop(marz11_pca$map,lagoon_shp)

feb20crop <- crop(feb20_pca$map,lagoon_shp)
marz11crop <- crop(marz11_pca$map,lagoon_shp)
marz19crop <- crop(marz19_pca$map,lagoon_shp)

feb20crop_pca <- rasterPCA(feb20crop)
marz11crop_pca <- rasterPCA(marz11crop)
marz19crop_pca <- rasterPCA(marz19crop)
par(mfrow=c(1,3))
plot(feb20crop_pca$map)
plot(marz11crop_pca$map)
plot(marz19crop_pca$map)






# MOVING WINDOW

feb20_pc1 <- feb20_pca$map$PC1
marz11_pc1 <- marz11_pca$map$PC1
marz19_pc1 <- marz19_pca$map$PC1
par(mfrow=c(1,3))
plot(feb20_pc1)
plot(marz11_pc1)
plot(marz19_pc1)

feb20_mw <- focal (feb20_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
marz11_mw <- focal (marz11_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
marz19_mw <- focal (marz19_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
clsd <- colorRampPalette(c('blue','green','pink','red','orange','yellow'))(100)
par(mfrow=c(1,3))
plot(feb20_mw, col=clsd)
plot(marz11_mw, col=clsd)
plot(marz19_mw, col=clsd)

feb20mw_cropped <- crop(feb20_mw,lagoon_shp)
marz11mw_cropped <- crop(marz11_mw,lagoon_shp)
marz19mw_cropped <- crop(marz19_mw,lagoon_shp)

par(mfrow=c(1,3))
plot(feb20mw_cropped, col=clsd)
plot(marz11mw_cropped, col=clsd)
plot(marz19mw_cropped, col=clsd)



#SUMMARY DELLE COMPONENTI NON CROPPED
summary(feb20_pca$model) 
#                            Comp.1      Comp.2      Comp.3
#Standard deviation     112.4212153 32.41154082 7.839134229
#Proportion of Variance   0.9191331  0.07639786 0.004469079
#Cumulative Proportion    0.9191331  0.99553092 1.000000000

summary(marz19_pca$model)
#                           Comp.1      Comp.2      Comp.3
#Standard deviation     69.6829683 21.23681719 7.128451679
#Proportion of Variance  0.9063343  0.08418098 0.009484742
#Cumulative Proportion   0.9063343  0.99051526 1.000000000

summary(marz11_pca$model)
#                           Comp.1      Comp.2      Comp.3
#Standard deviation     90.6106865 22.61181338 5.727743638
#Proportion of Variance  0.9378482  0.05840426 0.003747493
#Cumulative Proportion   0.9378482  0.99625251 1.000000000




# SUMMARY DELLE COMPONENTI CROPPATO --> VALORI SOLO DI ACQUA
#summary(feb20crop_pca$model)
#                            Comp.1      Comp.2      Comp.3
#Standard deviation     104.6909160 27.94909281 7.469126723
#Proportion of Variance   0.9290556  0.06621542 0.004728935
#Cumulative Proportion    0.9290556  0.99527106 1.000000000

# MARZO 19


#summary(marz19crop_pca$model)
#                           Comp.1      Comp.2      Comp.3
#Standard deviation     64.2115564 18.79066226 6.541294697
#Proportion of Variance  0.9123971  0.07813429 0.009468582
#Cumulative Proportion   0.9123971  0.99053142 1.000000000

#MARZO 11


#summary(marz19crop_pca$model)
#                          Comp.1      Comp.2      Comp.3
#Standard deviation     64.2115564 18.79066226 6.541294697
#Proportion of Variance  0.9123971  0.07813429 0.009468582
#Cumulative Proportion   0.9123971  0.99053142 1.000000000





# UNSUPERCLASS
#  unsuperclass n=4 classi
set.seed(25)
p1_unclass <- unsuperClass(fg2a_feb20, nClasses=4)
p2_unclass <- unsuperClass(fg2a_marz11, nClasses=4)
p3_unclass <- unsuperClass(fg2a_marz19, nClasses=4)

#plotta con par
set.seed(25)
 par(mfrow=c(1,3))
plot(p1_unclass$map)
plot(p2_unclass$map)
plot(p3_unclass$map)

# IMMAGINI UNSUPER CLASS CROPPATE
#plot(crop(p1_unclass$map,lagoon_shp))
#plot(crop(p2_unclass$map,lagoon_shp))
#plot(crop(p3_unclass$map,lagoon_shp))

# funzione PAR 
#par(mfrow=c(1,3))
#plot(crop(p1_unclass$map,lagoon_shp))
#plot(crop(p2_unclass$map,lagoon_shp))
#plot(crop(p3_unclass$map,lagoon_shp))


# IN QUESTO CASO LE DIMENSIONI delle immagini sono tutte diverse, utilizziamo la immagine più piccola per utilizzarla come cropper per le altre immagini 
# il secondo argomento è lo stampo (cioè le altre due immagini)
# questo perchè abbiamo bisogno di immagini con =estensione!! croppo con marzo 19 

# faccio il crop per avere tutte le dimensioni uguale per tutte le immagine (ovvero n pixel asse x per asse y) per confrontare le immagini in modo congruo e non avere errori 
# derivati dal numero di pixel diverso

crop20 <- crop(fg2a_feb20,fg2a_marz19)
crop11 <- crop(fg2a_marz11,fg2a_marz19)
crop19 <- fg2a_marz19

unclass19 <- unsuperClass(crop19, nClasses=4)
unclass11 <- unsuperClass(crop11, nClasses=4)
unclass20 <- unsuperClass(crop20, nClasses=4)

par(mfrow=c(1,3))
plot(unclass19$map)
plot(unclass11$map)
plot(unclass20$map)


# value = classi count = n pixel 
> freq(unclass19$map)
     value  count
[1,]     1  41532
[2,]     2 101834
[3,]     3 107774
[4,]     4  15860
> freq(unclass20$map)
     value count
[1,]     1 42530
[2,]     2 40568
[3,]     3 95501
[4,]     4 88401
> freq(unclass11$map)
     value  count
[1,]     1  29047
[2,]     2  40036
[3,]     3  36228
[4,]     4 161689

# se divido freq per il numero totale di pixel me lo trova per tutte le classi in automatico 
posso fare o una divisione oppure associamo il numero totale di pixel ad un oggetto 

# associo il n tot di pixel ad un oggetto 
tot_pix <- 267000
# dimensions : 712, 375, 267000, 3  (nrow, ncol, ncell, nlayers)

# faccio la divisione 
freq(unclass20$map)/tot_pix
> freq(unclass20$map)/tot_pix
            value     count
[1,] 3.745318e-06 0.1592884
[2,] 7.490637e-06 0.1519401
[3,] 1.123596e-05 0.3576816
[4,] 1.498127e-05 0.3310899


> freq(unclass11$map)/tot_pix
            value     count
[1,] 3.745318e-06 0.1087903
[2,] 7.490637e-06 0.1499476
[3,] 1.123596e-05 0.1356854
[4,] 1.498127e-05 0.6055768


> freq(unclass19$map)/tot_pix
            value      count
[1,] 3.745318e-06 0.15555056
[2,] 7.490637e-06 0.38140075
[3,] 1.123596e-05 0.40364794
[4,] 1.498127e-05 0.05940075

# LA CLASSE 2 DI 20FEB è LA CLASSE 4 DI MARZ11 questà è la torbidità 
# non abbiamo croppato l'immagine perchè avevamo bisogno di identificare rotte anche vicino al porto e alle rive 


# DATA FRAME è una tabella che utilizziamo con percentage per 
# us.dataframe per convertire in dataframe in oggetto
# argomento : cover (etichette della tabella che creiamo con vettore, lo crei con c= cioè un vettore)
# la nostra cover è l'etichetta e i valori all'interno che sono sempre vettori 
# con cover construiamo una colonna ovvero torbidità 
ESEMPIO

cover20 <- "torbidità 20febbraio"
cover11 <- "torbidità 11marzo"
cover19 <- "torbidità 19marzo"

percent20 <- 0.1519401
percent11 <- 0.6055768
percent19 <- 0.05940075

percentages<- data.frame(cover=c(cover20,cover11,cover19), percentage=c(percent20,percent11,percent19))
head(percentages)
ggplot(percentages, aes(x=cover, y=percentage, color=cover)) + geom_bar(stat="identity", fill="white")

ggplot(percentages, aes(x=cover, y=percent_1992, color=cover)) + geom_bar(stat="identity", fill="white")
ggplot(percentages, aes(x=cover, y=percent_2006, color=cover)) + geom_bar(stat="identity", fill="white")

p1 <- ggplot(percentages, aes(x=cover, y=percent_1992, color=cover)) + geom_bar(stat="identity", fill="white")
p2 <- ggplot(percentages, aes(x=cover, y=percent_2006, color=cover)) + geom_bar(stat="identity", fill="white")

grid.arrange(p1, p2, nrow=1)
# 11 marzo è molto "torbida" perchè c'era vento, è evidente però la differenza tra febbraio e marzo 11

 
                 
              
# PARTE DUE----------------------------------------------------------------------------------

#PCA
aprile2020 <- brick ("aprile2020_wgs84.tif")
aprile2019 <- brick ("aprile2019_wgs84.tif")
                 
set.seed(25)
apr20_pca <- rasterPCA(apr19)
marz11_pca <- rasterPCA(apr20)
apr20_pc1 <- feb20_pca$map$PC1
apr19_pc1 <- marz11_pca$map$PC1
apr20_mw <- focal (apr20_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
apr19_mw <- focal (apr19_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
clsd <- colorRampPalette(c('blue','yellow','pink','red','orange','green'))(100)
par(mfrow=c(1,2))
plot(apr20_mw, col=clsd)
plot(apr19_mw, col=clsd)



# UNSUPERCLASS
apr19 <- crop(aprile2019,aprile2020)
apr20 <- aprile2020

apri19_unclass <- unsuperClass(apr19, nClasses=4)
apri20_unclass <- unsuperClass(apr20, nClasses=4)

par(mfrow=c(1,2))
plot(apri20_unclass$map)
plot(apri19_unclass$map)

freq(apri20_unclass$map)
freq(apri19_unclass$map)

freq(apri19_unclass$map)
     value  count
[1,]     1 805518  # bianco, mi sembra uguale
[2,]     2 432379 # 4 classe del 20 aprile 
[3,]     3  85515
[4,]     4 257860 
freq(apri20_unclass$map)
     value  count
[1,]     1 790328 
[2,]     2 270363 
[3,]     3 100556
[4,]     4 420025 # 2 classe del 19 aprile

tot_pix_apr <- 1581272
freq(apri20_unclass$map)/tot_pix_apr
freq(apri19_unclass$map)/tot_pix_apr

freq(apri19_unclass$map)/tot_pix_apr
            value      count
[1,] 6.324023e-07 0.50941141
[2,] 1.264805e-06 0.27343746
[3,] 1.897207e-06 0.05407988
[4,] 2.529609e-06 0.16307125

 freq(apri20_unclass$map)/tot_pix_apr
            value      count
[1,] 6.324023e-07 0.49980522
[2,] 1.264805e-06 0.17097817
[3,] 1.897207e-06 0.06359184
[4,] 2.529609e-06 0.26562476

cover20apr <- "eterogeneità aprile2020"
cover19apr <- "eterogeneità aprile2019"
percentage2020 <- 26562476
percentage2019 <- 27343746

percentages <- data.frame(cover=c(cover20apr,cover19apr), percentage=c(percentage2020,percentage2019))
head(percentages)
ggplot(percentages, aes(x=cover, y=percentage, color=cover)) + geom_bar(stat="identity", fill="white")

                 
                 
#CROP DI TUTTE LE IMMAGINI CON LA PIù PICCOLA # NON FUNZIONA
                
feb20_cr <- crop(fg2a_feb20,aprile2020)                 
marz11_cr <- crop(fg2a_marz11,aprile2020)
marz19_cr <- crop(fg2a_marz19,aprile2020)
apr19_cr <- crop(aprile2019,aprile2020)
apr20_cr <- aprile2020
                 
                                  
percentages <- c(27343746, 0.1519401, 26562476, 0.6055768, 0.05940075)

tempo <- c("aprile2019", "febbraio2020", "aprile2020", "marzo2020", "marzo2020")
percentage <- data.frame(tempo,percentages)
                 
plot(percentage$percentages, type="b", lty="dashed", col="red", axes=FALSE, ann=FALSE)
lines(percentage$percentages, type="l", col="blue")
title(main="eterogeneità", col.main="blue", font.main="4", xlab="tempo", ylab="percentages")
box()
axis(1, at=1:5, lab=percentage$tempo)
axis(2, las=1, at=seq(min(percentage), max(percetage), 300))
                 
              
                 
 # CLICK
                 
                 aprile2019 <- brick ("aprile2019_wgs84.tif")
                 
                 plotRGB(aprile2019, r=1, g=2, b=3, stretch="lin")
click(aprile2019, n=Inf, id=FALSE, xy=FALSE, cell=FALSE, type="n", show=TRUE)
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 

                 
                 
                 
                 
                 
                 
 #################CORRETTO################                
                 
install.packages("viridis")
library (viridis)
library(raster)
library(rgdal)
library(gridExtra)
library(RStoolbox)
library(ggplot2)

setwd("C:/pt/")


                 
# CARICO IMMAGINI COMPOSTE DA TRE LAYERS
fg2a_feb20_imported<- brick("fig2a_wgs84.tif")
fg2a_marz11<- brick("fig2b_wgs84.tif")
fg2a_marz19<- brick("fig2c_wgs84.tif")

#CARICO LO SHAPEFILE DELL'AREA DI STUDIO
readOGR("files lavinia angelucci.shp")
lagoon_shp <- readOGR ("files lavinia angelucci.shp")

# plotto le immagini con RGB 
plotRGB(fg2a_feb20, r=1, g=2, b=3, stretch="hist")
plotRGB(fg2a_marz11, r=1, g=2, b=3, stretch="hist")
plotRGB(fg2a_marz19, r=1, g=2, b=3, stretch="hist")

# PLOT CON GRID.ARRANGE
p1 <- ggRGB(fg2a_feb20,1,2,3, stretch="hist")
p2 <-  ggRGB(fg2a_marz11,1,2,3, stretch="hist")
p3 <-  ggRGB(fg2a_marz19,1,2,3, stretch="hist")

#faccio grid.arrange
grid.arrange(p1, p2, p3, nrow=1)


### CROP IMMAGINI SU AREA DI STUDIO
p1_cropped <- crop(fg2a_feb20,lagoon_shp)
p2_cropped <- crop(fg2a_marz11,lagoon_shp)
p3_cropped <- crop(fg2a_marz19,lagoon_shp)

# plotto le immagini croppate con grid.arrange
gg_p1_cropped <- ggRGB(p1_cropped,1,2,3, stretch="hist")
gg_p2_cropped <- ggRGB(p2_cropped,1,2,3, stretch="hist")
gg_p3_cropped <- ggRGB(p3_cropped,1,2,3, stretch="hist")
grid.arrange(gg_p1_cropped, gg_p2_cropped, gg_p3_cropped, nrow=3)



#PCA, analisi multivariata ### LA SI PUò FARE DIRETTAMENTE SULL'IMMAGINE CROPPATA SE USO QUELLA DA ORA IN POI ###
#you take a dataset with many variables, and you simplify that dataset by turning your original variables into a smaller number of "Principal Components".
set.seed(25)
feb20_pca <- rasterPCA(fg2a_feb20)
marz11_pca <- rasterPCA(fg2a_marz11)
marz19_pca <- rasterPCA(fg2a_marz19)

### VEDERE SE CON HIST LE DIFFERENZE SONO MEGLIO VISUALIZZATE ###
ggRGB(feb20_pca$map,1,2,3, stretch="lin", q=0)
ggRGB(marz19_pca$map,1,2,3, stretch="lin", q=0)
ggRGB(marz11_pca$map,1,2,3, stretch="lin", q=0)
                                                 
### NON RIESCO A CAPIRE ###
plot20 <- lapply(1:3, function(x) ggRGB(feb20_pca$map,1,2,3, stretch="lin", q=0), x, geom_raster = TRUE)
plot11 <- lapply(1:3, function(x) ggRGB(marz11_pca$map,1,2,3, stretch="lin", q=0), x, geom_raster = TRUE)
plot19 <- lapply(1:3, function(x) ggRGB(marz19_pca$map,1,2,3, stretch="lin", q=0), x, geom_raster = TRUE)

 grid.arrange(plot20[[1]],plot20[[2]], plot20[[3]], ncol=2)
 grid.arrange(plot11[[1]],plot11[[2]], plot11[[3]], ncol=2)
 grid.arrange(plot19[[1]],plot19[[2]], plot19[[3]], ncol=2)
 
 #grid.arrange delle 3 immagini 
 grid.arrange(plot20[[1]],plot11[[1]], plot19[[1]], ncol=2)
 
# crop delle immagini PCA
### ELIMINARE LE SEGUENTI TRE RIGHE (CODICE RIDONDANTE), SI PUò SNELLIRE ANCORA LAVORANDO GIà DA PRIMA CON LE IMMAGINE CROPPATE
crop(feb20_pca$map,lagoon_shp)
crop(marz11_pca$map,lagoon_shp)
crop(marz11_pca$map,lagoon_shp)

feb20crop <- crop(feb20_pca$map,lagoon_shp)
marz11crop <- crop(marz11_pca$map,lagoon_shp)
marz19crop <- crop(marz19_pca$map,lagoon_shp)

feb20crop_pca <- rasterPCA(feb20crop)
marz11crop_pca <- rasterPCA(marz11crop)
marz19crop_pca <- rasterPCA(marz19crop)
par(mfrow=c(1,3))
plot(feb20crop_pca$map)
plot(marz11crop_pca$map)
plot(marz19crop_pca$map)






# MOVING WINDOW

feb20_pc1 <- feb20_pca$map$PC1
marz11_pc1 <- marz11_pca$map$PC1
marz19_pc1 <- marz19_pca$map$PC1
### PROVARE LEVELPLOT o ggPLOT ###
par(mfrow=c(1,3))
plot(feb20_pc1)
plot(marz11_pc1)
plot(marz19_pc1)

feb20_mw <- focal (feb20_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
marz11_mw <- focal (marz11_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
marz19_mw <- focal (marz19_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
### PROVARE CON GGplot ###
ggplot() +
geom_raster(feb20_mw, mapping = aes(x = x, y = y, fill = layer)) +
scale_fill_viridis(option="magma") +
ggtitle("sd of feb20_mw")
###
clsd <- colorRampPalette(c('blue','green','pink','red','orange','yellow'))(100)
par(mfrow=c(1,3))
plot(feb20_mw, col=clsd)
plot(marz11_mw, col=clsd)
plot(marz19_mw, col=clsd)

### LAVORARE DIRETTAMENTE CON LE IMMAGINI CROPPATE ###
feb20mw_cropped <- crop(feb20_mw,lagoon_shp)
marz11mw_cropped <- crop(marz11_mw,lagoon_shp)
marz19mw_cropped <- crop(marz19_mw,lagoon_shp)

par(mfrow=c(1,3))
plot(feb20mw_cropped, col=clsd)
plot(marz11mw_cropped, col=clsd)
plot(marz19mw_cropped, col=clsd)


### OK EVIDENZIARE LE SUMMARY MA IN ORDINE APPENA VIENE FATTA LA PCA ###
#SUMMARY DELLE COMPONENTI NON CROPPED
summary(feb20_pca$model) 
#                            Comp.1      Comp.2      Comp.3
#Standard deviation     112.4212153 32.41154082 7.839134229
#Proportion of Variance   0.9191331  0.07639786 0.004469079
#Cumulative Proportion    0.9191331  0.99553092 1.000000000

summary(marz19_pca$model)
#                           Comp.1      Comp.2      Comp.3
#Standard deviation     69.6829683 21.23681719 7.128451679
#Proportion of Variance  0.9063343  0.08418098 0.009484742
#Cumulative Proportion   0.9063343  0.99051526 1.000000000

summary(marz11_pca$model)
#                           Comp.1      Comp.2      Comp.3
#Standard deviation     90.6106865 22.61181338 5.727743638
#Proportion of Variance  0.9378482  0.05840426 0.003747493
#Cumulative Proportion   0.9378482  0.99625251 1.000000000




# SUMMARY DELLE COMPONENTI CROPPATO --> VALORI SOLO DI ACQUA 
### NON è VERO CHE è SOLO ACQUA, MEGLIO LAVORARE SOLO SU CROP CON GIUSTIFICAZIONE "AREA DI STUDIO" ###
#summary(feb20crop_pca$model)
#                            Comp.1      Comp.2      Comp.3
#Standard deviation     104.6909160 27.94909281 7.469126723
#Proportion of Variance   0.9290556  0.06621542 0.004728935
#Cumulative Proportion    0.9290556  0.99527106 1.000000000

# MARZO 19


#summary(marz19crop_pca$model)
#                           Comp.1      Comp.2      Comp.3
#Standard deviation     64.2115564 18.79066226 6.541294697
#Proportion of Variance  0.9123971  0.07813429 0.009468582
#Cumulative Proportion   0.9123971  0.99053142 1.000000000

#MARZO 11


#summary(marz19crop_pca$model)
#                          Comp.1      Comp.2      Comp.3
#Standard deviation     64.2115564 18.79066226 6.541294697
#Proportion of Variance  0.9123971  0.07813429 0.009468582
#Cumulative Proportion   0.9123971  0.99053142 1.000000000





# UNSUPERCLASS
#  unsuperclass n=4 classi
set.seed(25)
p1_unclass <- unsuperClass(fg2a_feb20, nClasses=4)
p2_unclass <- unsuperClass(fg2a_marz11, nClasses=4)
p3_unclass <- unsuperClass(fg2a_marz19, nClasses=4)

#plotta con par
### NON SERVE SET.SEED QUI ###
set.seed(25)
 par(mfrow=c(1,3))
plot(p1_unclass$map)
plot(p2_unclass$map)
plot(p3_unclass$map)

### LAVORARE DIRETTAMENTE CON LE IMMAGINI CROPPATE ###
# IMMAGINI UNSUPER CLASS CROPPATE
#plot(crop(p1_unclass$map,lagoon_shp))
#plot(crop(p2_unclass$map,lagoon_shp))
#plot(crop(p3_unclass$map,lagoon_shp))

# funzione PAR 
#par(mfrow=c(1,3))
#plot(crop(p1_unclass$map,lagoon_shp))
#plot(crop(p2_unclass$map,lagoon_shp))
#plot(crop(p3_unclass$map,lagoon_shp))


# IN QUESTO CASO LE DIMENSIONI delle immagini sono tutte diverse, utilizziamo la immagine più piccola per utilizzarla come cropper per le altre immagini 
# il secondo argomento è lo stampo (cioè le altre due immagini)
# questo perchè abbiamo bisogno di immagini con =estensione!! croppo con marzo 19 

# faccio il crop per avere tutte le dimensioni uguale per tutte le immagine (ovvero n pixel asse x per asse y) per confrontare le immagini in modo congruo e non avere errori 
# derivati dal numero di pixel diverso

### METTERE QUESTO ALL'INIZIO E POI LANCIARE UNSUPERCLASS, POSSIBILMENTE UTILIZZARE DIRETTAMENTE IL CROP CON LO SHP SEMPLIFICANDO IL TUTTO SENZA RICROPPARE ###
crop20 <- crop(fg2a_feb20,fg2a_marz19)
crop11 <- crop(fg2a_marz11,fg2a_marz19)
crop19 <- fg2a_marz19

unclass19 <- unsuperClass(crop19, nClasses=4)
unclass11 <- unsuperClass(crop11, nClasses=4)
unclass20 <- unsuperClass(crop20, nClasses=4)

par(mfrow=c(1,3))
plot(unclass19$map)
plot(unclass11$map)
plot(unclass20$map)


# value = classi count = n pixel 
> freq(unclass19$map)
     value  count
[1,]     1  41532
[2,]     2 101834
[3,]     3 107774
[4,]     4  15860
> freq(unclass20$map)
     value count
[1,]     1 42530
[2,]     2 40568
[3,]     3 95501
[4,]     4 88401
> freq(unclass11$map)
     value  count
[1,]     1  29047
[2,]     2  40036
[3,]     3  36228
[4,]     4 161689

# se divido freq per il numero totale di pixel me lo trova per tutte le classi in automatico 
posso fare o una divisione oppure associamo il numero totale di pixel ad un oggetto 

# associo il n tot di pixel ad un oggetto 
tot_pix <- 267000
# dimensions : 712, 375, 267000, 3  (nrow, ncol, ncell, nlayers)

# faccio la divisione 
freq(unclass20$map)/tot_pix
> freq(unclass20$map)/tot_pix
            value     count
[1,] 3.745318e-06 0.1592884
[2,] 7.490637e-06 0.1519401
[3,] 1.123596e-05 0.3576816
[4,] 1.498127e-05 0.3310899


> freq(unclass11$map)/tot_pix
            value     count
[1,] 3.745318e-06 0.1087903
[2,] 7.490637e-06 0.1499476
[3,] 1.123596e-05 0.1356854
[4,] 1.498127e-05 0.6055768


> freq(unclass19$map)/tot_pix
            value      count
[1,] 3.745318e-06 0.15555056
[2,] 7.490637e-06 0.38140075
[3,] 1.123596e-05 0.40364794
[4,] 1.498127e-05 0.05940075

# LA CLASSE 2 DI 20FEB è LA CLASSE 4 DI MARZ11 questà è la torbidità 
# non abbiamo croppato l'immagine perchè avevamo bisogno di identificare rotte anche vicino al porto e alle rive 


# DATA FRAME è una tabella che utilizziamo con percentage per 
# us.dataframe per convertire in dataframe in oggetto
# argomento : cover (etichette della tabella che creiamo con vettore, lo crei con c= cioè un vettore)
# la nostra cover è l'etichetta e i valori all'interno che sono sempre vettori 
# con cover construiamo una colonna ovvero torbidità 
ESEMPIO

cover20 <- "torbidità 20febbraio"
cover11 <- "torbidità 11marzo"
cover19 <- "torbidità 19marzo"

percent20 <- 0.1519401
percent11 <- 0.6055768
percent19 <- 0.05940075

percentages<- data.frame(cover=c(cover20,cover11,cover19), percentage=c(percent20,percent11,percent19))
head(percentages)
ggplot(percentages, aes(x=cover, y=percentage, color=cover)) + geom_bar(stat="identity", fill="white")


# 11 marzo è molto "torbida" perchè c'era vento, è evidente però la differenza tra febbraio e marzo 11

 

# PARTE DUE----------------------------------------------------------------------------------

#PCA
aprile2020 <- brick ("aprile2020_wgs84.tif")
aprile2019 <- brick ("aprile2019_wgs84.tif")
                 
set.seed(25)
apr20_pca <- rasterPCA(apr19)
marz11_pca <- rasterPCA(apr20)
apr20_pc1 <- feb20_pca$map$PC1
apr19_pc1 <- marz11_pca$map$PC1
apr20_mw <- focal (apr20_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
apr19_mw <- focal (apr19_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
clsd <- colorRampPalette(c('blue','yellow','pink','red','orange','green'))(100)
par(mfrow=c(1,2))
plot(apr20_mw, col=clsd)
plot(apr19_mw, col=clsd)



# UNSUPERCLASS
apr19 <- crop(aprile2019,aprile2020)
apr20 <- aprile2020

apri19_unclass <- unsuperClass(apr19, nClasses=4)
apri20_unclass <- unsuperClass(apr20, nClasses=4)

par(mfrow=c(1,2))
plot(apri20_unclass$map)
plot(apri19_unclass$map)

freq(apri20_unclass$map)
freq(apri19_unclass$map)

freq(apri19_unclass$map)
     value  count
[1,]     1 805518  # bianco, mi sembra uguale
[2,]     2 432379 # 4 classe del 20 aprile 
[3,]     3  85515
[4,]     4 257860 
freq(apri20_unclass$map)
     value  count
[1,]     1 790328 
[2,]     2 270363 
[3,]     3 100556
[4,]     4 420025 # 2 classe del 19 aprile

tot_pix_apr <- 1581272
freq(apri20_unclass$map)/tot_pix_apr
freq(apri19_unclass$map)/tot_pix_apr

freq(apri19_unclass$map)/tot_pix_apr
            value      count
[1,] 6.324023e-07 0.50941141
[2,] 1.264805e-06 0.27343746
[3,] 1.897207e-06 0.05407988
[4,] 2.529609e-06 0.16307125

 freq(apri20_unclass$map)/tot_pix_apr
            value      count
[1,] 6.324023e-07 0.49980522
[2,] 1.264805e-06 0.17097817
[3,] 1.897207e-06 0.06359184
[4,] 2.529609e-06 0.26562476

cover20apr <- "eterogeneità aprile2020"
cover19apr <- "eterogeneità aprile2019"
percentage2020 <- 26562476
percentage2019 <- 27343746

percentages <- data.frame(cover=c(cover20apr,cover19apr), percentage=c(percentage2020,percentage2019))
head(percentages)
ggplot(percentages, aes(x=cover, y=percentage, color=cover)) + geom_bar(stat="identity", fill="white")

                 
                 
CROP DI TUTTE LE IMMAGINI CON LA PIù PICCOLA
                
feb20_cr <- crop(fg2a_feb20,aprile2020)                 
marz11_cr <- crop(fg2a_marz11,aprile2020)
marz19_cr <- crop(fg2a_marz19,aprile2020)
apr19_cr <- crop(aprile2019,aprile2020)
apr20_cr <- aprile2020
                                  
                 
                 
                 
                 
                                 
percentages <- c(27343746, 0.1519401, 26562476, 0.6055768, 0.05940075)

tempo <- c("aprile2019", "febbraio2020", "aprile2020", "marzo2020", "marzo2020")
percentage <- data.frame(tempo,percentages)
plot(percentage$percentages, type="b", lty="dashed", col="red", axes=FALSE, ann=FALSE)
lines(percentage$percentages, type="l", col="blue")
title(main="eterogeneità", col.main="blue", font.main="4", xlab="tempo", ylab="percentages")
box()
axis(1, at=1:5, lab=percentage$tempo)
axis(2, las=1, at=seq(min(percentage), max(percetage), 300))

