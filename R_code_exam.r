# R_code_exam_LaviniaAngelucci


# library         
install.packages("viridis")
library (viridis) #per palette colori
library(raster) #per oggetti raster
library(rasterVis) #per levelplot
library(rgdal)
library(gridExtra)
library(RStoolbox)
library(ggplot2) #per ggplot
setwd("C:/pt/") # working directory
                 
# carico immagini da 3 layer suo brick
fg2a_feb20<- brick("fig2a_wgs84.tif")
fg2a_marz11<- brick("fig2b_wgs84.tif")
fg2a_marz19<- brick("fig2c_wgs84.tif")

#carico shape.files dell'aera di studio
readOGR("files lavinia angelucci.shp")
lagoon_shp <- readOGR ("files lavinia angelucci.shp")
                 
### crop immagini su area di studio 
p1_cropped <- crop(fg2a_feb20,lagoon_shp)
p2_cropped <- crop(fg2a_marz11,lagoon_shp)
p3_cropped <- crop(fg2a_marz19,lagoon_shp)

# plotto le immagini con RGB 
plotRGB(fg2a_feb20, r=1, g=2, b=3, stretch="hist")
plotRGB(fg2a_marz11, r=1, g=2, b=3, stretch="hist")
plotRGB(fg2a_marz19, r=1, g=2, b=3, stretch="hist")

# grid.arrange
p1 <- ggRGB(fg2a_feb20,1,2,3, stretch="hist")
p2 <-  ggRGB(fg2a_marz11,1,2,3, stretch="hist")
p3 <-  ggRGB(fg2a_marz19,1,2,3, stretch="hist")
grid.arrange(p1, p2, p3, nrow=1)

# grid.arrange delle immagini croppate
gg_p1_cropped <- ggRGB(p1_cropped,1,2,3, stretch="hist")
gg_p2_cropped <- ggRGB(p2_cropped,1,2,3, stretch="hist")
gg_p3_cropped <- ggRGB(p3_cropped,1,2,3, stretch="hist")
grid.arrange(gg_p1_cropped, gg_p2_cropped, gg_p3_cropped, nrow=1)

#PCA, analisi multivariata 
set.seed(25)
feb20_pca <- rasterPCA(fg2a_feb20)
marz11_pca <- rasterPCA(fg2a_marz11)
marz19_pca <- rasterPCA(fg2a_marz19)

#stretch lineare
ggRGB(feb20_pca$map,1,2,3, stretch="lin", q=0)
ggRGB(marz11_pca$map,1,2,3, stretch="lin", q=0)
ggRGB(marz19_pca$map,1,2,3, stretch="lin", q=0)

# grid.arrange
p20<- ggRGB(feb20_pca$map,1,2,3, stretch="lin", q=0)
p11 <-ggRGB(marz11_pca$map,1,2,3, stretch="lin", q=0)
p19<- ggRGB(marz19_pca$map,1,2,3, stretch="lin", q=0)
grid.arrange(p20,p11,p19, ncol=3)   

# summary delle componenti
summary(feb20_pca$model) 
#                            Comp.1      Comp.2      Comp.3
#Standard deviation     112.4212153 32.41154082 7.839134229
#Proportion of Variance   0.9191331  0.07639786 0.004469079
#Cumulative Proportion    0.9191331  0.99553092 1.000000000
                 summary(marz11_pca$model)
#                           Comp.1      Comp.2      Comp.3
#Standard deviation     90.6106865 22.61181338 5.727743638
#Proportion of Variance  0.9378482  0.05840426 0.003747493
#Cumulative Proportion   0.9378482  0.99625251 1.000000000

summary(marz19_pca$model)
#                           Comp.1      Comp.2      Comp.3
#Standard deviation     69.6829683 21.23681719 7.128451679
#Proportion of Variance  0.9063343  0.08418098 0.009484742
#Cumulative Proportion   0.9063343  0.99051526 1.000000000

# PC1
feb20_pc1 <- feb20_pca$map$PC1 
marz11_pc1 <- marz11_pca$map$PC1
marz19_pc1 <- marz19_pca$map$PC1
                 
# level.plot di PC1 sfrutto una gamma di colori più ampia e le immagini sono più definite
# visualizziamo inoltre i livelli di contorno     
levelplot(feb20_pc1)
levelplot(marz11_pc1)
levelplot(marz19_pc1)
dev.of()

# funzione focal moving window
feb20_mw <- focal (feb20_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
marz11_mw <- focal (marz11_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
marz19_mw <- focal (marz19_pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
                 
#ggplot con viridis opzione turbo
ggplot() +
geom_raster(feb20_mw, mapping = aes(x = x, y = y, fill = layer)) +
scale_fill_viridis(option="turbo") +
ggtitle("ggplot 20 FEBBRAIO")
ggplot() +
geom_raster(marz11_mw, mapping = aes(x = x, y = y, fill = layer)) +
scale_fill_viridis(option="turbo") +
ggtitle("ggplot 11 MARZO")
ggplot() +
geom_raster(marz19_mw, mapping = aes(x = x, y = y, fill = layer)) +
scale_fill_viridis(option="turbo") +
ggtitle("ggplot 19 MARZO")
                 
# colramppalettelocalmente 
clsd <- colorRampPalette(c('blue','green','pink','red','orange','yellow'))(100)
par(mfrow=c(1,3))
plot(feb20_mw, col=clsd)
plot(marz11_mw, col=clsd)
plot(marz19_mw, col=clsd)

# unsupervised classification
crop20 <- crop(fg2a_feb20,fg2a_marz19)
crop11 <- crop(fg2a_marz11,fg2a_marz19)
crop19 <- fg2a_marz19    
set.seed(25)
unclass20 <- unsuperClass(crop20, nClasses=4)
unclass11 <- unsuperClass(crop11, nClasses=4)
unclass19 <- unsuperClass(crop19, nClasses=4)
# plot con funzione par
par(mfrow=c(1,3))
plot(unclass19$map)
plot(unclass11$map)
plot(unclass20$map)

# funzione freq
freq(unclass19$map) # value = classi, count = n pixel 
#    value  count
#[1,]     1  41532
#[2,]     2 101834
#[3,]     3 107774
#[4,]     4  15860
freq(unclass20$map)
#     value count
#[1,]     1 42530
#[2,]     2 40568
#[3,]     3 95501
##[4,]     4 88401
freq(unclass11$map)
#     value  count
#[1,]     1  29047
#[2,]     2  40036
#[3,]     3  36228
#[4,]     4 161689

# associo il n tot di pixel ad un oggetto 
tot_pix <- 267000 # dimensions: 712, 375, 267000, 3  (nrow, ncol, ncell, nlayers)

# divido per n tot pixel
freq(unclass20$map)/tot_pix
#            value     count
#[1,] 3.745318e-06 0.1592884
#[2,] 7.490637e-06 0.1519401
#[3,] 1.123596e-05 0.3576816
#[4,] 1.498127e-05 0.3310899
freq(unclass11$map)/tot_pix
#            value     count
#[1,] 3.745318e-06 0.1087903
#[2,] 7.490637e-06 0.1499476
#[3,] 1.123596e-05 0.1356854
#[4,] 1.498127e-05 0.6055768
freq(unclass19$map)/tot_pix
#            value      count
#[1,] 3.745318e-06 0.15555056
#[2,] 7.490637e-06 0.38140075
#[3,] 1.123596e-05 0.40364794
#[4,] 1.498127e-05 0.05940075

# in scala temporale
cover20 <- "torbidità 20febbraio"
cover11 <- "torbidità 11marzo"
cover19 <- "torbidità 19marzo"

# % pixel che dispiegano torbidità 
percent20 <- 0.1519401
percent11 <- 0.6055768
percent19 <- 0.05940075

# ggplot geometria a barre
percentages<- data.frame(cover=c(cover20,cover11,cover19), percentage=c(percent20,percent11,percent19))
head(percentages)
ggplot(percentages, aes(x=cover, y=percentage, color=cover)) + geom_bar(stat="identity", fill="white")
