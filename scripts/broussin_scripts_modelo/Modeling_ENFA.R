#####ENFA Modeling
library(adehabitatHS)
library(dplyr)
library(raster)

## ----- Import the occurrences of the species
setwd("d:/th?se/data/occurrences/gadus_morhua")
occ <- read.csv("Gadus_morhua.csv")
occ <- occ %>% filter (Month == 1)
occ <- occ[,2:3]

## ----- Import the env. values for the period you're studying
setwd("d:/th?se/data/matrices/projections")
data <- read.csv("Mat_Env_Obs_1.csv")
#Isolation of complete coordinates
coord <- data[,1:2]
#Remove NAs (not supported by enfa modeling)
data <- na.omit(data)
#Isolation of env. var.
env <- data[,3:7]

## ----- Determine utilization weigth = number of occurrences in each pixel of the map
#Transformation of the coordinates into a spatial object
occ <- SpatialPoints(occ)
#Transformation of the coordinates into a spatial object
data <- SpatialPixelsDataFrame(data[,1:2], data[,3:7])
#slot function : transformation of spatial to regular data (for the output) - keeping only the weigth [,1]
weigth <- slot(count.points(occ, data), "data") [,1]

##The model needs centered and standardised env. var. (recomand use of dudi.pca)
env.pca <- dudi.pca(env, scannf = FALSE)

## --- Modeling
enfa <- enfa(env.pca, weigth, scannf = FALSE)
#Projections
pred <- predict(enfa, data)
#Binding with coordinates
pred.fin <- cbind(data@data[,1:2], pred@data)
#Adding the coordinates associated with NAs
  #Isolation of those coordinates 
coord <- rbind(coord, data@data[,1:2])3
coord <- coord %>% count(x, y) %>% filter(n == 1) %>% dplyr::select(-n)
coord <- cbind(coord, rep(NA, length(nrow(coord))))
colnames(coord)[3] <- "Approx.MD"
pred.fin <- rbind(pred.fin, coord)

raster <- rasterFromXYZ(pred.fin)
