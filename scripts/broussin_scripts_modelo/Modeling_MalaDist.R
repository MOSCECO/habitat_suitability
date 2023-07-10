library(adehabitatHS)
library(dplyr)
library(raster)

month <- c("Janv", "Fev", "Mars", "Avril", "Mai", "Juin", "Juillet",
           "Aout", "Sep", "Oct", "Nov", "Dec")

for (i in 1:12){
  
  setwd("d:/thèse/data/occurrences/gadus_morhua")
  occ <- read.csv("Gadus_morhua.csv")
  occ <- occ %>% filter (Month == i)
  occ <- occ[,2:3]
  
  setwd("d:/thèse/data/matrices/projections")
  data <- read.csv(paste0("Mat_Env_Obs_",i,".csv"))
  #Isolation of complete coordinates
  coord <- data[,1:2]
  data <- na.omit(data)
  
  occ <- SpatialPoints(occ)
  #Transformation of the coordinates into a spatial object
  data <- SpatialPixelsDataFrame(data[,1:2], data[,3:7])
  #slot function : transformation of spatial to regular data (for the output) - keeping only the weigth [,1]
  weigth <- slot(count.points(occ, data), "data") [,1]
  
  ## We first scale the maps
  slot(data, "data") <- dudi.pca(slot(data, "data"), scannf=FALSE)$tab
  
  ## habitat suitability mapping
  hsm <- mahasuhab(data, occ, type = "probability")
  hsm.fin <- cbind(hsm@coords, hsm@data)
  coord <- rbind(coord, hsm@coords)
  coord <- coord %>% count(x, y) %>% filter(n == 1) %>% dplyr::select(-n)
  coord <- cbind(coord, rep(NA, length(nrow(coord))))
  colnames(coord)[3] <- "MD"
  hsm.fin <- rbind(coord, hsm.fin)
  
  raster <- rasterFromXYZ(hsm.fin)
  
  setwd("d:/thèse/modeling/gadus_morhua/enfa")
  writeRaster(raster, paste0(i, "_Projection_", month[i], ".grd"), format = "raster")
  
}


