#####ENFA Modeling
library(adehabitatHS)
library(adehabitatMA)
library(dplyr)
library(raster)

## ----- Import the env. values associated with the occurrences of the species
setwd("e:/thèse/data/matrices/gadus_morhua/test sbt+o2 vs sbt")
env.occ <- read.csv("Mat_mod_G_morhua_50000_CH_reechantST_barents.csv")
env.occ <- env.occ%>%filter(resp.var==1)
occ <- env.occ[,1:2]
env <- env.occ[,c(5,6,7,9,10)]


## ----- Import the env. values for projections
setwd("e:/thèse/data/matrices/projections/environment_cmip_contemporain_mensuel")
proj <- read.csv("Projections_Env_CMIP-Celtic-mensuel_1950-2021.csv")
#Isolation of complete coordinates
coord.proj <- proj[,1:2]
#Remove NAs (not supported by enfa modeling)
proj <- na.omit(proj)
#Isolation of env. var.
env.proj <- proj[,-c(1:2)]

## ----- Determine utilization weigth = number of occurrences in each pixel of the map
#Transformation of the coordinates of occurrences into a spatial object
occ.sp <- SpatialPoints(occ)
#Transformation of the total map into a spatial object 
setwd("e:/thèse/data/environnement/observed")
global.map <- raster("Salinity_inter.grd")
global.map <- as.data.frame(global.map,xy=T)
global.map.sp <- SpatialPixelsDataFrame(global.map[,1:2],as.data.frame(global.map[,3]))
#slot function : transformation of spatial to regular data (for the output) - keeping only the weigth [,1]
weigth <- slot(count.points(occ.sp, global.map.sp), "data") [,1]

##The model needs centered and standardised env. var. (recomand use of dudi.pca)
env.pca <- dudi.pca(env, scannf = FALSE)

## --- Modeling
enfa <- enfa(env.pca, weigth, scannf = FALSE)


## --- Projections
time.period <- seq(1,(length(proj)-7),8)

for (i in 1:length(time.period)){
  
  #Transformation of the coordinates into a spatial object for proj
  proj.sp <- SpatialPixelsDataFrame(coord.proj, env.proj[3:2322,c(time.period[k],time.period[k]+1,time.period[k]+2,time.period[k]+3,time.period[k]+7)])
  pred <- predict(enfa, proj.sp)
  #Binding with coordinates
  pred.fin <- cbind(proj.sp@data[,1:2], pred@data)
  #Adding the coordinates associated with NAs
  #Isolation of those coordinates 
  coord <- rbind(coord, data@data[,1:2])
  coord <- coord %>% count(x, y) %>% filter(n == 1) %>% dplyr::select(-n)
  coord <- cbind(coord, rep(NA, length(nrow(coord))))
  colnames(coord)[3] <- "Approx.MD"
  pred.fin <- rbind(pred.fin, coord)
  
  raster <- rasterFromXYZ(pred.fin)
}


