library(raster)
library(CENFA)

setwd("d:/thèse/data/occurrences/gadus_morhua")
occ <- read.csv("Gadus_morhua.csv")
occ <- occ[,2:3]
field <- rep(1, length(nrow(occ)))
occ <- SpatialPoints(occ[,1:2])
setwd("d:/thèse/data/matrices/projections")
data <- read.csv("Mat_Env_Obs_1.csv")
r.env <- raster(ncol = 1440, nrow = 721, extent(-180.125,179.875,-90.125,90.125))

for (i in 3:7) {
  r <- rasterFromXYZ(data[,c(1:2,i)])
  names(r) <- colnames(data)[i]
  r.env <- stack(r.env, r)
}

enfa <- CENFA::enfa(r.env, occ, field = field )
s.map <- sensitivity_map(enfa)
pred <- predict(enfa)
