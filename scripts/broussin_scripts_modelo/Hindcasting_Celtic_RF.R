library(biomod2)
library(raster)

setwd("/mnt/beegfs/egoberville/Environnement_CMIP + Phyto")
new.env.complet <- read.csv("Projections_Env_CMIP-mensuel_1950-2021.csv")

ProjPresFin <- new.env.complet[3:2322,1:2]
RasterProjPresFin <- cbind(ProjPresFin, rep(1,length(ProjPresFin)))
RasterProjPresFin <- rasterFromXYZ(ProjPresFin)

time.period <- seq(3,(length(new.env.complet)-6),7)
modele <- c("RF")
pa <- c("R_5000", "R_10000", "R_30000", "R_50000", "CH_5000", "CH_10000", "CH_30000", "CH_50000")
sortie <- c("5000R", "10000R", "30000R", "50000R",
            "5000CH", "10000CH", "30000CH", "50000CH")

for (i in 1:length(modele)){
  
  for (j in 1:3){
    
    ProjPresFin <- new.env.complet[3:2322,1:2]
    RasterProjPresFin <- cbind(ProjPresFin, rep(1,length(ProjPresFin)))
    RasterProjPresFin <- rasterFromXYZ(ProjPresFin)
    
    for (k in 1:length(time.period)) {
      
      new.env <- new.env.complet[3:2322,c(1:2,time.period[k]:(time.period[k]+6))]
      colnames(new.env)<-c("X","Y","MLD","O2","pH","Salinity","SST","Phyto","Zoo")
      
      setwd(paste0("/mnt/beegfs/egoberville/Environnement_CMIP + Phyto/", modele[i], "/",modele[i],"_",pa[j],"/Gadus.morhua.", sortie[j]))
      load(paste0("Gadus.morhua.",sortie[j],".Gadus.morhua_",sortie[j],"FirstModeling.models.out"))
      mod <- get(paste0("Gadus.morhua.",sortie[j],".Gadus.morhua_",sortie[j],"FirstModeling.models.out"))
      
      setwd(paste0("/mnt/beegfs/egoberville/Environnement_CMIP + Phyto/", modele[i], "/",modele[i],"_",pa[j]))
      mySDMProjPres <- BIOMOD_Projection(modeling.output = mod ,
                                         new.env = new.env[,c(3:8)],
                                         xy.new.env = new.env[,1:2],
                                         proj.name = 'Present',
                                         selected.models = 'all',
                                         compress = 'gzip',
                                         build.clamping.mask = F,
                                         output.format = '.RData',
                                         do.stack=T)
      
      ProjPres <- data.frame(get_predictions(mySDMProjPres))
      ProjPresFin <- cbind(new.env[,1:2], ProjPres)
      
      RasterProjPres <- rasterFromXYZ(ProjPresFin, res = c(0.25, 0.25))
      RasterProjPres <- mean(RasterProjPres)
      RasterProjPresFin <- stack(RasterProjPresFin,RasterProjPres)
      
      
    }
    
    names(RasterProjPresFin) <- rep(1950:2021,each=12)
    writeRaster(RasterProjPresFin, "Proj_Env-CMIP-Celtic-mensuel_1950-2021.grd", format = "raster")
    
  }
}