library(biomod2)
library(raster)

setwd("/mnt/beegfs/egoberville")
new.env.complet <- read.csv("Projections_Env_CMIP-Celtic-mensuel_1950-2021.csv")
time.period <- seq(3,(length(new.env.complet)-7),8)

species<-c("Callionymus_lyra","Callionymus_maculatus","Ciliata_mustela","Phycis_blennoides",
           "Aphrodita_acuelata","Goneplax_rhomboides","Liocarcinus_depurator","Liocarcinus_holsatus","Liocarcinus_vernalis",
           "Liocarcinus_marmoreus","Munida_rugosa","Crangon_allmani","Crangon_crangon","Trisopterus_esmarkii","Trisopterus_luscus","Trisopterus_minutus",
           "Macropipus_tuberculatus","Nephrops_norvegicus")
species.mod<-c("Callionymus.lyra","Callionymus.maculatus","Ciliata.mustela","Phycis.blennoides",
               "Aphrodita.acuelata","Goneplax.rhomboides","Liocarcinus.depurator","Liocarcinus.holsatus","Liocarcinus.vernalis",
               "Liocarcinus.marmoreus","Munida.rugosa","Crangon.allmani","Crangon.crangon","Trisopterus.esmarkii","Trisopterus.luscus","Trisopterus.minutus",
               "Macropipus.tuberculatus","Nephrops.norvegicus")

occ.freq.folder<-c("1-3.e","1-3.3e","1-3.5e","2-3.e","2-3.3e","2-3.5e")
occ.freq.mod<-c("1.3.e","1.3.3e","1.3.5e","2.3.e","2.3.3e","2.3.5e")
plot_list<-list()
plot_list_species<-list()

for (i in 1:length(species)){
  
  for (j in 1:length(species)){
    
    ProjPresFin <- new.env.complet[3:nrow(new.env.complet),1:2]
    RasterProjPresFin <- cbind(ProjPresFin, rep(1,nrow(ProjPresFin)))
    RasterProjPresFin <- rasterFromXYZ(ProjPresFin)
    
    for (k in 1:length(time.period)) {
      
      new.env <- new.env.complet[3:nrow(new.env.complet),c(1:2,time.period[k]:(time.period[k]+7))]
      colnames(new.env)<-c("X","Y","MLD","O2","pH","Salinity","SST","Phyto","Zoo","SBT")
      
      setwd(paste0("/mnt/beegfs/egoberville/",species[i],"/RF.",species[i],".",occ.freq.folder[j],".sbt.sst/",
                   species.mod[i],".",occ.freq.mod[j],".RF"))
      load(paste0(species.mod[i],".",occ.freq.mod[j],".RF.",species[i],".",occ.freq.mod[j],".RF.models.out"))
      mod <- get(paste0(species.mod[i],".",occ.freq.mod[j],".RF.",species[i],".",occ.freq.mod[j],".RF.models.out"))
      
      setwd(paste0("/mnt/beegfs/egoberville/",species[i],"/RF.",species[i],".",occ.freq.folder[j],".sbt.sst"))
      mySDMProjPres <- BIOMOD_Projection(bm.mod = mod ,
                                         new.env = new.env[c("SBT","SST")],
                                         new.env.xy = new.env[,1:2],
                                         proj.name = paste0('Celtic.contemporain.',occ.freq.mod[j]),
                                         models.chosen = 'all',
                                         compress = 'gzip',
                                         build.clamping.mask = F,
                                         output.format = '.RData',
                                         do.stack=T)
      
      ProjPres <- data.frame(get_predictions(mySDMProjPres))
      ProjPresFin <- cbind(new.env[,1:2], ProjPres)
      
      RasterProjPres <- rasterFromXYZ(ProjPresFin[,c(1,2,8)], res = c(0.25, 0.25))
      RasterProjPres <- mean(RasterProjPres)
      RasterProjPresFin <- stack(RasterProjPresFin,RasterProjPres)
      
      
    }
    
    names(RasterProjPresFin) <- rep(1950:2021,each=12)
    setwd(paste0("/mnt/beegfs/egoberville/",species[i]))
    writeRaster(RasterProjPresFin, paste0("Proj_Env-CMIP-Celtic-mensuel_1950-2021_",occ.freq.mod[j],".grd"), format = "raster")
    
  }
  
}