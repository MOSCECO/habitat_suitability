library(FactoMineR)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(raster)
library(gridExtra)
library(ggpubr)

my.colors = colorRampPalette(c("#eeeeee","#1a74c6", "#46c0ae", 
                               "#d9ead3", "#ffc966", "#ffa500", "#f40000"))
#modele <- c("GAM_Random", "GAM_Disk", "RF_Random", "RF_Disk")
pc <- c(1, 2, 3, 4)
plot_list = list()

hab<-stack("Projection_Habitat.grd")
hab<-mean(hab)
  
env.b<-stack("Proj_Env-CMIP-Barents-mensuel_1950-2021.grd")
env.c<-stack("Proj_Env-CMIP-Celtic-mensuel_1950-2021.grd")
env.ns<-stack("Proj_Env-CMIP-NorthSea-mensuel_1950-2021.grd")
env.ws<-stack("Proj_Env-CMIP-WestScotland-mensuel_1950-2021.grd")

env.hab.b<-(env.b*crop(hab,extent(env.b)))/1000000
env.hab.b<-crop(env.hab.b,extent(-10.125,40,58.875,72))
env.hab.c<-(env.c*crop(hab,extent(env.c)))/1000000
env.hab.ns<-(env.ns*crop(hab,extent(env.ns)))/1000000
env.hab.ws<-(env.ws*crop(hab,extent(env.ws)))/1000000

proba<-as.data.frame(env.hab.ws,xy=T)
proba<-round(proba,digits=2)
#setwd(paste0("D://Stage M2/Modelling_Matrix/Resultats_Parcimonieux/", modele[i]))
#proba <- read.csv(paste0("Proba_Annee_", modele[i],".csv"))
proba.na <- na.omit(proba)
proba.t <- data.frame(t(proba.na))
  
  
proba.trans <- proba.t[3:866,]
proba.annuel <- proba.t[1:2,]
  
for (i in seq(1,864,12)){
    
  proba.sub <- proba.trans[c(i,i+1,i+2,i+3,i+4,i+5,i+6,i+7,i+8,i+9,i+10,i+11),]
  proba.sub <- as.data.frame(colMeans(proba.sub))
  proba.sub<- as.data.frame(t(proba.sub))
  proba.annuel <- rbind(proba.annuel,proba.sub)
    
}
  
  
acp <- PCA(proba.annuel[3:74,], scale.unit = TRUE, graph = TRUE)
acp.eig <- acp[["eig"]]
write.table(acp.eig, paste0("ACP_Eig_WestScotland.csv"), row.names = F, sep = ",")
  
#setwd(paste0("D://Stage M2/Modelling_Matrix/Resultats_Parcimonieux/", modele[i], "/ACP/Annee"))
  
for(j in 1:length(pc)){
    
  ####Coordonn?es des mois.ann?e sur PCx
  coord <- data.frame(acp$ind$coord[,pc[j]])
  coord <- cbind(seq(1950,2021), coord)
  colnames(coord) <- c("Date", paste0("Coord.PC.", pc[j]))
  #coord <- coord %>% separate(Date, c("X", "Year"))
  #coord$Date <- seq(1, 314, 1)
  write.table(coord, paste0("Coord.WestScotland.Annuel.PC.", pc[j], ".csv"), row.names = F, sep = ",")
  
  coord[,1] <- as.numeric(coord[,1])
  coord[,2] <- as.numeric(coord[,2])
    
  p <- ggplot(coord, aes(Date, coord[,2])) +
    geom_point() +
    geom_line(group = 1, color = "red") +
    #geom_vline(xintercept = c(seq(1993,2018,1)), linetype = "dashed") +
    ylab(paste("Coordonées PC", pc[j],".annuel.WestScotland.EnvHab.reechantST.WestScotland.RF", sep = ".")) +
    scale_x_continuous(breaks=seq(1950,2021,5), limits = c(1950,2021), labels = as.character(seq(1950,2021,5))) +
    theme(axis.text.x = element_text(angle = 0, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14))
  
  png(paste0("ACP_Hindcasting_WestScotland_Annuel_EnvHab_PAgeo_PC", pc[j], ".png"), width = 1000, height = 400)
  print(p)
  dev.off()
    
  ####Extraction des corr?lations de chaque pixel ? PC1
  cor.pixel <- data.frame(acp$var$cor[,pc[j]])
  cor.pixel <- cbind(proba.na[,1:2], cor.pixel)
  colnames(cor.pixel)[3] <- paste0("Correlation PC.", pc[j])
  ##Ajout des coordonn?es avec NA (Iles Brit) exclues de l'ACP
  proba <- as.data.table(proba)
  cor.pixel <- as.data.table(cor.pixel)
  setkey(proba, x, y)
  setkey(cor.pixel, x, y)
  cor.na <- proba[!cor.pixel]
  cor.na <- cbind(cor.na[,1:2], rep(NA,length(nrow(cor.na))))
  colnames(cor.na)[3] <- paste0("Correlation PC.", pc[j])
  cor.total <- rbind(cor.pixel, cor.na)
  #write.table(cor.total, paste("Correlation ? PC", pc[j], "csv", sep = "."), row.names = F, sep = ",")
    
  raster.correlation <- rasterFromXYZ(cor.total, res=c(0.25, 0.25))
  writeRaster(raster.correlation, paste("Correlation.WestScotland.Annuel.PAgeo.PC", pc[j], "grd", sep = "."), overwrite = T)
    
  points<-as.data.frame(raster.correlation,xy=T)
  colnames(points)<-c("x","y","layer")
  p <- ggplot(points, aes(x,y)) + 
    geom_raster(aes(fill = layer)) +
    scale_fill_gradientn(colours = my.colors(100), na.value = "#f3f6f4",limits = c(-1,1)) +
    ggtitle(paste0("Correlation.WestScotland.Annuel.EnvHab.PC",pc[j])) +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.title = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          panel.grid = element_blank(),
          panel.background = element_blank())
  plot_list[[j]] = p
}  

for (j in 1:4) {
    png(paste0("Correlation.WestScotland.Annuel.PC", pc[j], ".png"),width=1000,height=500)
    print(plot_list[[j]])
    dev.off()
}

