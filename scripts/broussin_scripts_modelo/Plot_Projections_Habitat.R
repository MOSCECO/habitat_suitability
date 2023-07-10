library(raster)
library(ggplot2)
library(gridExtra)
library(ggpubr)

##Création de l'échelle chromatique
my.colors = colorRampPalette(c("#eeeeee","#1a74c6", "#46c0ae", 
                               "#d9ead3", "#ffc966", "#ffa500", "#f40000"))

modele = c("ANN", "GAM", "GBM", "GLM") #c("GAM", "ANN", "RF", "GLM", "CTA", "FDA")

folder <- c("R_5000","R_10000", "R_30000", "R_50000",
            "CH_5000", "CH_10000", "CH_30000", "CH_50000")

for (h in 1:length(modele)){
  
  for (i in 1:length(folder)){
    setwd(paste0("d:/thèse/modeling/gadus_morhua/habitat/", modele[h],"/", modele[h],"_",folder[i]))
    
    raster <- stack(paste0("Projection_Habitat.grd"))
    raster <- mean(raster)
    points <- as.data.frame(raster, xy = T, na.rm = F)
    A <- (points$layer) / 1000
    points[,3] <- A
    pdf(paste0("Projections_",modele[h],"_",folder[i],".pdf"), width = 20, height = 10)
    p <- ggplot(points, aes(x,y)) + 
      geom_raster(aes(fill = layer)) +
      scale_fill_gradientn(colours = my.colors(100), na.value = "#f3f6f4") +
      ggtitle(paste0(modele[h],"_",folder[i]," (Habitat)")) +
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            legend.title = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            panel.grid = element_blank(),
            panel.background = element_blank())
    print(p)
    dev.off()
    
  }
}
