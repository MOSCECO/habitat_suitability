################# Retrieve each env. data for each occurrences #####
library(dplyr)
library(raster)
library(stringr)
library(tidyr)
library(beepr)

#### Create occurrences data ####
#setwd("d:/th?se/data/occurrences/gadus_morhua")
#occ <- read.csv("Gadus_morhua.csv")
#data <- occ[,2:5]
#matrice.fin <- data%>%filter(Year>1850)

setwd("e:/thèse/data/matrices/gadus_morhua/environement_CMIP")
pa <- read.csv("PA_R_G_morhua_5000.csv")
data <- pa[,1:4]
matrice.fin <- data%>%filter(Year>=1850)
#data<-rbind(occ,pa)

#### Import occurrences data ####
setwd("e:/thèse/data/occurrences/gadus_morhua")
data <- read.csv("Gadus_morhua.csv")
data <- data[,2:5]
data <- data%>%filter(Year>=1850)
matrice.fin <- data

# Delete rows without longitude or latitude or month or year
data <- na.omit(data)

#Create a colon with Year + Month*0.01 (1965 / 01 -> 1965.01)
data$YM_data <- (as.numeric(data$Year) + as.numeric(data$Month) * 0.01)
#Create a vector with croissant Year + Month and Remove duplicate
vec_YM_data <- as.data.frame(unique(sort(data$YM_data)))
colnames(vec_YM_data) <- c("YM_data")


#### Extract the env. values for each variables for each occurrences ####

#Names of your raster (one per env. var.)
env <- c("O2", "pH", "SST", "Salinity", "MLD", "Phyto","Zoo", "SBT")#c("O2", "pH", "SST", "Salinity", "MLD")

for (i in 1:length(env)) {
  
  setwd("f:/contemporain (1850-2025)")
  
  #Import the raster stack of the first env. var.
  raster <- stack(paste0(env[[i]],"_contemporain-mean_inter.gri"))

  #Rename each raster "YYYY.MM"
  YM_raster <- str_sub(as.character(labels(raster)),start = 2,end = 8)
  names(raster) = YM_raster
  
  # Cr?ation d'une matrice de deux lignes faisant correspondre les dates d'occurence avec les diff?rentes couches.
  matID <- as.data.frame(cbind(1:length(YM_raster), as.numeric(YM_raster)))
  colnames(matID) <- c("Number", "YM_raster")
  
  #Keep only date of occurrences posterior to the first layer of the env. var. and anterior to the last layer of the env. var.
  YM_data2 <- vec_YM_data %>% filter(YM_data >= matID[1,2] & YM_data <= matID[length(matID$Number),2])
  long <- c()
  lat <- c()
  date <- c()
  var <- c()

  for (j in YM_data2$YM_data){
    layer_j <- subset(raster, matID$Number[matID$YM_raster == j])
    occ <- data[data$YM_data == j,]
    var <- c(var, raster::extract(layer_j, occ[,1:2]))
    date <- c(date, occ$YM_data)
    lat <- c(lat, occ$Y)
    long <- c(long, occ$X)
    
  }
  
  
  matrice <- data.frame(cbind(long, lat, date, var))
  matrice <- matrice %>% separate(date, c("Year", "Month"))
  matrice$Month <- replace(matrice$Month, matrice$Month==1, 10)
  colnames(matrice) <- c("X", "Y", "Year", "Month", env[[i]])
  matrice[,3] <- as.integer(matrice[,3])
  matrice[,4] <- as.integer(matrice[,4])
  matrice.fin <- left_join(matrice,matrice.fin,by=c("X","Y","Year","Month"))
  
  #write.table(matrice, paste0("Mat_PA_CH_5000_", env[[i]], ".csv"), row.names = FALSE, sep = ",")

}

pa.fin <- left_join(pa,matrice.fin,by=c("X","Y","Year","Month"))
pa.fin <- pa.fin[,c(1:9,11,10)]
pa.fin <- na.omit(pa.fin)

setwd("e:/thèse/data/matrices/gadus_morhua/environement_cmip")
write.csv(pa.fin,paste0("Mat_PA_R_5000.csv"),row.names=F)

beep()

