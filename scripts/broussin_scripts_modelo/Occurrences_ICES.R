library(icesDatras)
library(icesVocab)

setwd("d:/thèse/data/occurrences/gadus_morhua")

survey <- getSurveyList()
aphia.cod <- findAphia("cod")

vars <- c("ShootLat","ShootLong","Year","Month","Day","Survey")
ices.cod <- data.frame(X=numeric(),Y=numeric(),Year=numeric(),
                   Month=numeric(),Day=numeric(),Dataset=character())

for (i in 16:length(survey)){
  
  cod.step <- getCatchWgt("IS-IDPS",years=1950:2020,quarters=1:4,aphia=aphia.cod)
  cod.step <- cod.step[vars]
  ices.cod <- rbind(ices.cod,cod.step)
  
}

beep()
  
write.csv(ices.cod,"Gadus_morhua_ICES.csv")

