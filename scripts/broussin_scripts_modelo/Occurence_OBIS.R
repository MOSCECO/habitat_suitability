#####Occurence datasets OBIS

#Download occurence dataset - GBIF

library(robis)

Occ_Obis <- occurrence(taxonid = 126501,
                       startdate = as.Date("1753-01-01"),
                       enddate = as.Date("2022-02-01"))

Occ_Obis<-setNames(Occ_Obis, tolower(names(Occ_Obis)))


#Delete occurences without coordinates

library(dplyr)
Occ_Obis2 <- filter(Occ_Obis, !is.na(decimallongitude) | !is.na(decimallatitude))

#Coordinates with a precision of at least 100 km

Occ_Obis2 <- Occ_Obis %>% 
  filter(as.numeric(coordinatePrecision)<= 100 | is.na(coordinatePrecision))

#Keep the colum we are interesed in

vars <- c("scientificname", "decimallongitude","decimallatitude",
          "date_year","month", "day") #,
          #"basisOfRecord", "individualCount", "coordinatePrecision") #,"year","month")
Occ_Obis2<- Occ_Obis[vars]

colnames(Occ_Obis2)<- c("Name","X", "Y","Year","Month",
                       "Day") #, "Basis of record", "Coordinate uncertainty") #, "Year", "Month")

#Remove duplicate

Occ_Obis2 <- unique(Occ_Obis2)

#Add a colum with the name of the library (OBIS)

Occ_Obis2$Dataset <- rep.int("OBIS", length(nrow(Occ_Obis2)))

write.table(Occ_Obis2, file ="OBIS.csv",row.names=FALSE, sep=",")
                    