library(rgbif)
library(dplyr)

############## Download occurrences from gbif ################

##Count the number of occurrences of the species available on gbif website (the taxon key must be retrieve on gbif.com)
Occ_Nb <- occ_count(taxonKey = 2222168)

##Define witch species you are working on (exact scientific name)
SP_Name <- "Crangon allmani"

##Create an empty matrix with the same number of rows than the number of occurrences
##Number of colones to be define, how many variables do you want to keep?
Save_Data <- data.frame(matrix(ncol = 6, nrow = Occ_Nb))
##Rename the colones 
colnames(Save_Data)<- c("Name","X", "Y", "Year", "Month", "Day")
##Define the first row number of the matrix to be fulfill with the downloading info. Obviously we start with the first row
start <- 1


##Define the boundaries years of the time period you want to download,  a : finish, b : start
a <- 2021
b <- 1900
c <- a-b


for (i in 1:c) {

##Download occurrences of year a   
  Save <- occ_data(taxonKey = 2222168,
             hasCoordinate = TRUE,
             hasGeospatialIssue = FALSE,
             #year = (a),
             limit = 100000)

##Select the variables you want to keep (the same than the ones you define in Save_Data)  
  Save <- setNames(Save$data, tolower(names(Save$data)))
  vars <- c("species", "decimallongitude","decimallatitude", "year", "month", "day")
  Save <- Save[vars]

##Download the occurrences to your empty matrix, starting from row 1 (start), then row 2 (start + 1) etc  
  for(n in 1:nrow(Save)){
    Save_Data[start,] = Save[n,]
    start <- start + 1
  }

##Redefine start as the number of row already fulfill + 1
##ex : if you downloaded 10 occurrences for year 2021, you want to put occurrences from year 2020 in row 11, 12...
  start <- (nrow(subset(Save_Data, Name == SP_Name)) + 1)
##Redefine the year  
  a <- a - 1
  
}

############## Clean the matrix ################

##Transform it into a data frame without keeping the empty rows

##Remove duplicates
Save_Data <- unique(Save_Data)


[!duplicated(dat[,c('id','id2')]),]
