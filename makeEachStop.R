# Written by Kenan Arica for CS 424 Proj2, SP22
# NOTE: THIS IS A UTILITY FILE. IT SHOULD NOT BE RAN WHOLE. IT SIMPLY HOLDS CODE TO EXECUTE IN CHUNKS WHEN YOU NEED IT. 

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(scales)
library(leaflet)

# implement a system similar to read / write permissions on linux

getLineColor <- function(r) { 
  # discard the first row
  r <- head(r, 1)
  print(r)
  color <- "000000000"
  colors <- data.frame(matrix(ncol=9, nrow=1))
  colnames(colors) <- c("red", "blue", "green", "brown", "purple", "purple_exp", "pink", "orange", "yellow")
  # inefficient function but idk how else to do this
  
  if(r$RED == "true") {
    substr(color, 1, 1) <- "1"
  }
  if(r$BLUE == "true") {
    substr(color, 2, 2) <- "1"
  }
  if(r$G == "true") {
    substr(color, 3, 3) <- "1"
  }
  if(r$BRN == "true") {
    substr(color, 4, 4) <- "1"
  }
  if(r$P == "true") {
    colors$purple <- 1
    substr(color, 5, 5) <- "1"
    
  }
  if(r$Pexp == "true") {
    colors$purple_exp <- 1
    substr(color, 6, 6) <- "1"
    
  }
  if(r$Pnk == "true") {
    colors$pink <- 1
    substr(color, 7, 7) <- "1"
    
  }
  if(r$O == "true") {
    colors$orange <- 1
    substr(color, 8, 8) <- "1"
    
  }
  if(r$Y == "true") {
    colors$yellow <- 1
    substr(color, 9, 9) <- "1"
    
  }
  print(color)
  return(color)
}

setwd("~/class/424/424proj2")


ridership <- read.table(file = "ridership.tsv", sep = "\t", header = TRUE, quote = "")

# fix the date, add useful information
# ridership$date_ymd <- ymd(paste(year(mdy(ridership$date)), month(mdy(ridership$date)), day(mdy(ridership$date)), sep = "-"))
# ridership$month <- month(ridership$date_ymd)
# ridership$month_char <- month.abb[month(ridership$date_ymd)]
# ridership$year <- year(ridership$date_ymd)
# ridership$day <- day(ridership$date_ymd)
# ridership$day_of_week <- weekdays(ridership$date_ymd)

# load in our lat / long files
latlong <- read.table(file = "latlong.tsv", sep = "\t", header = TRUE, quote = "" , fill=TRUE, row.names = NULL)

ridership$date <- NA
ridership$daytype <- NA

 #code to write stop files from ridership + latlong dataframes
for(stop in unique(latlong$MAP_ID)) {

  # latlong has 2 of every stop for it's entries, but the location is the same so we just need that. pick the first one
  loc <- latlong[latlong$MAP_ID == stop, "Location"][1]
  lineColor <- getLineColor(latlong[latlong$MAP_ID == stop, ])
  
  # subset our current stop
  currentStop <- data.frame(ridership[ridership$station_id == stop, ], loc, lineColor)
  currentStop$X <- NA

  tableName = paste("datafiles/", stop, ".csv", sep = "")
  write.csv(currentStop, tableName)

}


# code to load all files into one big dataframe
# for some reason, this code requires the current dir to be in datafiles to work in under 20 seconds
setwd("datafiles")
files = list.files(".", ".csv", full.names = TRUE)
summed <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

names(latlong)[names(latlong)=="MAP_ID"] <- "station_id"

blueLine <- summed[summed$station_id %in% latlong[latlong$BLUE == "true", ]$station_id, ]
redLine <- summed[summed$station_id %in% latlong[latlong$RED == "true", ]$station_id, ]
greenLine <- summed[summed$station_id %in% latlong[latlong$G == "true", ]$station_id, ]
purpleLine <- summed[summed$station_id %in% latlong[latlong$P == "true", ]$station_id, ]
purpleExpLine <- summed[summed$station_id %in% latlong[latlong$Pexp == "true", ]$station_id, ]
brownLine <- summed[summed$station_id %in% latlong[latlong$Brn == "true", ]$station_id, ]
orangeLine <- summed[summed$station_id %in% latlong[latlong$O == "true", ]$station_id, ]
yellowLine <- summed[summed$station_id %in% latlong[latlong$Y == "true", ]$station_id, ]
pinkLine <- summed[summed$station_id %in% latlong[latlong$Pnk == "true", ]$station_id, ]

# for KEVIN: this is how you read in the data properly. it wards off the quotation marks and modified col names.
#z <- as.data.frame(read.csv(tableName, fileEncoding = "UTF-8-BOM"))

# kevin copy this code

ride_metadata = data.frame(matrix(ncol=3, nrow=0))
colnames(ride_metadata) <- c("stopname", "mapID", "loc")
for(filename in list.files("datafiles")) { 
  print(filename)
  filename <- paste("datafiles/", filename, sep="")
  currentStop <- as.data.frame(read.csv(filename, fileEncoding = "UTF-8-BOM"))
  firstRow = head(currentStop, 1)
  cur_mapid <- firstRow$station_id
  cur_name <- firstRow$stationname
  cur_loc <- firstRow$loc
  
  ride_metadata <- rbind(ride_metadata, c(cur_name, cur_mapid, cur_loc))
  
}



