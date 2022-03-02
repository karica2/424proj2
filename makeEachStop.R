
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(scales)
library(leaflet)


setwd("~/class/424/424proj2")


ridership <- read.table(file = "ridership.tsv", sep = "\t", header = TRUE, quote = "")

# fix the date, add useful information
ridership$date_ymd <- ymd(paste(year(mdy(ridership$date)), month(mdy(ridership$date)), day(mdy(ridership$date)), sep = "-"))
ridership$month <- month(ridership$date_ymd)
ridership$month_char <- month.abb[month(ridership$date_ymd)]
ridership$year <- year(ridership$date_ymd)
ridership$day <- day(ridership$date_ymd)
ridership$day_of_week <- weekdays(ridership$date_ymd)

# load in our lat / long files
latlong <- read.table(file = "latlong.tsv", sep = "\t", header = TRUE, quote = "" , fill=TRUE, row.names = NULL)



print(unique(latlong$MAP_ID))
for(stop in unique(latlong$MAP_ID)) {
  # latlong has 2 of every stop for it's entries, but the location is the same so we just need that. pick the first one
  
  loc <- latlong[latlong$MAP_ID == stop, "Location"][1]
  lineColor <- getLineColor(latlong[latlong$MAP_ID == stop, ])
  currentStop <- data.frame(ridership[ridership$station_id == stop, ], loc, lineColor)
  
  tableName = paste("datafiles/", stop, ".csv", sep = "")
  write.csv(currentStop, tableName)
}


# for KEVIN: this is how you read in the data properly. it wards off the quotation marks and modified col names.
#z <- as.data.frame(read.csv(tableName, fileEncoding = "UTF-8-BOM"))


getLineColor <- function(r) { 
  # discard the first row
  r <- head(r, 1)
  color <- ""
  # inefficient function but idk how else to do this
  if(r$RED == "true") {
    color <- "red"
  }
  if(r$BLUE == "true") {
    color <- "blue"
  }
  if(r$G == "true") {
    color <- "green"
  }
  if(r$BRN == "true") {
    color <- "brown"
  }
  if(r$P == "true") {
    color <- "purple"
  }
  if(r$Pexp == "true") {
    color <- "purple_exp"
  }
  if(r$Pnk == "true") {
    color <- "pink"
  }
  if(r$O == "true") {
    color <- "orange"
  }
  return(color)
  
}


