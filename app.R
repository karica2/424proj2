# proj2 app
# group is Kevin Eliott and Kenan Arica
# team name: Swag

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(scales)
library(leaflet)

setwd("~/class/424/424proj2")

# read in our ridership file
ridership <- read.table(file = "ridership.tsv", sep = "\t", header = TRUE, quote = "")

# fix the date, add useful information
ridership$date_ymd <- ymd(paste(year(mdy(ridership$date)), month(mdy(ridership$date)), day(mdy(ridership$date)), sep = "-"))
ridership$month <- month(ridership$date_ymd)
ridership$month_char <- month.abb[month(ridership$date_ymd)]
ridership$year <- year(ridership$date_ymd)
ridership$day <- day(ridership$date_ymd)
ridership$day_of_week <- weekdays(ridership$date_ymd)
# ridership$date_correct <- ymd(paste(ridership$Year, ridership$Month, "01", sep="-"))

# add lat / long
latlong <- read.table(file = "latlong.tsv", sep = "\t", header = TRUE, quote = "" , fill=TRUE, row.names = NULL)

# match values of ridership dataframe to lat/long entries of system file

# loop through each entry of latlong 
# we only actually have to do this once. it takes a while, and the updated ridership can be found in ridership_loc.csv
ridership[, 'location'] = NA
ridership[, 'BLUE'] = NA
ridership[, 'G'] = NA
ridership[, 'BRN'] = NA
ridership[, 'P'] = NA
ridership[, 'Pexp'] = NA
ridership[, 'Y'] = NA# x <- 0
ridership[, 'Pnk'] = NA
ridership[, 'O'] = NA

for(stop in 1:nrow(latlong) ){
  loc <- latlong[stop, "Location"]
  # print(stop)

  stopnum <- latlong[stop, "MAP_ID"]
  #print(latlong[stop, "RED"])
  ridership[ridership$station_id == stopnum, ]$location <- loc
  # append the cols for what line it is
  ridership[ridership$station_id == stopnum, ]$RED <- latlong[stop, "RED"]
  ridership[ridership$station_id == stopnum, ]$BLUE <-   loc <- latlong[stop, "BLUE"]
  ridership[ridership$station_id == stopnum, ]$G <-   loc <- latlong[stop, "G"]
  ridership[ridership$station_id == stopnum, ]$BRN <-   loc <- latlong[stop, "BRN"]
  ridership[ridership$station_id == stopnum, ]$P <-   loc <- latlong[stop, "P"]
  ridership[ridership$station_id == stopnum, ]$Pexp <-   loc <- latlong[stop, "Pexp"]
  ridership[ridership$station_id == stopnum, ]$Y <-   loc <- latlong[stop, "Y"]
  ridership[ridership$station_id == stopnum, ]$Pnk <-   loc <- latlong[stop, "Pnk"]
  ridership[ridership$station_id == stopnum, ]$O <-   loc <- latlong[stop, "O"]

  }

write.table(x = ridership, file = "ridership_loc.tsv", sep = "\t")
# set all entries in ridership that share a station name to have the same lat/long 
uicBlue <- subset(ridership, BLUE == "true")
uicRed <- subset(ridership, RED == "true")
uicGreen <- subset(ridership, G == "true")
uicBrown <- subset(ridership, BRN == "true")
uicPurple <- subset(ridership, P == "true")
uicPink <- subset(ridership, Pnk == "true")
uicYellow <- subset(ridership, O == "true")

write.table(x = uicBlue, file = "uic_Blue.tsv", sep = "\t")
write.table(x = uicRed, file = "uic_Red.tsv", sep = "\t")
write.table(x = uicGreen, file = "uic_Green.tsv", sep = "\t")
write.table(x = uicBrown, file = "uic_Brown.tsv", sep = "\t")
write.table(x = uicPurple, file = "uic_Purple.tsv", sep = "\t")
write.table(x = uicPink, file = "uic_Pink.tsv", sep = "\t")
write.table(x = uicYellow, file = "uic_Yellow.tsv", sep = "\t")
# write.table(x = ridership, file = "ridership_loc.tsv", sep = "\t")


nrow(uicBlue) + nrow(uicRed) + nrow(uicGreen) + nrow(uicBrown) + nrow(uicPurple) + nrow(uicPink) + nrow(uicYellow)
nrow(ridership)


# maybe for each color line, pull those stops and then add location data?
blueStops <- subset(latlong, BLUE == "true")
blueLine <- subset(ridership, station_id %in% blueStops$MAP_ID)
write.csv(x = blueLine, file = "b-line.csv")

redStops <- subset(latlong, RED == "true")
redLine <- subset(ridership, station_id %in% redStops$MAP_ID)
write.csv(x = redLine, file = "r-line.csv")
