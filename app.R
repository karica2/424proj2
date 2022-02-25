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
# ridership[, 'location'] = NA
# x <- 0
# for(stop in 1:nrow(latlong) ){ 
#   loc <- latlong[stop, "Location"]
#   print(loc)
#   
#   stopnum <- latlong[stop, "MAP_ID"]
#   ridership[ridership$station_id == stopnum, ]$location <- loc
# }

write.table(x = ridership, file = "ridership_loc.tsv", sep = "\t")
# set all entries in ridership that share a station name to have the same lat/long 

