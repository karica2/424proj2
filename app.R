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
latlong <- read.table(file = "latlong.tsv", sep = "\t", header = TRUE, quote = "" , fill=TRUE, row.names = NULL)

# for some reason these lines of code only work when the current dir is datafiles. 
setwd("datafiles")

# NOTE: I DID NOT COME UP WITH THE FOLLOWING TWO LINES OF CODE. I USED A SOLUTION BY leerssej
# TAKEN FROM: https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
# ALL CREDIT GOES TO THE AUTHOR. 
# I checked with the professor if I could use this solution in a piazza post and got the green light. 
files = list.files(".", ".csv", full.names = TRUE)
# summed <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

names(latlong)[names(latlong)=="MAP_ID"] <- "station_id"

# make our lines
blueLine <- summed[summed$station_id %in% latlong[latlong$BLUE == "true", ]$station_id, ]
redLine <- summed[summed$station_id %in% latlong[latlong$RED == "true", ]$station_id, ]
greenLine <- summed[summed$station_id %in% latlong[latlong$G == "true", ]$station_id, ]
purpleLine <- summed[summed$station_id %in% latlong[latlong$P == "true", ]$station_id, ]
purpleExpLine <- summed[summed$station_id %in% latlong[latlong$Pexp == "true", ]$station_id, ]
brownLine <- summed[summed$station_id %in% latlong[latlong$Brn == "true", ]$station_id, ]
orangeLine <- summed[summed$station_id %in% latlong[latlong$O == "true", ]$station_id, ]
yellowLine <- summed[summed$station_id %in% latlong[latlong$Y == "true", ]$station_id, ]
pinkLine <- summed[summed$station_id %in% latlong[latlong$Pnk == "true", ]$station_id, ]



ui <- fluidPage(
  "Hello, world!",
  fluidRow(
    
    # interaction quirk: changing the date selector, and subsequently using the 'next day' or 'previous day' button should change the date selected on the date selector.  
    
    column(width = 3, dateInput(inputId = "currentDate", label = "current date", value = "2021-08-23", min = "2000-01-01", max="2021-12-31")), 
    column(width = 9, plotOutput("allStops"))
  )
)
server <- function(input, output, session) {
  
  # need to find a way to sort this data
  allStopsReactive <- reactive({ summed[summed$date_ymd == input$currentDate, ] })
  
  output$allStops <- renderPlot({
    
    allData <- allStopsReactive()
    ggplot(data=allData, aes(x=stationname, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + scale_y_continuous("Rides", labels = scales::comma) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  })
  
}
shinyApp(ui, server)
