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
# library(tidyverse)

setwd("~/class/424/424proj2")
latlong <- read.table(file = "latlong.tsv", sep = "\t", header = TRUE, quote = "" , fill=TRUE, row.names = NULL)

# for some reason these lines of code only work when the current dir is datafiles. 
setwd("datafiles")

# NOTE: I DID NOT COME UP WITH THE FOLLOWING TWO LINES OF CODE. I USED A SOLUTION BY leerssej
# TAKEN FROM: https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
# ALL CREDIT GOES TO THE AUTHOR. 
# I checked with the professor if I could use this solution in a piazza post and got the green light. 
files = list.files(".", ".csv", full.names = TRUE)
# READ: for testing purposes, you only need to run this code once, as it takes like 13-15 seconds. once you have the data in your env, you can comment it out. 
# summed <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
names(latlong)[names(latlong)=="MAP_ID"] <- "station_id"

latlong_unique <-  latlong[!duplicated(latlong$station_id), ]
latlong_unique <- latlong_unique[order(latlong_unique$STATION_NAME), ]

lineOptions <- c("All", "Blue", "Red", "Green", "Brown", "Purple", "Purple Express", "Pink", "Orange", "Yellow")

getLineValues <- function(lineColor) { 

  lineValues <- switch(
    lineColor,
    "All" = latlong_unique$STATION_NAME,
    "Blue" = latlong_unique[latlong_unique$BLUE == "true", ]$STATION_NAME,
    "Red" = latlong_unique[latlong_unique$RED == "true", ]$STATION_NAME,
    "Green" = latlong_unique[latlong_unique$G == "true", ]$STATION_NAME,
    "Brown" = latlong_unique[latlong_unique$BRN == "true", ]$STATION_NAME,
    "Purple" = latlong_unique[latlong_unique$P == "true", ]$STATION_NAME,
    "Purple Express" = latlong_unique[latlong_unique$Pexp == "true", ]$STATION_NAME,
    "Pink" = latlong_unique[latlong_unique$Pnk == "true", ]$STATION_NAME,
    "Orange" = latlong_unique[latlong_unique$O == "true", ]$STATION_NAME,
    "Yellow" = latlong_unique[latlong_unique$Y == "true", ]$STATION_NAME
  )
  return(lineValues)
  }


  
  
# dataframes that hold all rows pertaining to their own lines. we probably wont need them, but not bad to have. 
blueLine <- summed[summed$station_id %in% latlong[latlong$BLUE == "true", ]$station_id, ]
redLine <- summed[summed$station_id %in% latlong[latlong$RED == "true", ]$station_id, ]
greenLine <- summed[summed$station_id %in% latlong[latlong$G == "true", ]$station_id, ]
purpleLine <- summed[summed$station_id %in% latlong[latlong$P == "true", ]$station_id, ]
purpleExpLine <- summed[summed$station_id %in% latlong[latlong$Pexp == "true", ]$station_id, ]
brownLine <- summed[summed$station_id %in% latlong[latlong$Brn == "true", ]$station_id, ]
orangeLine <- summed[summed$station_id %in% latlong[latlong$O == "true", ]$station_id, ]
yellowLine <- summed[summed$station_id %in% latlong[latlong$Y == "true", ]$station_id, ]
pinkLine <- summed[summed$station_id %in% latlong[latlong$Pnk == "true", ]$station_id, ]

allStops <- data.frame(latlong_unique$station_id, latlong_unique$STATION_NAME)

ui <- fluidPage(
  "Hello, world!",
  fluidRow(
    
    # interaction quirk: changing the date selector, and subsequently using the 'next day' or 'previous day' button should change the date selected on the date selector.  
    
    column(width = 3, 
           fluidRow(dateInput
                    (inputId = "currentDate", label = "current date", value = "2021-08-23", min = "2000-01-01", max="2021-12-31")
                    ),
           fluidRow(column(width = 6, actionButton(inputId = "prevDay", "◄ Previous Day ")), column(width = 6, actionButton(inputId = "nextDay", "Next Day ►"))
                    ),
            fluidRow(selectInput(inputId = "currentLine", choices = lineOptions, label = "Select Line")),
            fluidRow(selectizeInput(inputId = "currentStop", choices = latlong_unique$STATION_NAME, label = "Select Stop")),
            fluidRow(textOutput("selectedStopTitle"))
           ),
    
    column(width = 9, plotOutput("allStops"))
  )
)
server <- function(input, output, session) {
  

  
  observeEvent(input$nextDay, {
    updateDateInput(session = session, inputId = "currentDate", value = as.Date(input$currentDate) + 1)
  })
  observeEvent(input$prevDay, {
    updateDateInput(session = session, inputId = "currentDate", value = as.Date(input$currentDate) - 1)
  })
  
  observeEvent(input$currentLine, {
    updateSelectizeInput(session = session, inputId = "currentStop", choices = getLineValues(input$currentLine), selected = getLineValues(input$currentLine)[1])
  })
  
  allStopsReactive <- reactive({ subset(summed, date_ymd == input$currentDate) })
  selectedStopReactive <- reactive({ subset(latlong_unique, STATION_NAME == input$currentStop ) })
  selectedStopDataReactive <- reactive({ subset(summed, STATION_NAME == input$currentStop )})
  
  output$allStops <- renderPlot({
    
    allData <- allStopsReactive()
    ggplot(data=allData, aes(x=stationname, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + scale_y_continuous("Rides", labels = scales::comma) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
    })
  
  output$selectedStopTitle <- renderText(
    {
      current_stop <- selectedStopReactive()
      print(current_stop)
      paste("Ridership for ", input$currentStop)
    }
  )
  
}
shinyApp(ui, server)

# TODO: 
# Adapt Proj1 code to just be 4 output functions that each yield a table. 


