# proj2 app
# group is Kevin Elliott and Kenan Arica
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

latlong <- read.table(file = "latlong.tsv", sep = "\t", header = TRUE, quote = "" , fill=TRUE, row.names = NULL)

# for some reason these lines of code only work when the current dir is datafiles. 
# setwd("datafiles")

# NOTE: I DID NOT COME UP WITH THE FOLLOWING TWO LINES OF CODE. I USED A SOLUTION BY leerssej
# TAKEN FROM: https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
# ALL CREDIT GOES TO THE AUTHOR. 
# I checked with the professor if I could use this solution in a piazza post and got the green light. 
files = list.files("datafiles/", ".csv", full.names = TRUE)
# READ: for testing purposes, you only need to run this code once, as it takes like 13-15 seconds. once you have the data in your env, you can comment it out. 
summed <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
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

#### kevin section

# unused for now
# mapIcons <- iconList(
#   red = makeIcon("redMarker.png"),
#   blue = makeIcon("blueMarker.png"),
#   brown = makeIcon("brownMarker.png"),
#   green = makeIcon("greenMarker.png"),
#   orange = makeIcon("orangeMarker.png"),
#   purple = makeIcon("purpleMarker.png"),
#   purple_exp = makeIcon("purpleMarker.png"),
#   pink = makeIcon("pinkMarker.png"),
#   yellow = makeIcon("yellowMarker.png"),
#   grey = makeIcon("greyMarker.png")
# )

weekdayNums <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

monthNums <- seq(1, 12)
months <- month.abb[monthNums]

# map data is used to create the vectors that are given to the leaflet for markers
mapData <- data.frame(summed$loc, summed$stationname,summed$station_id)
mapData <- unique.data.frame(mapData)
colnames(mapData) <- c("loc","stationname","station_id")
#View(mapData)

mainThemeColor = "blue"

latVector <- vector(mode="numeric", length=0)
longVector <- vector(mode="numeric", length=0)
nameVector <- vector(mode = "character", length=0)
idVector <- vector(mode = "character", length=0)
colorVector <- vector(mode = "character", length=0)


latString = ""
longString = ""
tempString = ""
nameString = ""
colorString = ""
idString = ""
index = 0

# for loop just grabs each value and adds it to the corresponding vector
# the lat long strings are hard sub stringed to those chars so if one is in a
# different format it probably will be discarded or plotted way off
for(row in 1:nrow(mapData)) {

  tempString <- mapData[row,"loc"]
  tempString <- substring(tempString,2,nchar(tempString)-1)
  
  latString <- sub(",.*", "",tempString)
  #print(testLatString)
  
  longString <- sub(".*,", "",tempString)
  #print(testLongString)
  
  #latString <-substring(tempString,2,10)
  #longString <- substring(tempString,13,22)

  latVector[row]  <- as.numeric(latString)
  longVector[row] <- as.numeric(longString)

  tempNameString <- mapData[row,"stationname"]
  nameString <- tempNameString
  nameVector[row] <- nameString
  
  tempIdString <- mapData[row,"station_id"]
  idString <- tempIdString
  idVector[row] <- idString

  # tempColorString <- mapData[row,"color"]
  # colorString <- tempColorString
  # colorVector[row] <- colorString
  # #print(colorVector[row])

  index <- index + 1

}



##################
# TODO: find why ohare is not on map
# TODO: make gradient in map for higher vs lower values

ui <- dashboardPage(dashboardHeader(title = "CS424 Project 2"), dashboardSidebar(disable = TRUE, collapsed = FALSE), dashboardBody(
  
  # interaction quirk: changing the date selector, and subsequently using the 'next day' or 'previous day' button should change the date selected on the date selector.  
  
  column(width = 9, 
         # major row 1
         fluidRow(
           column(1, 
                  fluidRow(box(width = 12, height = "25vh", "Written by Kenan Arica and Kevin Elliott for CS 424 SP22. 
                    This dashboard is designed to allow users to CTA stop ridership by day, and by stop, with the leaflet giving geospatial capabilites.
                    Dataset provided by the CTA, and can be found at the Chicago Data Portal.")),
                  fluidRow(dateInput(inputId = "currentDate", label = "current date", value = "2021-08-23", min = "2000-01-01", max="2021-12-31")
                  ),
                  fluidRow(column(width = 6, actionButton(inputId = "prevDay", "◄ Previous Day ")), column(width = 6, actionButton(inputId = "nextDay", "Next Day ►"))
                  ),
                  fluidRow(radioButtons(inputId = "useAlphabetical", choices = c("Alphabetical", "Ascending"), label = "Order by:", selected = "Alphabetical"))
           ),
           column(11, box(title="All Stop Ridership", width=12, height = "40vh", background = mainThemeColor,
                          conditionalPanel(condition = "input.useAlphabetical == 'Alphabetical'", 
                                           plotOutput("allStopsAlphabetical", height = "30vh") ),
                          conditionalPanel(condition = "input.useAlphabetical == 'Ascending'", 
                                           plotOutput("allStopsNumeric", height = "30vh") ),
           ))
         ),
         # major row 2
         fluidRow(
           column(1,
                  fluidRow(selectInput(inputId = "currentLine", choices = lineOptions, label = "Select Line")),
                  fluidRow(selectizeInput(inputId = "currentStop", choices = latlong_unique$STATION_NAME, label = "Select Stop")),
                  fluidRow(textOutput("selectedStopTitle")),
           ),
           column(11, 
                  # for the month
                  column(3, box(width = 12, title = "By Month", background = mainThemeColor,
                                fluidRow(
                                  conditionalPanel(
                                    condition = "input.monthly_table_toggle == 'No'",
                                    column(width = 12, plotOutput("MonthlyPlot", height = "35vh"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.monthly_table_toggle == 'Yes'",
                                    column(width = 12, dataTableOutput("MonthlyTable", height="35vh"))
                                  ),
                                  
                                ),
                                fluidRow(box(width = 12, background = mainThemeColor, radioButtons(input="monthly_table_toggle", label="Show table", choices=c("Yes", "No"), selected = "No"))) # find the comma
                  )
                  ),
                  # for the year
                  column(3, box(width = 12, title = "By Year", background = mainThemeColor,
                                fluidRow(
                                  conditionalPanel(
                                    condition = "input.yearly_table_toggle == 'No'",
                                    column(width = 12, plotOutput("YearlyPlot", height = "35vh"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.yearly_table_toggle == 'Yes'",
                                    column(width = 12, dataTableOutput("YearlyTable", height="35vh"))
                                  ),
                                  
                                ),
                                fluidRow(box(width = 12, background = mainThemeColor, radioButtons(input="yearly_table_toggle", label="Show table", choices=c("Yes", "No"), selected = "No"))) # find the comma
                  )),
                  column(4, box(width = 12, title = "By Day", background = mainThemeColor,
                                fluidRow(
                                  conditionalPanel(
                                    condition = "input.daily_table_toggle == 'No'",
                                    column(width = 12, plotOutput("DailyPlot", height = "35vh"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.daily_table_toggle == 'Yes'",
                                    column(width = 12, dataTableOutput("DailyTable", height="35vh"))
                                  ),
                                  
                                ),
                                fluidRow(box(width = 12, background = mainThemeColor, radioButtons(input="daily_table_toggle", label="Show table", choices=c("Yes", "No"), selected = "No"))) # find the comma
                  )),
                  column(2, box(width = 12, title = "By Week", background = mainThemeColor,
                                fluidRow(
                                  conditionalPanel(
                                    condition = "input.weekly_table_toggle == 'No'",
                                    column(width = 12, plotOutput("WeekdayPlot", height = "35vh"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.weekly_table_toggle == 'Yes'",
                                    column(width = 12, dataTableOutput("WeeklyTable", height="35vh"))
                                  ),
                                  
                                ),
                                fluidRow(box(width = 12, background = mainThemeColor, radioButtons(input="weekly_table_toggle", label="Show table", choices=c("Yes", "No"), selected = "No"))) # find the comma
                                
                  ))
                  # column(3, box(width = 12, title = "By Year", background = mainThemeColor, plotOutput("YearlyPlot", height="35vh"))), 
                  #column(3, box(width = 12, title = "By Day", background = mainThemeColor, plotOutput("DailyPlot", height="35vh"))), 
                  #column(3, box(width = 12, title = "By Weekday", background = mainThemeColor, plotOutput("WeekdayPlot", height="35vh"))), 
           )
           
         )
  ),
  column(width = 3, box(title = "Map of Stops", width = 12, background = mainThemeColor, leafletOutput("map", height="75vh")))
)
)
server <- function(input, output, session) {
  
  map = createLeafletMap(session, 'map')
  
  observeEvent(input$nextDay, {
    updateDateInput(session = session, inputId = "currentDate", value = as.Date(input$currentDate) + 1)
  })
  observeEvent(input$prevDay, {
    updateDateInput(session = session, inputId = "currentDate", value = as.Date(input$currentDate) - 1)
  })
  
  observeEvent(input$currentLine, {
    updateSelectizeInput(session = session, inputId = "currentStop", choices = getLineValues(input$currentLine), selected = getLineValues(input$currentLine)[1])
  })
  
  allStopsReactive <- reactive({ subset(summed, summed$date_ymd == input$currentDate) })
  selectedStopReactive <- reactive({ subset(latlong_unique, latlong_unique$STATION_NAME == input$currentStop) })
  selectedStopDataReactive <- reactive({ subset(summed, summed$station_id == head(latlong_unique[latlong_unique$STATION_NAME == input$currentStop, ], 1)$station_id & summed$year == year(input$currentDate))})
  selectedStopDataYearly <- reactive({ subset(summed, summed$station_id == head(latlong_unique[latlong_unique$STATION_NAME == input$currentStop, ], 1)$station_id) })
  
  output$allStopsNumeric <- renderPlot({
    
    allData <- allStopsReactive()
    allDataSorted <- data.frame(allData$stationname, allData$rides)
    colnames(allDataSorted) <- c("stationname", "rides")
    allDataSorted <- allDataSorted[order(allDataSorted$rides), ]
    allDataSorted$stationname <- factor(allDataSorted$stationname, levels = allDataSorted$stationname) 
    
    ggplot(data=allDataSorted, aes(x=stationname, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + scale_y_continuous("Rides", labels = scales::comma) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + labs(x="Station")
    
  })
  
  output$allStopsAlphabetical <- renderPlot({
    
    allData <- allStopsReactive()
    ggplot(data=allData, aes(x=stationname, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + scale_y_continuous("Rides", labels = scales::comma) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + labs(x="Station")
    
  })
  
  output$selectedStopTitle <- renderText({
    current_stop <- selectedStopReactive()
    paste("Ridership for ", input$currentStop)
  })
  
  output$MonthlyPlot <- renderPlot({
    
    monthNums <- seq(1, 12)
    months <- month.abb[monthNums]
    
    current_stop <- selectedStopDataReactive()
    ggplot(data=current_stop, aes(x=month_char, y=rides)) + geom_bar(stat = "identity", fill="#098CF9")  + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Months", limits = months) + labs(x="Month", y = "Rides")
    
  })
  
  output$MonthlyTable <- DT::renderDataTable(DT::datatable({
    
    current_stop <- selectedStopDataReactive()
    
    month_table_df <- data.frame(Month=character(), Rides=integer())
    for(mon in months) {
      num <- with(current_stop, sum(rides[month_char == mon]))
      rowdf <- data.frame(Month=mon, Rides=num)
      month_table_df <- rbind(month_table_df, rowdf)
    }
    data <- month_table_df
    data
  }, options = list(pageLength = 12, searching = FALSE), rownames = FALSE))
  
  output$YearlyPlot <- renderPlot({
    
    current_stop <- selectedStopDataYearly()
    ggplot(data=current_stop, aes(x=year, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + scale_y_continuous("Rides", labels = scales::comma) + scale_x_continuous("Year", breaks = seq(2001, 2021)) 
    
    
  })
  
  output$YearlyTable <- DT::renderDataTable(DT::datatable({
    
    current_stop <- selectedStopDataYearly()
    
    year_table_df <- data.frame(Year=double(), Rides=integer())
    
    for(currentYear in 2001:2021) {
      
      num <- with(current_stop, sum(rides[currentYear == year]))
      rowdf <- data.frame(Year=currentYear, Rides=num)
      year_table_df <- rbind(year_table_df, rowdf)
      
    }
    data <- year_table_df
    data
    
  }, options = list(pageLength = 10, searching = FALSE), rownames = FALSE))
  
  output$WeeklyTable <- DT::renderDataTable(DT::datatable({
    
    current_stop <- selectedStopDataReactive()
    
    weekday_table_df <- data.frame(Weekday=character(), Rides=integer())
    
    
    for(wkday in weekdayNums) {
      # print(wkday)
      rides <- current_stop[current_stop$day_of_week == wkday, ]$rides
      # print(rides)
      num <- sum(rides)
      rowdf <- data.frame(Weekday=wkday, Rides=num)
      weekday_table_df <- rbind(weekday_table_df, rowdf)
    }
    data <- weekday_table_df
    data
    
  }, options = list(pageLength = 7, searching = FALSE), rownames = FALSE))
  
  output$DailyPlot <- renderPlot({
    
    current_stop <- selectedStopDataReactive()
    ggplot(data=current_stop, aes(x=as.Date(date_ymd), y=rides)) + geom_bar(stat="identity", fill = "#098CF9") + scale_x_date(date_breaks = "1 month") + labs(x = "Day", y = "Rides") 
    
    
  })
  
  output$DailyTable <- DT::renderDataTable(DT::datatable({
    
    current_stop <- selectedStopDataReactive()
    
    day_table_df <- data.frame(Day=character(), Rides=integer())
    i <- 1
    
    current_stop <- current_stop[order(as.Date(current_stop$date_ymd, format = "%Y-&m-%d")), ]
    for(i in 1:nrow(current_stop)) {
      
      rowdf <- data.frame(Day = current_stop[i, ]$date_ymd, Rides = current_stop[i, ]$rides)
      day_table_df <- rbind(day_table_df, rowdf)
    }
    data <- day_table_df
    data
    
  }, options = list(pageLength = 10, searching = FALSE), rownames = FALSE))  
  
  output$WeekdayPlot <- renderPlot({
    
    current_stop <- selectedStopDataReactive()
    ggplot(data=current_stop, aes(x=day_of_week, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + scale_y_continuous("Rides", labels = scales::comma)  + scale_x_discrete("Weekdays", limits=weekdayNums, labels=c("Sunday" = "Sun","Monday" = "Mon", "Tuesday" = "Tues", "Wednesday" = "Wed", "Thursday" = "Thurs", "Friday" = "Fri", "Saturday" = "Sat"))
    
  })
  
  # uses default map, to change add the line
  # addProviderTiles("Esri.WorldImagery") %>%
  # or 
  # addProviderTiles("Esri.WorldGrayCanvas") %>%
  # we can probably just make a reactive variable with those strings to change it
  # quickly
  # 144->147
  output$map <- renderLeaflet({
    
    #addProviderTiles(theme) %>%
    leaflet() %>%
      addTiles() %>%  
      setView(lng =-87.658323, lat = 41.879036, zoom = 12) %>%
      addTiles("Ersi",group = "Default")%>%
      addProviderTiles("Esri.WorldGrayCanvas", group = "Grayscale") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("Default","Grayscale","Satellite"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomleft"
      ) %>%
      addMarkers(lat = latVector[1:147], lng = longVector[1:147], label = nameVector[1:147], layerId = nameVector[1:147])
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    #text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng, "Name ", click$id)
    text<-paste("Now displaying data for: ", click$id)
    
    proxy <- leafletProxy("map")
    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
  
  observeEvent(input$map_marker_click, {
    
    # TODO: if we ever switch to stop names using latlong this will break
    
    click <- input$map_marker_click
    stop_name <- click$id
    # we've got our stop name, grab the ID for it
    station_id <- head(summed[summed$stationname == stop_name, ], 1)$station_id
    # grab the station name from latlong using the ID
    # print(colnames(latlong_unique))
    station_name <- latlong_unique[latlong_unique$station_id == station_id, ]$STATION_NAME
    summed_station_name <- head(summed[summed$station_id == station_id, ], 1)$stationname
    print(station_name)
    updateSelectizeInput(session = session, inputId = "currentStop", choices = getLineValues(input$currentLine), selected = station_name)
  })
  
}
shinyApp(ui, server)
# TODO: 
# ur mom lol
# Adapt Proj1 code to just be 4 output functions that each yield a table. 


