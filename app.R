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

# unused markers for coloring markers for their line colors
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

# updated markers start at a darker gradient so the lowest tier has increased visibility
# icons are from https://www.iconsdb.com/custom-color/map-marker-2-icon.html and are the 32x32 size
# palettes are from https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=9
mapIcons <- iconList(
  tier5 = makeIcon("tier5.png"),
  tier4 = makeIcon("tier4.png"),
  tier3 = makeIcon("tier3.png"),
  tier2 = makeIcon("tier2.png"),
  tier1 = makeIcon("tier1.png"),
  tier0 = makeIcon("greyMarker.png")
)

weekdayNums <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

monthNums <- seq(1, 12)
months <- month.abb[monthNums]

# map data is used to create the vectors that are given to the leaflet for markers
mapData <- data.frame(summed$loc, summed$stationname,summed$station_id)
mapData <- unique.data.frame(mapData)
colnames(mapData) <- c("loc","stationname","station_id")

mainThemeColor = "blue"

# used for static map placement, unused for reactive placement of maps
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

# set up the static vectors for first time placement
for(row in 1:nrow(mapData)) {
  
  tempString <- mapData[row,"loc"]
  # remove ( ) at the start and end 
  tempString <- substring(tempString,2,nchar(tempString)-1)
  # everything before the ,
  latString <- sub(",.*", "",tempString)
  # everything after the ,
  longString <- sub(".*,", "",tempString)
  
  latVector[row]  <- as.numeric(latString)
  longVector[row] <- as.numeric(longString)
  
  tempNameString <- mapData[row,"stationname"]
  nameString <- tempNameString
  nameVector[row] <- nameString
  
  tempIdString <- mapData[row,"station_id"]
  idString <- tempIdString
  idVector[row] <- idString
}



##################

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
           column(11, 
                  fluidRow(h3(textOutput("selectedDayTitle"))),
                  box(title="All Stop Ridership", width=12, height = "40vh", background = mainThemeColor,
                      
                      conditionalPanel(condition = "input.useAlphabetical == 'Alphabetical'", 
                                       column(11, plotOutput("allStopsAlphabetical", height = "30vh")),
                                       column(1, box(width = 12, background = mainThemeColor, dataTableOutput("AllStopsTableAlphabetical", height = "30vh")))),
                      conditionalPanel(condition = "input.useAlphabetical == 'Ascending'", 
                                       column(11, plotOutput("allStopsNumeric", height = "30vh")),
                                       column(1, box(width = 12, background = mainThemeColor, dataTableOutput("AllStopsTableNumeric", height = "30vh")))),
                  ))
         ),
         # major row 2
         
         fluidRow(
           column(1,
                  fluidRow(selectInput(inputId = "currentLine", choices = lineOptions, label = "Select Line")),
                  fluidRow(selectizeInput(inputId = "currentStop", choices = latlong_unique$STATION_NAME, label = "Select Stop")),
           ),
           column(11, 
                  # for the month
                  fluidRow(h3(textOutput("selectedStopTitle"))),
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
  column(width = 3, box(title = "Map of Stops", width = 12, background = mainThemeColor, leafletOutput("map", height = "85vh")))
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
  
  currentDateReactive <- reactive(input$currentDate)
  allStopsReactive <- reactive({ subset(summed, summed$date_ymd == input$currentDate) })
  selectedStopReactive <- reactive({ subset(latlong_unique, latlong_unique$STATION_NAME == input$currentStop) })
  selectedStopDataReactive <- reactive({ subset(summed, summed$station_id == head(latlong_unique[latlong_unique$STATION_NAME == input$currentStop, ], 1)$station_id & summed$year == year(input$currentDate))})
  selectedStopDataYearly <- reactive({ subset(summed, summed$station_id == head(latlong_unique[latlong_unique$STATION_NAME == input$currentStop, ], 1)$station_id) })
  
  ############## PLOT CODE ##############
  
  
  output$AllStopsTableNumeric <- DT::renderDataTable(DT::datatable({
    
    allData <- allStopsReactive()
    allDataSorted <- data.frame(allData$stationname, allData$rides)
    colnames(allDataSorted) <- c("stationname", "rides")
    allDataSorted <- allDataSorted[order(-allDataSorted$rides), ]
    allDataSorted$stationname <- factor(allDataSorted$stationname, levels = allDataSorted$stationname) 
    colnames(allDataSorted) <- c("Station Name", "rides")
    
    data <- allDataSorted
    data
    
  }, options = list(pageLength = 10, searching = FALSE), rownames = FALSE ))
  
  output$AllStopsTableAlphabetical <- DT::renderDataTable(DT::datatable({
    
    allData <- allStopsReactive()
    allData <- allData[order(allData$stationname), ]
    allDataSorted <- data.frame(allData$stationname, allData$rides)
    # allDataSorted <- allDataSorted[order(allDataSorted$allData.stationname), ]
    # print(allDataSorted)
    colnames(allDataSorted) <- c("Station Name", "rides")    
    # allDataSorted <- allDataSorted[order(-allDataSorted$rides), ]
    # allDataSorted$stationname <- factor(allDataSorted$stationname, levels = allDataSorted$stationname) 
    data <- allDataSorted
    data
    
  }, options = list(pageLength = 10, searching = FALSE), rownames = FALSE ))
  
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
    current_date <- currentDateReactive()
    current_year <- year(current_date)
    # print(current_stop)
    paste("Ridership for ", current_stop$STATION_NAME, " for ", current_year)
  })
  
  output$selectedDayTitle <- renderText({
    current_stop <- selectedStopReactive()
    current_date <- currentDateReactive()
    day_of_week <- weekdays(current_date)
    
    paste("Ridership for ", day_of_week, " ", current_date)
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
  
  ############## LEAFLET CODE ##############
  
  
  # map is responsible for creating the initial map as well as updating it when
  # a marker is pressed or a date is changed, along with any legends and map controls
  output$map <- renderLeaflet({
    
    # get the data from the reactive variable selected by user
    allData <- allStopsReactive()
    allDataMap <- data.frame(allData$stationname, allData$rides, allData$loc)
    allDataMap[ , 'color'] <- NA
    allDataMap[ , 'lat'] <- NA
    allDataMap[ , 'long'] <- NA
    colnames(allDataMap) <- c("stationname", "rides","loc","color","lat","long")
    
    # this loop takes the loc column and changes it so it can be supplied to the map
    for(row in 1:nrow(allData)) {
      
      tempString <- allData[row,"loc"]
      tempString <- substring(tempString,2,nchar(tempString)-1)

      latString <- sub(",.*", "",tempString)
      longString <- sub(".*,", "",tempString)

      allDataMap[row,"lat"] <- as.numeric(latString)
      allDataMap[row,"long"] <- as.numeric(longString)
    }
  
    # this loop assigns tiers to each stops ridership so the gradient can be displayed
    for(row in 1:nrow(allDataMap)) {
      
      tempRides <- allDataMap[row,"rides"]
      print(tempRides)
      
      # Tier 5 is the darkest and tier0 is a gray 
      if(tempRides > 3000){
        allDataMap[row,"color"] <- "tier5"
      }
      else if(tempRides > 2000){
        allDataMap[row,"color"] <- "tier4"
      }
      else if(tempRides > 1000){
        allDataMap[row,"color"] <- "tier3"
      }
      else if(tempRides > 500){
        allDataMap[row,"color"] <- "tier2"
      }
      else if(tempRides > 250){
        allDataMap[row,"color"] <- "tier1"
      }
      else{
        allDataMap[row,"color"] <- "tier0"
      }
      
    }
    View(allDataMap)
    # View(summed)
    
    # First the view is set a little west of the center of the city with a zoom that covers most stops
    # then the default map is applied with the other options set as groups
    # a control box is added which allows swapping of said groups
    # markers are added from the prepared dataframe, which takes each value in the specified column
    # finally a legend is added for the color gradient
    leaflet(allDataMap) %>%
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
      addMarkers(lat = allDataMap$lat, lng = allDataMap$long, label = allDataMap$stationname,layerId = allDataMap$stationname,icon = ~mapIcons[color]) %>%
      addLegend("bottomright", 
                colors =c("#8C8C8C","#fc8d59",  "#ef6548", "#d7301f", "#b30000", "#7f0000"),
                labels= c( "<250", "<500","<1000","<2000","<3000",">3000"),
                title= "Ridership per Stop",
                opacity = 1)
  })
  
  # map marker click shows which stop is pressed on click
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    text<-paste("Now displaying data for: ", click$id)
    
    proxy <- leafletProxy("map")
    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
  
  # second map marker click is responsible for updating which station is being
  # focused on after the user presses on it
  observeEvent(input$map_marker_click, {
    
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