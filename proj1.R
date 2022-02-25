library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(scales)

setwd("~/class/424/proj1")


# Load in our data
# make sure to disable quotes
ridership <- read.table(file = "ridership.tsv", sep = '\t', header = TRUE, quote="")

# fix the date
# ridership$date_ymd <- ymd(paste(year(mdy(ridership$date)), month(mdy(ridership$date)), day(mdy(ridership$date)), sep="-"))
# ridership$month <- month(ridership$date_ymd)
# ridership$month_char <- month.abb[month(ridership$date_ymd)]
# ridership$year <- year(ridership$date_ymd)
# ridership$day <- day(ridership$date_ymd)
# ridership$day_of_week <- weekdays(ridership$date_ymd)
# ridership$date_correct <- ymd(paste(ridership$Year, utility$Month, "01", sep="-"))

# TODO: Make these subsets into their own files

# uic <- subset(ridership, stationname == "UIC-Halsted")
# hare <- subset(ridership, stationname == "O'Hare Airport")
# jackson <- subset(ridership, stationname == "Jackson/Dearborn")

# first station is UIC-HALSTED
uic <- as.data.frame(read.csv("uicHalsted.csv", fileEncoding = "UTF-8-BOM"))
uic$date_ymd <- as.Date(uic$date_ymd)

# second station is O'Hare
hare <- as.data.frame(read.csv("oHare.csv", fileEncoding = "UTF-8-BOM"))
hare$date_ymd <- as.Date(hare$date_ymd)
#third station is Jackson
jackson <- as.data.frame(read.csv("jackson.csv", fileEncoding = "UTF-8-BOM"))
jackson$date_ymd <- as.Date(jackson$date_ymd)


#UICperYear = ggplot(data=uic, aes(x=year, y=rides)) 
#UICperYear = UICperYear + geom_bar(stat = "identity", fill="#098CF9") 
#UICperYear = UICperYear + ggtitle("UIC Per Year Ridership") 
#UICperYear = UICperYear + scale_y_continuous(labels = scales::comma) 
#UICperYear = UICperYear + scale_x_continuous(breaks = seq(2001, 2021, 2))


#uic <- read.table("uicHalsted.csv", sep=",", header = TRUE, quote="")
#hare <- read.table("ohare.csv", sep=",", header = TRUE, quote="")

uic2021 <- subset(uic, year == 2021)

monthNums <- seq(1, 12)
months <- month.abb[monthNums]

weekdayNums <- seq(1, 7)
weekdayNums <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")


# for(val in 2001:2021) {
# 
#   uic_this_year = subset(uic, year == val)
#   monthly_title <- paste("UIC", val,  "Monthly Ridership")
#   UICperMonth = ggplot(data=uic_this_year, aes(x=month_char, y=rides))
#   UICperMonth = UICperMonth + geom_bar(stat = "identity", fill="#098CF9")
#   UICperMonth = UICperMonth + ggtitle(monthly_title)
#   UICperMonth = UICperMonth + scale_y_continuous(labels = scales::comma)
#   UICperMonth = UICperMonth + scale_x_discrete("", limits = months)
#   print(UICperMonth)
# 
#   # make the chart
#   month_table_df <- data.frame(Month=character(), Rides=integer())
# 
#   for(mon in months) {
#     num <- with(uic_this_year, sum(rides[month_char == mon]))
#     rowdf <- data.frame(Month=mon, Rides=num)
#     month_table_df <- rbind(month_table_df, rowdf)
#   }
#   month_table = datatable(month_table_df, options = list(searching = FALSE, pagelength = 2))
#   print(month_table)
# 
#   # TODO: ADD TITLE TO TABLE
# 
#   UICperweekday = ggplot(data=uic_this_year, aes(x=day_of_week, y=rides))
#   UICperweekday = UICperweekday + geom_bar(stat = "identity", fill="#098CF9")
#   UICperweekday = UICperweekday + ggtitle("UIC 2021 Weekday Ridership")
#   UICperweekday = UICperweekday + scale_y_continuous(labels = scales::comma)
#   UICperweekday = UICperweekday + scale_x_discrete("", limits=weekdayNums, labels=c("Sunday" = "Sun","Monday" = "Mon", "Tuesday" = "Tues", "Wednesday" = "Wed", "Thursday" = "Thurs", "Friday" = "Fri", "Saturday" = "Sat"))
#   #UICperweekday
#   weekday_table_df <- data.frame(Weekday=character(), Rides=integer())
# 
#   for(wkday in weekdayNums) {
#     num <- with(uic_this_year, sum(rides[day_of_week == wkday]))
#     rowdf <- data.frame(Weekday=wkday, Rides=num)
#     weekday_table_df <- rbind(weekday_table_df, rowdf)
#   }
#   week_table = datatable(weekday_table_df, options = list(searching = FALSE, pagelength = 2))
#   week_table
# }
# 


# make plot based on year number 



# make the shiny page
ui <- dashboardPage(
  dashboardHeader(title = "CS424 Proj1"),
  dashboardSidebar(
    disable = TRUE, collapsed = FALSE),
  dashboardBody(
  
  fluidRow(
    box(width = 2, "Written by Kenan Arica for CS 424, in Spring 2022. Dataset is taken from", tags$a(href="https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f
", "Chicago Data Portal"), "and is CTA ridership from 2001-2021. Created to explore interesting trends in CTA ridership across the UIC-Halsted, O-Hare, and Jackson Blue line stops."),
    column(8, box(width = 12,
                  
                  
                  conditionalPanel(
                    condition = "input.stop1isGraph == 'yes'",
                    plotOutput("plot1", height ="35vh")
                  ),
                  conditionalPanel(
                    condition = "input.stop1isGraph == 'no'",
                    dataTableOutput("table1", height = "35vh")
                  )
                  , title = "Stop 1", solidHeader = TRUE, background = "blue")
    ),
    box(width = 2)
    ),
  
    
  fluidRow(
    column(2, box(height = "40vh",
      sliderInput(inputId = "Year",
                  label = "Year:",
                  min = 2001,
                  max = 2021,
                  value = 2001,
                  sep=""),
      selectInput(
        inputId = "stop1",
        label = "Stop",
        choices = c("UIC-Halsted", "O-Hare", "Jackson"),
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      selectInput(
        inputId = "stop1Type",
        label = "Ridership by: ",
        choices = c("Year", "Month", "Day", "Weekday"),
        selected = "Year",
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      radioButtons("stop1isGraph", "Display as: ",
                   choices = list("Graph" = "yes", "Table" = "no"),selected = "yes")
      , title = "Stop 1", background = "black", width = 12)
    ),
    column(8, box(width = 12,
        
    
               conditionalPanel(
                 condition = "input.stop2isGraph == 'yes'",
                 plotOutput("plot2", height="35vh")
               ),
               conditionalPanel(
                 condition = "input.stop2isGraph == 'no'",
                 dataTableOutput("table2", height="35vh")
               )
    , title = "Stop 2", solidHeader = TRUE, background = "blue")
    ),
    column(2, box(height = "40vh",
      sliderInput(inputId = "Year2",
                  label = "Year:",
                  min = 2001,
                  max = 2021,
                  value = 2001,
                  sep=""),
      selectInput(
        inputId = "stop2",
        label = "Stop",
        choices = c("UIC-Halsted", "O-Hare", "Jackson"),
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      selectInput(
        inputId = "stop2Type",
        label = "Ridership by: ",
        choices = c("Year", "Month", "Day", "Weekday"),
        selected = "Year",
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      radioButtons("stop2isGraph", "Display as: ",
                   choices = list("Graph" = "yes", "Table" = "no"),selected = "yes")
      , title = "Stop 2", background = "black", width = 12)
    ),
  )

  ))
  
server <- function(input, output, session) {

uicReactive <- reactive({subset(uic, uic$year == input$Year)})
hareReactive <- reactive({subset(hare, hare$year == input$Year)})
jacksonReactive <- reactive({subset(jackson, jackson$year == input$Year)})

uicReactive2 <- reactive({subset(uic, uic$year == input$Year2)})
hareReactive2 <- reactive({subset(hare, hare$year == input$Year2)})
jacksonReactive2 <- reactive({subset(jackson, jackson$year == input$Year2)})


stops = c(uic, hare)
stopReactive <- reactive({subset(stops[input$stop1isGraph], stops[input$stop1isGraph] == input$Year)})

output$plot1 <- renderPlot({
  
  title_theme <- theme(plot.title=element_text(family='', colour='Black', size=20))
  
  # get the dataset we're using
    stop1Data <- NULL
    if(input$stop1 == "UIC-Halsted") {
      stop1Data <- uicReactive()
    } 
    else if(input$stop1 == "O-Hare") { 
      stop1Data <- hareReactive()
    }
    else if(input$stop1 == "Jackson") { 
      stop1Data <- jacksonReactive()
    }
    # make our titles
    monthly_title <- paste(input$stop1, input$Year,  "Monthly Ridership")
    weekly_title <- paste(input$stop1, input$Year,  "Weekly Ridership")
    daily_title <- paste(input$stop1, input$Year,  "Daily Ridership")
    yearly_title <- paste(input$stop1, input$Year,  "Yearly Ridership")
    # get the metric for the graph
    
    # Month
    if(input$stop1Type == "Month") {
      
        ggplot(data=stop1Data, aes(x=month_char, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle(monthly_title) + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Months", limits = months) + labs(x="Month", y = "Rides") + title_theme
    }
    # Weekday
    else if(input$stop1Type == "Weekday") { 
      
      ggplot(data=stop1Data, aes(x=day_of_week, y=rides)) + geom_bar(stat = "identity", fill="#098CF9")  + ggtitle(weekly_title) + scale_y_continuous("Rides", labels = scales::comma)  + scale_x_discrete("Weekdays", limits=weekdayNums, labels=c("Sunday" = "Sun","Monday" = "Mon", "Tuesday" = "Tues", "Wednesday" = "Wed", "Thursday" = "Thurs", "Friday" = "Fri", "Saturday" = "Sat")) + title_theme
    }
    # Day 
    else if(input$stop1Type == "Day") { 
      ggplot(data=stop1Data, aes(x=date_ymd, y=rides)) + geom_bar(stat="identity", fill = "#098CF9") + ggtitle(daily_title) + scale_x_date(date_breaks = "1 month") + labs(x = "Day", y = "Rides") + title_theme
    }
    # Year
    else if(input$stop1Type == "Year") {
      
      # get our total dataset
      if(input$stop1 == "UIC-Halsted") {
        stop1Data <- uic
      } else if(input$stop1 == "O-Hare") { 
        stop1Data <- hare
      }
      else if(input$stop1 == "Jackson") { 
        stop1Data <- jackson
      }
      ggplot(data=stop1Data, aes(x=year, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle(yearly_title) + scale_y_continuous("Rides", labels = scales::comma) + scale_x_continuous("Year", breaks = seq(2001, 2021)) + title_theme
    }
  
})

output$plot2 <- renderPlot({
  
  title_theme <- theme(plot.title=element_text(family='', colour='Black', size=20))
  
  # get the dataset we're using
  stop1Data <- NULL
  if(input$stop2 == "UIC-Halsted") {
    stop2Data <- uicReactive2()
  } 
  else if(input$stop2 == "O-Hare") { 
    stop2Data <- hareReactive2()
  }
  else if(input$stop2 == "Jackson") { 
    stop2Data <- jacksonReactive2()
  }
  # make our titles
  monthly_title <- paste(input$stop2, input$Year2,  "Monthly Ridership")
  weekly_title <- paste(input$stop2, input$Year2,  "Weekly Ridership")
  daily_title <- paste(input$stop2, input$Year2,  "Daily Ridership")
  yearly_title <- paste(input$stop2, input$Year2,  "Yearly Ridership")
  # get the metric for the graph
  
  # Month
  if(input$stop2Type == "Month") {
    
    ggplot(data=stop2Data, aes(x=month_char, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle(monthly_title) + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Months", limits = months) + labs(x="Month", y = "Rides") + title_theme
  }
  # Weekday
  else if(input$stop2Type == "Weekday") { 
    
    ggplot(data=stop2Data, aes(x=day_of_week, y=rides)) + geom_bar(stat = "identity", fill="#098CF9")  + ggtitle(weekly_title) + scale_y_continuous("Rides", labels = scales::comma)  + scale_x_discrete("Weekdays", limits=weekdayNums, labels=c("Sunday" = "Sun","Monday" = "Mon", "Tuesday" = "Tues", "Wednesday" = "Wed", "Thursday" = "Thurs", "Friday" = "Fri", "Saturday" = "Sat")) + title_theme
  }
  # Day 
  else if(input$stop2Type == "Day") {
    
    ggplot(data=stop2Data, aes(x=as.Date(date_ymd), y=rides)) + geom_bar(stat="identity", fill = "#098CF9") + ggtitle(daily_title) + scale_x_date(date_breaks = "1 month") + labs(x = "Day", y = "Rides") + title_theme
  }
  # Year
  else if(input$stop2Type == "Year") {
    
    # get our total dataset
    if(input$stop2 == "UIC-Halsted") {
      stop2Data <- uic
    } else if(input$stop2 == "O-Hare") { 
      stop2Data <- hare
    }
    else if(input$stop2 == "Jackson") { 
      stop2Data <- jackson
    }
    ggplot(data=stop2Data, aes(x=year, y=rides)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle(yearly_title) + scale_y_continuous("Rides", labels = scales::comma) + scale_x_continuous("Year", breaks = seq(2001, 2021)) + title_theme
  }
  
})

output$table1 <- DT::renderDataTable(DT::datatable({
  
  stop1Data <- NULL
  if(input$stop1 == "UIC-Halsted") {
    stop1Data <- uicReactive()
  } else if(input$stop1 == "O-Hare") { 
    stop1Data <- hareReactive()
  }
  else if(input$stop1 == "Jackson") { 
    stop1Data <- jacksonReactive()
  }
  
  monthly_title <- paste(input$stop1, input$Year,  "Monthly Ridership")
  weekly_title <- paste(input$stop1, input$Year,  "Weekly Ridership")
  daily_title <- paste(input$stop1, input$Year,  "Daily Ridership")
  yearly_title <- paste(input$stop1, input$Year,  "Yearly Ridership")
  
  # TODO: Abstract this to days, years, months, weekdays
  # TODO: add conditionalPanel to the row which can decide to show table or graph
  if(input$stop1Type == "Month") { 
    month_table_df <- data.frame(Month=character(), Rides=integer())
    for(mon in months) {
      num <- with(stop1Data, sum(rides[month_char == mon]))
      rowdf <- data.frame(Month=mon, Rides=num)
      month_table_df <- rbind(month_table_df, rowdf)
    }
    data <- month_table_df
    data
  }
  else if(input$stop1Type == "Weekday") { 
  
      weekday_table_df <- data.frame(Weekday=character(), Rides=integer())
    
      for(wkday in weekdayNums) {
        num <- with(stop1Data, sum(rides[day_of_week == wkday]))
        rowdf <- data.frame(Weekday=wkday, Rides=num)
        weekday_table_df <- rbind(weekday_table_df, rowdf)
      }
      data <- weekday_table_df
      data
    
  }
  else if(input$stop1Type == "Year") { 
    if(input$stop1 == "UIC-Halsted") {
      stop1Data <- uic
    } else if(input$stop1 == "O-Hare") { 
      stop1Data <- hare
    }
    else if(input$stop1 == "Jackson") { 
      stop1Data <- jackson
    }
    year_table_df <- data.frame(Year=double(), Rides=integer())
    
    for(currentYear in 2001:2021) {
      
      num <- with(stop1Data, sum(rides[currentYear == year]))
      rowdf <- data.frame(Year=currentYear, Rides=num)
      year_table_df <- rbind(year_table_df, rowdf)
    
    }
    data <- year_table_df
    data
  }
  else if(input$stop1Type == "Day") { 
    
    day_table_df <- data.frame(Day=character(), Rides=integer())
    i <- 1

        stop1Data <- stop1Data[order(as.Date(stop1Data$date_ymd, format = "%Y-&m-%d")), ]
    for(i in 1:nrow(stop1Data)) {

      rowdf <- data.frame(Day = stop1Data[i, ]$date_ymd, Rides = stop1Data[i, ]$rides)
      day_table_df <- rbind(day_table_df, rowdf)
      }
    data <- day_table_df
    data
    
  }
  
}, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))

output$table2 <- DT::renderDataTable(DT::datatable({
  
  stop2Data <- NULL
  if(input$stop2 == "UIC-Halsted") {
    stop2Data <- uicReactive2()
  } else if(input$stop2 == "O-Hare") { 
    stop2Data <- hareReactive2()
  }
  else if(input$stop2 == "Jackson") { 
    stop2Data <- jacksonReactive2()
  }
  
  monthly_title <- paste(input$stop2, input$Year2,  "Monthly Ridership")
  weekly_title <- paste(input$stop2, input$Year2,  "Weekly Ridership")
  daily_title <- paste(input$stop2, input$Year2,  "Daily Ridership")
  yearly_title <- paste(input$stop2, input$Year2,  "Yearly Ridership")
  
  # TODO: Abstract this to days, years, months, weekdays
  # TODO: add conditionalPanel to the row which can decide to show table or graph
  if(input$stop2Type == "Month") { 
    month_table_df <- data.frame(Month=character(), Rides=integer())
    for(mon in months) {
      num <- with(stop2Data, sum(rides[month_char == mon]))
      rowdf <- data.frame(Month=mon, Rides=num)
      month_table_df <- rbind(month_table_df, rowdf)
    }
    data <- month_table_df
    data
  }
  else if(input$stop2Type == "Weekday") { 
    
    weekday_table_df <- data.frame(Weekday=character(), Rides=integer())
    
    for(wkday in weekdayNums) {
      num <- with(stop2Data, sum(rides[day_of_week == wkday]))
      rowdf <- data.frame(Weekday=wkday, Rides=num)
      weekday_table_df <- rbind(weekday_table_df, rowdf)
    }
    data <- weekday_table_df
    data
    
  }
  else if(input$stop2Type == "Year") { 
    if(input$stop2 == "UIC-Halsted") {
      stop2Data <- uic
    } else if(input$stop2 == "O-Hare") { 
      stop2Data <- hare
    }
    else if(input$stop2 == "Jackson") { 
      stop2Data <- jackson
    }
    year_table_df <- data.frame(Year=double(), Rides=integer())
    
    for(currentYear in 2001:2021) {
      
      num <- with(stop2Data, sum(rides[currentYear == year]))
      rowdf <- data.frame(Year=currentYear, Rides=num)
      year_table_df <- rbind(year_table_df, rowdf)
      
    }
    data <- year_table_df
    data
  }
  else if(input$stop2Type == "Day") { 
    
    day_table_df <- data.frame(Day=character(), Rides=integer())
    i <- 1
    
    stop2Data <- stop2Data[order(as.Date(stop2Data$date_ymd, format = "%Y-&m-%d")), ]
    for(i in 1:nrow(stop2Data)) {
      
      rowdf <- data.frame(Day = stop2Data[i, ]$date_ymd, Rides = stop2Data[i, ]$rides)
      day_table_df <- rbind(day_table_df, rowdf)
    }
    data <- day_table_df
    data
    
  }
  
}, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)
), rownames = FALSE))



}
shinyApp(ui, server)



