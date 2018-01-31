#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("C:/Users/burcakagridag/Documents/BikeNYCVis")

require(shiny)
require(rCharts)
require(RJSONIO)
require(rCharts)
require(RColorBrewer)
require(httr)
library(leaflet.extras)

options(stringsAsFactors = F)

library(dplyr)
library(ggmap)
library(timeDate)
library(shinythemes)
library(shiny)
library(leaflet)
library(jsonlite)
library(shinydashboard)
require(ggplot2)
library(plotly)
require(scales)
library(lubridate)
library(viridis)




ui <- (navbarPage("NYC Bike Transportation",
               
                  tabPanel("About", 
                           fluidPage(theme = shinytheme("united"),
                                     
                                     mainPanel(
                                       includeHTML("index.html")
                                     ))),
                  tabPanel("NYC", 
                           fluidPage(theme = shinytheme("united"),
                                     includeCSS(path = "AdminLTE.css"),
                                     includeCSS(path = "shinydashboard.css"),
                                     mainPanel(
                                       fluidRow(
                                         infoBox("Population","8,6 mn", icon = icon("users"), fill= TRUE),
                                         infoBox("Number of Bike Stations","815", icon = icon("bicycle"), fill= TRUE, color = "purple"),
                                         infoBox("Total Number of Users","206,585", icon = icon("users"), fill= TRUE, color = "yellow")
                                       ),
                                       fluidRow(
                                         infoBox("Visitors","60,5 mn", icon = icon("info"), fill= TRUE),
                                         infoBox("Domestic Visitors","47,8 mn", icon = icon("info"), fill= TRUE, color = "purple"),
                                         infoBox("International Visitors","12,7 mn", icon = icon("info"), fill= TRUE, color = "yellow")
                                  
                                       ),
                                  
                                       plotOutput("plot"),
                                       p("Based on 2016 data"))
                                   )
                  
                  ),
                   
                   tabPanel("General Analysis",
                            fluidPage(theme = shinytheme("united"),
                                      
                                      sidebarLayout(
                                        sidebarPanel(
   
                                          selectInput("selectPlots",
                                                      label = h3("Pick your plot"),
                                                      choices = list("Rented Bikes by Hour of the Day",
                                                                     "Rented Bikes by Hour of the Day/Week",
                                                                     "Rented Bikes by Hour of the Day/Week HeatMap",
                                                                     "Trip Durations",
                                                                     "Trip durations by pickup date/h",
                                                                     "Trip durations by pickup date/w",
                                                                     "Trip durations by User Type",
                                                                     "Rented Bikes by User Type",
                                                                     "Rented Bikes by User Age"
                                                                     ),
                                                      
                                                                    
                                                      selected = "Rented Bikes by Hour of the Day")
    
                                        ),
                                        mainPanel(
                                          fluidRow(plotOutput("selectPlots"), 
                                                   plotOutput("HeatMapPlot"))
                                          
                                          
                                          
                                        )
                                      )
                            )
                   ),
                  tabPanel("Temperature Effects",
                           fluidPage(theme = shinytheme("united"),
                                     sidebarLayout( 
                                       mainPanel(
                                         
                                         plotlyOutput("selectPlotly")
                                         
                                         
                                       ),
                                    
                                       sidebarPanel(
                                         
                                         selectInput("selectPlotly",
                                                     label = h3("Pick your plot"),
                                                     choices = list("Temperature vs. #of Rentals",
                                                                    "Temperature&Day Effects on Rentals"
                                                                    
                                                     ),
                                                     
                                                     
                                                     selected = "Temperature vs. #of Rentals")
                                         
                                       ),
                                     
                           )
                  )),
                   tabPanel("Heat Maps",
                            fluidPage(theme = shinytheme("united"),
                                      
                                      sidebarLayout(
                                        sidebarPanel(
         
                                          selectInput("month",
                                                      label = h3("Pick a month"),
                                                      choices = list("January", "February", "March", "April","May","June","July","August","September", "October", "November", "December"),
                                                      selected = "Janary"),
                                          
                                          sliderInput("time",
                                                      label = h3("Select Hour"),
                                                      min = 0,
                                                      max = 24,
                                                      value = c(0,24)),
                                          
                                          radioButtons("weekend",
                                                       label = h3("Weekend/Weekday"),
                                                       choices = list("Weekdays","Weekends","Any Day"),
                                                       selected = "Any Day"),
                                          
                                          radioButtons("rain",
                                                       label = h3("Rain"), 
                                                       choices = list("Light Rain","Heavy Rain", "No Rain"),
                                                       selected = "No Rain"),
                                          
                                          radioButtons("temp",
                                                       label = h3("Temperature"),
                                                       choices = list("Hot", "Cold","Any"),
                                                       selected = "Any"),
                                          
                                          actionButton("show","Show Heatmap")
                                        ),
                                        
                                        mainPanel(
                                          h1("Bike Rental Heatmaps", align="center"),
                                          #plotOutput("heatM", width="700px", height="700px")
                                          leafletOutput("heatM")
                              
                                        )
                                        
                                      )
                            )
                   ),
                   
                  tabPanel("Check available bikes",
                           leafletOutput("map_")
                  )
                   
                   
))





getStationInfo <- function()
{
json_data <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")

n <- length(json_data[[1]])
#DF <- structure(json_data, row.names = c(NA, -n), class = "data.frame")
bikeTest2 <- json_data$data

bikeTest2 <- lapply(bikeTest2, function(station){within(station, { 
  short_name <- NULL
  region_id <- NULL
  rental_methods <- NULL
  rental_url <- NULL
  eightd_has_key_dispenser <- NULL
  capacity <- NULL
  station_id <- as.numeric(station_id)
  
})})
bikeTest2 <- bikeTest2[[1]]

return(bikeTest2)

}





drawMap <- function(url){
  
  #dataBike = content(GET(url))
  json_data <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_status.json")
  
  n <- length(json_data[[1]])
  #DF <- structure(json_data, row.names = c(NA, -n), class = "data.frame")
  dataBikeJ <- json_data$data
 
  dataBikeMat <- dataBikeJ[[1]]
  
  bikeTest2 <- getStationInfo()
  
  dataComb <- merge(dataBikeMat,bikeTest2,by="station_id")
  dataComb <- dataComb %>% filter(lat > 0)
  dataBike <- split(dataComb, seq(nrow(dataComb)))
  
  bike_empty <- makeAwesomeIcon(icon = "bicycle", markerColor = 'red', iconColor = 'black',library="fa")
  bike_full <- makeAwesomeIcon(icon = "bicycle", markerColor = 'green', iconColor = 'black',library="fa")
  bike_almFull <- makeAwesomeIcon(icon = "bicycle", markerColor = 'blue', iconColor = 'black',library="fa")
  bike_almEmpty <- makeAwesomeIcon(icon = "bicycle", markerColor = 'gray', iconColor = 'black',library="fa")
  bike_half <- makeAwesomeIcon(icon = "bicycle", markerColor = 'orange', iconColor = 'black',library="fa")
  
  
  dataBike <- lapply(dataBike, function(station){within(station, { 
    
    id= as.numeric(station_id)
    bikes = num_bikes_available
    free = num_docks_available
    latitude= lat
    longitude = lon
    #latitude = bikeTest2 %>% filter(station_id == id) %>% select(lat)
    #longitude= bikeTest2 %>% filter(station_id == id) %>% select(lon)
    
    stat = as.numeric(bikes)/((as.numeric(bikes) + as.numeric(free)))
    
    if(as.numeric(bikes) == 0) 
    {
      stat = 0
      availability = "NONE" 
    }
    
    else if(as.numeric(stat) == 0)
    {
      availability = "ALMOSTEMPTY" 
    }
    
    else if(as.numeric(stat)!= 0 && as.numeric(stat) <= 0.4)
    {
      
      availability = "ALMOSTEMPTY"
      
    }
    
    else if(as.numeric(stat) > 0.4 && as.numeric(stat) < 0.6)
    {
      
      availability = "HALF"
      
    }
    
    else if(as.numeric(stat) >= 0.6 && as.numeric(stat) < 0.9)
    {
      
      availability = "NEARLYFULL"
    }
    
    else if(as.numeric(stat) >=  0.9)
    {
      
      availability = "FULL"
    }
    
    timestamp = as.POSIXct(json_data[1]$last_updated,  origin = "1970-01-01", tz = "GMT")
    
    #day = sapply(strsplit((timestamp), " "), "[", 1)
    
    #time = sapply(strsplit((timestamp), " "), "[", 2)
    
    time = format(timestamp, "%T")
    day= as.Date(timestamp,format='%Y-%m-%d')
    
    popupData = iconv(whisker::whisker.render(
      '<b>{{name}}</b><br>
      <b>Bikes Available: </b> {{bikes}} <br>
      <b>Empty Docks:</b> {{free}} <br>
      <b>Update Day: </b> {{day}}<br>
      <b>Update Time: </b> {{time}}'
    ), from = 'latin1', to = 'UTF-8')
    
    timestamp <- NULL
    num_bikes_available <- NULL
    num_docks_available <- NULL
    num_bikes_disabled <- NULL
    num_docks_disabled <- NULL
    is_installed <- NULL
    is_renting <- NULL
    is_returning <- NULL
    eightd_has_available_keys <- NULL
    lon <- NULL
    lat<- NULL
    
  })
  })
  
  
  df <- lapply(dataBike, function(stations) # Loop through each "play"
  {
    
    data.frame(matrix(unlist(stations), ncol=13, byrow=T))
  })
  
  
  # Now you have a list of data frames, connect them together in
  # one single dataframe
  df <- do.call(rbind, df)
  
  
  names(df) = c("station_id","last_reported","name","popupData","day", "time", "availability", "stat", "longitude", "latitude", "free", "bikes", "id")
  
  df$latitude <- as.numeric(df$latitude)
  df$longitude <- as.numeric(df$longitude)
  
  
  centerLat = mean(df$latitude)
  centerLon = mean(df$longitude)
  
  
  L1 <- leaflet(df)
  L1 <- addTiles(L1)
  
  
  L1 %>% setView(lng = centerLon, lat = centerLat, zoom = 100)

    
    dataNone <- df %>% filter(availability == "NONE")
    dataFull <- df %>% filter(availability == "FULL")
    dataNF <- df %>% filter(availability == "NEARLYFULL")
    dataAE <- df %>% filter(availability == "ALMOSTEMPTY")
    dataHalf <- df %>% filter(availability == "HALF")
    
  L1 %>%
    addAwesomeMarkers(data = dataNone, dataNone$longitude,dataNone$latitude,
                      icon = bike_empty, group = "Empty",  popup = paste((dataNone$popupData))) %>%
    addAwesomeMarkers(data = dataFull, dataFull$longitude, dataFull$latitude,
                      icon = bike_full, group ="Full", popup = paste((dataFull$popupData))) %>%
    addAwesomeMarkers(data = dataNF, dataNF$longitude,dataNF$latitude,
                      icon = bike_almFull, group="Nearly Full", popup = paste((dataFull$popupData))) %>%
    addAwesomeMarkers(data = dataAE, dataAE$longitude, dataAE$latitude,
                      icon = bike_almEmpty, group ="Almost Empty",popup = paste((dataAE$popupData))) %>%
    addAwesomeMarkers(data = dataHalf, dataHalf$longitude, dataHalf$latitude,
                      icon = bike_half, group = "Half", popup = paste((dataHalf$popupData)))%>%
    
    addLegend(
      position = 'topright',
      colors = c("red", "gray", "orange", "darkcyan", "green"),
      labels = c("Empty", "Almost Empty", "Half", "Nearly Full", "Full"), opacity = 1,
      title = 'Station Bike Availability') %>%
    
    addLayersControl(
      overlayGroups = c("Empty","Full","Almost Empty","Nearly Full","Half"),
      options = layersControlOptions(collapsed = TRUE)
    )
  
}



server <- function(input, output) {
 
  
  output$map_ <- renderLeaflet({
    
    #drawMap("http://api.citybik.es/citibikenyc.json")
    drawMap("https://gbfs.citibikenyc.com/gbfs/en/station_status.json")
    
})

  output$plot <- renderPlot({

    ggplot(data=sumDataMonth, aes(x=mon, y=`sum(count)`)) +
      geom_bar(stat="identity", position = "dodge", fill= "purple") +
      theme_bw() + xlab("Month") + ylab("# of Bikes") + ggtitle(paste("Average Bike Usage")) +
      theme(axis.title = element_text(size = 24), axis.text = element_text(size=12), title = element_text(size=30),
            legend.title = element_blank()) +
      scale_x_continuous(breaks=1:12, labels=c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep", "Oct", "Nov", "Dec"))
  })  
  
  
  selectPlots <- reactive({
  if(input$selectPlots == "Rented Bikes by Hour of the Day")
  {
    attach(sumYear2)
    par(mfrow=c(2,1))
    ggplot(data=sumYear2, aes(x=time, y=count)) + 
           geom_point(size=5, color="magenta") +
           theme_light(base_size=20) +
           xlab("Hour of the Day") + ylab("Number of Bike Rentals")  +
           scale_x_datetime(breaks = date_breaks("3 hours"), labels=date_format("%I:%M %p")) +
           theme(axis.title = element_text(size = 24), axis.text = element_text(size=12), 
                 title = element_text(size=30), legend.title = element_blank()) +
      ggtitle("Busy times 9AM-12PM & 6PM-12AM")
  }
        
   else if(input$selectPlots == "Rented Bikes by Hour of the Day/Week") 
   {
         sumYear %>%
           group_by(day,time) %>%
           count() %>%
           ggplot() +aes(time, n, color = day) +
           geom_line(size = 1.5) +
           labs(x = "Hour of the day", y = "Number of Bike Rentals")+
           scale_x_datetime(breaks = date_breaks("3 hours"), labels=date_format("%I:%M %p")) +  
     ggtitle("Weekdays are more preferred for bike rentals, 3-6PM is busier for the weekends")
   }    
    
    else if(input$selectPlots == "Rented Bikes by Hour of the Day/Week HeatMap") 
    {  
    dataN <-(sumDataPre %>%
               mutate(x= wday(sumDataPre$date)) %>%
               group_by(hour, x) %>%
               count ())
    dataN %>%
      ggplot(aes(hour, x, fill = n)) +
      geom_tile() +
      labs(x = "Hour of the day", y = "Day of the week") +
      scale_fill_distiller(palette = "Spectral") +
      ggtitle("11AM - 9PM")
    
    }
     else if(input$selectPlots == "Trip Durations")
     {
  
         ggplot(data=sumDataPre) + (aes(tripDuration)) +
           geom_histogram(fill = "purple", bins = 150, breaks = seq(0, 5000, by = 100)) +
       ggtitle("Average trip duration ~8 mins")
     }
    
    else if(input$selectPlots == "Trip durations by pickup date/h")
    {
        
            ggplot(data=sumDataPre) + (aes(hour, tripDuration)) +
           geom_boxplot(fill='gray', color="darkred") +
           scale_fill_hue(l=40, c=35) +
           labs(y = "Trip duration in sc", x = "Hour of the day") +
           scale_y_continuous(limits = c(0, 2000)) + 
        ggtitle("Trip Duration is not effected by the hour of the day")
          
    }   
    else if(input$selectPlots == "Trip durations by pickup date/w" ) 
    {
           
           sumDataPre %>%
           mutate(wday = wday(start, label = TRUE)) %>%
           ggplot() + (aes(wday, tripDuration, fill = wday)) +
           geom_boxplot() +
           labs(y = "Trip duration in sc", x = "Day of the week") +
           scale_y_continuous(limits = c(0, 3000)) +
        ggtitle("On weekends, the trip durations are higher ~16 mins")
    }    
        else if(input$selectPlots == "Trip durations by User Type")
        {
           
           sumDataPre %>%
           mutate(typ = sumDataPre$usertype) %>%
           ggplot() + (aes(typ, tripDuration, color = typ)) +
           geom_violin() +
           labs(y = "Trip duration in sc", x = "Day of the week") +
           scale_y_continuous(limits = c(0, 3000))
        }
    
    else if(input$selectPlots == "Rented Bikes by User Type" )
    {
           
           sumDataPre %>%
           mutate(typ = sumDataPre$usertype) %>%
           group_by(date,typ) %>%
           count() %>%
           ggplot() +aes(date, n, color = typ) +
           geom_point(size = 3) +
           labs(x = "Date", y = "count")
    }
    else if(input$selectPlots =="Rented Bikes by User Age" )
    {
           
           
           dataN<- (sumDataPre %>% group_by(`birth year`) %>% count())
           dataN <- filter(dataN, `birth year` > 1910)

           ggplot(data= dataN,  aes(x=`birth year`, y=n)) +
           geom_bar(stat="identity", position = "dodge", fill= "magenta", breaks = seq(1910, 2000, by = 5)) +
           theme_bw() + xlab("Birth Year") + ylab("# of Bikes") +
             ggtitle("25-31 age group tend to rent more bikes")
 
    }   
         
  
  })
  

  output$selectPlots <- renderPlot({
    selectPlots()
  })
  
  output$HeatMapPlot<- renderPlot({
    
    
    
    
  })
  
  output$selectPlotly <- renderPlotly({
    if(input$selectPlotly == "Temperature vs. #of Rentals"){
    plot_ly(sumData, x = ~avgTmp, y = ~count, type="scatter", text = paste("Number of Rentals: ", sumData$count), mode = "markers", color = ~avgTmp, size = ~avgTmp)
    }
    else if(input$selectPlotly == "Temperature&Day Effects on Rentals"){
    dataN <-(sumData %>%
        group_by(date,avgTmp))
      p <-  ggplot(data = dataN, aes(date, count, color = avgTmp)) +
        geom_point(position=position_jitter(w=0.0, h=0.0),aes(color=avgTmp),alpha=0.8) +
        scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))+
        labs(x = "Date", y = "count") + 
        ggtitle("Young age group sesonality effect for October")
      ggplotly(p)
      #plot_ly(sumData, x = ~avgTmp, y = ~count, type="scatter", text = paste("Number of Rentals: ", sumData$count), mode = "markers", color = ~avgTmp, size = ~avgTmp)
    }
    
    else if(input$selectPlotly == "Temperature&Hour Effects on Rentals"){
      
      dataN <-(sumDataPre %>%
                 mutate(x= wday(sumDataPre$date, label = TRUE)) %>%
                 group_by(hour, x) %>%
                   count ())
      dataN %>%
      ggplot(aes(hour, x, fill = n)) +
        geom_tile() +
        labs(x = "Hour of the day", y = "Day of the week") +
        scale_fill_distiller(palette = "Spectral") +
        ggtitle("Young age group sesonality effect for October")
    
        
      #pl <- pl + geom_point(position=position_jitter(h=0),aes(color=avgTmp),alpha=0.4)
     # pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
     # ggplotly(pl)
      #plot_ly(sumData, x = ~avgTmp, y = ~count, type="scatter", text = paste("Number of Rentals: ", sumData$count), mode = "markers", color = ~avgTmp, size = ~avgTmp)
    }
    
    
    
    
  }) 
  
 
  filterData <- reactive({
    # Month filter
    if (input$month == "January") {
      sumDataFilt <- sumDataPreSel %>% filter(., mon == "1")
    }
    if (input$month == "February") {
      sumDataFilt <- sumDataPreSel %>% filter(., mon == "2")
    }
    if (input$month == "March") {
      sumDataFilt <- sumDataPreSel %>% filter(., mon == "3")
    }
    if (input$month == "April") {
      sumDataFilt <- sumDataPreSel %>% filter(., mon == "4")
    }
    if (input$month == "May") {
      sumDataFilt <- sumDataPreSel %>% filter(mon == "5")
    }
    if (input$month == "June") {
      sumDataFilt <- sumDataPreSel %>% filter(mon == "6")
    }
    if (input$month == "July") {
      sumDataFilt <- sumDataPreSel %>% filter(mon == "7")
    }
    if (input$month == "August") {
      sumDataFilt <- sumDataPreSel %>% filter(mon == "8")
    }
    if (input$month == "September") {
      sumDataFilt <- sumDataPreSel %>% filter(mon == "9")
    }
    if (input$month == "October") {
      sumDataFilt <- sumDataPreSel %>% filter(mon == "10")
    }
    if (input$month == "November") {
      sumDataFilt <- sumDataPreSel %>% filter(mon == "11")
    }
    if (input$month == "December") {
      sumDataFilt <- sumDataPreSel %>% filter(mon == "12")
      class(sumDataFilt)
    }
    
    # Time filter
    mintime <- input$time[1]
    maxtime <- input$time[2]
    sumDataFilt <- sumDataFilt %>% filter(., hour >= mintime, hour <= maxtime)
    
    # Weekend filter
    if (input$weekend == "Weekends") {
      sumDataFilt <- sumDataFilt %>% filter(., weekend == TRUE)
    }
    if (input$weekend == "Weekdays") {
      sumDataFilt <- sumDataFilt %>% filter(., weekend == FALSE)
    }
    
    # Rain filter
    
    if (input$rain == "Heavy Rain") {
      sumDataFilt <- sumDataFilt %>% filter(., precipitation > 0, precipitation <= 0.3 )
    }

    if (input$rain == "Light Rain") {
      sumDataFilt <- sumDataFilt %>% filter(., precipitation > 0.7)
    }
    
    # Temp filter
    if (input$temp == "Hot") {
      sumDataFilt <- sumDataFilt %>% filter(., maxTmp > 70)
    }
    # Temp filter
    if (input$temp == "Cold") {
      sumDataFilt <- sumDataFilt %>% filter(., minTmp < 50)
    }
    
    sumDataFilt <- as.data.frame(sumDataFilt)
  })
  

  
  showHeatmaps <-  eventReactive(input$show, {
  
    
    dataHeat<- filterData()
    centerLatH = mean(dataHeat$stLat)
    centerLonH = mean(dataHeat$stLon)
    leaflet(dataHeat) %>% 
      setView(lng = centerLonH, lat = centerLatH, zoom = 12) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addHeatmap(lng = dataHeat$stLon, lat = dataHeat$stLat,
                 max = 100, gradient = "Reds", radius = 12, 
                 minOpacity = 0.25, blur = 3) 


  })
  

  output$heatM <- renderLeaflet({
    showHeatmaps()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

