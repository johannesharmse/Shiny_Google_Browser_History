# Author: Johannes Harmse
# Date Created: 14-01-2018
# Last Modified: 18-01-2018

library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(DT)
#library(rjson)
library(jsonlite)
library(lubridate)

options(shiny.maxRequestSize=100*1024^2) 

time_range_start <- NULL
time_range_end <- NULL

ui <- fluidPage(
  h2("Google Chrome History Assistant"), 
  br(), 
  fluidRow(
    column(3, 
           fileInput("history_json", 
                     "Select Chrome History JSON File",
                    accept = c("text/json", 
                               "json", ".json")
           ), 
           fileInput("location_json", 
                     "Select Chrome Location JOSN File",
                     accept = c("text/json", 
                                "json", ".json")
           ),  
           h3("Filter Criteria"), 
           uiOutput("dates"), 
           sliderInput("time", "Time Range (Hours of Day)",
                       min = 0, max = 24,
                       value = c(0,24)), 
           checkboxGroupInput("days", "Days of Week:",
                              c("Monday" = "Mon",
                                "Tuesday" = "Tue",
                                "Wednesday" = "Wed", 
                                "Thursday" = "Thu", 
                                "Friday" = "Fri", 
                                "Saturday" = "Sat", 
                                "Sunday" = "Sun"), 
                              selected = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
           actionButton("Proceed", "Proceed")
    ), 
    column(9, 
    fluidRow(column(4, 
           DT::dataTableOutput('top_websites'), # https://yihui.shinyapps.io/DT-rows/
           DT::dataTableOutput('webpages')), 
    
    column(5, 
           leafletOutput("map"))), 
    
    br(), 
    
    fluidRow(column(9, 
                     plotOutput("bars", click = "plot_click"))
             )
    
  )
  )
)


# ui <- bootstrapPage(
#   tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#   # leafletOutput("map", width = "100%", height = "100%"),
#   fluidPage(fluidRow(top = 10, right = 10, 
#                 dataTableOutput('table')
#                 )) 
#absolutePanel(top = 10, right = 10,
#sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
#            value = range(quakes$mag), step = 0.1
#),
#selectInput("colors", "Color Scheme",
#            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
#),
#checkboxInput("legend", "Show legend", TRUE), 
#fileInput("file1", "Choose Personal Google Files",
#          accept = c(
#            "text/csv",
#            "text/comma-separated-values,text/plain",
#            ".csv")
#)
#), 
# dataTableOutput('table')
#)

server <- function(input, output, session) {
  
  history <- reactive({
    if (length(input$history_json) > 0){
      temp <- fromJSON(input$history_json$datapath)
      temp <- temp$`Browser History`
      colnames(temp) <- c('icon', 'page_transition', 'title', 'url', 'id', 'time')
      temp <- temp %>% select(time, title, url)
      
      # clean data
      
      temp <- temp %>% 
        mutate(time = as_datetime(as.numeric(time)/1000000))
      
    }else{
      temp <- data_frame('time' = character(0), 'title' = character(0), 'url' = character(0))
    }
    
    
    return(temp)
    
  })
  
  location <- reactive({
    if (length(input$location_json) > 0){
      temp <- fromJSON(input$location_json$datapath)
      temp <- temp$locations
      colnames(temp) <- c('time', 'lat', 'long', 'a', 'b', 'c', 'd', 'e')
      temp <- temp %>% select(time, lat, long)
      
      # clean data
      
      temp <- temp %>% 
        mutate(time = as_datetime(as.numeric(time)/1000), 
               long = long/10^7, 		
               lat = lat/10^7)
      
      temp <- temp %>% 
        mutate(year = year(time), 
               year_day = yday(time))
      
      # temp <- temp %>% 
      #   arrange(time) %>% 
      #   mutate(day = ymd(substr(as.character(time), 1, 10))) #%>% 
      #   # select(time, lat, long, day)
      # 
      # temp <- temp %>% 
      #   distinct(lat, long, .keep_all = TRUE)
      
      
      # as.numeric(as.duration(as.interval(as_datetime(as.numeric(test[[1,1]])/1000) - as_datetime(as.numeric(test[[1000,1]])/1000), start = as_datetime(as.numeric(test[[1,1]])/1000))), "minutes")
    }else{
      temp <- data_frame('time' = character(0), 'lat' = character(0), 'long' = character(0), 
                         'year' = character(0), 'year_day' = character(0))
    }
    
    return(temp)
    
  })
  
  time_range <- reactive({
    if (length(input$history_json) > 0){
      temp <- history()
      time_min <- min(temp$time)
      time_max <- max(temp$time)
    }else{
      time_min <- Sys.Date() - 365
      time_max <- Sys.Date()
    }
    return(c(time_min, time_max))
  })
  
  output$dates <- renderUI({
    date_range <- time_range()
    dateRangeInput('dateRange',
                   label = 'Date range input:',
                   start = date_range[1], 
                   end = date_range[2] - days(1), 
                   min = date_range[1], 
                   max = date_range[2]
                   )
  })
  
  date_range_df <- reactive({
    
    if (length(input$history_json) > 0 && length(input$dateRange) > 0){
      temp <- history()
      temp <- temp %>% filter(time >= input$dateRange[1] & time <= input$dateRange[2])
      temp <- temp %>% 
        mutate(year = year(time), 
               year_day = yday(time))
      return(temp)
      
    }else{
      return(data_frame('fail' = character(0)))
    }
    
  })
  
  hour_range <- reactive({
    
    if (length(input$history_json) > 0){
      temp <- history()
      
      temp <- temp %>% 
        mutate(year = year(time), 
               year_day = yday(time))
      
      hour_filt <- temp[0, ]
      
      for (y in 1:length(unique(temp$year))){
        temp_year <- temp %>% filter(year == unique(temp$year)[y])
        for (day in 1:length(unique(temp_year$year_day))){
          temp_day <- temp_year %>% filter(year_day == unique(temp_year$year_day)[day])
          temp_day <- temp_day %>% filter(hour(time) >= input$time[1] & hour(time) < input$time[2])
          
          if (nrow(hour_filt) == 0){
            hour_filt <- temp_day
          }else{
            hour_filt <- rbind(hour_filt, temp_day)
          }
          
        }
      }
      
      
      # hour_filt <- hour_filt %>% select(-year, -year_day)
      
      return(hour_filt)
      
    }
    
  })
  
  days_df <- reactive({
    
    if (length(input$history_json) > 0 && length(input$days) > 0){
      temp <- history()
      temp <- temp %>% filter(wday(time, label = TRUE) %in% input$days)
      temp <- temp %>% 
        mutate(year = year(time), 
               year_day = yday(time))
      return(temp)
      
    }else{
      return(data_frame('fail' = character(0)))
    }
    
  })
  
  browser_locations <- reactive({
    if (length(input$history_json) > 0 && length(input$location_json)){
      websites <- top_websites()
      locations <- location()
      
      location_filt <- websites[0, ]
      
      for (y in 1:length(unique(websites$year))){
        temp_year <- websites %>% filter(year == unique(websites$year)[y])
        for (day in 1:length(unique(temp_year$year_day))){
          temp_day <- temp_year %>% filter(year_day == unique(temp_year$year_day)[day])
          # temp_day <- temp_day %>% filter(hour(time) >= input$time[1] & hour(time) < input$time[2])

          locations_day <- locations %>% mutate(hour = hour(time))
          locations_day <- locations_day %>%
            filter(year %in% temp_day$year &
            year_day %in% temp_day$year_day &
            hour %in% hour(temp_day$time))

          locations_day <- locations_day %>%
            group_by(year, year_day, hour) %>%
            summarise(mean_lat = nth(x = lat, n = as.integer(n()/2)),
                      mean_long = nth(x = long, n = as.integer(n()/2)))

          if (nrow(location_filt) == 0){
            location_filt <- locations_day
          }else{
            location_filt <- rbind(location_filt, locations_day)
          }



        }

      }
      
      return(location_filt)
    }
  })
  
  
  top_websites <- reactive({
    if (length(input$history_json) > 0 && 
        length(input$location_json) > 0){ #&& length(input$dates) > 0){
      websites <- history()# %>% select(title)
      dates_filter <- date_range_df()
      days_filter <- days_df()
      hours_filter <- hour_range()
      websites <- semi_join(hours_filter, dates_filter, by = c('year', 'year_day'))
      websites <- semi_join(websites, days_filter, by = c('year', 'year_day'))
      websites <- websites %>% mutate(hour = hour(time))
      return(websites)
    }
  })
  
  top_websites_df <- reactive({
    if (length(input$history_json) > 0 && 
        length(input$location_json) > 0){
      websites <- top_websites()
      locations <- browser_locations()
      # locations <- location()
      #location_filt <- websites[0, ]
      websites <- left_join(websites, locations, by = c('year', 'year_day', 'hour'))
      return(websites)
    }else{
      return(data_frame('mean_lat' = numeric(0), 'mean_long' = numeric(0)))
    }
  })
  
  websites_display_df <- reactive({
    if (length(input$history_json) > 0 && 
        length(input$location_json) > 0){
      websites_full <- top_websites_df()
      websites_display <- websites_full %>% 
        select(title, year, hour)
      return(websites_display)
    }
  })
  
  # # Reactive expression for the data subsetted to what the user selected
  # filteredData <- reactive({
  #   quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  # })
  # 
  # # This reactive expression represents the palette function,
  # # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, quakes$mag)
  # })
  # 
  # output$map <- renderLeaflet({
  #   # Use leaflet() here, and only include aspects of the map that
  #   # won't need to change dynamically (at least, not unless the
  #   # entire map is being torn down and recreated).
  #   leaflet(quakes) %>% addTiles() %>%
  #     fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  # })
  # 
  # # Incremental changes to the map (in this case, replacing the
  # # circles when a new color is chosen) should be performed in
  # # an observer. Each independent set of things that can change
  # # should be managed in its own observer.
  # observe({
  #   pal <- colorpal()
  #   
  #   leafletProxy("map", data = filteredData()) %>%
  #     clearShapes() %>%
  #     addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
  #                fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste0("<h1>", mag, "</h1>")
  #     )
  # })
  # 
  # # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("map", data = quakes)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomright",
  #                         pal = pal, values = ~mag
  #     )
  #   }
  # })
  
  #output$table <- renderDataTable(iris)
  
  # https://yihui.shinyapps.io/DT-rows/
  
  ## output$top_websites <- DT::renderDataTable(data.frame("Top_Websites" = c("Facebook", "RStudio", "YouTube")))
  output$top_websites <- DT::renderDataTable({websites_display_df()}, options = list(pageLength = 5))
  
  output$bars <- renderPlot({ggplot(data.frame("Top_Words" = c("Python", "R", "Despacito",
                                                               "definition", "for", "loop",
                                                               "UBC", "Vancouver", "YouTube", "programming"),
                                               "Count" = c(121, 99, 98, 67, 55, 52, 27, 25, 20, 15)), 
                                    aes(x = Top_Words, y = Count, fill = Count)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Most Frequent Words - Click on bars to exclude/include related webpages", 
           x = "Top occurring words", y = "Number of occurrances")})
  
  
  # https://yihui.shinyapps.io/DT-radio/
  output$webpages <- DT::renderDataTable(data.frame("Webpages" = c("https://www.quora.com/profile/Johannes-Harmse", 
                                                             "https://shiny.rstudio.com/gallery/",
                                                            "https://www.youtube.com/watch?v=Av3PDFBwVKs",
                                                             "https://stats.stackexchange.com/questions/239890/how-to-plot-the-log-likelihood-associated-with-each-iteration-of-em-algorithm-an/239927",
                                                            "https://github.com/johannesharmse",
                                                            "https://en.wikipedia.org/wiki/Akaike_information_criterion")), 
                                         escape = FALSE, 
                                         selection = 'none', 
                                         server = TRUE, 
                                         options = list(dom = 't', 
                                                        paging = TRUE, 
                                                        odering = TRUE), 
                                         callback = JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
                                                       $this.attr('id', this.data()[0]);
                                                       $this.addClass('shiny-input-radiogroup');
});
                                                       Shiny.unbindAll(table.table().node());
                                                       Shiny.bindAll(table.table().node());")
  )
  
  
  
  # output$sel = renderPrint({
  #  str(sapply(month.abb, function(i) input[[i]]))
  #})
  
  map <- reactive({
    if (length(input$history_json) > 0 && 
        length(input$location_json) > 0){
      return(leaflet(top_websites_df()) %>% addTiles() %>%
              fitBounds(~min(mean_long), ~min(mean_lat), ~max(mean_long), ~max(mean_lat)) %>% 
               addMarkers(lng = ~mean_long, lat = ~mean_lat, clusterOptions = markerClusterOptions()))
    }else{
      return(leaflet(quakes) %>% addTiles() %>%
               fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)))
    }
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    map()
    # leaflet(map()) %>% addTiles() %>%
    #   fitBounds(~min(mean_long), ~min(mean_lat), ~max(mean_long), ~max(mean_lat))
  })
  
  observe({
    # pal <- colorpal()

    #leafletProxy("map", data = top_websites_df()) %>%
    #  clearShapes() %>%
    #  addMarkers(lng = ~mean_long, lat = ~mean_lat, #color = "#777777",
                 # fillColor = ~pal(mag),
                 #fillOpacity = 0.7# , 
                 # popup = ~paste0("<h1>", mag, "</h1>"
    #             clusterOptions = markerClusterOptions())
      #)
  })
  
  
}

shinyApp(ui, server)