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
                     "Select Chrome Location JSON File",
                     accept = c("text/json", 
                                "json", ".json")
           ),  
           h3("Filter Criteria"), 
           selectizeInput('timezone', label = 'Select time zone', choices = OlsonNames(), selected = "GMT", multiple = FALSE), 
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
           actionButton("proceed", "Proceed")
    ), 
    column(9, 
    fluidRow(column(4, 
                    textInput('search_terms', 'Search phrases', placeholder = 'Add a search phrase'), 
                    actionButton('add_term', 'Add phrase'), 
                    DT::dataTableOutput('search_term_table'), 
                    actionButton('remove_terms', 'Remove selected terms from search'), 
           DT::dataTableOutput('top_websites'), # https://yihui.shinyapps.io/DT-rows/
           DT::dataTableOutput('webpages')
           ), 
    
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
      
      attr(temp$time, "tzone") <- input$timezone
      
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
      
      attr(temp$time, "tzone") <- input$timezone
      
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
  
  date_range_df <- eventReactive(input$proceed, {
    
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
  
  hour_range <- eventReactive(input$proceed, {
    
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
  
  days_df <- eventReactive(input$proceed, {
    
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
  
  browser_locations <- eventReactive(input$proceed, {
    if (length(input$history_json) > 0 && length(input$location_json) > 0){
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

          if (nrow(locations_day)){
            locations_day <- locations_day %>%
              group_by(year, year_day, hour) %>%
              summarise(mean_lat = ifelse(n() > 0, nth(x = lat, n = ceiling(n()/2)), lat),
                        mean_long = ifelse(n() > 0, nth(x = long, n = ceiling(n()/2)), long))

            if (nrow(location_filt) == 0){
              location_filt <- locations_day
            }else if(nrow(locations_day) > 0){
              #locations_day <- locations_day %>% filter(mean_lat)
              location_filt <- rbind(location_filt, locations_day)
            }
          }  



        }

      }
      
      for (row in 1:nrow(location_filt)){
        if (is.na(location_filt[row , 'mean_lat'])){
          location_filt[row , 'mean_lat'] <- location_filt[row - 1, 'mean_lat']
          location_filt[row , 'mean_long'] <- location_filt[row - 1, 'mean_long']
        }
      }
      
      return(location_filt)
    }
  })
  
  
  top_websites <- eventReactive(input$proceed, {
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
  
  top_websites_df <- eventReactive(input$proceed, {
    if (length(input$history_json) > 0 && 
        length(input$location_json) > 0){
      websites <- top_websites()
      locations <- browser_locations()
      # locations <- location()
      #location_filt <- websites[0, ]
      websites <- left_join(websites, locations, by = c('year', 'year_day', 'hour'))
      for (row in 1:nrow(websites)){
        if (is.na(websites[row, "mean_lat"])){
          websites[row, "mean_lat"] = websites[row-1, "mean_lat"]
          websites[row, "mean_long"] = websites[row-1, "mean_long"]
        }
      }
      
      websites$id = 1:nrow(websites)
      
      return(websites)
      
    }else{
      return(data_frame('mean_lat' = numeric(0), 'mean_long' = numeric(0)))
    }
  })
  
  websites_display_df <- eventReactive(input$proceed, {
    if (!is.null(input$history_json) && 
        !is.null(input$location_json)){
      websites_full <- top_websites_df()
      websites_display <- websites_full %>% 
        mutate(title = tolower(iconv(title, to = "ASCII//TRANSLIT"))) %>% 
        select(title, year, hour)
      return(websites_display)
    }else{
      return(data_frame('Websites' = c('No data available')))
    }
  })
  
  # https://yihui.shinyapps.io/DT-rows/
  
  ## output$top_websites <- DT::renderDataTable(data.frame("Top_Websites" = c("Facebook", "RStudio", "YouTube")))
  output$top_websites <- DT::renderDataTable({
    if (!is.null(search_list$terms) && 
        length(search_list$terms) > 0){
      websites_display_df() %>% 
        filter(grepl(pattern = paste0(search_list$terms, collapse = '|'), 
                     x = title, ignore.case = TRUE))
    }else if(is.null(input$history_json) || 
             is.null(input$location_json) || 
             input$proceed == 0){
      data_frame('Webpages' = c('No data available'))
    }else{
      websites_display_df()
    }
    }, 
      options = list(pageLength = 5, search = list(regex = TRUE, caseInsensitive = FALSE)))
  
  output$bars <- renderPlot({ggplot(data.frame("Top_Words" = c("Python", "R", "Despacito",
                                                               "definition", "for", "loop",
                                                               "UBC", "Vancouver", "YouTube", "programming"),
                                               "Count" = c(121, 99, 98, 67, 55, 52, 27, 25, 20, 15)), 
                                    aes(x = Top_Words, y = Count, fill = Count)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Most Frequent Words - Click on bars to exclude/include related webpages", 
           x = "Top occurring words", y = "Number of occurrances")})
  
  
  # https://yihui.shinyapps.io/DT-radio/
  output$webpages <- DT::renderDataTable({clicks_df()
    }, options = list(pageLength = 5))
  
  map <- reactive({ #(!is.null(search_list$terms) || (length(input$history_json) > 0 && length(input$location_json) > 0)), {
    #((!is.null(input$top_websites_search) && 
    #                                            any(unlist(input$top_websites_search) != "") && 
    #                                            length(unlist(input$top_websites_search)) > 0)), {
    if (length(input$history_json) > 0 && 
        length(input$location_json) > 0 && 
        input$proceed > 0){
      
      if (length(search_list$terms) > 0){
        df <- top_websites_df() %>% 
          filter(grepl(pattern = paste0(search_list$terms, collapse = '|'), x = title, ignore.case = TRUE))
      }else{
        df <- top_websites_df()
      }
        
      return(leaflet(df, width=500, height=400) %>% addTiles() %>% 
               #setView(lng = centre_long, lat = centre_lat, zoom = 18) %>% 
              #fitBounds(min_long, min_lat, max_long, max_lat) %>% 
               addMarkers(lng = ~mean_long, lat = ~mean_lat, clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = FALSE), clusterId = ~id))
    }else{
      return(leaflet(quakes, width=500, height=400) %>% addTiles() %>%
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
  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  clicks_df <- reactive({
    if (!is.null(input$history_json) && 
        !is.null(input$location_json) && 
        input$proceed > 0){
      bounds <- plot_boundaries()
      websites_bounds <- top_websites_df()
      websites_bounds <- websites_bounds %>%
        filter(mean_long >= bounds$long[1] &
                 mean_long <= bounds$long[2] &
                 mean_lat >= bounds$lat[1] &
                 mean_lat <= bounds$lat[2])
      
      #websites_bounds <- data_frame('long' = unlist(list(0, unlist(bounds$long))), 'lat' = unlist(list(0, bounds$lat)))
      
    #}
    # if (!is.null(data_of_click$clickedMarker)){
    #   websites_marker <- top_websites_df()
    #   websites_marker <- websites_marker[unlist(data_of_click$clickedMarker), ]
    }else{
      websites_bounds <- data_frame('Websites' = c('No data available'))
    }
    return(websites_bounds)
  })
  
  plot_boundaries <- eventReactive(input$map_bounds, {
    #map <- map()
    if (!is.null(input$map_bounds)){
      
      lat_south <- input$map_bounds['south']
      lat_north <- input$map_bounds['north']
      lng_west <- input$map_bounds['west']
      lng_east <- input$map_bounds['east']
      
      return(list('lat' = c(lat_south, lat_north),  'long' = c(lng_west, lng_east)))
    }
    
  })
  
  search_list <- reactiveValues(terms = NULL)
  
  observeEvent(input$add_term, {
    search_list$terms <- unlist(list(search_list$terms, input$search_terms))
  })
  
  observeEvent(input$remove_terms, {
    if (length(input$search_term_table_rows_selected) > 0){
      search_list$terms <- search_list$terms[-input$search_term_table_rows_selected]
    }
  })
  
  search_terms <- eventReactive(search_list$terms, {
    if (!is.null(search_list$terms)){
      search_df <- data_frame('Search terms' = search_list$terms)
    }else{
      search_df <- data_frame('Search terms' = c('No search phrases selected'))
    }
    
    return(search_df)
    
  })
  
  output$search_term_table <- DT::renderDataTable({search_terms()}, options = list(pageLength = 5, search = list(regex = TRUE, caseInsensitive = FALSE)))
  
  
  
  
  
}

shinyApp(ui, server)