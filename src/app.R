# Author: Johannes Harmse
# Date Created: 14-01-2018
# Last Modified: 26-01-2018

# load libraries
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(DT)
library(jsonlite)
library(lubridate)
library(stringr)
library(shinydashboard)

# increase bundle and upload file size limit
options(shiny.maxRequestSize=3000*1024^2)
options(rsconnect.max.bundle.size=3145728000)

# initialising variables
time_range_start <- NULL
time_range_end <- NULL
history_default <- readRDS(file = 'data/history.rds')
location_default <- readRDS(file = 'data/location.rds')

# dashboard sidebar
sidebar <- dashboardSidebar(
  tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
    ), 
  fileInput("history_json",
                     h6("Select Browser History JSON File"),
                    accept = c("text/json",
                               "json", ".json")
           ),
           fileInput("location_json",
                     h6("Select Location JSON File"),
                     accept = c("text/json",
                                "json", ".json")
           ),
           h4("Filter Criteria"),
           selectizeInput('timezone', label = h6('Time zone'), choices = OlsonNames(), selected = "GMT", multiple = FALSE),
           uiOutput("dates"),
           sliderInput("time", h6("Time Range (Hours of Day)"),
                       min = 0, max = 24,
                       value = c(0,24)),
           checkboxGroupInput("days", h6("Days of Week:"),
                              choiceNames = list(h6('Monday'),
                                                 h6('Tuesday'),
                                                 h6('Wednesday'),
                                                 h6('Thursday'),
                                                 h6('Friday'),
                                                 h6('Saturday'),
                                                 h6('Sunday')),
                              choiceValues = list("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                              selected = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
           actionButton("proceed", "Proceed")
)

# dashboard body
body <- dashboardBody(tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
    ),#theme = 'bootstrap.css', 
  fluidRow(column(3,
                    textInput('search_terms', h6('Search phrases'), placeholder = 'Add a search phrase'),
                    actionButton('add_term', 'Add phrase'),
                    br(),
                    br(),
                    DT::dataTableOutput('search_term_table'),
                    br(),
                    actionButton('remove_terms', 'Remove phrases')
           ),

    column(6,
           leafletOutput("map"))),

    br(),

    fluidRow(column(9, tabsetPanel(
      tabPanel("Top 10", plotOutput("bars", click = "plot_click")),
      tabPanel("Summary",DT::dataTableOutput('top_websites')),
      tabPanel("Details",DT::dataTableOutput('webpages'))))
             )
)

ui <- dashboardPage(
  dashboardHeader(title = 'Browser History Assistant', titleWidth = 450),
  sidebar,
  body
)

# server
server <- function(input, output, session) {
  
  # instructions
  observeEvent(input$proceed, {
    if (input$proceed == 0){ 
      data_path$modal_closed <- F
      showModal(modalDialog(
          h1("How to get started"),
          br(), 
          paste0("You can use sample data to explore the app. This is recommended for first time users."), 
          br(), 
          br(), 
          paste0("Alternatively use the file upload buttons in the sidebar menu to use your personal data."), 
          br(), 
          br(), 
          actionButton('default', 'Use Sample Data'), 
          # actionButton('personality', "Own Data"), 
          br(), 
          easyClose = TRUE
        ))
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$default,{
      data_path$modal_closed <- TRUE
      removeModal()
      showModal(modalDialog(
          h1("One Last Thing!"),
          br(), 
          paste0("Use the sidebar menu to filter the data according to your preferences. 
                 Click the 'Proceed' button at the bottom of the sidebar menu once you are ready. 
                 This will load the data for interaction."), 
          br(), 
          br(), 
          paste0("You can then use the interactive map and phrase search bar for further exploration."), 
          br(), 
          easyClose = TRUE
        ))
      
      
    })
  
  observe({
      if(data_path$modal_closed){
        data_path$default <- TRUE
      }
    })
  
  data_path <- reactiveValues(default = FALSE, modal_closed = F)
  data_df <- reactiveValues(history = NULL, location = NULL)
  
  observeEvent(data_path$default,  {
    if (data_path$default){                          
      if (length(data_default$history) > 0){
        temp <- history_default
        attr(temp$time, "tzone") <- input$timezone
        
        data_df$location <- location_default
        
        attr(data_df$location$time, "tzone") <- input$timezone
      
        data_df$location <- data_df$location %>% 
          mutate(year = year(time), 
                 year_day = yday(time))
        
      }else{
        temp <- data_frame('time' = character(0), 'title' = character(0), 'url' = character(0))
      }
      
      data_df$history <- temp
      # return(temp)
    }
    
  }, ignoreNULL = TRUE)
  
  # default path
  data_default <- list('history' = c('https://raw.githubusercontent.com/johannesharmse/Shiny_Google_Browser_History/master/data/sample%20Browser%20History.json'), 
                       'location' = c('https://raw.githubusercontent.com/johannesharmse/Shiny_Google_Browser_History/master/data/sample%20Location%20History.json'))
  
  # load user browser history file
  history <- observeEvent(input$history_json, {
                               
    if (length(input$history_json) > 0){
      temp <- fromJSON(input$history_json$datapath)$`Browser History`
      colnames(temp) <- c('icon', 'page_transition', 'title', 'url', 'id', 'time')
      temp <- temp %>% select(time, title, url)
      
      # clean data
      temp <- temp %>% 
        mutate(time = as_datetime(as.numeric(time)/1000000))
      
      attr(temp$time, "tzone") <- input$timezone
      
    }else{
      temp <- data_frame('time' = character(0), 'title' = character(0), 'url' = character(0))
    }
    
    data_df$history <- temp
    #return(temp)
    
  }, ignoreNULL = FALSE)
  
  location <- observeEvent(input$location_json, {
    if (length(input$location_json) > 0){
      temp <- fromJSON(input$location_json$datapath)$locations %>% select(1:3)
      colnames(temp) <- c('time', 'lat', 'long')
      #temp <- temp %>% select(time, lat, long)
      
      # clean data
      
      temp <- temp %>% 
        mutate(time = as_datetime(as.numeric(time)/1000), 
               long = long/10^7, 		
               lat = lat/10^7)
      
      attr(temp$time, "tzone") <- input$timezone
      
      temp <- temp %>% 
        mutate(year = year(time), 
               year_day = yday(time))
      
      
    }else{
      temp <- data_frame('time' = character(0), 'lat' = character(0), 'long' = character(0),
                         'year' = character(0), 'year_day' = character(0))
    }
    
    data_df$location <- temp
    #return(temp)
    
  })
  
  times <- reactiveValues(min = Sys.Date() - 365, max = Sys.Date())
  
  observeEvent({
    input$proceed}, {
    #input$history_json
    
    #data_df$history}, {
    #if (length(input$history_json) > 0){
    #  temp <- history()
    #  time_min <- min(temp$time)
    #  time_max <- max(temp$time)
    #}else 
    if(!is.null(data_df$history)){
      temp <- data_df$history
      time_min <- min(temp$time)
      time_max <- max(temp$time)
    }else{
      time_min <- Sys.Date() - 365
      time_max <- Sys.Date()
    }
      
    times$min <- time_min
    times$max <- time_max
      #return(c(time_min, time_max))
    
  })
  
  output$dates <- renderUI({
    #date_range <- time_range()
    dateRangeInput('dateRange',
                   label = h6('Date range input:'),
                   start = isolate(times$min), # date_range[1], 
                   end = as_datetime(isolate(times$max)) - days(1), #date_range[2] - days(1), 
                   min = isolate(times$min), #date_range[1], 
                   max = isolate(times$max) #date_range[2]
                   )
  })
  
  date_range_df <- eventReactive(input$proceed, {
    
    if (#(length(input$history_json) > 0 || 
         !is.null(data_df$history) && 
        length(input$dateRange) > 0){
      temp <- data_df$history
      #temp <- history()
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
    
    if (#length(input$history_json) > 0
      !is.null(data_df$history)){
      
      temp <- data_df$history
      #temp <- history()
      
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
    
    if (!is.null(data_df$history) && length(input$days) > 0){
      #length(input$history_json) > 0 && length(input$days) > 0){
      #temp <- history()
      temp <- data_df$history
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
    if (!is.null(data_df$history) && !is.null(data_df$location)){
    #if (length(input$history_json) > 0 && length(input$location_json) > 0){
      websites <- top_websites()
      #locations <- location()
      locations <- data_df$location
      
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
    if (!is.null(data_df$history) && !is.null(data_df$location)){
    #if (length(input$history_json) > 0 && 
    #    length(input$location_json) > 0){ #&& length(input$dates) > 0){
      
      #websites <- history()# %>% select(title)
      websites <- data_df$history
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
    if (!is.null(data_df$history) && !is.null(data_df$location)){
    #if (length(input$history_json) > 0 && 
    #    length(input$location_json) > 0){
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
  
  websites_display_df <- eventReactive(input$map_bounds, {
    if (!is.null(data_df$history) && !is.null(data_df$location) && 
    #if (!is.null(input$history_json) && 
    #    !is.null(input$location_json) && 
        input$proceed > 0 && 
        !is.null(plot_boundaries())){
      
      websites_full <- top_websites_df()
      bounds <- plot_boundaries()
      
      websites_full <- websites_full %>%
        filter(mean_long >= bounds$long[1] &
                 mean_long <= bounds$long[2] &
                 mean_lat >= bounds$lat[1] &
                 mean_lat <= bounds$lat[2])
      
      websites_display <- websites_full %>% 
        mutate(title = tolower(iconv(title, to = "ASCII//TRANSLIT"))) %>% 
        select(title, year, hour, url)
      
      
      if (nrow(websites_display) > 0){
      
        for(row in 1:nrow(websites_display)){
          if (is.na(websites_display[[row, 'url']])){
            websites_display[row, 'page'] <- NA
          }else{
            if(length(str_locate_all(websites_display[[row, 'url']], '//.*?/')[[1]]) > 0){
              if((str_locate_all(websites_display[[row, 'url']], '//.*?/')[[1]][1, 'end'] - 1) > 
                (str_locate_all(websites_display[[row, 'url']], '//.*?/')[[1]][1, 'start'] + 2)){
                end <- str_locate_all(websites_display[[row, 'url']], '//.*?/')[[1]][1, 'end'] - 1
              }else{
                end <- as.double(nchar(websites_display[[row, 'url']]))
              }
              
              websites_display[row, 'page'] <- substr(websites_display[[row, 'url']], 
                     (str_locate_all(websites_display[[row, 'url']], '//.*?/')[[1]][1, 'start'] + 2), 
                     end)
            }else{
              if(length(str_locate_all(websites_display[[row, 'url']], '//')[[1]]) > 0){
                websites_display[row, 'page'] <- substr(websites_display[[row, 'url']], 
                       str_locate_all(websites_display[[row, 'url']], '//')[[1]][1, 'start'] + 2, 
                       as.double(nchar(websites_display[[row, 'url']])))
              }else{
                websites_display[row, 'page'] <- websites_display[[row, 'url']]
              }
              
            }
          }
        }
        
      }
      
      return(websites_display)
      
    }else{
      return(data_frame('Websites' = c('No data available')))
    }
  })
  
  output$top_websites <- DT::renderDataTable({
    
  withProgress(message = 'Your table is being generated', value = NULL, {
    Sys.sleep(1)
  #})
    
    # progress <- shiny::Progress$new()
    # progress$set(message = "Your table is being generated", value = NULL)
    # on.exit(progress$close())  
    
      if (!is.null(search_list$terms) && 
          length(search_list$terms) > 0 && 
          !is.null(websites_display_df()) && 
          nrow(websites_display_df()) > 1){
        
        websites_display <- websites_display_df() %>%
          #clicks_df() %>% 
          filter(grepl(pattern = paste0(search_list$terms, collapse = '|'), 
                       x = title, ignore.case = TRUE))
        
        websites_display <- websites_display %>% 
          group_by(page) %>% 
          summarise(visits = n()) %>% 
          arrange(desc(visits))
        
      }else if(#!is.null(input$history_json) && 
               #!is.null(input$location_json) && 
              !is.null(data_df$history) && 
              !is.null(data_df$location) &&  
              !is.null(input$proceed) && 
               input$proceed > 0 && 
               !is.null(input$map_bounds) && 
               !is.null(websites_display_df()) && 
               nrow(websites_display_df()) > 1){
        #clicks_df()
        websites_display <- websites_display_df()
        
        websites_display <- websites_display %>% 
          group_by(page) %>% 
          summarise(visits = n()) %>% 
          arrange(desc(visits))
        
        #websites_display #%>% 
        ##select(time, title, url)
        
      }else{
        websites_display <- data_frame('Webpages' = c('No data available'))
      }
     })
    
    

    
    return(websites_display)
    
    }, 
      options = list(pageLength = 5, search = list(regex = TRUE, caseInsensitive = FALSE)))
  
  output$bars <- renderPlot({
    
    withProgress(message = 'Your graph is being generated', value = NULL, {
    Sys.sleep(1)
    #})
      
      if(#!is.null(input$history_json) && 
         #!is.null(input$location_json) && 
        !is.null(data_df$history) && 
        !is.null(data_df$location) &&  
        !is.null(input$proceed) && 
         input$proceed > 0 && 
         !is.null(input$map_bounds) && 
         !is.null(websites_display_df()) && 
         nrow(websites_display_df()) > 1){
        
        if (!is.null(search_list$terms) && 
            length(search_list$terms) > 0){
          websites_display <- websites_display_df() %>% 
            filter(grepl(pattern = paste0(search_list$terms, collapse = '|'), 
                         x = title, ignore.case = TRUE))
        }else{
          websites_display <- websites_display_df()
        }
        
        websites_plot <- websites_display %>% 
          group_by(page) %>% 
          summarise(visits = n()) %>% 
          arrange(desc(visits))
        
        websites_plot <- websites_plot[1:10, ]
        
        plot <- ggplot(data = websites_plot, aes(x = page, y = visits)) + 
          geom_bar(fill = 'turquoise', stat = "identity") + 
          labs(title = 'Most frequently visited websites', 
               x = 'Website', 
               y = 'Number of Visits')
        
        # return(plot)
        
      }else{
        plot <- ggplot()
        # return(ggplot())
      }
     })
    
    return(plot)
    
  })
  
  output$webpages <- DT::renderDataTable({
    
    withProgress(message = 'Your table is being generated', value = NULL, {
     Sys.sleep(1)
    #})
    
    # progress <- shiny::Progress$new()
    # progress$set(message = "Your table is being generated", value = NULL)
    # on.exit(progress$close())
      
      if (!is.null(search_list$terms) && 
          length(search_list$terms) > 0){
        websites_display <- websites_display_df() %>%
          filter(grepl(pattern = paste0(search_list$terms, collapse = '|'), 
                       x = title, ignore.case = TRUE))
        
        # websites_display
        
      }else if(#!is.null(input$history_json) && 
               #!is.null(input$location_json) && 
              !is.null(data_df$history) && 
              !is.null(data_df$location) &&  
              !is.null(input$proceed) && 
               (input$proceed > 0) && 
               !is.null(input$map_bounds) && 
               !is.null(websites_display_df()) && 
               nrow(websites_display_df()) > 1){
        
        websites_display <- websites_display_df()
        
        # websites_display
        
      }else{
        websites_display <- data_frame('Webpages' = c('No data available'))
      }
      
     })
    
    return(websites_display)

    }, options = list(pageLength = 5))
  
  map <- reactive({
    if (#length(input$history_json) > 0 && 
        #length(input$location_json) > 0 && 
      !is.null(data_df$history) && 
      !is.null(data_df$location) &&   
      input$proceed > 0){
      
      if (length(search_list$terms) > 0){
        df <- top_websites_df() %>% 
          filter(grepl(pattern = paste0(search_list$terms, collapse = '|'), x = title, ignore.case = TRUE))
      }else{
        df <- top_websites_df()
      }
        
      return(leaflet(df, width=500, height=400) %>% addTiles() %>% 
               addMarkers(lng = ~mean_long, lat = ~mean_lat, clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = FALSE), clusterId = ~id))
    }else{
      return(leaflet(data_frame('lat' = c(-33.9249), 'long' = c(18.4241)), width=500, height=400) %>% addTiles() %>%
               fitBounds(~min(long)-0.5, ~min(lat)-0.5, ~max(long)+0.5, ~max(lat)+0.5)
               )
    }
  })
  
  output$map <- renderLeaflet({
    
    withProgress(message = 'Your map is being generated', value = NULL, {
      Sys.sleep(1)
    #})
    
    # progress <- shiny::Progress$new()
    # progress$set(message = "Your map is being generated", value = NULL)
    # on.exit(progress$close())
    
    map <- map()
      
    })
      
    return(map)
    
  })
  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  clicks_df <- reactive({
    if (#!is.null(input$history_json) && 
        #!is.null(input$location_json) && 
      !is.null(data_df$history) && 
      !is.null(data_df$location) &&   
      input$proceed > 0){
      
      websites_bounds <- websites_display_df()

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
  
  search_terms <- reactive({
    if (!is.null(search_list$terms)){
      search_df <- data_frame(' ' = search_list$terms)
    }else{
      search_df <- data_frame(' ' = c('No search phrases'))
    }
    
    return(search_df)
    
  })
  
  output$search_term_table <- DT::renderDataTable({
    search_terms()
    }, options = list(pageLength = 5, dom = 't', paging = FALSE))
  
  
  
  
  
}

shinyApp(ui, server)