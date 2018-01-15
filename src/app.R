# Author: Johannes Harmse
# Date Created: 14-01-2018
# Last Modified: 14-01-2018

library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(DT)

ui <- fluidPage(
  h2("Google Chrome History Assistant"), 
  br(), 
  fluidRow(
    column(3, 
           fileInput("file1", "Choose Personal Google Files",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
           ), 
           h3("Filter Criteria"), 
           dateRangeInput('dateRange',
                          label = 'Date range input:',
                          start = Sys.Date() - 365, end = Sys.Date()
           ), 
           sliderInput("range", "Time Range (Hours of Day)",
                       min = 0, max = 24,
                       value = c(0,24)), 
           checkboxGroupInput("variable", "Days of Week:",
                              c("Monday" = "mon",
                                "Tuesday" = "tues",
                                "Wednesday" = "wed", 
                                "Thursday" = "thu", 
                                "Friday" = "fri", 
                                "Saturday" = "sat", 
                                "Sunday" = "sun")), 
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
           
           
           
#            
#            #dataTableOutput('table')
#            h3(), 
#            h3("Browsing Locations"), 
#            br(), 
#            h4("Click on a pin for browser summary"), 
#            br(), 
#            
#            br(), 
#            
#            
#            
#            
#            
#             
#            plotOutput("bars", click = "plot_click")
#     )
#   )
# )


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
  
  output$top_websites <- DT::renderDataTable(data.frame("Top_Websites" = c("Facebook", "RStudio", "YouTube")))
  
  
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
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  
  
  #output$dateRangeText  <- renderText({
  #  paste("input$dateRange is", 
  #        paste(as.character(input$dateRange), collapse = " to ")
  #  )
  #})
  
}

shinyApp(ui, server)