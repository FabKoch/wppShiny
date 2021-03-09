mapMinichartUI <- function(id) {
  tagList(
    selectInput(NS(id,"year"), 
                "Jahre:",
                choices = wppYear,
                selected = "2020"),
    
    pickerInput(NS(id, "age"),
                "Altersgruppen:",
                choices = wppAge,
                selected = c(
                  "0-4",
                  "40-44",
                  "60-64"),
                multiple = TRUE,
                options = list(`multiple-separator` = " | ",`actions-box` = TRUE,
                               `deselect-all-text` = "Keine",
                               `select-all-text` = "Alle"),
                pickerOptions(
                  maxOptions = 3,
                  maxOptionsText = "Maximum erreicht"
                )
    ),
    
    leafletOutput(NS(id, "mapMini"))
  )
  
}


mapMinichartServer <- function(id) {
  moduleServer(id, function(input, output, session){ 

    
    polygon <- reactive(map_polygon_EU %>% 
                          filter(year == input$year))
    
    centroidData <- reactive(map_centroid_EU %>% 
                               filter(year == input$year) %>% 
                               select(
                                 contains(input$age)))
    
    centroids <- reactive(map_centroid_EU %>% 
                            filter(year == input$year)) 
    

    
    
    output$mapMini <- renderLeaflet({
      
      ns <- session$ns
      
      updatePickerInput(session = session, inputId = ns("age"),
                        choices = wppAge)


      # Map
      leaflet(polygon()) %>% 
      addTiles(tilesURL) %>% 
      addPolygons() %>%
      addMinicharts(
        centroids()$x, centroids()$y,
        chartdata = centroidData(),
        colorPalette = d3.schemeCategory10,
        width = 45, height = 45,
        maxValues = 4)
      
    })
    

  })
}