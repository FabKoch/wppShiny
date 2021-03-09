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
                  "80-84"
                  ),
                multiple = TRUE,
                options = list(`multiple-separator` = " | ",`actions-box` = TRUE,
                               `deselect-all-text` = "Keine",
                               `select-all-text` = "Alle"),
                pickerOptions(
                  maxOptions = 4,
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
      
      updatePickerInput(session = session, 
                        inputId = ns("age"),
                        choices = wppAge)
      
      # TODO add fill based on HWB/mio
      # https://rstudio.github.io/leaflet/choropleths.html
      # bindata <- polygon()
      # 
      # bins <- cartography::getBreaks(
      #   bindata$pop_total,
      #   method = "equal")
      # 
      # pal <- colorBin("YlOrRd", domain = bindata$pop_total, bins = bins)

      
      # Map
      leaflet(polygon()) %>% 
      addTiles(tilesURL) %>% 
      addPolygons(
        fillColor = "gray",
        weight = 2,
        opacity = 0.7,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7) %>%
      addMinicharts(
        centroids()$x, centroids()$y,
        chartdata = centroidData(),
        colorPalette = d3.schemeCategory10,
        width = 45, height = 45,
        # funzt nicht, man kann mehr auswählen
        maxValues = 4)
      
    })
    

  })
}