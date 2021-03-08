mapMinichartUI <- function(id) {
  tagList(
    selectInput(NS(id,"integer"), 
                "Jahre:",
                choices = wppYear,
                selected = "2020"),
    
    pickerInput(NS(id, "age"),
                "Altersgruppen:",
                choices = wppAge,
                selected = c(
                  "0-4",
                  "30-34",
                  "60-64"),
                multiple = TRUE,
                options = list(`multiple-separator` = " | ",`actions-box` = TRUE,
                               `deselect-all-text` = "Keine",
                               `select-all-text` = "Alle")
    )
  )
  
}


mapMinichartServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data <- reactive(data_map_europe  %>% 
                       filter(age == input$age) 
                     )
                     
    polygon <- reactive(map_polygon_EU %>% 
                          filter(year == input$year)
                        )
    
    centroidData <- reactive(map_centroid_EU %>% 
                           filter(year == input$year) %>% 
                           select(
                             contains("-")) 
                         )
    
    centroids <- reactive(map_centroid_EU %>% 
                               filter(year == input$year)
                         ) 
    
    centroid <- centroids()
    
    output$mapMini <- renderLeaflet({
      polygon() %>% 
      addTiles(tilesURL) %>% 
      addPolygons() %>% 
      addMinicharts(
        centroid$x, centroid$y,
        chartdata = centroidData(),
        colorPalette = d3.schemeCategory10,
        width = 45, height = 45)
      
    })
    

  })
}