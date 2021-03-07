mapMinichartUI <- function(id) {
  tagList(
    selectInput(NS(id,"integer"), 
                "Jahre:",
                choices = wppYear,
                selected = "2020"),
    
    pickerInput(NS(id, "var"),
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


histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(mtcars[[input$var]])
    
    output$mapMini <- renderLeaflet({
      
    })
    

  })
}