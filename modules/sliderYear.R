sliderPopYearUI <- function(id) {
  tagList(
    selectInput(NS(id,"integer"), "Jahre:",
                choices = wppYear,
                selected = "2020"),
    
  )
  
}