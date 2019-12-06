#' colordendLinesUI
#'
#' @param id  namespace id
#'
#' @return shiny UImodule
#' @export
#'

colordendLinesUI <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("absPanelDendLines"))
  
}


#' modDendLines_WellPanel UI
#'
#' @param ns shiny namespace
#'
#' @return shiny ui
#' @export
#'
modDendLines_WellPanel <- function(ns){
  shiny::absolutePanel(
    class = "dendMod_WellPanel",
    bottom = "50%",
    right =  "0%",
    width = "20%",
    fixed = TRUE,
    draggable = TRUE,
    style = "z-index:1002;",
    shiny::wellPanel(
      shiny::h4("Adjust Dendrogram Lines"),
      shiny::selectInput(ns("colorByLines"),
                         "Color By:",
                         c("None" = "none",
                           "Choose Number of Groups" = "groups",
                           "Color by cutting at height" = "height"
                         ),
                         selected = "groups"
      ),
      shiny::conditionalPanel(
        condition = "input.colorByLines == 'height'", ns = ns,
        shiny::numericInput(ns("cutHeightLines"),
                            label = shiny::h5(shiny::strong("Cut Tree at Height")),
                            value = 0,
                            step = 0.1,
                            min = 0)
        
        
      ),
      shiny::conditionalPanel(
        condition = "input.colorByLines == 'groups'", ns = ns,
        shiny::numericInput(ns("chosenKLines"),
                            label = shiny::h5(shiny::strong("Choose the number of groups")),
                            value = 1,
                            step = 1,
                            min = 1)
      ),
      
      shiny::numericInput(ns("dendLineWidth"),
                          "Line Width",
                          value = 1,
                          min = 1,
                          max = 10,
                          step = 1
      ),
      
      shiny::actionButton(ns("closeDendLines"),
                          "Close")
      
    )
  )
  
}