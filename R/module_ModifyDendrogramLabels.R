#' Modify (color/size) dendrogram Labels
#'
#' @param id namespace id
#'
#' @return ui for coloring dendrogram labels
#' @export
#'

colordendLabelsUI <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("absPanelDendLabels"))
}



#' modDendLabels_WellPanel UI
#'
#' @param ns shiny namespace
#'
#' @return shiny ui
#' @export
#'
modDendLabels_WellPanel <- function(ns) {
  shiny::absolutePanel(
    class = "dendMod_WellPanel",
    bottom = "0%",
    right = "0%",
    width = "20%",
    fixed = TRUE,
    draggable = TRUE,
    style = "z-index:1002;",
    shiny::wellPanel(
      shiny::h4("Adjust Dendrogram Labels"),
      shiny::selectInput(ns("colorByLabels"),
                         "Color By:",
                         c("None" = "none",
                           "Choose Number of Groups" = "groups",
                           "Color by cutting at height" = "height"
                         ),
                         selected = "groups"
      ),
      shiny::conditionalPanel(
        condition = "input.colorByLabels == 'height'", ns = ns,
        shiny::numericInput(ns("cutHeightLabels"),
                            label = shiny::h5(shiny::strong("Cut Tree at Height")),
                            value = 0,
                            step = 0.1,
                            min = 0)
        
        
      ),
      shiny::conditionalPanel(
        condition = "input.colorByLabels == 'groups'", ns = ns,
        shiny::numericInput(ns("chosenKLabels"),
                            label = shiny::h5(shiny::strong("Choose the number of groups")),
                            value = 1,
                            step = 1,
                            min = 1)
      ),
      
      shiny::numericInput(ns("dendLabelSize"),
                          "Label Size",
                          value = 1,
                          min = 0,
                          max = 5,
                          step = .1
      ),
      
      shiny::actionButton(ns("closeDendLabels"),
                          "Close")
      
    )
  )
}

