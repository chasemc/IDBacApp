#' Modify (color/size) dendrogram lines
#'
#' @param id  namespace id
#'
#' @return shiny UImodule
#' @export
#'

dendDotsUI <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("absPaneldendDots"))
  
}



#' modDendDotsMod_WellPanel UI
#'
#' @param ns shiny namespace
#'
#' @return shiny ui
#' @export
#'
modDendDotsMod_WellPanel <- function(ns) {
  shiny::absolutePanel(
    class = "dendMod_WellPanel",
    top = "30%",
    right =  "0%",
    width = "30%",
    fixed = F,
    draggable = T,
    style = "z-index:1002;",
    shiny::wellPanel(class = "dendDots_WellPanel",
                     fluidRow(
                       fluidRow(shiny::actionButton(ns("closeDendDots"),
                                                    "Close"),
                                shiny::actionButton(ns("removeDendDots"),
                                                    "Clear"),
                                uiOutput(ns("metaColumns")),
                                uiOutput(ns("sampleFactorMapColors")))
                     )
    ))
}