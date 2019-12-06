

#' Append metadata to the dendrogrm's labels
#'
#' @param id  namespace id
#'
#' @return shiny UImodule
#' @export
#'
appendDendLabsUI <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("absPanelmodAppendDendLabels"))
  
}



#' modAppendDendLabels_WellPanel UI
#'
#' @param ns shiny namespace
#'
#' @return shiny ui
#' @export
#'
modAppendDendLabels_WellPanel <- function(ns){
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
                       fluidRow(p("sdsd"),
                                uiOutput(ns("appendmetaColumns"))
                                
                       ),
                       
                       shiny::actionButton(ns("closeModAppendDendLabels"),
                                           "Close")
                     ))
  )
}


