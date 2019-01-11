
#' colordendLinesUI
#'
#' @param id  namespace id
#'
#' @return shiny UImodule
#' @export
#'

colordendLinesUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::absolutePanel(
    bottom = "50%",
    right =  "0%",
    width = "20%",
    fixed = TRUE,
    draggable = TRUE,
    style = "z-index:1002;",


    shiny::wellPanel(
      shiny::h4("Adjust Dendrogram Lines"),
      shiny::selectInput(ns("colorBy"),
                         "Color By:",
                         c("None" = "none",
                           "Choose Number of Groups" = "groups",
                           "Color by cutting at height" = "height",
                           "Color by sample info" = "metadata"
                         ),
                         selected = "groups"
      ),
      shiny::conditionalPanel(
        condition = "input.colorBy == 'height'", ns = ns,
        shiny::numericInput(ns("cutHeight"),
                            label = shiny::h5(shiny::strong("Cut Tree at Height")),
                            value = 0,
                            step = 0.1,
                            min = 0)


      ),
      shiny::conditionalPanel(
        condition = "input.colorBy == 'groups'", ns = ns,
        shiny::numericInput(ns("chosenK"),
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

      shiny::actionButton("closeLineModification",
                          "Close")

    )
  )
}




#' colordendLines
#'
#' @param input a
#' @param output a
#' @param session a
#' @param dendrogram a
#' @param plotHeight a
#'
#' @return a
#' @export
#'

colordendLines <- function(input,
                           output,
                           session,
                           dendrogram,
                           plotHeight=500){

  dendrogram <- IDBacApp::changeDendPartColor(dendrogram = dendrogram,
                                          colorBy = input$colorBy,
                                          colorBlindPalette = colorBlindPalette(),
                                          cutHeight = input$cutHeight,
                                          chosenK = input$chosenK,
                                          part = "branches")

  dendrogram <- IDBacApp::changeDendPartSize(dendrogram = dendrogram,
                                   dendPartSize = input$dendLineWidth,
                                   part = "branches")

  return(dendrogram)

}










