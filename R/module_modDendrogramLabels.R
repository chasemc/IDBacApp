#' colordendLabelsUI
#'
#' @param id namespace id
#'
#' @return ui for coloring dendrogram labels
#' @export
#'

colordendLabelsUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::absolutePanel(
    bottom = "0%",
    right = "0%",
    width = "20%",
    fixed = TRUE,
    draggable = TRUE,
    style = "z-index:1002;",
    shiny::wellPanel(
      shiny::h4("Adjust Dendrogram Labels"),
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

      shiny::numericInput(ns("dendLabelSize"),
                          "Label Size",
                          value = 1,
                          min = 0,
                          max = 5,
                          step = .1
      ),

      shiny::actionButton("closeLabelsModification",
                   "Close")

    )
    )
}




#' colordendLabels
#'
#' @param input shiny modules default
#' @param output shiny modules default
#' @param session shiny modules default
#' @param dendrogram dendrogram whose labels are to be modified
#'
#' @return a
#' @export
#'

colordendLabels <- function(input,
                            output,
                            session,
                            dendrogram){

  dendrogram <- IDBacApp::changeDendPartColor(dendrogram = dendrogram,
                                     colorBy = input$colorBy,
                                     colorBlindPalette = colorBlindPalette(),
                                     cutHeight = input$cutHeight,
                                     chosenK = input$chosenK,
                                     part = "labels")

  dendrogram <- IDBacApp::changeDendPartSize(dendrogram = dendrogram,
                                   dendPartSize = input$dendLabelSize,
                                   part = "labels")


  return(dendrogram)

}
