#' dendAndDotsUI
#'
#' @param id
#'
#' @return
#' @export

dendAndDotsUI <- function(id) {
  ns <- NS(id)

  absolutePanel(
    bottom = "50%",
    right = "50%",
    width ="50%",
    fixed = TRUE,
    draggable = TRUE,
    style="z-index:1002;",
    wellPanel(
     
      
      

    )
  )
}



#' dendAndDots
#'
#' @param input default shiny
#' @param output default shiny
#' @param session default shiny
#' @param dendrogram dendrogram input (eg stats::as.dendrogram)
#'
#' @return


dendAndDots <- function(input,
                            output,
                            session,
                            dendrogram){

  dendrogram <- dendVis::changeDendPartColor(dendrogram = dendrogram,
                                             colorBy = input$colorBy,
                                             colorBlindPalette = colorBlindPalette(),
                                             cutHeight = input$cutHeight,
                                             chosenK = input$chosenK,
                                             part = "labels")

  dendrogram <- dendVis::changeDendPartSize(dendrogram = dendrogram,
                                            dendPartSize = input$dendLabelSize,
                                            part = "labels")


  return(dendrogram)

}







