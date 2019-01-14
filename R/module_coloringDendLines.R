

#' colordendLinesServercolordendLinesUI
#'
#' @param id NA
#'
#' @return NA
#' @export 
#'
#' @examples NA
colordendLinesServercolordendLinesUI <- function(id) {
  ns <- NS(id)
  
  absolutePanel(
    bottom = "50%",
    right = "40%",
    width ="25%",
    fixed = TRUE, 
    draggable = TRUE,
    style = "opacity: 0.80",
    wellPanel(
      h4("Adjust Dendrogram Lines"),
      selectInput(ns("colorBy"), 
                  "Color By:", 
                  c("None" = "none",
                    "Choose Number of Groups" = "groups",
                    "Color by cutting at height" = "height",
                    "Color by sample info" = "metadata"
                  ),
                  selected = "height"
      ),
      conditionalPanel(
        condition = "input.colorBy == 'height'", ns = ns,
        numericInput(ns("cutHeight"), 
                     label = h5(strong("Cut Tree at Height")),
                     value = .5,
                     step = 0.1,
                     min = 0)
        
        
      ),     
      conditionalPanel(
        condition = "input.colorBy == 'groups'", ns = ns,
        numericInput(ns("chosenK"), 
                     label = h5(strong("Choose the number of groups")),
                     value = 1,
                     step = 1,
                     min = 1)
      ),     
      
      shiny::numericInput(ns("dendLineWidth"),
                          "Line Width", 
                          value = 1,
                          min = 1,
                          max = 10,
                          step = .2
      ),
      
      actionButton(ns("closeLineModification"),
                   "Close")
      
    ),
    #Adjust Dendrogram Lines
    #----    
    
    style = "opacity: 0.92"
  )
}




#' colordendLinesServer
#'
#' @param input NA
#' @param output  NA
#' @param session  NA
#' @param dendrogram  NA
#' @param plotHeight  NA
#'
#' @return NA
#' @export
#'
#' @examples NA
colordendLinesServer <- function(input,
                                 output,
                                 session,
                                 dendrogram, 
                                 plotHeight=500){
  
  dendrogram <- IDBacApp::changeDendLinesColor(dendrogram = dendrogram,
                                               colorBy = input$colorBy,
                                               colorBlindPalette = IDBacApp::colorBlindPalette(),
                                               cutHeight = input$cutHeight,
                                               chosenK = input$chosenK)
  
  dendrogram <- IDBacApp::changeDendLinesWidth(dendrogram = dendrogram,
                                               width = input$dendLineWidth)
  
  return(dendrogram) 
  
}





#' changeDendLinesColor
#'
#' @param dendrogram NA
#' @param colorBy  NA
#' @param colorBlindPalette  NA
#' @param cutHeight  NA
#' @param chosenK  NA
#'
#' @return NA
#' @export
#'
#' @examples NA
changeDendLinesColor <- function(dendrogram,
                                 colorBy,
                                 colorBlindPalette,
                                 cutHeight = 0,
                                 chosenK = 1){
  
  if(class(dendrogram) != "dendrogram"){
    warning("Dendrogram input wasn't of class \"dendrogram\"")
  } else if (class(colorBy) != "character") {
    warning(paste0("colorBy was type ", class(colorBy), ", expected character"))
  } else if (class(colorBlindPalette) != "character") {
    warning(paste0("colorBlindPalette was type ", class(colorBlindPalette), ", expected character vector."))
  } else{
    
    if(colorBy == "none") {
      #Intentionally Blank
      return(dendrogram)
      
    } else if(colorBy == "height") {
      
      dendrogram <- dendextend::color_branches(dend = dendrogram, 
                                   h = cutHeight,
                                   col = as.vector(colorBlindPalette[1:length(unique(cutree(dendrogram, h = cutHeight)))])
                                   
      )
      
    } else if(colorBy == "groups") {
      
      dendrogram <- dendextend::color_branches(dend = dendrogram,
                                   k = chosenK, 
                                   col = as.vector(colorBlindPalette[1:length(unique(cutree(dendrogram, k = chosenK)))])
                                   
      )
      
    } else if(colorBy == "metadata") {
      return(
        dendrogram 
      )
    }
  }
  return(dendrogram)  
  
  
  
}



#' changeDendLinesWidth
#'
#' @param dendrogram NA
#' @param width NA
#'
#' @return NA
#' @export
#'
#' @examples NA
changeDendLinesWidth <- function(dendrogram,
                                 width){
  
  if(class(dendrogram) != "dendrogram"){
    warning("Dendrogram input wasn't of class \"dendrogram\"")
    return(dendrogram)
  } else if (!is.numeric(width)) {
    warning(paste0("width was type ", class(width), ", expected numeric"))
    return(dendrogram)
  } else {
    return(
      set(dendrogram, "branches_lwd", width)
    )
  }
  
  
  
  
  
}

