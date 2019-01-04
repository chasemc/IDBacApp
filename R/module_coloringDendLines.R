

colordendLinesUI <- function(id) {
  ns <- NS(id)
  
  absolutePanel(
    bottom = "50%", right = "40%", width="25%",
    fixed = TRUE, draggable = TRUE,
    #Adjust Dendrogram Lines
    #----
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




colordendLines <- function(input,
                           output,
                           session,
                           dendrogram){
  
  dendrogram <- IDBacApp::changeDendLinesColor(dendrogram = dendrogram,
                                               colorBy = input$colorBy,
                                               colorBlindPalette = IDBacApp::colorBlindPalette(),
                                               cutHeight = input$cutHeight,
                                               chosenK = input$chosenK)
  
  dendrogram <- IDBacApp::changeDendLinesWidth(dendrogram = dendrogram,
                                               width = input$dendLineWidth)
  
  return(dendrogram) 

}
  


















  output$hclustPlot <- renderPlot({
    
    par(mar = c(5, 5, 5, dendparmar))
    
    if (input$colorBy == "groups"){
      
      coloredDend() %>%
        hang.dendrogram %>% 
        plot(horiz = TRUE, lwd = 8)
      
    } else if (input$colorBy == "height"){
      
      coloredDend()  %>%  
        hang.dendrogram %>% 
        plot(horiz = TRUE, lwd = 8)
      
      abline(v = input$cutHeight, lty = 2)
      
    } else if (input$colorBy == "metadata"){
      
      if(is.null(input$sampleMap$datapath)){
        # No sample mapping selected
        dendro()$dend %>%
          hang.dendrogram %>% 
          plot(horiz = TRUE, lwd = 8)
      }
    }
    
    
  }, height = plotHeight)
  
  
  
  
  
  
  
  
  





















































# dendextend::set.... branches_col - set the color of branches (using assign_values_to_branches_edgePar)
# dendextend::set.... branches_lwd - set the line width of branches (using assign_values_to_branches_edgePar)


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
      
      dendrogram <- color_branches(dend = dendrogram, 
                                   h = cutHeight,
                                   col = as.vector(colorBlindPalette[1:length(unique(cutree(dendrogram, h = cutHeight)))])
                                   
      )
      
    } else if(colorBy == "groups") {
      
      dendrogram <- color_branches(dend = dendrogram,
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

