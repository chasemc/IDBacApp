
dendrogramCreationUI <- function(id) {
  ns <- NS(id)
  
  absolutePanel(
    bottom = "70%", right = "40%", width="25%",
    fixed = TRUE, draggable = TRUE,
    wellPanel(
      selectInput(ns("distance"), 
                  label = h5(strong("Distance Algorithm")),
                  choices = list("cosine" = "cosineD",
                                 "euclidean" = "euclidean",
                                 "maximum" = "maximum",
                                 "manhattan" = "manhattan",
                                 "canberra" = "canberra",
                                 "binary" = "binary",
                                 "minkowski"= "minkowski"),
                  selected = "cosine"),
      selectInput(ns("clustering"), 
                  label = h5(strong("Clustering Algorithm")),
                  choices = list("ward.D" = "ward.D",
                                 "ward.D2" = "ward.D2",
                                 "single" = "single", 
                                 "complete" = "complete",
                                 "average (UPGMA)" = "average",
                                 "mcquitty (WPGMA)" = "mcquitty",
                                 "median (WPGMC)" = "median",
                                 "centroid (UPGMC)" = "centroid"),
                  selected = "average"),
      radioButtons(ns("booled"), 
                   label = h5(strong("Include peak intensities, or use presence/absence?")),
                   choices = list("Presence/Absence" = 1, 
                                  "Intensities" = 2)),
      actionButton(ns("closeLineModification"),
                   "Close")
    ),
    style = "opacity: 0.92"
  )
}





proteinDendrogramDrawer2 <- function(input,
                                    output,
                                    session,
                                    dendrogram
                                    
                                    ){
  if (input$kORheight == "1"){
    
    coloredDend() %>%
      hang.dendrogram %>% 
      plot(horiz = TRUE, lwd = 8)
    
  } else if (input$kORheight == "2"){
    
    coloredDend()  %>%  
      hang.dendrogram %>% 
      plot(horiz = TRUE, lwd = 8)
    
    abline(v = input$cutHeight, lty = 2)
    
  } else if (input$kORheight == "3"){
    
    if(is.null(input$sampleMap$datapath)){
      # No sample mapping selected
      dendro()$dend %>%
        hang.dendrogram %>% 
        plot(horiz = TRUE, lwd = 8)
    } else {
      if(input$colDotsOrColDend == "1"){
        
        coloredDend() %>%  
          hang.dendrogram %>% 
          plot(.,horiz=T)
        
        IDBacApp::colored_dots(coloredDend()$bigMatrix, 
                               coloredDend()$shortenedNames,
                               rowLabels = names(coloredDend()$bigMatrix),
                               horiz = T,
                               sort_by_labels_order = FALSE)
      } else {
        coloredDend()  %>%
          hang.dendrogram %>% 
          plot(., horiz = T)
      }
    }
  }  


}



