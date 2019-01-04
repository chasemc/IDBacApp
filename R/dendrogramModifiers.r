#' @title Functions to color and adjust dendrogram 
#' @export
#' @rdname coloringDendrogram
#' 
#' @param useDots If TRUE then draw dend and dots
#' @param cutByHeight
#' @param useKMeans
#' @param useMetadata
#' @param excelFilePath
#' @param chosenIdColumn
#' @param chosenMetaColumn
#' @param dendrogram
#' @param cutHeight
#' @param cutHeight
#' @param chosenColorsMeta
#' @param colorsChosen

#' @return dendrogram


coloringDendrogram <- function(dendrogram,
                               useDots,
                               useHeight,
                               useKMeans,
                               useMetadata,
                               excelFilePath,
                               chosenIdColumn,
                               chosenMetaColumn,
                               
                               cutHeight,
                               chosenK,
                               
                               chosenColorsMeta,
                               drawAbline,
                               colorsChosen,
                               colorBlindPalette,
                               colorBy,
                               dendLineWidth){

  
  
  
  dendrogram <-  changeDendLinesColor(dendrogram = dendrogram,
                                      colorBy = colorBy,
                                      colorBlindPalette = colorBlindPalette,
                                      cutHeight = cutHeight)
  dendrogram <- changeDendLinesWidth(dendrogram = dendrogram,
                              width = dendLineWidth)
  
  return(dendrogram)
}





# dendextend::set.... branches_col - set the color of branches (using assign_values_to_branches_edgePar)
# dendextend::set.... branches_lwd - set the line width of branches (using assign_values_to_branches_edgePar)


changeDendLinesColor <- function(dendrogram,
                                 colorBy,
                                 colorBlindPalette,
                                 cutHeight = 0){
  
 
  
  
  
  
  if(class(dendrogram) != "dendrogram"){
    warning("Dendrogram input wasn't of class \"dendrogram\"")
  } else if (class(colorBy) != "character") {
    warning(paste0("colorBy was type ", class(colorBy), ", expected character"))
  }
  else if (class(colorBlindPalette) != "character") {
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
                       k = cutHeight, 
                       col = as.vector(colorBlindPalette[1:length(unique(cutree(dendrogram, k = cutHeight)))])
        
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





