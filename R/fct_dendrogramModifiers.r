#' changeDendPartColor
#'
#' @param dendrogram dendrogram to modify
#' @param colorBy color by choosing number of groups, cut height, or metadata
#' @param colorBlindPalette colorblind palette
#' @param cutHeight height to cut dendrogram
#' @param chosenK number of groups for kmeans
#' @param part modify dendrogram's labels or lines
#'
#' @return modified dendrogram
#' 
#'

changeDendPartColor <- function(dendrogram,
                                colorBy,
                                colorBlindPalette,
                                cutHeight = 0,
                                chosenK = 1,
                                part){
  
  if(part == "branches" ){
    dendFunc <- dendextend::color_branches
  } else if (part == "labels") {
    dendFunc <- dendextend::color_labels
  }
  wet <- dendrogram
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
      req(cutHeight)
      if(cutHeight > attributes(dendrogram)$height) {
        cutHeight <- attributes(dendrogram)$height 
      }
      if(cutHeight < 0){
        cutHeight <- 0
      }
      
      
      dendrogram <- tryCatch(
        dendFunc(dend = dendrogram,
                 h = cutHeight,
                 col = as.vector(colorBlindPalette[1:length(unique(dendextend::cutree(dendrogram, h = cutHeight)))])),
        error = function(x) dendrogram,
        finally = function(x) "Error when cutting dendrogram by height"
      )
      
    } else if(colorBy == "groups") {
      
      req(chosenK)
      
      if(is.na(chosenK)){
        chosenK <- 1
      }
      
      if (chosenK > attributes(dendrogram)$members) {
        chosenK <- attributes(dendrogram)$members
      } 
      
      dendrogram <- tryCatch(
        dendFunc(dend = dendrogram,
                 k = round(chosenK, digits = 0),
                 col = as.vector(colorBlindPalette[1:length(unique(dendextend::cutree(dendrogram, k = chosenK)))])),
        error = function(x) "Error when cutting dendrogram by Kmeans"
        )
                 
        
      
    } else if(colorBy == "metadata") {
      return(
        dendrogram
      )
    }
  }
  return(dendrogram)
  
  
  
}





#' changeDendPartSize
#'
#' @param dendrogram dendrogram to modify
#' @param dendPartSize numeric passed to change label size or edge widths
#' @param part change label size or edge widths
#'
#' @return modified dendrogram
#' 
#'

changeDendPartSize <- function(dendrogram,
                               dendPartSize,
                               part){
  
  if(part == "branches" ){
    dendFunc <- "branches_lwd"
  } else if (part == "labels") {
    dendFunc <- "labels_cex"
  }
  
  
  if(class(dendrogram) != "dendrogram"){
    warning("Dendrogram input wasn't of class \"dendrogram\"")
    return(dendrogram)
  } else if (!is.numeric(dendPartSize)) {
    warning(base::paste0("size was type ", base::class(dendPartSize), ", expected numeric"))
    return(dendrogram)
  } else {
    return(
      dendextend::set(dendrogram, dendFunc, dendPartSize)
    )
  }
  
  
  
  
  
}
