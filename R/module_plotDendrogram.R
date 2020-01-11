#' Function for plotting dendrogram
#'
#' @param dendrogram dendrogram
#' @param dendOrPhylo whether to plot leaves as hanging or not
#' @param selectMetaColumn plotting dend and dots
#' @param appendDendLabels column in metadata sqlite table to append to dend labels
#' @param colorsChosen chosen colors for dend and dots 
#' @param cutHeightLines cutHeightLines
#' @param colorByLines colorByLines
#' @param removeDendDots removeDendDots
#' @param cutHeightLabels cutHeightLabels
#' @param boots plot bootstraps
#' @param colorByLabels colorByLabels 
#' @param pool pool reactive
#'
#' @return plot
#' @export
#'
plotDendrogram <- function(dendrogram = dendrogram,
                           dendOrPhylo = dendOrPhylo(),
                           selectMetaColumn = input$selectMetaColumn,
                           appendDendLabels = input$appendDendLabels,
                           colorsChosen = colorsChosen(),
                           cutHeightLines = input$cutHeightLines,
                           colorByLines = input$colorByLines,
                           colorByLabels = input$colorByLabels,
                           removeDendDots = input$removeDendDots,
                           cutHeightLabels = input$cutHeightLabels,
                           boots = boots()$bootstraps,
                           pool = pool()){
  
  
  dendrogram_labels <- labels(dendrogram$dendrogram)

  print(appendDendLabels)
  
  if (!is.null(appendDendLabels)) {

    new_labels <- metadataFromId(strainID = dendrogram_labels,
                                   metadataColumn = appendDendLabels,
                                   pool = pool)[,2]

    dendrogram_labels <- paste0(dendrogram_labels, " ", new_labels)
    remove(new_labels)
  }

  
  
  if (dendOrPhylo == "Dendrogram") {
    plot(dendextend::set_labels(dendrogram$dendrogram, dendrogram_labels),
         horiz = T)
  } else if (dendOrPhylo == "Phylogram") {
    plot(dendextend::hang.dendrogram(dendextend::set_labels(dendrogram$dendrogram, dendrogram_labels),
                                     hang = 0),
         horiz = T)
  }
  
  
  
  if (!is.null(selectMetaColumn[[1]])) {
    
    if (removeDendDots == 1) {
      
    } else {
      
      trimdLabsDend <- dendrogram$dendrogram
      
      dendextend::set_labels(trimdLabsDend,
                             strtrim(labels(dendrogram_labels), 20))
      
      IDBacApp::runDendDots(rawDendrogram =  dendrogram$dendrogram,
                            trimdLabsDend = trimdLabsDend,
                            pool = pool,
                            columnID = selectMetaColumn,
                            colors = colorsChosen,
                            text_shift = 1)
    }
  }
  
  if (!is.null(colorByLines)) {
    if (colorByLines == "height") {
      graphics::abline(v = cutHeightLines, lty = 2)
      
    }
  }
  
  if (!is.null(colorByLabels)) {
    if (colorByLabels == "height") {
      graphics::abline(v = cutHeightLabels, lty = 2)
    }
  }
  if (boots[1] != "") {
    
    IDBacApp::bootlabels.hclust(stats::as.hclust(dendrogram$dendrogram), 
                                boots,
                                horiz = TRUE,
                                col = "blue")
  }
}