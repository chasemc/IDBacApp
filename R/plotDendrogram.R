#' Function for plotting dendrogram
#'
#' @param dendrogram dendrogram
#' @param dendOrPhylo whether to plot leaves as hanging or not
#' @param selectMetaColumn plotting dend and dots
#' @param colorsChosen chosen colors for dend and dots 
#' @param cutHeightLines cutHeightLines
#' @param colorByLines colorByLines
#' @param closeDendDots closeDendDots
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
                            colorsChosen = colorsChosen(),
                            cutHeightLines = input$cutHeightLines,
                            colorByLines = input$colorByLines,
                            colorByLabels = input$colorByLabels,
                            closeDendDots = input$closeDendDots,
                            cutHeightLabels = input$cutHeightLabels,
                            boots = boots()$bootstraps,
                            pool = pool()){
  
  if (dendOrPhylo == "Dendrogram") {
    plot(dendrogram$dendrogram, horiz = T)
  } else if (dendOrPhylo == "Phylogram") {
    plot(dendextend::hang.dendrogram(dendrogram$dendrogram,
                                     hang = 0),
         horiz = T)
  }
  
  
  
  if (!is.null(selectMetaColumn[[1]])) {
    
    if (closeDendDots == 1) {
      
    } else {
      
      trimdLabsDend <- dendrogram$dendrogram
      
      dendextend::set_labels(trimdLabsDend,
                             strtrim(labels(trimdLabsDend), 20))
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