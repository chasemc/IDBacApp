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
#'
#'
plotDendrogram <- function(dendrogram,
                           dendOrPhylo,
                           selectMetaColumn,
                           appendDendLabels,
                           colorsChosen,
                           cutHeightLines,
                           colorByLines,
                           colorByLabels,
                           removeDendDots,
                           cutHeightLabels,
                           boots,
                           pool) {
  dendrogram_labels <- labels(dendrogram$dendrogram)

  if (!is.null(appendDendLabels)) {
    new_labels <- idbac_get_metadata(
      strainID = dendrogram_labels,
      metadataColumn = appendDendLabels,
      pool = pool
    )[, 2]
    dendrogram_labels <- paste0(dendrogram_labels, " ", new_labels)
    remove(new_labels)
  }

  if (dendOrPhylo == "Dendrogram") {
    plot(dendextend::set_labels(dendrogram$dendrogram, dendrogram_labels),
      horiz = T
    )
  } else if (dendOrPhylo == "Phylogram") {
    plot(
      dendextend::hang.dendrogram(dendextend::set_labels(dendrogram$dendrogram, dendrogram_labels),
        hang = 0
      ),
      horiz = T
    )
  }

  if (!is.null(selectMetaColumn[[1]])) {
    if (removeDendDots == 1) {
    } else {
      trimdLabsDend <- dendrogram$dendrogram
      dendextend::set_labels(
        trimdLabsDend,
        strtrim(labels(dendrogram_labels), 20)
      )
      runDendDots(
        rawDendrogram = dendrogram$dendrogram,
        trimdLabsDend = trimdLabsDend,
        pool = pool,
        columnID = selectMetaColumn,
        colors = colorsChosen,
        text_shift = 1
      )
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
  if (is.numeric(boots) && sum(boots) > 0L) {
    bootlabels.hclust(stats::as.hclust(dendrogram$dendrogram),
      boots,
      horiz = TRUE,
      col = "blue"
    )
  }
}
