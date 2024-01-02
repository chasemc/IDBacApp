#' colorPicker for dend and dots
#'
#' @param levs levels (reactiveValue)
#' @param ns shiny namespace
#'
#' @return list of html for each level with colors chosen
#'
#'
colorPicker <- function(levs,
                        ns) {
  lapply(
    seq_along(levs()),
    function(x) {
      do.call(
        (colourpicker::colourInput),
        list(
          inputId = ns(paste0(
            "factor-",
            make.unique(rep("dendDotsColors", length(levs())))[[x]]
          )),
          label = levs()[[x]],
          value = "blue",
          allowTransparent = T
        )
      )
    }
  )
}
