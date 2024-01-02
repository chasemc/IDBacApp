# Taken from shinyBS package
#' Title
#'
#' @param tag .
#' @param class .
#'
#' @return .
#'
#'
addClass <- function(tag, class) {
  if (!is.null(class)) {
    tag$attribs$class <- trimws(paste(tag$attribs$class, class), "both")
  }
  tag
}
#' Title
#'
#' @param tag .
#'
#' @return .
#'
#'
getAttribs <- function(tag) {
  tag$attribs
}

#' Title
#'
#' @param ... .
#' @param id  .
#' @param multiple  .
#' @param open  .
#'
#' @return .
#'
#'
bsCollapse <- function(..., id = NULL, multiple = FALSE, open = NULL) {
  if (!multiple & length(open) > 1) {
    open <- open[1]
  }
  panels <- list(...)
  for (i in seq(length(panels))) {
    #
    #     if(getAttribs(panels[[i]])$value %in% open) {
    #       panels[[i]]$children[[2]] <- addClass(panels[[i]]$children[[2]], "in")
    #     }
    if (!multiple) {
      panels[[i]]$children[[1]]$children[[1]]$children[[1]] <- addAttribs(panels[[i]]$children[[1]]$children[[1]]$children[[1]], "data-parent" = paste0("#", id))
    }
  }
  bsTag <- shiny::tags$div(class = "panel-group sbs-panel-group", "data-sbs-multi" = multiple, id = id, role = "tablist", panels)
}

#' Title
#'
#' @param title .
#' @param ...  .
#' @param value ..
#' @param style  .
#'
#' @return .
#'
#'
bsCollapsePanel <- function(title, ..., value = title, style = NULL) {
  content <- list(...)
  id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(1, 1, 1000000))))
  if (is.null(value)) {
    value <- title
  }
  if (is.null(style)) {
    style <- "default"
  }
  bsTag <- shiny::tags$div(
    class = paste0("panel panel-", style), value = value,
    shiny::tags$div(
      class = "panel-heading", role = "tab", id = paste0("heading_", id),
      shiny::tags$h4(
        class = "panel-title",
        shiny::tags$a("data-toggle" = "collapse", href = paste0("#", id), title)
      )
    ),
    shiny::tags$div(
      id = id, class = "panel-collapse collapse", role = "tabpanel",
      shiny::tags$div(class = "panel-body", content)
    )
  )
}
#' Title
#'
#' @param tag .
#' @param ...  .
#'
#' @return .
#'
#'
addAttribs <- function(tag, ...) {
  a <- list(...)
  for (i in seq(length(a))) {
    tag$attribs[names(a)[i]] <- a[[i]]
  }
  return(tag)
}

#' from shinyBS
#'
#' @param session https://github.com/ebailey78/shinyBS/blob/c329f8ce43e44579cafbb16fc3109fb69d403e57/R/updateCollapse.R
#' @param id https://github.com/ebailey78/shinyBS/blob/c329f8ce43e44579cafbb16fc3109fb69d403e57/R/updateCollapse.R
#' @param open https://github.com/ebailey78/shinyBS/blob/c329f8ce43e44579cafbb16fc3109fb69d403e57/R/updateCollapse.R
#' @param close https://github.com/ebailey78/shinyBS/blob/c329f8ce43e44579cafbb16fc3109fb69d403e57/R/updateCollapse.R
#' @param style https://github.com/ebailey78/shinyBS/blob/c329f8ce43e44579cafbb16fc3109fb69d403e57/R/updateCollapse.R
#'
#' @return https://github.com/ebailey78/shinyBS/blob/c329f8ce43e44579cafbb16fc3109fb69d403e57/R/updateCollapse.R
#'
#'
updateCollapse <- function(session, id, open = NULL, close = NULL, style = NULL) {
  data <- dropNulls(list(open = open, close = close, style = style))
  session$sendInputMessage(id, data)
}

#' from shinyBS
#'
#' @param x https://github.com/ebailey78/shinyBS/blob/c329f8ce43e44579cafbb16fc3109fb69d403e57/R/updateCollapse.R
#'
#' @return https://github.com/ebailey78/shinyBS/blob/c329f8ce43e44579cafbb16fc3109fb69d403e57/R/updateCollapse.R
#'
#'
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}
