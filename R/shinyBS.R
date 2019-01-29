# Taken from shinyBS package

addClass <- function(tag, class) {
  
  if(!is.null(class)) {
    tag$attribs$class <- trimws(paste(tag$attribs$class, class), "both")
  }
  
  tag
  
}

getAttribs <- function(tag) {
  tag$attribs
}


bsCollapse <- function(..., id = NULL, multiple = FALSE, open = NULL) {
  

  if(!multiple & length(open) > 1) {
    open <- open[1]
  }
  
  panels <- list(...)
  
  for(i in seq(length(panels))) {
    if(getAttribs(panels[[i]])$value %in% open) {
      panels[[i]]$children[[2]] <- addClass(panels[[i]]$children[[2]], "in")
    }
    if(!multiple) {
      panels[[i]]$children[[1]]$children[[1]]$children[[1]] <- addAttribs(panels[[i]]$children[[1]]$children[[1]]$children[[1]], 'data-parent' = paste0("#", id))
    }
  }
  
  bsTag <- shiny::tags$div(class = "panel-group sbs-panel-group", "data-sbs-multi" = multiple, id=id, role = "tablist", panels)
  

  
}



bsCollapsePanel <- function(title, ..., value = title, style = NULL) {
  
  content <- list(...)
  
  id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(1, 1, 1000000))))
  if(is.null(value)) {
    value = title
  }
  if(is.null(style)) {
    style = "default"
  }
  
  bsTag <- shiny::tags$div(class = paste0("panel panel-", style), value = value,
                           shiny::tags$div(class = "panel-heading", role = "tab", id = paste0("heading_", id),
                                           shiny::tags$h4(class = "panel-title",
                                                          shiny::tags$a("data-toggle" = "collapse", href = paste0("#", id), title)
                                           )
                           ),
                           shiny::tags$div(id = id, class = "panel-collapse collapse", role = "tabpanel",
                                           shiny::tags$div(class = "panel-body", content)         
                           )
  )
  

}

addAttribs <- function(tag, ...) {
  a <- list(...)
  for(i in seq(length(a))) {
    tag$attribs[names(a)[i]] = a[[i]]
  }
  return(tag)
}