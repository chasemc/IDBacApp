
#' chooserInput
#'
#' @param inputId  NA
#' @param leftLabel  NA
#' @param rightLabel  NA
#' @param leftChoices  NA
#' @param rightChoices  NA
#' @param size  NA
#' @param multiple NA 
#'
#' @return NA
#' 
#'

chooserInput <- function(inputId, 
                         leftLabel,
                         rightLabel,
                         leftChoices,
                         rightChoices,
                         size = 5,
                         multiple = FALSE) {
# From: https://github.com/rstudio/shiny-examples/blob/master/036-custom-input-control/chooser.R

  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)

  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL

  tagList(
    singleton(tags$head(
      tags$script(src="www/chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            tags$label("Available Samples"),
            tags$br(),
            
            tags$select(class="left", size=size, multiple=multiple, leftChoices, width = "100%")
            
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-right", "right-arrow fa-3x"), br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            tags$label("Selected Samples"),
            tags$br(),
            
            tags$select(class="right", size=size, multiple=multiple, rightChoices, width = "100%")
        )
    )
  )
}
