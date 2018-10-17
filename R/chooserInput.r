# From: https://github.com/rstudio/shiny-examples/blob/master/036-custom-input-control/chooser.R

chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {

  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)

  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL

  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
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
        tags$br(),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-down", "right-arrow fa-3x"),
            icon("arrow-circle-o-up", "left-arrow fa-3x")
        ),
        tags$br(),
        div(class="chooser-container chooser-right-container",
            tags$label("Selected Samples"),
            tags$br(),
            
            tags$select(class="right", size=size, multiple=multiple, rightChoices, width = "100%")
        )
    )
  )
}
