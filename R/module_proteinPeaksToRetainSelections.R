#' peakRetentionSettings_UI
#'
#' @param id namespace
#'
#' @return NA
#' @export
#'
peakRetentionSettings_UI <- function(id,
                                     minMass = 3000, 
                                     maxMass = 15000){
  ns <- NS(id)
  
  tagList(
    
    div(class = "tooltippy", "Percent Presence", 
        span(class = "tooltippytext", 
             p("In what percentage of replicates must a peak be
               present to be kept? (0-100%) (Experiment/Hypothesis dependent)")
        )
    ),
    numericInput(ns("percentPresence"), 
                 label = NULL,
                 value = 70,
                 step = 10,
                 min = 0,
                 max = 100,
                 width = "50%"),
    div(class = "tooltippy", "Signal to Noise Ratio", 
        span(class = "tooltippytext", 
             p("Peaks with an SNR below the selected value will be removed from analyses.")
        )
    ),
    numericInput(ns("SNR"),
                 label = NULL,
                 value = 4,
                 step = 0.5,
                 min = 1.5,
                 max = 100,
                 width = "50%"),
    p("Lower Mass Cutoff"),
    numericInput(ns("lowerMass"), 
                 label = NULL,
                 value = minMass,
                 step = 50,
                 width = "50%"),
    p("Upper Mass Cutoff"),
    numericInput(ns("upperMass"), 
                 label = NULL,
                 value = maxMass,
                 step = 50,
                 width = "50%")
  )
  
  
} 


#' peakRetentionSettings_Server
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#'
#' @return NA
#' @export
#'

peakRetentionSettings_Server <- function(input,
                                         output,
                                         session){
  
  #return(reactive(shiny::reactiveValuesToList(input)))
  return(input)
}
