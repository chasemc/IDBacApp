#' peakRetentionSettings_UI
#'
#' @param id namespace
#' @param min_mass starting min mass
#' @param max_mass starting max mass
#'
#' @return NA
#' @export
#'
peakRetentionSettings_UI <- function(id,
                                     min_mass = 3000, 
                                     max_mass = 15000){
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
                 value = min_mass,
                 min = 1,
                 step = 50,
                 width = "50%"),
    p("Upper Mass Cutoff"),
    numericInput(ns("upperMass"), 
                 label = NULL,
                 value = max_mass,
                 step = 50,
                 width = "50%"),
    div(class = "tooltippy", "ppm tolerance", 
        span(class = "tooltippytext", 
             p("Note: This tolerance effects everything except the mirror plots. Must be set to greater than 200.")
        )
    ),
    numericInput(ns("ppm"), 
                 label = NULL,
                 value = 1000,
                 min = 200,
                 step = 100,
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
