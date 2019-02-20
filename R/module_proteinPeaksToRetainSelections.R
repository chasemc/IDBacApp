peakRetentionSettings_UI <- function(id){
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
    div(class = "tooltippy", "Signal to Noise Cutoff", 
        span(class = "tooltippytext", 
             p("Choose an appropriate SNR for your spectra. In the picture below, the SNR of peaks decreases ")
        )
    ),
    numericInput(ns("SNR"),
                 label = NULL,
                 value = 4,
                 step = 0.5,
                 min = 1.5,
                 max = 100,width = "50%"),
    p("Lower Mass Cutoff"),
    numericInput(ns("lowerMass"), 
                 label = NULL,
                 value = 3000,
                 step = 50,
                 width = "50%"),
    p("Upper Mass Cutoff"),
    numericInput(ns("upperMass"), 
                 label = NULL,
                 value = 15000,
                 step = 50,
                 width = "50%")
  )
  
  
} 


peakRetentionSettings_Server <- function(input,
                                         output,
                                         session){
  
  
  
  return(reactive(shiny::reactiveValuesToList(input)))
  
}

















# p("Note: Settings selected above will be used in all later analyses."),
# p("Note 2: Displayed spectra represent the mean spectrum for a sample. Example: if you observe a peak
#                  in your mean spectrum but it isn't represented as a red or blue line, then either it doesn't occur often enough across your replicates
#                  or its signal to noise ratio is less than what is selected.")
# ),
# mainPanel(
#   fluidRow(plotOutput("inversePeakComparisonPlot",
#                       brush = brushOpts(
#                         id = "plot2_brush",
#                         resetOnNew = TRUE)),
#            h3("Click and Drag on the plot above to zoom (Will zoom in plot below)"),
#            plotOutput("inversePeakComparisonPlotZoom")
#   )
# )
# )
# }
# 

