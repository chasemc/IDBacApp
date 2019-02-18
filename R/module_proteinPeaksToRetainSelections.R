peakRetentionSettings_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    numericInput(ns("percentPresence"), 
                 label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%) (Experiment/Hypothesis dependent)"),
                 value = 70,
                 step = 10,
                 min = 0,
                 max = 100),
    numericInput(ns("SNR"),
                 label = h5(strong("Signal To Noise Cutoff")),
                 value = 4,
                 step = 0.5,
                 min = 1.5,
                 max = 100),
    numericInput(ns("lowerMass"), 
                 label = h5(strong("Lower Mass Cutoff")),
                 value = 3000,
                 step = 50),
    numericInput(ns("upperMass"), 
                 label = h5(strong("Upper Mass Cutoff")),
                 value = 15000,
                 step = 50)
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



proteinMirrorPlotsSettings_UI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput("plotOut"),
    downloadButton("downloadInverse", 
                   label = "Download Main Plot"),
    downloadButton("downloadInverseZoom", 
                   label = "Download Zoomed Plot")
  )
  
}  

proteinMirrorPlots_UI <- function(id){
  ns <- NS(id)
  fluidRow(plotOutput("inversePeakComparisonPlot",
                      brush = brushOpts(
                        id = "plot2_brush",
                        resetOnNew = TRUE)),
           h3("Click and Drag on the plot above to zoom (Will zoom in plot below)"),
           plotOutput("inversePeakComparisonPlotZoom")
  )
  
}

proteinMirrorPlots_UI <- function(input,
                                  output,
                                  session,
                                  workingDB){
  
  
  
  inverseComparisonNames <- reactive({
    conn <- pool::poolCheckout(workingDB$pool())
    
    a <- DBI::dbGetQuery(conn, "SELECT DISTINCT `Strain_ID`
                               FROM IndividualSpectra
                               WHERE (`proteinPeaks` IS NOT NULL)")
    pool::poolReturn(conn)
    
    a[ ,1]
    
  })



output$plotOut <- renderUI({
  
  tagList(
  selectInput("Spectra1", label = h5(strong("Spectrum 1 (positive y-axis)"), 
                                     br(),
                                     "(Peak matches to bottom spectrum are blue, non-matches are red)"),
              choices = session$ns(inverseComparisonNames())), 
  selected = inverseComparisonNames()[[1]] ,
  selectInput("Spectra2", 
              label = h5(strong("Spectrum 2 (negative y-axis)")),
              choices = session$ns(inverseComparisonNames()),
              selected = session$ns(inverseComparisonNames())[[1]])
  )
  
})

}





