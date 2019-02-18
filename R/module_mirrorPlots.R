

proteinMirrorPlotsSettings_UI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("mirrorSpectraSelector")),
    downloadButton(ns("downloadInverse"), 
                   label = "Download Main Plot"),
    downloadButton(ns("downloadInverseZoom"), 
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

#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#' @param workingDB 
#' @param proteinOrSmall values = "proteinPeaks" or "smallMoleculePeaks" 
#'
#' @return
#' @export
#'
#' @examples
proteinMirrorPlots_UI <- function(input,
                                  output,
                                  session,
                                  workingDB,
                                  proteinOrSmall){
  
  
  inverseComparisonNames <- reactive({
    conn <- pool::poolCheckout(workingDB$pool())
    
    a <- DBI::dbGetQuery(conn, glue::glue( "SELECT DISTINCT `Strain_ID`
                                            FROM IndividualSpectra
                                            WHERE (`{proteinOrSmall}` IS NOT NULL)"))
    pool::poolReturn(conn)
    
    a[ ,1]
    
  })
  
  
  
  output$mirrorSpectraSelector <- renderUI({
    
    tagList(
      selectInput("Spectra1", label = h5(strong("Spectrum 1 (positive y-axis)"), 
                                         br(),
                                         "(Peak matches to bottom spectrum are blue, non-matches are red)"),
                  choices = session$ns(inverseComparisonNames()), 
                  selected = session$ns(inverseComparisonNames())[[1]]),
      selectInput("Spectra2", 
                  label = h5(strong("Spectrum 2 (negative y-axis)")),
                  choices = session$ns(inverseComparisonNames()),
                  selected = session$ns(inverseComparisonNames())[[1]])
    )
    
  })
  
}





