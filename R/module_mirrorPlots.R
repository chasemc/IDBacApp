
#' mirrorPlotsSettings_UI
#'
#' @param id namespace
#'
#' @return NA
#' @export
#'
mirrorPlotsSettings_UI <- function(id){
  ns <- NS(id)
  uiOutput(ns("mirrorSpectraSelector"))
}

#' mirrorPlotDownload_UI
#'
#' @param id namespace
#'
#' @return NA
#' @export
#'
mirrorPlotDownload_UI <- function(id){
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadInverse"), 
                   label = "Download SVG"),
uiOutput(ns("normSpecUi"))
    
  )
  
}  

#' mirrorPlots_UI
#'
#' @param id  namespace
#'
#' @return NA
#' @export
#'
mirrorPlots_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(ns("inversePeakComparisonPlot")
      )
      
    ))
  
}

#' Title
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#' @param workingDB reactive containing SQLite DB pool
#' @param proteinOrSmall  '>' for protein '<' for small mol
#'
#' @return NA
#' @export
#'
mirrorPlots_Server <- function(input,
                               output,
                               session,
                               workingDB,
                               proteinOrSmall){
  
  
  
  output$normSpecUi <- renderUI({
    
    radioButtons(session$ns("normSpec"),
                 label = ("Normalize Spectra?"),
                 choices = list("Raw Spectra" = 1L, 
                                "Normalized" = 2L), 
                 selected = 1L)
    
  })
  
 
  
  inverseComparisonNames <- reactive({
    conn <- pool::poolCheckout(workingDB$pool())
    
    a <- DBI::dbGetQuery(conn, glue::glue("SELECT DISTINCT Strain_ID
                                            FROM IndividualSpectra
                                            WHERE maxMass {proteinOrSmall} 6000"))
    pool::poolReturn(conn)
    
    a[ , 1]
    
  })
  
  
  
  output$mirrorSpectraSelector <- renderUI({
    
    tagList(
      tags$div(id = 'proteinMirror',
               class = 'mirror_select',
               column(width = 5, offset = 1,
                      selectizeInput(session$ns("Spectra1"), 
                                     label = strong("Spectrum 1 (positive y-axis)"),
                                     options = list(maxOptions = 10000),
                                     choices = c("Choose one" = "", inverseComparisonNames()))
               ),
               column(width = 6,
                      selectizeInput(session$ns("Spectra2"),
                                     label = strong("Spectrum 2 (negative y-axis)"),
                                     options = list(maxOptions = 10000),
                                     choices = c("Choose one" = "", inverseComparisonNames()))
               )
      )
    )
    
  })
  
  
  
  
  
  dataForInversePeakComparisonPlot <- reactive({
    
    if (input$normSpec == 1L) {
      normalizeSpectra <- FALSE
    } else if (input$normSpec == 2L) {
      normalizeSpectra <- TRUE
      }
    
    IDBacApp::assembleMirrorPlots(sampleID1 = input$Spectra1,
                                  sampleID2 = input$Spectra2,
                                  peakPercentPresence = input$percentPresence,
                                  lowerMassCutoff = input$lowerMass,
                                  upperMassCutoff = input$upperMass,
                                  minSNR = input$SNR,
                                  tolerance = 0.002,
                                  pool1 = workingDB$pool(),
                                  pool2 = workingDB$pool(),
                                  normalizeSpectra = normalizeSpectra)
  })
  
  # Used in the the inverse-peak plot for zooming ---------------------------
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  
  
  
  # Output for the non-zoomed mirror plot
  output$inversePeakComparisonPlot <- plotly::renderPlotly({
    
    IDBacApp::mirrorPlot(mirrorPlotEnv = dataForInversePeakComparisonPlot())
    
    
  })
  
  
  # Output the zoomed mirror plot -------------------------------------------
  
  
  # Download svg of zoomed mirror plot --------------------------------------
  
  output$downloadInverse <- downloadHandler(
    filename = function(){paste0("top-",input$Spectra1,"_","bottom-",input$Spectra2,"-Zoom.svg")
    },
    content = function(file1){
      
      svglite::svglite(file1, width = 10, height = 8, bg = "white",
                       pointsize = 12, standalone = TRUE)
      
      IDBacApp::baserMirrorPlot(mirrorPlotEnv = dataForInversePeakComparisonPlot())
      
      dev.off()
      if (file.exists(paste0(file1, ".svg")))
        file.rename(paste0(file1, ".svg"), file1)
      
    })
  
  
  
  
  
}


#' mquantSpecFromSQL
#'
#' @param checkedPool checkedPool
#' @param sampleID sampleID 
#' @param proteinOrSmall '>' for protein '<' for small mol
#'
#' @return single, averaged MALDIquant spectrum
#' @export
#'
mquantSpecFromSQL <- function(checkedPool,
                              sampleID, 
                              proteinOrSmall){
  
  query <-  DBI::dbSendStatement(glue::glue("SELECT massTable.massVector, IndividualSpectra.spectrumIntensity
                                   FROM massTable
                                   LEFT JOIN IndividualSpectra
                                   ON massTable.spectrumMassHash = IndividualSpectra.spectrumMassHash
                                   WHERE Strain_ID == ?
                                   AND maxMass {proteinOrSmall} 6000"),
                                 con = checkedPool)
  
  
  DBI::dbBind(query, list(as.character(as.vector(sampleID))))
  result <- DBI::dbFetch(query)
  DBI::dbClearResult(query)
  
  result <- lapply(1:nrow(result),
                   function(x){
                     cbind(IDBacApp::deserial(rawToChar(fst::decompress_fst(result[x, 1][[1]]))),
                           IDBacApp::deserial(rawToChar(fst::decompress_fst(result[x, 2][[1]]))))
                   })
  
  
  lapply(result,
         function(x){
           MALDIquant::createMassSpectrum(mass = x[ , 1],
                                          intensity = x[ , 2])
         }
  )
  
}



