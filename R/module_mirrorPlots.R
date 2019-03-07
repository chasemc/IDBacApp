
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
                   label = "Download SVG")
    
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
      
      column(width = 5, offset = 1,
             selectizeInput(session$ns("Spectra1"), 
                         label = strong("Spectrum 1 (positive y-axis)"),
                         options= list(maxOptions = 2000),
                         choices = inverseComparisonNames(), 
                         selected = inverseComparisonNames()[[1]])
      ),
      column(width = 6,
             selectInput(session$ns("Spectra2"), 
                         label = strong("Spectrum 2 (negative y-axis)"),
                         options= list(maxOptions = 2000),
                         choices = inverseComparisonNames(),
                         selected = inverseComparisonNames()[[1]])
      )
    )
    
  })
  
  dataForInversePeakComparisonPlot <- reactive({
    
    mirrorPlotEnv <- new.env(parent = parent.frame())
    
    # connect to sql
    conn <- pool::poolCheckout(workingDB$pool())
    
    # get protein peak data for the 1st mirror plot selection
    
    
    
    mirrorPlotEnv$peaksSampleOne <- IDBacApp::collapseReplicates(checkedPool = conn,
                                                                 sampleIDs = input$Spectra1,
                                                                 peakPercentPresence = input$percentPresence,
                                                                 lowerMassCutoff = input$lowerMass,
                                                                 upperMassCutoff = input$upperMass,
                                                                 minSNR = input$SNR,
                                                                 tolerance = 0.002,
                                                                 protein = TRUE) 
    
    
    
    
    mirrorPlotEnv$peaksSampleTwo <- IDBacApp::collapseReplicates(checkedPool = conn,
                                                                 sampleIDs = input$Spectra2,
                                                                 peakPercentPresence = input$percentPresence,
                                                                 lowerMassCutoff = input$lowerMass,
                                                                 upperMassCutoff = input$upperMass,
                                                                 minSNR = input$SNR,
                                                                 tolerance = 0.002,
                                                                 protein = TRUE)
    
    
    
    
    # pSNR= the User-Selected Signal to Noise Ratio for protein
    
    # Remove peaks from the two peak lists that are less than the chosen SNR cutoff
    mirrorPlotEnv$SampleOneSNR <-  which(MALDIquant::snr(mirrorPlotEnv$peaksSampleOne) >= input$SNR)
    mirrorPlotEnv$SampleTwoSNR <-  which(MALDIquant::snr(mirrorPlotEnv$peaksSampleTwo) >= input$SNR)
    
    
    mirrorPlotEnv$peaksSampleOne@mass <- mirrorPlotEnv$peaksSampleOne@mass[mirrorPlotEnv$SampleOneSNR]
    mirrorPlotEnv$peaksSampleOne@snr <- mirrorPlotEnv$peaksSampleOne@snr[mirrorPlotEnv$SampleOneSNR]
    mirrorPlotEnv$peaksSampleOne@intensity <- mirrorPlotEnv$peaksSampleOne@intensity[mirrorPlotEnv$SampleOneSNR]
    
    mirrorPlotEnv$peaksSampleTwo@mass <- mirrorPlotEnv$peaksSampleTwo@mass[mirrorPlotEnv$SampleTwoSNR]
    mirrorPlotEnv$peaksSampleTwo@snr <- mirrorPlotEnv$peaksSampleTwo@snr[mirrorPlotEnv$SampleTwoSNR]
    mirrorPlotEnv$peaksSampleTwo@intensity <- mirrorPlotEnv$peaksSampleTwo@intensity[mirrorPlotEnv$SampleTwoSNR]
    
    # Binpeaks for the two samples so we can color code similar peaks within the plot
    
    validate(
      need(sum(length(mirrorPlotEnv$peaksSampleOne@mass),
               length(mirrorPlotEnv$peaksSampleTwo@mass)) > 0,
           "No peaks found in either sample, double-check the settings or your raw data.")
    )
    temp <- MALDIquant::binPeaks(c(mirrorPlotEnv$peaksSampleOne, mirrorPlotEnv$peaksSampleTwo), tolerance = .002)
    
    
    
    mirrorPlotEnv$peaksSampleOne <- temp[[1]]
    mirrorPlotEnv$peaksSampleTwo <- temp[[2]]
    
    
    # Set all peak colors for positive spectrum as red
    mirrorPlotEnv$SampleOneColors <- rep("red", length(mirrorPlotEnv$peaksSampleOne@mass))
    # Which peaks top samaple one are also in the bottom sample:
    temp <- mirrorPlotEnv$peaksSampleOne@mass %in% mirrorPlotEnv$peaksSampleTwo@mass
    # Color matching peaks in positive spectrum blue
    mirrorPlotEnv$SampleOneColors[temp] <- "blue"
    remove(temp)
    
    
    
    mirrorPlotEnv$spectrumSampleOne <- IDBacApp::mquantSpecFromSQL(checkedPool = conn,
                                                                   sampleID = input$Spectra1, 
                                                                   proteinOrSmall = '>')
    
    
    
    
    mirrorPlotEnv$spectrumSampleTwo <- IDBacApp::mquantSpecFromSQL(checkedPool = conn,
                                                                   sampleID = input$Spectra2, 
                                                                   proteinOrSmall = '>')
    
    
    pool::poolReturn(conn)
    # Return the entire saved environment
    mirrorPlotEnv
    
  })
  
  # Used in the the inverse-peak plot for zooming ---------------------------
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  
  
  
  # Output for the non-zoomed mirror plot
  output$inversePeakComparisonPlot <- plotly::renderPlotly({
    
    mirrorPlot(mirrorPlotEnv = dataForInversePeakComparisonPlot())
    
    
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
                     cbind(jsonlite::fromJSON(rawToChar(fst::decompress_fst(result[x, 1][[1]]))),
                           jsonlite::fromJSON(rawToChar(fst::decompress_fst(result[x, 2][[1]]))))
                   })
  
  
  result <- lapply(result,
                   function(x){
                     MALDIquant::createMassSpectrum(mass = x[ , 1],
                                                    intensity = x[ , 2])
                   }
  )
  
  MALDIquant::averageMassSpectra(result,
                                 method = "mean") 
}



