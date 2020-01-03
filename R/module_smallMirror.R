

#' mirrorPlotsSettings_UI
#'
#' @param id namespace
#'
#' @return NA
#' @export
#'
smallmirrorPlotsSampleSelect_UI <- function(id){
  ns <- NS(id)
  uiOutput(ns("mirrorSpectraSelector"))
}



#' smallmirrorPlots_UI
#'
#' @param id  namespace
#'
#' @return NA
#' @export
#'
smallmirrorPlots_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    plotOutput(ns("inversePeakComparisonPlot"), 
               brush = brushOpts(ns("brush_mirror"), 
                                 clip = FALSE,
                                 delay = 2000))
  )
  
}

#' Title
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#' @param workingDB reactive containing SQLite DB pool
#' @param proteinOrSmall values = "proteinPeaks" or "smallMoleculePeaks" 
#'
#' @return NA
#' @export
#'
smallmirrorPlots_Server <- function(input,
                                    output,
                                    session,
                                    workingDB,
                                    proteinOrSmall){
  
  
  inverseComparisonNames <- reactive({
    conn <- pool::poolCheckout(workingDB$pool())
    
    a <- DBI::dbGetQuery(conn, glue::glue("SELECT DISTINCT strain_id
                                           FROM spectra 
                                           WHERE max_mass < 6000"))
    pool::poolReturn(conn)
    
    a[ ,1]
    
  })
  
  
  
  output$mirrorSpectraSelector <- renderUI({
    
    tagList(
      tags$div(id='proteinMirror',
               class='mirror_select',
               column(width = 5, offset = 1,
                      selectizeInput(session$ns("Spectra1"), 
                                     label = strong("Spectrum 1 (positive y-axis)"),
                                     options= list(maxOptions = 10000),
                                     choices = inverseComparisonNames(), 
                                     selected = inverseComparisonNames()[[1]])
               ),
               column(width = 6,
                      selectizeInput(session$ns("Spectra2"), 
                                     label = strong("Spectrum 2 (negative y-axis)"),
                                     options= list(maxOptions = 10000),
                                     choices = inverseComparisonNames(),
                                     selected = inverseComparisonNames()[[1]])
               )
      )
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  dataForInversePeakComparisonPlot <- reactive({
    
    mirrorPlotEnv <- new.env(parent = parent.frame())
    
    # connect to sql
    
    
    # get protein peak data for the 1st mirror plot selection
    
    
    
    mirrorPlotEnv$peaksSampleOne <- IDBacApp::collapseReplicates(pool = workingDB$pool(),
                                                                 sampleIDs = input$Spectra1,
                                                                 peakPercentPresence = input$percentPresence,
                                                                 lowerMassCutoff = input$lowerMass,
                                                                 upperMassCutoff = input$upperMass,
                                                                 minSNR = input$SNR,
                                                                 tolerance = 0.002,
                                                                 protein = FALSE)[[1]] 
    
    
    
    mirrorPlotEnv$peaksSampleTwo <- IDBacApp::collapseReplicates(pool = workingDB$pool(),
                                                                 sampleIDs = input$Spectra2,
                                                                 peakPercentPresence = input$percentPresence,
                                                                 lowerMassCutoff = input$lowerMass,
                                                                 upperMassCutoff = input$upperMass,
                                                                 minSNR = input$SNR,
                                                                 tolerance = 0.002,
                                                                 protein = FALSE)[[1]]
    
    
    
    
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
    
    
    
    mirrorPlotEnv$spectrumSampleOne <- MALDIquant::averageMassSpectra(IDBacApp::mquantSpecFromSQL(pool = workingDB$pool(),
                                                                                                  sampleID = input$Spectra1, 
                                                                                                  protein = FALSE,
                                                                                                  smallmol = TRUE))
    
    
    
    
    mirrorPlotEnv$spectrumSampleTwo <- MALDIquant::averageMassSpectra(IDBacApp::mquantSpecFromSQL(pool = workingDB$pool(),
                                                                                                  sampleID = input$Spectra2, 
                                                                                                  protein = FALSE,
                                                                                                  smallmol = TRUE))

    
    
    
    
    
    
    pool::poolReturn(conn)
    # Return the entire saved environment
    mirrorPlotEnv
    
  })
  
  # Used in the the inverse-peak plot for zooming ---------------------------
  
  
  ranges2 <- reactive({
    ranges2 <- list()
    ranges2$y1 <- input$brush_mirror$ymin
    ranges2$y2 <- input$brush_mirror$ymax
    ranges2$x1 <- input$brush_mirror$xmin
    ranges2$x2 <- input$brush_mirror$xmax
    
    if (is.null(input$brush_mirror$ymax)) {
      a <- max(dataForInversePeakComparisonPlot()$spectrumSampleOne@mass)
      b <- max(dataForInversePeakComparisonPlot()$spectrumSampleTwo@mass)
      a <- max(a, b)
      w <- min(dataForInversePeakComparisonPlot()$spectrumSampleOne@mass)
      b <- min(dataForInversePeakComparisonPlot()$spectrumSampleTwo@mass)
      b <- min(w, b)
      ranges2$y2 <- max(dataForInversePeakComparisonPlot()$spectrumSampleOne@intensity)
      ranges2$y1 <- -max(dataForInversePeakComparisonPlot()$spectrumSampleTwo@intensity)
      ranges2$x1 <- b
      ranges2$x2 <- a
    }
    
    
    return(ranges2)
  })  
  
  
  
  
  
  
  # Output for the non-zoomed mirror plot
  output$inversePeakComparisonPlot <- renderPlot({
    
    mirrorPlotEnv <- dataForInversePeakComparisonPlot()
    
    yscale1 <- 100 / max(mirrorPlotEnv$spectrumSampleOne@intensity)
    yscale2 <- 100 / max(mirrorPlotEnv$spectrumSampleTwo@intensity)
    
    mirrorPlotEnv$spectrumSampleOne@intensity <- mirrorPlotEnv$spectrumSampleOne@intensity * yscale1 
    mirrorPlotEnv$spectrumSampleTwo@intensity <- mirrorPlotEnv$spectrumSampleTwo@intensity * yscale2 
    
    
    
    #Create peak plots and color each peak according to whether it occurs in the other spectrum
    plot(x = mirrorPlotEnv$spectrumSampleOne@mass,
         y = mirrorPlotEnv$spectrumSampleOne@intensity,
         ylim = c(ranges2()$y1, ranges2()$y2),
         xlim = c(ranges2()$x1,ranges2()$x2),
         type = "l",
         col = adjustcolor("Black", alpha = 0.3),
         xlab = "m/z",
         ylab = "Intensity")
    lines(x = mirrorPlotEnv$spectrumSampleTwo@mass,
          y = -mirrorPlotEnv$spectrumSampleTwo@intensity)
    rect(xleft = mirrorPlotEnv$peaksSampleOne@mass - 0.5,
         ybottom = 0,
         xright = mirrorPlotEnv$peaksSampleOne@mass + 0.5,
         ytop = ((mirrorPlotEnv$peaksSampleOne@intensity) * max(mirrorPlotEnv$spectrumSampleOne@intensity) / max(mirrorPlotEnv$peaksSampleOne@intensity)),
         border = mirrorPlotEnv$SampleOneColors)
    rect(xleft = mirrorPlotEnv$peaksSampleTwo@mass - 0.5,
         ybottom = 0,
         xright = mirrorPlotEnv$peaksSampleTwo@mass + 0.5,
         ytop = -((mirrorPlotEnv$peaksSampleTwo@intensity) * max(mirrorPlotEnv$spectrumSampleTwo@intensity) / max(mirrorPlotEnv$peaksSampleTwo@intensity)),
         border = rep("grey", times = length(mirrorPlotEnv$peaksSampleTwo@intensity)))
    
    
    session$resetBrush("brush_mirror")
    
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

