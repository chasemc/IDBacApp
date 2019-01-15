#' mirrorPlotUI
#'
#' @param id NA
#'
#' @return NA
#' @export 
#'
#' @examples NA
mirrorPlotUI <- function(id) {
  ns <- shiny::NS(id)
  
 uiOutput(ns("theUI"))

}



#' mirrorPlotServer
#'
#' @param input NA
#' @param output NA
#' @param session NA
#'
#' @return NA
#' @export
#'
#' @examples NA
mirrorPlotServer <- function(input,
                           output,
                           session,
                           dbPool
){
  
  
  
 output$theUI <-  renderUI({
   ns <- session$ns
    sidebarLayout(
      sidebarPanel(width = 3, style = "background-color:#7777770d",
                   selectInput(ns("Spectra1"), label = h5(strong("Spectrum 1 (up)"), 
                                                          br(),
                                                          "(Peak matches to bottom spectrum are blue, non-matches are red)"),
                               choices = inverseComparisonNames()), 
                   selected = inverseComparisonNames()[[1]] ,
                   selectInput(ns("Spectra2"), 
                               label = h5(strong("Spectrum 2 (down)")),
                               choices = inverseComparisonNames(),
                               selected = inverseComparisonNames()[[1]]),
                   downloadButton(ns("downloadInverse"), 
                                  label = "Download Main Plot"),
                   downloadButton(ns("downloadInverseZoom"), 
                                  label = "Download Zoomed Plot"),
                   numericInput(ns("percentPresenceP"), 
                                label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%) (Experiment/Hypothesis dependent)"),value = 70,step=10,min=0,max=100),
                   numericInput(ns("pSNR"),
                                label = h5(strong("Signal To Noise Cutoff")),
                                value = 4,
                                step= 0.5,
                                min = 1.5,
                                max = 100),
                   numericInput(ns("lowerMass"), 
                                label = h5(strong("Lower Mass Cutoff")),
                                value = 3000,
                                step = 50),
                   numericInput(ns("upperMass"), 
                                label = h5(strong("Upper Mass Cutoff")),
                                value = 15000,
                                step = 50),
                   p("Note: Mass Cutoff and Percent Replicate values selected here will be used in all later analyses."),
                   p("Note 2: Displayed spectra represent the mean spectrum for a sample. Example: if you observe a peak
                     in your mean spectrum but it isn't represented as a red or blue line, then either it doesn't occur often enough across your replicates
                     or its signal to noise ratio is less than what is selected.")
                   ),
      mainPanel(
        fluidRow(plotOutput(ns("inversePeakComparisonPlot"),
                            brush = brushOpts(
                              id = ns("plot2_brush"),
                              resetOnNew = TRUE)),
                 h3("Click and Drag on the plot above to zoom (Will zoom in plot below)"),
                 plotOutput(ns("inversePeakComparisonPlotZoom"))
        )
      )
                   )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Retrieve all available sample names that have protein peak data
  # Used to display inputs for user to select for mirror plots
  #----
  inverseComparisonNames <- reactive({
    
    db <- dplyr::tbl(dbPool, "IndividualSpectra")
    db %>%
      filter(proteinPeaks != "NA") %>%
      select(Strain_ID) %>%
      distinct() %>%
      collect() %>%
      pull()
    
  })
  
  #This retrieves data a processes/formats it for the mirror plots
  #----
  dataForInversePeakComparisonPlot <- reactive({
    
    mirrorPlotEnv <- new.env(parent = parent.frame())
    
    # connect to sql
    db <- dplyr::tbl(dbPool, "IndividualSpectra")
    conn <- pool::poolCheckout(dbPool)
    
    # get protein peak data for the 1st mirror plot selection
    db %>%
      filter(Strain_ID %in% input$Spectra1) %>%
      filter(proteinPeaks != "NA") %>%
      select(spectrumSHA) %>%
      pull %>%
      IDBacApp::collapseProteinReplicates(fileshas = .,
                                          db = dbPool,
                                          proteinPercentPresence = input$percentPresenceP,
                                          lowerMassCutoff = input$lowerMass,
                                          upperMassCutoff = input$upperMass,
                                          dbConnection = conn) %>%
      return(.) -> mirrorPlotEnv$peaksSampleOne
    
    # get protein peak data for the 2nd mirror plot selection
    db %>%
      filter(Strain_ID %in% input$Spectra2) %>%
      filter(proteinPeaks != "NA") %>%
      select(spectrumSHA) %>%
      pull %>%
      IDBacApp::collapseProteinReplicates(fileshas = .,
                                          db = dbPool,
                                          proteinPercentPresence = input$percentPresenceP,
                                          lowerMassCutoff = input$lowerMass,
                                          upperMassCutoff = input$upperMass,
                                          dbConnection = conn) %>%
      return(.) -> mirrorPlotEnv$peaksSampleTwo
    pool::poolReturn(conn)
    
    
    # pSNR= the User-Selected Signal to Noise Ratio for protein
    
    # Remove peaks from the two peak lists that are less than the chosen SNR cutoff
    mirrorPlotEnv$SampleOneSNR <-  which(MALDIquant::snr(mirrorPlotEnv$peaksSampleOne) >= input$pSNR)
    mirrorPlotEnv$SampleTwoSNR <-  which(MALDIquant::snr(mirrorPlotEnv$peaksSampleTwo) >= input$pSNR)
    
    
    mirrorPlotEnv$peaksSampleOne@mass <- mirrorPlotEnv$peaksSampleOne@mass[mirrorPlotEnv$SampleOneSNR]
    mirrorPlotEnv$peaksSampleOne@snr <- mirrorPlotEnv$peaksSampleOne@snr[mirrorPlotEnv$SampleOneSNR]
    mirrorPlotEnv$peaksSampleOne@intensity <- mirrorPlotEnv$peaksSampleOne@intensity[mirrorPlotEnv$SampleOneSNR]
    
    mirrorPlotEnv$peaksSampleTwo@mass <- mirrorPlotEnv$peaksSampleTwo@mass[mirrorPlotEnv$SampleTwoSNR]
    mirrorPlotEnv$peaksSampleTwo@snr <- mirrorPlotEnv$peaksSampleTwo@snr[mirrorPlotEnv$SampleTwoSNR]
    mirrorPlotEnv$peaksSampleTwo@intensity <- mirrorPlotEnv$peaksSampleTwo@intensity[mirrorPlotEnv$SampleTwoSNR]
    aa<<-mirrorPlotEnv$peaksSampleOne
    # Binpeaks for the two samples so we can color code similar peaks within the plot
    
    validate(
      need(sum(length(mirrorPlotEnv$peaksSampleOne@mass),
               length(mirrorPlotEnv$peaksSampleTwo@mass)) > 0,
           "No peaks found in either sample, double-check the settings or your raw data.")
    )
    temp <- binPeaks(c(mirrorPlotEnv$peaksSampleOne, mirrorPlotEnv$peaksSampleTwo), tolerance = .02)
    
    
    
    mirrorPlotEnv$peaksSampleOne <- temp[[1]]
    mirrorPlotEnv$peaksSampleTwo <- temp[[2]]
    
    
    # Set all peak colors for positive spectrum as red
    mirrorPlotEnv$SampleOneColors <- rep("red", length(mirrorPlotEnv$peaksSampleOne@mass))
    # Which peaks top samaple one are also in the bottom sample:
    temp <- mirrorPlotEnv$peaksSampleOne@mass %in% mirrorPlotEnv$peaksSampleTwo@mass
    # Color matching peaks in positive spectrum blue
    mirrorPlotEnv$SampleOneColors[temp] <- "blue"
    remove(temp)
    
    # Retrieve the full spectra for each of the samples and average them
    db %>%
      filter(Strain_ID %in% input$Spectra2) %>%
      filter(proteinSpectrum != "NA") %>%
      select(proteinSpectrum) %>%
      pull %>%
      lapply(., function(x) unserialize(memDecompress(x, type= "gzip"))) %>%
      unlist(., recursive = TRUE) %>%
      MALDIquant::averageMassSpectra(., method = "mean") %>%
      return(.) -> mirrorPlotEnv$spectrumSampleTwo
    
    
    db %>%
      filter(Strain_ID %in% input$Spectra1) %>%
      filter(proteinSpectrum != "NA") %>%
      select(proteinSpectrum) %>%
      pull %>%
      lapply(., function(x) unserialize(memDecompress(x, type= "gzip"))) %>%
      unlist(., recursive = TRUE) %>%
      MALDIquant::averageMassSpectra(., method = "mean") %>%
      return(.) -> mirrorPlotEnv$spectrumSampleOne
    
    
    # Return the entire saved environment
    mirrorPlotEnv
    
  })
  
  
  
  
  
  
  
  #Used in the the inverse-peak plot for zooming
  #----
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  
  
  
  
  
  # Output for the non-zoomed mirror plot
  #----
  output$inversePeakComparisonPlot <- renderPlot({
    
    mirrorPlotEnv <- dataForInversePeakComparisonPlot()
    
    #Create peak plots and color each peak according to whether it occurs in the other spectrum
    plot(x = mirrorPlotEnv$spectrumSampleOne@mass,
         y = mirrorPlotEnv$spectrumSampleOne@intensity,
         ylim = c(-max(mirrorPlotEnv$spectrumSampleTwo@intensity),
                  max(mirrorPlotEnv$spectrumSampleOne@intensity)),
         type = "l",
         col = adjustcolor("Black", alpha=0.3),
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
    
    # Watch for brushing of the top mirror plot
    observe({
      brush <- input$plot2_brush
      if (!is.null(brush)) {
        ranges2$x <- c(brush$xmin, brush$xmax)
        ranges2$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges2$x <- NULL
        ranges2$y <- c(-max(mirrorPlotEnv$spectrumSampleTwo@intensity),
                       max(mirrorPlotEnv$spectrumSampleOne@intensity))
      }
    })
  })
  
  
  
  
  
  # Output the zoomed mirror plot
  #----
  output$inversePeakComparisonPlotZoom <- renderPlot({
    
    mirrorPlotEnv <- dataForInversePeakComparisonPlot()
    
    plot(x = mirrorPlotEnv$spectrumSampleOne@mass,
         y = mirrorPlotEnv$spectrumSampleOne@intensity,
         xlim = ranges2$x, ylim = ranges2$y,
         type = "l",
         col = adjustcolor("Black", alpha=0.3),
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
  })
  
  
  # Download svg of top mirror plot
  #----
  output$downloadInverse <- downloadHandler(
    filename = function(){
      paste0("top-", input$Spectra1,"_", "bottom-", input$Spectra2, ".svg")
      
    }, 
    content = function(file1){
      
      svglite::svglite(file1,
                       width = 10,
                       height = 8, 
                       bg = "white",
                       pointsize = 12,
                       standalone = TRUE)
      
      mirrorPlotEnv <- dataForInversePeakComparisonPlot()
      
      #Create peak plots and color each peak according to whether it occurs in the other spectrum
      plot(x = mirrorPlotEnv$spectrumSampleOne@mass,
           y = mirrorPlotEnv$spectrumSampleOne@intensity,
           ylim = c(-max(mirrorPlotEnv$spectrumSampleTwo@intensity),
                    max(mirrorPlotEnv$spectrumSampleOne@intensity)),
           type = "l",
           col = adjustcolor("Black", alpha=0.3),
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
      legend(max(mirrorPlotEnv$spectrumSampleOne@mass) * .6,
             max(max(mirrorPlotEnv$spectrumSampleOne@intensity)) * .7,
             legend = c(paste0("Top: ", input$Spectra1), 
                        paste0("Bottom: ", input$Spectra2)),
             col = c("black", "black"),
             lty = 1:1,
             cex = 1)
      
      dev.off()
      if (file.exists(paste0(file1, ".svg")))
        file.rename(paste0(file1, ".svg"), file1)
    })
  
  
  # Download svg of zoomed mirror plot
  #----
  output$downloadInverseZoom <- downloadHandler(
    filename = function(){paste0("top-",input$Spectra1,"_","bottom-",input$Spectra2,"-Zoom.svg")
    },
    content = function(file1){
      
      svglite::svglite(file1, width = 10, height = 8, bg = "white",
                       pointsize = 12, standalone = TRUE)
      
      mirrorPlotEnv <- dataForInversePeakComparisonPlot()
      
      plot(x = mirrorPlotEnv$spectrumSampleOne@mass,
           y = mirrorPlotEnv$spectrumSampleOne@intensity,
           xlim = ranges2$x, ylim = ranges2$y,
           type = "l",
           col = adjustcolor("Black", alpha=0.3),
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
      legend(max(ranges2$x) * .85,
             max(ranges2$y) * .7, 
             legend = c(paste0("Top: ", input$Spectra1),
                        paste0("Bottom: ", input$Spectra2)),
             col = c("black", "black"),
             lty = 1:1,
             cex = 1)
      
      dev.off()
      if (file.exists(paste0(file1, ".svg")))
        file.rename(paste0(file1, ".svg"), file1)
      
    })
  
  
  
  
  
  
  
  
}