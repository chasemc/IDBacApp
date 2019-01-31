#' Main UI of IDBac
#'
#' @param input input
#' @param output output
#' @param session session
#'
#' @return IDBac server
#' @export
#'
app_server <- function(input, output, session) {

#######
# For during Dev
  options(shiny.reactlog = TRUE)
  workingDirectory <- getwd()
#######
  
  # Register sample-choosing JS
  shiny::registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
    if (is.null(data)) {
      NULL
    } else {
      list(left = as.character(data$left), right = as.character(data$right))
    }}, force = TRUE)
  
    
# Setup working directories

# Create a directory for temporary mzml files
tempMZDir <- file.path(workingDirectory, "temp_mzML")
dir.create(tempMZDir)

# Cleanup mzML temp folder on initialization of app
file.remove(list.files(tempMZDir,
                       pattern = ".mzML",
                       recursive = FALSE,
                       full.names = TRUE))





# Conversions Tab ---------------------------------------------------------


callModule(IDBacApp::convertDataTabServer,
           "convertDataTab",
           tempMZDir)


observeEvent(input$processToAnalysis,  
             ignoreInit = TRUE, {
               updateTabsetPanel(session, "mainIDBacNav",
                                 selected = "sqlUiTab")
               removeModal()
             })

# SQL Tab -----------------------------------------------------------------


# Find the available databases, and make reactive so can be updated if more are created
availableDatabases <- reactiveValues(db = tools::file_path_sans_ext(list.files(workingDirectory,
                                                                               pattern = ".sqlite",
                                                                               full.names = FALSE)))

databasePools <- reactiveValues(userDBcon = NULL)



#This "observe" event creates the SQL tab UI.
observeEvent(availableDatabases$db,{
  if (length(availableDatabases$db) > 0) {
    appendTab(inputId = "mainIDBacNav",
              tabPanel("Select/Manipulate Experiments",
                       value = "sqlUiTab",
                       IDBacApp::databaseTabUI("sqlUIcreator")
                       
              )
    )
  
  }
})


workingDB <- callModule(IDBacApp::databaseTabServer,
             "sqlUIcreator",
             workingDirectory = workingDirectory,
             availableExperiments = availableDatabases)


# 
# 
# observeEvent(pw()$move,{
#   
#   workingDB$pool() <- workingDB$pool$userDBCon
# 
# 
#   updateTabsetPanel(session, "mainIDBacNav",
#                     selected = "inversePeaks")
# 
#   })



# Trigger add tabs --------------------------------------------------------
observeEvent(workingDB$move$selectExperiment,
             once = T,
             ignoreInit = TRUE, {

               print("go2")


               appendTab(inputId = "mainIDBacNav",
                         tabPanel("Compare Two Samples (Protein)",
                                  value = "inversePeaks",
                                  uiOutput("inversepeakui")
                         )
               )
               appendTab(inputId = "mainIDBacNav",
                         tabPanel("Hierarchical Clustering (Protein)",
                                  uiOutput("Heirarchicalui")
                         )
               )
               appendTab(inputId = "mainIDBacNav",
                         tabPanel("Metabolite Association Network (Small-Molecule)",
                                  IDBacApp::ui_smallMolMan()
                         )
               )


               })








     




# Mirror Plots ------------------------------------------------------------

      # Mirror plot UI
      #----
      output$inversepeakui <-  renderUI({
        
        sidebarLayout(
          sidebarPanel(width = 3, style = "background-color:#7777770d",
                       selectInput("Spectra1", label = h5(strong("Spectrum 1 (up)"), 
                                                          br(),
                                                          "(Peak matches to bottom spectrum are blue, non-matches are red)"),
                                   choices = inverseComparisonNames()), 
                       selected = inverseComparisonNames()[[1]] ,
                       selectInput("Spectra2", 
                                   label = h5(strong("Spectrum 2 (down)")),
                                   choices = inverseComparisonNames(),
                                   selected = inverseComparisonNames()[[1]]),
                       downloadButton("downloadInverse", 
                                      label = "Download Main Plot"),
                       downloadButton("downloadInverseZoom", 
                                      label = "Download Zoomed Plot"),
                       numericInput("percentPresenceP", 
                                    label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%) (Experiment/Hypothesis dependent)"),
                                    value = 70,
                                    step = 10,
                                    min = 0,
                                    max = 100),
                       numericInput("pSNR",
                                    label = h5(strong("Signal To Noise Cutoff")),
                                    value = 4,
                                    step = 0.5,
                                    min = 1.5,
                                    max = 100),
                       numericInput("lowerMass", 
                                    label = h5(strong("Lower Mass Cutoff")),
                                    value = 3000,
                                    step = 50),
                       numericInput("upperMass", 
                                    label = h5(strong("Upper Mass Cutoff")),
                                    value = 15000,
                                    step = 50),
                       p("Note: Mass Cutoff and Percent Replicate values selected here will be used in all later analyses."),
                       p("Note 2: Displayed spectra represent the mean spectrum for a sample. Example: if you observe a peak
                         in your mean spectrum but it isn't represented as a red or blue line, then either it doesn't occur often enough across your replicates
                         or its signal to noise ratio is less than what is selected.")
          ),
          mainPanel(
            fluidRow(plotOutput("inversePeakComparisonPlot",
                                brush = brushOpts(
                                  id = "plot2_brush",
                                  resetOnNew = TRUE)),
                     h3("Click and Drag on the plot above to zoom (Will zoom in plot below)"),
                     plotOutput("inversePeakComparisonPlotZoom")
            )
          )
        )
      })
      
# Retrieve all available sample names that have protein peak data
# Used to display inputs for user to select for mirror plots
      #----
inverseComparisonNames <- reactive({
  conn <- pool::poolCheckout(workingDB$pool())
  
  
  
  
  a <-DBI::dbGetQuery(conn, "SELECT DISTINCT `Strain_ID`
                              FROM IndividualSpectra
                              WHERE (`proteinPeaks` IS NOT NULL)")
  pool::poolReturn(conn)
  
  a[ ,1]
  
})
      
      
      #This retrieves data a processes/formats it for the mirror plots
      #----
      dataForInversePeakComparisonPlot <- reactive({
        
        mirrorPlotEnv <- new.env(parent = parent.frame())
        
        # connect to sql
        conn <- pool::poolCheckout(workingDB$pool())
        
        # get protein peak data for the 1st mirror plot selection
        
        mirrorPlotEnv$peaksSampleOne <- IDBacApp::collapseReplicates(checkedPool = conn,
                                                                     sampleIDs = input$Spectra1,
                                                                     peakPercentPresence = input$percentPresenceP,
                                                                     lowerMassCutoff = input$lowerMass,
                                                                     upperMassCutoff = input$upperMass,
                                                                     minSNR = 6,
                                                                     tolerance = 0.002,
                                                                     protein = TRUE) 
        
        
        
        
        mirrorPlotEnv$peaksSampleTwo <- IDBacApp::collapseReplicates(checkedPool = conn,
                                                                     sampleIDs = input$Spectra2,
                                                                     peakPercentPresence = input$percentPresenceP,
                                                                     lowerMassCutoff = input$lowerMass,
                                                                     upperMassCutoff = input$upperMass,
                                                                     minSNR = 6,
                                                                     tolerance = 0.002,
                                                                     protein = TRUE)
        
        
       
    
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
        
        
        query <- DBI::dbSendStatement("SELECT `proteinSpectrum`
                                      FROM IndividualSpectra
                                      WHERE (`proteinSpectrum` IS NOT NULL)
                                      AND (`Strain_ID` = ?)",
                                      con = conn)
        
        
        DBI::dbBind(query, list(as.character(as.vector(input$Spectra1))))
        mirrorPlotEnv$spectrumSampleOne <- DBI::dbFetch(query)
        DBI::dbClearResult(query)
        
        mirrorPlotEnv$spectrumSampleOne <- lapply(mirrorPlotEnv$spectrumSampleOne[ , 1],
                                                  function(x){
                                                    unserialize(memDecompress(x, 
                                                                              type = "gzip"))
                                                    })
        mirrorPlotEnv$spectrumSampleOne <- unlist(mirrorPlotEnv$spectrumSampleOne, recursive = TRUE)
        mirrorPlotEnv$spectrumSampleOne <- MALDIquant::averageMassSpectra(mirrorPlotEnv$spectrumSampleOne,
                                                                          method = "mean") 
          
        
        
        
        query <- DBI::dbSendStatement("SELECT `proteinSpectrum`
                                      FROM IndividualSpectra
                                      WHERE (`proteinSpectrum` IS NOT NULL)
                                      AND (`Strain_ID` = ?)",
                                      con = conn)
        
        
        DBI::dbBind(query, list(as.character(as.vector(input$Spectra2))))
        mirrorPlotEnv$spectrumSampleTwo <- DBI::dbFetch(query)
        DBI::dbClearResult(query)
        
        mirrorPlotEnv$spectrumSampleTwo <- lapply(mirrorPlotEnv$spectrumSampleTwo[ , 1],
                                                  function(x){
                                                    unserialize(memDecompress(x, 
                                                                              type = "gzip"))
                                                  })
        mirrorPlotEnv$spectrumSampleTwo <- unlist(mirrorPlotEnv$spectrumSampleTwo, recursive = TRUE)
        mirrorPlotEnv$spectrumSampleTwo <- MALDIquant::averageMassSpectra(mirrorPlotEnv$spectrumSampleTwo,
                                                                          method = "mean") 
        
        
        pool::poolReturn(conn)
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
      


# Protein processing ------------------------------------------------------

# User chooses which samples to include
      #-----


chosenProteinSampleIDs <-  shiny::callModule(IDBacApp::sampleChooser_server,
                                                 "proteinSampleChooser",
                                                 pool = workingDB$pool,
                                                 allSamples = FALSE,
                                                 whetherProtein = TRUE,
                                                 selectedDB = workingDB$move)

      
      # Merge and trim protein replicates
      #----
      collapsedPeaksP <- reactive({
        req(length(chosenProteinSampleIDs$addSampleChooser$right) > 1)
        # For each sample:
        # bin peaks and keep only the peaks that occur in input$percentPresenceP percent of replicates
        # merge into a single peak list per sample
        # trim m/z based on user input
        # connect to sql
        conn <- pool::poolCheckout(workingDB$pool())
        temp <- lapply(chosenProteinSampleIDs$addSampleChooser$right,
                       function(ids){
                         IDBacApp::collapseReplicates(checkedPool = conn,
                                                      sampleIDs = ids,
                                                      peakPercentPresence = input$percentPresenceP,
                                                      lowerMassCutoff = input$lowerMass,
                                                      upperMassCutoff = input$upperMass, 
                                                      minSNR = 6, 
                                                      tolerance = 0.002,
                                                      protein = TRUE)
                       })
        
        
        names(temp) <- chosenProteinSampleIDs$addSampleChooser$right
        pool::poolReturn(conn)
        
        
                return(temp)
        
      })
      
      
     
      
     
      


proteinMatrix <- reactive({
  req(input$lowerMass, input$upperMass)
  pm <- IDBacApp::peakBinner(peakList = collapsedPeaksP(),
                             ppm = 2000,
                             massStart = input$lowerMass,
                             massEnd = input$upperMass)
  
  do.call(rbind, pm)
})


proteinDendrogram <- reactiveValues(dendrogram  = NULL)

observe({
proteinDendrogram$dendrogram <- shiny::callModule(IDBacApp::dendrogramCreator,
                                                  "proteinHierOptions",
                                                  proteinMatrix = proteinMatrix)
})



# 
      # #Create the hierarchical clustering based upon the user input for distance method and clustering technique
      # #----
      # observe({
      #   proteinDendrogram <- reactiveValues(dendrogram = shiny::callModule(IDBacApp::dendrogramCreator,
      #                                                                      "prot",
      #                                                                      proteinMatrix()))
      # })
      # 
      
      
      
      # PCoA Calculation
      #----
      pcoaResults <- reactive({
        # number of samples should be greater than k
        shiny::req(nrow(as.matrix(proteinDistance())) > 10)
        IDBacApp::pcoaCalculation(proteinDistance())
      })
      
      
      # output Plotly plot of PCoA results
      #----
      output$pcoaPlot <- plotly::renderPlotly({
        
        colorsToUse <- dendextend::leaf_colors(coloredDend())
        
        if (any(is.na(as.vector(colorsToUse)))) {
          colorsToUse <-  dendextend::labels_colors(coloredDend())
        }
        
        colorsToUse <- cbind.data.frame(fac = as.vector(colorsToUse),
                                        nam = (names(colorsToUse)))
        pcaDat <- merge(pcoaResults(),
                        colorsToUse,
                        by = "nam")
        
        plot_ly(data = pcaDat,
                x = ~Dim1,
                y = ~Dim2,
                z = ~Dim3,
                type = "scatter3d",
                mode = "markers",
                marker = list(color = ~fac),
                hoverinfo = 'text',
                text = ~nam) %>%
          layout(
            xaxis = list(
              title = ""
            ),
            yaxis = list(
              title = " "
            ),
            zaxis = list(
              title = ""
            ))
      })
      
      
      # PCA Calculation
      #----
      pcaResults <- reactive({
        # number of samples should be greater than k
        #  shiny::req(nrow(as.matrix(proteinMatrix())) > 4)
        
        
        IDBacApp::pcaCalculation(dataMatrix = proteinMatrix(),
                                 logged = TRUE,
                                 scaled = TRUE, 
                                 missing = .00001)
      })
      
      
      # Output Plotly plot of PCA results
      #----
      output$pcaPlot <- plotly::renderPlotly({
        
        colorsToUse <- dendextend::leaf_colors(coloredDend())
        
        if (any(is.na(as.vector(colorsToUse)))) {
          colorsToUse <-  dendextend::labels_colors(coloredDend())
        }
        
        colorsToUse <- cbind.data.frame(fac = as.vector(colorsToUse), 
                                        nam = (names(colorsToUse)))
        pcaDat <- pcaResults()  
        pcaDat <- merge(pcaResults(),
                        colorsToUse, 
                        by = "nam")
        plot_ly(data = pcaDat,
                x = ~Dim1,
                y = ~Dim2,
                z = ~Dim3,
                type = "scatter3d",
                mode = "markers",
                marker = list(color = ~fac),
                hoverinfo = 'text',
                text = ~nam) %>%
          layout(
            xaxis = list(
              title = ""
            ),
            yaxis = list(
              title = " "
            ),
            zaxis = list(
              title = ""
            ))
        
      })
      
      
      # Calculate tSNE based on PCA calculation already performed
      #----
      tsneResults <- reactive({
        shiny::req(nrow(as.matrix(proteinMatrix())) > 15)
        
        IDBacApp::tsneCalculation(dataMatrix = proteinMatrix(),
                                  perplexity = input$tsnePerplexity,
                                  theta = input$tsneTheta,
                                  iterations = input$tsneIterations)
        
      })
      
      
      # Output Plotly plot of tSNE results
      #----
      output$tsnePlot <- plotly::renderPlotly({
        
        colorsToUse <- dendextend::leaf_colors(coloredDend())
        
        if (any(is.na(as.vector(colorsToUse)))) {
          colorsToUse <-  dendextend::labels_colors(coloredDend())
        }
        
        colorsToUse <- cbind.data.frame(fac = as.vector(colorsToUse), 
                                        nam = (names(colorsToUse)))
        pcaDat <- merge(tsneResults(), 
                        colorsToUse,
                        by = "nam")
        
        plot_ly(data = pcaDat,
                x = ~Dim1,
                y = ~Dim2,
                z = ~Dim3,
                type = "scatter3d",
                mode = "markers",
                marker = list(color = ~fac),
                hoverinfo = 'text',
                text = ~nam)
      })
      
      
      
      #------------------------------------------------------------------------------
      # Protein Hierarchical clustering calculation and plotting
      #------------------------------------------------------------------------------
      
      
      # Create Heir ui
      #----
      output$Heirarchicalui <-  renderUI({
    
        if (is.null(input$Spectra1)) {
          fluidPage(
            h1(" There is no data to display",
               img(src = "errors/hit3.gif",
                   width = "200",
                   height = "100")),
            br(),
            h4("Troubleshooting:"),
            tags$ul(
              tags$li("Please ensure you have followed the instructions in the \"PreProcessing\" tab, and then visited the
                      \"Compare Two Samples\" tab."),
              tags$li("If you have already tried that, make sure there are \".rds\" files in your IDBac folder, within a folder
                      named \"Peak_Lists\""),
              tags$li("If it seems there is a bug in the software, this can be reported on the",
                      a(href = "https://github.com/chasemc/IDBacApp/issues",
                        target = "_blank",
                        "IDBac Issues Page at GitHub.",
                        img(border = "0",
                            title = "https://github.com/chasemc/IDBacApp/issues",
                            src = "GitHub.png",
                            width = "25",
                            height = "25")))
            )
          )
        } else {
    
          IDBacApp::ui_proteinClustering()
          
        }
      })
      
      
      
      # UI of paragraph explaining which variables were used
      #----
      output$proteinReport <- renderUI(

        p("This dendrogram was created by analyzing ",tags$code(length(labels(proteinDendrogram$dendrogram))), " samples,
          and retaining peaks with a signal to noise ratio above ",tags$code(input$pSNR)," and occurring in greater than ",tags$code(input$percentPresenceP),"% of replicate spectra.
          Peaks occuring below ",tags$code(input$lowerMass)," m/z or above ",tags$code(input$upperMass)," m/z were removed from the analyses. ",
          "For clustering spectra, ",tags$code(input$distance), " distance and ",tags$code(input$clustering), " algorithms were used.")
      )
      
      
      # Markdown report generation and download
      #----
      output$downloadReport <- downloadHandler(
        filename = function() {
          paste('my-report', sep = '.', switch(
            input$format,  HTML = 'html'
          ))
        },
        content = function(file) {
          src <- normalizePath('report.Rmd')
          
          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'report.Rmd', overwrite = TRUE)
          
          library(rmarkdown)
          out <- render('C:/Users/chase/Documents/GitHub/IDBacApp/ResultsReport.Rmd', switch(
            input$format,
            HTML = html_document()
          ))
          file.rename(out, file)
        }
      )
      
      
      
    

proteinDendColors <- shiny::callModule(IDBacApp::dendDotsServer,
                                       "proth",
                                       dendrogram = proteinDendrogram,
                                       pool = workingDB$pool,
                                       plotWidth = reactive(input$dendparmar),
                                       plotHeight = reactive(input$hclustHeight))

      
      
      
      
      
      # This observe controls the generation and display of the
      # protein hierarchical clustering page
      
      
      observe({  
        req(!is.null(proteinDendrogram$dendrogram))

        smallProtDend <-  shiny::callModule(IDBacApp::manPageProtDend,
                                          "manProtDend",
                                          dendrogram = proteinDendrogram,
                                          colorByLines = proteinDendColors$colorByLines,
                                          cutHeightLines = proteinDendColors$cutHeightLines,
                                          colorByLabels = proteinDendColors$colorByLabels,
                                          cutHeightLabels = proteinDendColors$cutHeightLabels,
                                          plotHeight = reactive(input$hclustHeightNetwork),
                                          plotWidth =  reactive(input$dendparmar2))

        
      })
      
      
      
      
      
      
      
      
      
      
      
      
      
      # Download svg of dendrogram
      #----
      output$downloadHeirSVG <- downloadHandler(
        filename = function(){paste0("Dendrogram.svg")
        },
        content = function(file1){
          
          svglite::svglite(file1, 
                           width = 10,
                           height = plotHeight() / 100,
                           bg = "white",
                           pointsize = 12, 
                           standalone = TRUE)
          
          par(mar = c(5, 5, 5, input$dendparmar))
          
          if (input$kORheight == "1") {
            
            proteinDendrogram$dendrogram %>% 
              dendextend::color_branches(k = input$kClusters) %>% 
              plot(horiz = TRUE, lwd = 8)
            
          } else if (input$kORheight == "2") {
            
            proteinDendrogram$dendrogram %>% 
              dendextend::color_branches(h = input$cutHeight) %>%
              plot(horiz = TRUE, lwd = 8)
            abline(v = input$cutHeight,
                   lty = 2)
            
          } else if (input$kORheight == "3") {
            
            par(mar = c(5, 5, 5, input$dendparmar))
            if(input$colDotsOrColDend == "1") {
              
              coloredDend()  %>%
                hang.dendrogram %>% 
                plot(., horiz = T)
              IDBacApp::colored_dots(coloredDend()$bigMatrix,
                                     coloredDend()$shortenedNames,
                                     rowLabels = names(coloredDend()$bigMatrix),
                                     horiz = T,
                                     sort_by_labels_order = FALSE)
            } else {
              coloredDend()  %>%  
                hang.dendrogram %>% 
                plot(., horiz = T)
            }
          }
          dev.off()
          if (file.exists(paste0(file1, ".svg")))
            file.rename(paste0(file1, ".svg"), file1)
        }
      )
      
      
      # Download dendrogram as Newick
      #----
      output$downloadHierarchical <- downloadHandler(
        
        filename = function() {
          paste0(Sys.Date(), ".newick")
        },
        content = function(file) {
          ape::write.tree(as.phylo(proteinDendrogram$dendrogram), file = file)
        }
      )
      
      
     
      
      
    
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      #------------------------------------------------------------------------------
      # Small molecule data processing
      #------------------------------------------------------------------------------
      
      # -----------------
     #  
     #  
     #  
     #  observe({
     #    w<-chosenProteinSampleIDs$addSampleChooser$right
     #    w<-input$dendparmar
     # pp <<-  shiny::callModule(IDBacApp::dendDotsServer,
     #                      "proteinMANpage",
     #                      dendrogram = proteinDendrogram$dendrogram,
     #                      pool = workingDB$pool(),
     #                      plotWidth=input$dendparmar,
     #                      plotHeight = input$hclustHeight)
     # er<<-reactiveValuesToList(input)
     #    
     #  })
     #  
     #  
      
      
      
      
      
      
      
      
      subtractedMatrixBlank <- reactive({
    
    
      aw <-  getSmallMolSpectra(pool = workingDB$pool(),
                           sampleIDs,
                           dendrogram = proteinDend$dendroReact(),
                           ymin = 0,
                           ymax = 20000,
                           matrixIDs = NULL,
                           peakPercentPresence = input$percentPresenceSM,
                           lowerMassCutoff = input$lowerMassSM,
                           upperMassCutoff = input$upperMassSM,
                           minSNR = input$smSNR)
          
        
        
      })
      
    
      #----
      smallMolNetworkDataFrame <- reactive({
        
        IDBacApp::smallMolDFtoNetwork(peakList = subtractedMatrixBlank())
        
        
      })
      
      
      
      ppp <- reactive({
        
        if (length(subtractedMatrixBlank()) > 9) {
          
          
          
          zz <<- intensityMatrix(subtractedMatrixBlank())
          zz[is.na(zz)] <- 0
          zz[is.infinite(zz)] <- 0
          
          
          pc <- FactoMineR::PCA(zz,
                                graph = FALSE,
                                ncp = 3,
                                scale.unit = T)
          pc <- pc$ind$coord
          pc <- as.data.frame(pc)
          nam <- unlist(lapply(subtractedMatrixBlank(), function(x) x@metaData$Strain))
          pc <- cbind(pc,nam)
          
          azz <-  calcNetwork()$wc$names[1:length(calcNetwork()$temp)]
          azz <- match(nam, azz)
          
          pc <- cbind(pc, as.vector(IDBacApp::colorBlindPalette()()[calcNetwork()$wc$membership[azz], 2] ))
          colnames(pc) <- c("Dim1", "Dim2", "Dim3", "nam", "color") 
          pc
        }else{FALSE}
      })
      
      
      
      
      output$smallMolPca <- plotly::renderPlotly({
        
        yep <- as.data.frame(ppp(), stringsAsFactors = FALSE)
        plot_ly(data = yep,
                x = ~Dim1,
                y = ~Dim2,
                z = ~Dim3,
                type = "scatter3d",
                mode = "markers",
                #          marker = list(color = ~fac),
                hoverinfo = 'text',
                text = ~nam, 
                color = ~ I(color)  )
      })
      
      
      
      
      
      
      
      #----
      
      output$downloadSmallMolNetworkData <- downloadHandler(
        filename = function(){"SmallMolecule_Network.csv"
        },
        content = function(file){
          write.csv(as.matrix(smallMolNetworkDataFrame()),
                    file,
                    row.names = FALSE)
        }
      )
      
      
      
      
      
      
      
      #This creates the network plot and calculations needed for such.
      #----
      calcNetwork <- reactive({
        net <- new.env(parent = parent.frame())
        
        temp <- NULL
        
        
        for (i in 1:length(subtractedMatrixBlank())) {
          temp <- c(temp,subtractedMatrixBlank()[[i]]@metaData$Strain)
        }
        aqww <- smallMolNetworkDataFrame()
        
        a <- as.undirected(graph_from_data_frame(smallMolNetworkDataFrame()))
        a <- igraph::simplify(a)
        wc <- fastgreedy.community(a)
        
        b <- igraph_to_networkD3(a, group = (wc$membership)) # zero indexed
        
        z <- b$links
        zz <- b$nodes
        
        biggerSampleNodes <- rep(1,times =length(zz[,1]))
        zz <- cbind(zz,biggerSampleNodes)
        zz$biggerSampleNodes[which(zz[,1] %in% temp)] <- 50
        
        net$z <- z
        net$zz <- zz
        net$wc <- wc
        net$temp <- temp
        net
        
      })
      
      
      output$metaboliteAssociationNetwork <- networkD3::renderSimpleNetwork({
        
        
        cbp <- as.vector(IDBacApp::colorBlindPalette()()[1:100,2])
        
        
        YourColors <- paste0('d3.scaleOrdinal()
                             .domain([',paste0(shQuote(1:100), collapse = ", "),'])
                             .range([', paste0(shQuote(cbp), collapse = ", "),' ])')
        
    
        forceNetwork(Links = awq2$z, 
                     Nodes = awq2$zz, 
                     Source = "source",
                     Nodesize = "biggerSampleNodes",
                     Target = "target",
                     NodeID = "name",
                     Group = "group",
                     opacity = 1,
                     opacityNoHover = 0.8, 
                     zoom = TRUE,
                     colourScale = JS(YourColors))
        
      })
      
      
      
      
      # -----------------
      # User input changes the height of the heirarchical clustering plot within the network analysis pane
      plotHeightHeirNetwork <- reactive({
        return(as.numeric(input$hclustHeightNetwork))
      })
      
     
    
    # Suggested Reporting Paragraphs ------------------------------------------
    
      # Output a paragraph about which paramters were used to create the currently-displayed MAN
      #----
      output$manReport <- renderUI({
        p("This MAN was created by analyzing ", tags$code(length(subtractedMatrixBlank())), " samples,", if (input$matrixSamplePresent == 1) {("subtracting a matrix blank,") } else {},
          "and retaining peaks with a signal to noise ratio above ", tags$code(input$smSNR), " and occurring in greater than ", tags$code(input$percentPresenceSM), "% of replicate spectra.
          Peaks occuring below ", tags$code(input$lowerMassSM), " m/z or above ", tags$code(input$upperMassSM), " m/z were removed from the analysis. ")
      })
      
      
   
      
    
    # Updating IDBac ----------------------------------------------------------
    
      # Updating IDBac Functions
      #----
      observeEvent(input$updateIDBac, 
                   ignoreInit = TRUE, {
        withConsoleRedirect <- function(containerId, expr) {
          # Change type="output" to type="message" to catch stderr
          # (messages, warnings, and errors) instead of stdout.
          txt <- capture.output(results <- expr, type = "message")
          if (length(txt) > 0) {
            insertUI(paste0("#", containerId), where = "beforeEnd",
                     ui = paste0(txt, "\n", collapse = "")
            )
          }
          results
        }
        
        showModal(modalDialog(
          title = "IDBac Update",
          tags$li(paste0("Installed Version: ")),
          tags$li(paste0("Latest Stable Release: ")),
          easyClose = FALSE, 
          size = "l",
          footer = "",
          fade = FALSE
        ))
        
        internetPing <- !suppressWarnings(system(paste("ping -n 1", "www.google.com")))
        
        if (internetPing == TRUE) {
          internetPingResponse <- "Successful"
          showModal(modalDialog(
            title = "IDBac Update",
            tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
            tags$li(paste0("Installed Version: ")),
            tags$li(paste0("Latest Stable Release: ")),
            easyClose = FALSE,
            size = "l",
            footer = "",
            fade = FALSE
          ))
          
          Sys.sleep(.75)
          
          # Currently installed version
          local_version <- tryCatch(packageVersion("IDBacApp"),
                                    error = function(x) paste("Installed version is latest version"),
                                    finally = function(x) packageVersion("IDBacApp"))
          
          showModal(modalDialog(
            title = "IDBac Update",
            tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
            tags$li(paste0("Installed Version: ", local_version)),
            tags$li(paste0("Latest Stable Release: ")),
            easyClose = FALSE,
            size = "l",
            footer = "",
            fade = FALSE
          ))
          
          Sys.sleep(.75)
          
          showModal(modalDialog(
            title = "IDBac Update",
            tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
            tags$li(paste0("Installed Version: ", local_version)),
            tags$li(paste0("Latest Stable Release: ")),
            easyClose = FALSE,
            size = "l",
            footer = "",
            fade = FALSE
          ))
          
          Sys.sleep(.75)
          
          # Latest GitHub Release
          getLatestStableVersion <- function(){
            base_url <- "https://api.github.com/repos/chasemc/IDBacApp/releases/latest"
            response <- httr::GET(base_url)
            parsed_response <- httr::content(response, 
                                             "parsed",
                                             encoding = "utf-8")
            parsed_response$tag_name
          }
          
          latestStableVersion <- try(getLatestStableVersion())
          
          showModal(modalDialog(
            title = "IDBac Update",
            tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
            tags$li(paste0("Installed Version: ", local_version)),
            tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
            easyClose = FALSE,
            size = "l",
            footer = "",
            fade = FALSE
          ))
          
          if (class(latestStableVersion) == "try-error") {
            
            showModal(modalDialog(
              title = "IDBac Update",
              tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
              tags$li(paste0("Installed Version: ", local_version)),
              tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
              tags$li("Unable to connect to IDBac GitHub repository"),
              easyClose = TRUE, 
              size = "l",
              footer = "",
              fade = FALSE
            ))
            
          } else {
            # Check current version # and the latest github version. If github v is higher, download and install
            # For more info on version comparison see: https://community.rstudio.com/t/comparing-string-version-numbers/6057/6
            downFunc <- function() {
              devtools::install_github(paste0("chasemc/IDBacApp@",
                                              latestStableVersion),
                                       force = TRUE,
                                       quiet = F, 
                                       quick = T)
              message(
                tags$span(
                  style = "color:red;font-size:36px;", "Finished. Please Exit and Restart IDBac."))
            }
            
            if (as.character(local_version) == "Installed version is latest version") {
              
              showModal(modalDialog(
                title = "IDBac Update",
                tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
                tags$li(paste0("Installed Version: ", local_version)),
                tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
                tags$li("Updating to latest version... (please be patient)"),
                pre(id = "console"),
                easyClose = FALSE,
                size = "l",
                footer = "",
                fade = FALSE
              ))
              
              withCallingHandlers(
                downFunc(),
                message = function(m) {
                  shinyjs::html("console",
                                m$message, 
                                TRUE)
                }
              )
              
            } else if (compareVersion(as.character(local_version), 
                                     as.character(latestStableVersion)) == -1) {
              
              showModal(modalDialog(
                title = "IDBac Update",
                tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
                tags$li(paste0("Installed Version: ", local_version)),
                tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
                tags$li("Updating to latest version... (please be patient)"),
                pre(id = "console"),
                easyClose = FALSE, 
                size = "l",
                footer = "",
                fade = FALSE
              ))
              
              withCallingHandlers(
                downFunc(),
                message = function(m) {
                  shinyjs::html("console", 
                                m$message,
                                TRUE)
                }
              )
              
            } else {
              
              showModal(modalDialog(
                title = "IDBac Update",
                tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
                tags$li(paste0("Installed Version: ", local_version)),
                tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
                tags$li("Latest Version is Already Installed"),
                easyClose = TRUE,
                size = "l",
                fade = FALSE,
                footer = modalButton("Close")
              ))
            }
          }
          
        } else {
          # if internet ping is false:
          
          internetPingResponse <- "Unable to Connect"
          showModal(modalDialog(
            title = "IDBac Update",
            tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
            tags$li(paste0("Installed Version: ")),
            tags$li(paste0("Latest Stable Release: ")),
            easyClose = FALSE,
            size = "l",
            footer = "",
            fade = FALSE
          ))
          
        }
      })
      
    
    # Old Database Stuff ------------------------------------------------------
    
      # Popup summarizing the final status of the conversion
      popupDBCreation <- function(failed = FALSE){
        modalDialog(
          title = "Are you sure?",
          p("There is already a database with this name."),
          p(paste0("Pressing save below will append to the existing database: \"", isolate(input$newDatabaseName),"\"")),
          footer = tagList(actionButton("saveNewDatabase", paste0("Append to: \"", isolate(input$newDatabaseName),"\"")), modalButton("Close"))
        )}
      
    
      
      
      #--------------------------------------
      
      
      
      #  The following code is necessary to stop the R backend when the user closes the browser window
      #   session$onSessionEnded(function() {
      # file.remove(list.files(tempMZDir,
      #                        pattern = ".mzML",
      #                        recursive = FALSE,
      #                        full.names = TRUE))
      #      stopApp()
      #      q("no")
      #    })
      
      
      # wq <-pool::dbPool(drv = RSQLite::SQLite(),
      #              dbname = paste0("wds", ".sqlite"))
      # 
      # onStop(function() {
      #   pool::poolClose(wq)
      #   print(wq)
      # }) # important!
      
      
}

