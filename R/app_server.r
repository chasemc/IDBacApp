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
  
  
  # Develepment Functions ---------------------------------------------------
  options(shiny.reactlog = TRUE)
  sqlDirectory <- getwd()
  
  
  # Register sample-choosing JS ---------------------------------------------
  
  shiny::registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
    if (is.null(data)) {
      NULL
    } else {
      list(left = as.character(data$left), right = as.character(data$right))
    }}, force = TRUE)
  
  
  
  # Setup working directories -----------------------------------------------
  # This  doesn't go in modules, so that temp folder cleanup is sure to happen more often
  # Create a directory for temporary mzml files
  tempMZDir <- file.path(sqlDirectory, "temp_mzML")
  dir.create(tempMZDir)
  
  # Cleanup mzML temp folder on initialization of app
  file.remove(list.files(tempMZDir,
                         pattern = ".mzML",
                         recursive = FALSE,
                         full.names = TRUE))
  
  
  # Conversions Tab ---------------------------------------------------------
  
  
  callModule(IDBacApp::convertDataTab_Server,
             "convertDataTab",
             tempMZDir = tempMZDir,
             sqlDirectory = sqlDirectory)
  
  
  observeEvent(input$processToAnalysis,  
               ignoreInit = TRUE, {
                 updateTabsetPanel(session, "mainIDBacNav",
                                   selected = "sqlUiTab")
                 removeModal()
               })
  
  # SQL Tab -----------------------------------------------------------------
  
  
  # Find the available databases, and make reactive so can be updated if more are created
  availableDatabases <- reactiveValues(db = tools::file_path_sans_ext(list.files(sqlDirectory,
                                                                                 pattern = ".sqlite",
                                                                                 full.names = FALSE)))
  

  
  observeEvent(input$mainIDBacNav,
               ignoreInit = TRUE, {
                 req(input$mainIDBacNav == "sqlUiTab")
                 availableDatabases$db <- tools::file_path_sans_ext(list.files(sqlDirectory,
                                                                               pattern = ".sqlite",
                                                                               full.names = FALSE))
  
  })
  
  
  workingDB <- callModule(IDBacApp::databaseTabServer,
                          "sqlUIcreator",
                          sqlDirectory = sqlDirectory,
                          availableExperiments = availableDatabases)
  
  
  
  
  # Trigger add tabs --------------------------------------------------------
  
  
  #This "observe" event creates the SQL tab UI.
  observeEvent(availableDatabases$db,
               ignoreNULL = TRUE,
               once = TRUE, {
                 
                 appendTab(inputId = "mainIDBacNav",
                           tabPanel("Work With Previous Experiments",
                                    value = "sqlUiTab",
                                    IDBacApp::databaseTabUI("sqlUIcreator")
                                    
                           )
                 )
                 
                 
               })

  
  observeEvent(workingDB$move$selectExperiment,
               ignoreInit = TRUE, {
                 removeTab(inputId = "mainIDBacNav",
                           target = "inversePeaks"
                           
                           
                 )
                 removeTab(inputId = "mainIDBacNav",
                           target = "Hierarchical Clustering (Protein)"
                 )
                 
                 
                 removeTab(inputId = "mainIDBacNav",
                           target = "Metabolite Association Network (Small-Molecule)"
                           
                 )
                 
                 pool <- pool::poolCheckout(workingDB$pool())
                 p <- DBI::dbGetQuery(pool, "SELECT COUNT(*) FROM IndividualSpectra WHERE proteinPeaks IS NOT NULL")[,1]
                 s <- DBI::dbGetQuery(pool, "SELECT COUNT(*) FROM IndividualSpectra WHERE smallMoleculePeaks IS NOT NULL")[,1]
                 pool::poolReturn(pool)
                 if (p > 0) {
                   
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
                 }
                 if (s > 0){
                   appendTab(inputId = "mainIDBacNav",
                             tabPanel("Metabolite Association Network (Small-Molecule)",
                                      IDBacApp::ui_smallMolMan()
                             )
                   )
                 }
                 
               })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Mirror Plots ------------------------------------------------------------
  
  # Mirror plot UI
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
  inverseComparisonNames <- reactive({
    conn <- pool::poolCheckout(workingDB$pool())
    
    
    
    
    a <-DBI::dbGetQuery(conn, "SELECT DISTINCT `Strain_ID`
                              FROM IndividualSpectra
                              WHERE (`proteinPeaks` IS NOT NULL)")
    pool::poolReturn(conn)
    
    a[ ,1]
    
  })
  
  
  #This retrieves data a processes/formats it for the mirror plots
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
  
  # Used in the the inverse-peak plot for zooming ---------------------------
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  
  
  
  # Output for the non-zoomed mirror plot
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
  
  
  # Output the zoomed mirror plot -------------------------------------------
  
  
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
  
  
  # Download svg of top mirror plot -----------------------------------------
  
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
  
  
  # Download svg of zoomed mirror plot --------------------------------------
  
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
  
  
  # User chooses which samples to include -----------------------------------
 # chosenProteinSampleIDs <- reactiveValues(chosen = NULL)
  
    chosenProteinSampleIDs <- shiny::callModule(IDBacApp::sampleChooser_server,
                                                       "proteinSampleChooser",
                                                       pool = workingDB$pool,
                                                       allSamples = FALSE,
                                                       whetherProtein = TRUE)
  
  
  # Collapse peaks ----------------------------------------------------------
  
 # collapsedPeaksForDend <- reactiveValues(vals = NULL)  
  
#observe({
  collapsedPeaksForDend <- reactive({
    req(!is.null(chosenProteinSampleIDs$chosen))
    req(length(chosenProteinSampleIDs$chosen) > 0)
    req(workingDB$pool())
    # For each sample:
    # bin peaks and keep only the peaks that occur in input$percentPresenceP percent of replicates
    # merge into a single peak list per sample
    # trim m/z based on user input
    # connect to sql
    isolate(
    conn <- pool::poolCheckout(workingDB$pool())
    )
    temp <- lapply(chosenProteinSampleIDs$chosen,
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
    temp <- lapply(chosenProteinSampleIDs$chosen,
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
    
    pool::poolReturn(conn)
    
    if (length(proteinSamplesToInject$chosen$chosen) > 0) {
      
      conn <- pool::poolCheckout(proteinSamplesToInject$db())
      
      temp <- c(temp, lapply(proteinSamplesToInject$chosen$chosen,
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
                )
                pool::poolReturn(conn)
                
      
      names(temp) <- c(chosenProteinSampleIDs$chosen, proteinSamplesToInject$chosen$chosen)
      
    } else {
      names(temp) <- chosenProteinSampleIDs$chosen
      
    }
    
    
    
    return(temp)
    
  })
  
  
  
  
  proteinSamplesToInject <- callModule(IDBacApp::selectInjections_server,
                                       "proteinInject",
                                       sqlDirectory = sqlDirectory,
                                       availableExperiments = availableDatabases,
                                       watchMainDb = workingDB$move)
  
  # Protein matrix ----------------------------------------------------------
  
  
  proteinMatrix <- reactive({
    req(input$lowerMass, input$upperMass)
    req(!is.null(collapsedPeaksForDend()))
    validate(need(input$lowerMass < input$upperMass, "Lower mass cutoff should be higher than upper mass cutoff."))
    pm <- IDBacApp::peakBinner(peakList = collapsedPeaksForDend(),
                               ppm = 2000,
                               massStart = input$lowerMass,
                               massEnd = input$upperMass)
    
  do.call(rbind, pm)
    
    
  })
  
  proteinDendrogram <- reactiveValues(dendrogram  = NULL)
  
  
  observeEvent(workingDB$move$selectExperiment, {
    proteinDendrogram$dendrogram <- NULL
    
      })
  
 dendMaker <- shiny::callModule(IDBacApp::dendrogramCreator,
                       "proteinHierOptions",
                       proteinMatrix = proteinMatrix)

  
  observe({
    req(nrow(proteinMatrix()) > 2)
    proteinDendrogram$dendrogram <- dendMaker()$dend
  })
  
  
  
  proteinDendColored <- shiny::callModule(IDBacApp::dendDotsServer,
                                          "proth",
                                          dendrogram = proteinDendrogram,
                                          pool = workingDB$pool,
                                          plotWidth = reactive(input$dendparmar),
                                          plotHeight = reactive(input$hclustHeight),
                                          boots = dendMaker)
  
  
  unifiedProteinColor <- reactive(dendextend::labels_colors(proteinDendrogram$dendrogram))
  
  
  #  PCoA Calculation -------------------------------------------------------
  
  
  pcoaResults <- reactive({
    # number of samples should be greater than k
    shiny::req(nrow(as.matrix(proteinDistance())) > 10)
    IDBacApp::pcoaCalculation(proteinDistance())
  })
  
  
  
  # PCoA Plot ---------------------------------------------------------------
  
  
  output$pcoaPlot <- plotly::renderPlotly({
    
    colorsToUse <- 
      
      if (any(is.na(as.vector(colorsToUse)))) {
        colorsToUse <-  dendextend::labels_colors(coloredDend())
      }
    
    colorsToUse <- cbind.data.frame(fac = as.vector(colorsToUse),
                                    nam = (names(colorsToUse)))
    pcaDat <- merge(pcoaResults(),
                    colorsToUse,
                    by = "nam")
    
    plotly::plot_ly(data = pcaDat,
                    x = ~Dim1,
                    y = ~Dim2,
                    z = ~Dim3,
                    type = "scatter3d",
                    mode = "markers",
                    marker = list(color = ~fac),
                    hoverinfo = 'text',
                    text = ~nam) %>%
      plotly::layout(
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
  
  
  # PCA Calculation  --------------------------------------------------------
  
  
  callModule(IDBacApp::pca_Server,
             "proteinPCA",
             dataframe = proteinMatrix,
             namedColors = unifiedProteinColor)
  
  
  
  
  
  
  # Calculate tSNE based on PCA calculation already performed ---------------
  
  tsneResults <- reactive({
    shiny::req(nrow(as.matrix(proteinMatrix())) > 15)
    
    IDBacApp::tsneCalculation(dataMatrix = proteinMatrix(),
                              perplexity = input$tsnePerplexity,
                              theta = input$tsneTheta,
                              iterations = input$tsneIterations)
    
  })
  
  
  
  # Output Plotly plot of tSNE results --------------------------------------
  
  
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
  
  
  
  
  # Protein Hierarchical clustering calculation and plotting ----------------
  
  
  
  # Create Protein Dendrogram UI --------------------------------------------
  
  output$Heirarchicalui <-  renderUI({
    
    if (is.null(input$Spectra1)) {
      fluidPage(
        h1(" There is no data to display", align = "center"),
        br(),
        h4("Troubleshooting:"),
        tags$ul(
          tags$li("Please visit the
                      \"Compare Two Samples (Protein)\" tab first."),
          tags$li("If it seems like there is a bug in the software, this can be reported on the",
                  a(href = "https://github.com/chasemc/IDBacApp/issues",
                    target = "_blank",
                    "IDBac Issues Page at GitHub.",
                    img(border = "0",
                        title = "https://github.com/chasemc/IDBacApp/issues",
                        src = "www/GitHub.png",
                        width = "25",
                        height = "25")
                  )
                  
          )
        )
      )
    } else {
      
      IDBacApp::ui_proteinClustering()
      
    }
  })
  
  
  
  
  # Paragraph to relay info for reporting protein ---------------------------
  
  
  output$proteinReport <- renderUI(
    
    p("This dendrogram was created by analyzing ",tags$code(length(labels(proteinDendrogram$dendrogram))), " samples,
          and retaining peaks with a signal to noise ratio above ",tags$code(input$pSNR)," and occurring in greater than ",tags$code(input$percentPresenceP),"% of replicate spectra.
          Peaks occuring below ",tags$code(input$lowerMass)," m/z or above ",tags$code(input$upperMass)," m/z were removed from the analyses. ",
      "For clustering spectra, ",tags$code(input$distance), " distance and ",tags$code(input$clustering), " algorithms were used.")
  )
  
  
  # Generate Rmarkdown report -----------------------------------------------
  
  
  
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
      
      out <- render('C:/Users/chase/Documents/GitHub/IDBacApp/ResultsReport.Rmd', switch(
        input$format,
        HTML = html_document()
      ))
      file.rename(out, file)
    }
  )
  
  
  
  
  
  # Download svg of dendrogram ----------------------------------------------
  
  
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
        if (input$colDotsOrColDend == "1") {
          
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
  
  
  
  # Download dendrogram as Newick -------------------------------------------
  
  
  output$downloadHierarchical <- downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), ".newick")
    },
    content = function(file) {
      ape::write.tree(as.phylo(proteinDendrogram$dendrogram), file = file)
    }
  )
  
  
  
  # Small molecule data processing ------------------------------------------
  
  
  # Display protien dend fro brushing for small mol -------------------------
  
  smallProtDend <-  shiny::callModule(IDBacApp::manPageProtDend_Server,
                                      "manProtDend",
                                      dendrogram = proteinDendrogram,
                                      colorByLines = proteinDendColored$colorByLines,
                                      cutHeightLines = proteinDendColored$cutHeightLines,
                                      colorByLabels = proteinDendColored$colorByLabels,
                                      cutHeightLabels = proteinDendColored$cutHeightLabels,
                                      plotHeight = reactive(input$hclustHeightNetwork),
                                      plotWidth =  reactive(input$dendparmar2))
  
  
  
  # Small mol pca Calculation -----------------------------------------------
  
  callModule(IDBacApp::pca_Server,
             "smallMolPcaPlot",
             dataframe = smallMolDataFrame,
             namedColors = function() NULL)
  

# Small mol ---------------------------------------------------------------

  output$matrixSelector <- renderUI({
    IDBacApp::bsCollapse(id = "collapseMatrixSelection",
                         open = "Panel 1",
                         IDBacApp::bsCollapsePanel(p("Select a Sample to Subtract", 
                                                      align = "center"),
                                                   selectInput("selectMatrix",
                                                               label = "",
                                                               choices = c("None", smallMolIDs()))
                                                   
                         )
    )
})

  
  
  smallMolIDs <- reactive({
    checkedPool <- pool::poolCheckout(workingDB$pool())
    # retrieve all Strain_IDs in db that have small molecule spectra
    sampleIDs <- glue::glue_sql("SELECT DISTINCT `Strain_ID`
                                FROM `IndividualSpectra`
                                WHERE (`smallMoleculePeaks` IS NOT NULL)",
                                .con = checkedPool)
    sampleIDs <- DBI::dbGetQuery(checkedPool, sampleIDs)
    
     pool::poolReturn(checkedPool)
    
    return(sampleIDs)
    
  })  
  
    
  
  subtractedMatrixBlank <- reactive({
    
    
    samples <-  IDBacApp::getSmallMolSpectra(pool = workingDB$pool(),
                                             sampleIDs = NULL,
                                             dendrogram = proteinDendrogram$dendrogram,
                                             brushInputs = smallProtDend,
                                             matrixIDs = NULL,
                                             peakPercentPresence = input$percentPresenceSM,
                                             lowerMassCutoff = input$lowerMassSM,
                                             upperMassCutoff = input$upperMassSM,
                                             minSNR = input$smSNR)
    
    if ( (input$selectMatrix != "None") ) {
      
      matrixSample <- IDBacApp::getSmallMolSpectra(pool = workingDB$pool(),
                                               sampleIDs = input$selectMatrix,
                                               dendrogram = proteinDendrogram$dendrogram,
                                               brushInputs = smallProtDend,
                                               matrixIDs = NULL,
                                               peakPercentPresence = input$percentPresenceSM,
                                               lowerMassCutoff = input$lowerMassSM,
                                               upperMassCutoff = input$upperMassSM,
                                               minSNR = input$smSNR)
      
      
      
      samples <- MALDIquant::binPeaks(c(matrixSample, samples),
                                     tolerance = .002)
      
      
      for (i in 2:(length(samples))) {
      
      toKeep <- !samples[[i]]@mass %in% samples[[1]]@mass 
      
      samples[[i]]@mass <- samples[[i]]@mass[toKeep]
      samples[[i]]@intensity <- samples[[i]]@intensity[toKeep]
      samples[[i]]@snr <- samples[[i]]@snr[toKeep]
      
      }
      
      samples <- samples[-1]
      
      
      
    } else {
      
      
      samples <- MALDIquant::binPeaks(samples, tolerance = .002)
    }
    

return(samples)

    
    
    
    
  })
  
  
  
  # Small mol MAN serve module ----------------------------------------------
  
  
  callModule(IDBacApp::MAN_Server,
             "smMAN",
             subtractedMatrixBlank = subtractedMatrixBlank)
  
  
  
  
  # Small molecule data frame reactive --------------------------------------
  
  smallMolDataFrame <- reactive({
    
    smallNetwork <- MALDIquant::intensityMatrix(subtractedMatrixBlank())
    temp <- NULL
    
    for (i in 1:length(subtractedMatrixBlank())) {
      temp <- c(temp,subtractedMatrixBlank()[[i]]@metaData$Strain)
    }
    
    rownames(smallNetwork) <- temp
    smallNetwork[is.na(smallNetwork)] <- 0
    as.matrix(smallNetwork)
  })
  
  
  
  
  # plotHeightHeirNetwork ---------------------------------------------------
  
  # User input changes the height of the heirarchical clustering plot within the network analysis pane
  plotHeightHeirNetwork <- reactive({
    return(as.numeric(input$hclustHeightNetwork))
  })
  
  
  
  
  # Suggested Reporting Paragraphs for small molecule data ------------------
  
  output$manReport <- renderUI({
    p("This MAN was created by analyzing ", tags$code(length(subtractedMatrixBlank())), " samples,",
      if (input$selectMatrix != "None") {
        ("subtracting a matrix blank,") 
        } else {},
      " retaining peaks with a signal to noise ratio above ", tags$code(input$smSNR), ", and occurring in greater than ", tags$code(input$percentPresenceSM), "% of replicate spectra.
          Peaks occuring below ", tags$code(input$lowerMassSM), " m/z or above ", tags$code(input$upperMassSM), " m/z were removed from the analysis. ")
  })
  
  
  
  # Updating IDBac ----------------------------------------------------------
  
  
  # Updating IDBac Functions
  
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
  
  
  
  
  
  # Code to stop shiny/R when app is closed ---------------------------------
  
  
  
  
  
  #  The following code is necessary to stop the R backend when the user closes the browser window
    # session$onSessionEnded(function() {
    # 
    #    stopApp()
    #    q("no")
    #  })
    # 
  
  # wq <-pool::dbPool(drv = RSQLite::SQLite(),
  #              dbname = paste0("wds", ".sqlite"))
  # 
  # onStop(function() {
  #   pool::poolClose(wq)
  #   print(wq)
  # }) # important!
  
  
}

