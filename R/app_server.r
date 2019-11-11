#' @importFrom magrittr "%>%"
#' @import shiny
#' @import dendextend
#' @import rhandsontable

NULL

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
  
  set.seed(42)
  if (isTRUE(getOption("shiny.testmode"))) {
    options(idbac_testing = TRUE)
  }
  
  # Development Functions ---------------------------------------------------
  
  IDBacApp::newUpdate()
  
  
  sqlDirectory <- reactiveValues(sqlDirectory = IDBacApp::findIdbacHome())
  
  # module changes sqlDirectory reactiveValue
  callModule(IDBacApp::selectDirectory_Server,
             "userWorkingDirectory",
             sqlDirectory)
  
  
  output$userWorkingDirectoryText <- renderText(sqlDirectory$sqlDirectory)
  
  
  
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
  tempMZDir <- file.path(tempdir(), "temp_mzML")
  dir.create(tempMZDir)
  
  # Cleanup mzML temp folder on initialization of app
  file.remove(list.files(tempMZDir,
                         pattern = ".mzML",
                         recursive = FALSE,
                         full.names = TRUE))
  
  availableDatabases <- reactiveValues(db = NULL)
  
  
  # Set constants -----------------------------------------------------------
  
  # m/z to separate small molecule and protein spectra:
  # If max mass is > than this, will be classified as protein spectrum 
  smallProteinMass <- 6000  
  
  
  # Conversions Tab ---------------------------------------------------------
  
  pwizAvailable <- IDBacApp::findMSconvert()
  
  
  
  callModule(IDBacApp::convertDataTab_Server,
             "convertDataTab",
             tempMZDir = tempMZDir,
             sqlDirectory = sqlDirectory,
             availableExperiments = availableDatabases,
             pwizAvailable = pwizAvailable)
  
  
  observeEvent(input$processToAnalysis,  
               ignoreInit = TRUE, {
                 updateTabsetPanel(session, "mainIDBacNav",
                                   selected = "sqlUiTab")
                 removeModal()
               })
  
  # SQL Tab -----------------------------------------------------------------
  
  
  # Find the available databases, and make reactive so can be updated if more are created
  
  
  observe({
    samps <- tools::file_path_sans_ext(list.files(sqlDirectory$sqlDirectory,
                                                  pattern = ".sqlite",
                                                  full.names = FALSE,
                                                  recursive = FALSE)
    )
    if (length(samps) == 0) {
      availableDatabases$db <- NULL
    } else {
      availableDatabases$db <- samps
    }
    
    
  })
  
  
  workingDB <- callModule(IDBacApp::databaseTabServer,
                          "sqlUIcreator",
                          sqlDirectory = sqlDirectory,
                          availableExperiments = availableDatabases)
  
  
  
  
  # Trigger add tabs --------------------------------------------------------
  
  
  #This "observe" event creates the SQL tab UI.
  observeEvent(availableDatabases$db,
               ignoreNULL = TRUE,
               once = TRUE,{
                 if (length(availableDatabases$db) > 0) {
                   
                   appendTab(inputId = "mainIDBacNav",
                             tabPanel("Work with Previous Experiments",
                                      value = "sqlUiTab",
                                      IDBacApp::databaseTabUI("sqlUIcreator")
                                      
                             )
                   )
                   
                 }
               })
  
  
  observeEvent(workingDB$move$selectExperiment,
               ignoreInit = TRUE, {
                 
                 removeTab(inputId = "mainIDBacNav",
                           target = "Protein Data Analysis"
                 )
                 removeTab(inputId = "mainIDBacNav",
                           target = "Small Molecule Data Analysis"
                 )
                 pool <- pool::poolCheckout(workingDB$pool())
                 
                 p <- DBI::dbGetQuery(pool, glue::glue("SELECT 1 
                                                       FROM IndividualSpectra 
                                                       WHERE maxMass > {smallProteinMass}
                                                       LIMIT 1;"))
                 s <- DBI::dbGetQuery(pool, glue::glue("SELECT 1 
                                                       FROM IndividualSpectra 
                                                       WHERE maxMass > {smallProteinMass}
                                                       LIMIT 1;"))
                 
                 p <- nrow(p)
                 s <- nrow(s)
                 
                 pool::poolReturn(pool)
                 if (p > 0) {
                   appendTab(inputId = "mainIDBacNav",
                             tabPanel("Protein Data Analysis",
                                      uiOutput("Heirarchicalui")
                             )
                   )
                 }
                 if (s > 0) {
                   appendTab(inputId = "mainIDBacNav",
                             tabPanel("Small Molecule Data Analysis",
                                      IDBacApp::ui_smallMolMan()
                             )
                   )
                 }
               })
  
  
  
  
  # Mirror Plots ------------------------------------------------------------
  
  
  proteinPeakSettings <-  callModule(IDBacApp::peakRetentionSettings_Server,
                                     "protMirror")
  
  callModule(IDBacApp::mirrorPlots_Server,
             "protMirror",
             workingDB,
             proteinOrSmall = '>')
  
  
  smallPeakSettings <- callModule(IDBacApp::peakRetentionSettings_Server,
                                  "smallMirror")
  callModule(IDBacApp::smallmirrorPlots_Server,
             "smallMirror",
             workingDB,
             proteinOrSmall = "smallMoleculePeaks")
  
  
  
  
  
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
    # bin peaks and keep only the peaks that occur in proteinPeakSettings$percentPresence percent of replicates
    # merge into a single peak list per sample
    # trim m/z based on user input
    # connect to sql

    temp <- lapply(chosenProteinSampleIDs$chosen,
                   function(ids){
                     IDBacApp::collapseReplicates(pool = isolate(workingDB$pool()),
                                                  sampleIDs = ids,
                                                  peakPercentPresence = proteinPeakSettings$percentPresence,
                                                  lowerMassCutoff = proteinPeakSettings$lowerMass,
                                                  upperMassCutoff = proteinPeakSettings$upperMass, 
                                                  minSNR = proteinPeakSettings$SNR, 
                                                  tolerance = 0.002,
                                                  protein = TRUE)[[1]]
                   })

    
    
    # Inject samples into dendrogram
    if (length(proteinSamplesToInject$chosen$chosen) > 0) {
      

      temp <- c(temp, lapply(proteinSamplesToInject$chosen$chosen,
                             function(ids){
                               IDBacApp::collapseReplicates(pool = proteinSamplesToInject$db(),
                                                            sampleIDs = ids,
                                                            peakPercentPresence = proteinPeakSettings$percentPresence,
                                                            lowerMassCutoff = proteinPeakSettings$lowerMass,
                                                            upperMassCutoff = proteinPeakSettings$upperMass, 
                                                            minSNR = proteinPeakSettings$SNR, 
                                                            tolerance = 0.002,
                                                            protein = TRUE)[[1]]
                             })
      )
      
      
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
    
    req(!is.null(collapsedPeaksForDend()))
    req(any(!emptyProtein()))
    req(length(!emptyProtein() > 3))
    
    validate(need(proteinPeakSettings$lowerMass >= 2000,
                  "Lower mass cutoff must be greater than 2,000"))
    validate(need(proteinPeakSettings$upperMass <= 20000,
                  "Lower mass cutoff must be less than 20,000"))
    validate(need(proteinPeakSettings$lowerMass < proteinPeakSettings$upperMass,
                  "Lower mass cutoff should be higher than upper mass cutoff."))

    IDBacApp::createFuzzyVector(massStart = proteinPeakSettings$lowerMass,
                         massEnd = proteinPeakSettings$upperMass,
                         ppm = proteinPeakSettings$ppm,
                         massList = lapply(collapsedPeaksForDend()[!emptyProtein()], function(x) x@mass),
                         intensityList = lapply(collapsedPeaksForDend()[!emptyProtein()], function(x) x@intensity))
      
      
      
      
      
      
    
  })
  
  
  
  
  
  emptyProtein <- reactive({
    unlist(lapply(collapsedPeaksForDend(),
                  MALDIquant::isEmpty))
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
  
  
  hclustHeightReactive <- reactive({
    validate(need(input$hclustHeight > 20, 
                  '"Exapand Dendrogram" must be >20')
             )
    input$hclustHeight
  })
  
  dendparmarReactive <- reactive({
    validate(need(input$dendparmar > 0, 
                  '"Adjust right margin of dendrogram" must be >1')
    )
    input$dendparmar
  })
  
  
  proteinDendColored <- shiny::callModule(IDBacApp::dendDotsServer,
                                          "proth",
                                          dendrogram = proteinDendrogram,
                                          pool = workingDB$pool,
                                          plotWidth = reactive(dendparmarReactive()),
                                          plotHeight = reactive(hclustHeightReactive()),
                                          boots = dendMaker,
                                          dendOrPhylo = reactive(input$dendOrPhylo),
                                          emptyProtein = emptyProtein)
  
  
  unifiedProteinColor <- reactive(dendextend::labels_colors(proteinDendrogram$dendrogram))
  
  
  #  PCoA Calculation -------------------------------------------------------
  
  
  
  proteinPcoaCalculation <- reactive({
    
    IDBacApp::pcoaCalculation(distanceMatrix = dendMaker()$distance)
    
  })
  
  callModule(IDBacApp::popupPlot_server,
             "proteinPCOA",
             dataFrame = proteinPcoaCalculation,
             namedColors = unifiedProteinColor,
             plotTitle = "Principle Coordinates Analysis")
  
  
  # PCA Calculation  --------------------------------------------------------
  
  
  
  proteinPcaCalculation <- reactive({
    
    IDBacApp::pcaCalculation(dataMatrix = proteinMatrix(),
                             logged = TRUE,
                             scaled = TRUE,
                             centered = TRUE,
                             missing = 0.00001)
  })
  
  
  
  
  callModule(IDBacApp::popupPlot_server,
             "proteinPCA",
             dataFrame = proteinPcaCalculation,
             namedColors = unifiedProteinColor,
             plotTitle = "Principle Components Analysis")
  
  
  
  
  
  
  # Calculate tSNE based on PCA calculation already performed ---------------
  
  
  callModule(IDBacApp::popupPlotTsne_server,
             "tsnePanel",
             data = proteinMatrix,
             plotTitle = "t-SNE",
             namedColors = unifiedProteinColor)
  
  
  
  
  # Protein Hierarchical clustering calculation and plotting ----------------
  
  
  
  # Create Protein Dendrogram UI --------------------------------------------
  
  output$Heirarchicalui <-  renderUI({
    
    
    IDBacApp::ui_proteinClustering()
    
    
  })
  
  # Paragraph to relay info for reporting protein ---------------------------
  
  
  output$proteinReport <- renderUI({
    req(!is.null(chosenProteinSampleIDs$chosen))
    req(length(chosenProteinSampleIDs$chosen) > 2)
    req(!is.null(attributes(proteinDendrogram$dendrogram)$members))
    
    
    shiny::tagList(
      h4("Suggestions for Reporting Protein Analysis:"),
      p("This dendrogram was created by analyzing ",tags$code(attributes(proteinDendrogram$dendrogram)$members), " samples,
          and retaining peaks with a signal to noise ratio above ",tags$code(proteinPeakSettings$SNR)," and occurring in greater than ",tags$code(proteinPeakSettings$percentPresence),"% of replicate spectra.
          Peaks occuring below ",tags$code(proteinPeakSettings$lowerMass)," m/z or above ",tags$code(proteinPeakSettings$upperMass)," m/z were removed from the analyses. ",
        "For clustering spectra, ",tags$code(input$distance), " distance and ",tags$code(input$clustering), " algorithms were used.")
    )
    
  })
  
  
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
      
      out <- rmarkdown::render('C:/Users/chase/Documents/GitHub/IDBacApp/ResultsReport.Rmd', switch(
        input$format,
        HTML = rmarkdown::html_document()
      ))
      file.rename(out, file)
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
  
  
  
  output$smallMolPcaPlot <- plotly::renderPlotly({
    
    req(nrow(smallMolDataFrame()) > 2,
        ncol(smallMolDataFrame()) > 2)
    
    hello <<- smallMolDataFrame()
    print("1")
    princ <- IDBacApp::pcaCalculation(smallMolDataFrame())
    namedColors <- NULL
    
    if (is.null(namedColors)) {
      colorsToUse <- cbind.data.frame(fac = rep("#000000", nrow(princ)), 
                                      princ)
    } else {
      
      colorsToUse <- cbind.data.frame(fac = as.vector(namedColors), 
                                      nam = (names(namedColors)))
      
      
      colorsToUse <- merge(princ,
                           colorsToUse, 
                           by = "nam")
    }
    
    plotly::plot_ly(data = colorsToUse,
                    x = ~Dim1,
                    y = ~Dim2,
                    z = ~Dim3,
                    type = "scatter3d",
                    mode = "markers",
                    marker = list(color = ~fac),
                    hoverinfo = 'text',
                    text = ~nam) 
    
  })
  # Small mol ---------------------------------------------------------------
  
  output$matrixSelector <- renderUI({
    IDBacApp::bsCollapse(id = "collapseMatrixSelection",
                         open = "Panel 1",
                         IDBacApp::bsCollapsePanel(p("Select a Sample to Subtract", 
                                                     align = "center"),
                                                   tags$div(id='selectMatrixBlank',
                                                            class='mirror_select',
                                                            selectizeInput("selectMatrix",
                                                                           label = "",
                                                                           options= list(maxOptions = 2000),
                                                                           choices = c("None", smallMolIDs()))
                                                   )
                                                   
                         )
    )
  })
  
  
  
  smallMolIDs <- reactive({
    checkedPool <- pool::poolCheckout(workingDB$pool())
    # retrieve all Strain_IDs in db that have small molecule spectra
    
    sampleIDs <- DBI::dbGetQuery(checkedPool, glue::glue("SELECT DISTINCT Strain_ID
                                                          FROM IndividualSpectra 
                                                          WHERE maxMass < {smallProteinMass}"))
    
    pool::poolReturn(checkedPool)
    
    return(sampleIDs)
    
  })  
  
  
  
  
  
  subtractedMatrixBlank <- reactiveValues(maldiQuantPeaks = NULL,
                                          sampleIDs = NULL)
  observe({
    req(workingDB$pool(),
        smallPeakSettings$percentPresence,
        smallPeakSettings$lowerMass,
        smallPeakSettings$upperMass,
        smallPeakSettings$SNR,
        input$selectMatrix)
    
    validate(need(smallPeakSettings$lowerMass < smallPeakSettings$upperMass, "Upper mass cutoff must be greater than lower mass cutoff."))
    
    
    samples <- IDBacApp::getSmallMolSpectra(pool = workingDB$pool(),
                                            sampleIDs = NULL,
                                            dendrogram = proteinDendrogram$dendrogram,
                                            brushInputs = smallProtDend,
                                            matrixIDs = NULL,
                                            peakPercentPresence = smallPeakSettings$percentPresence,
                                            lowerMassCutoff = smallPeakSettings$lowerMass,
                                            upperMassCutoff = smallPeakSettings$upperMass,
                                            minSNR = smallPeakSettings$SNR)
    ids <- samples$sampleIDs
    samples <- samples$maldiQuantPeaks
    
    # if no samples are selected, stop
    req(length(ids) > 0)
    
    aq<-sum(sapply(samples, function(x) length(x@mass))) > 1
    
    if(!aq){
      subtractedMatrixBlank$maldiQuantPeaks <- NULL
      subtractedMatrixBlank$sampleIDs <- NULL
    } else {
      
      
      if ((input$selectMatrix != "None")) {
        
        matrixSample <- IDBacApp::getSmallMolSpectra(pool = workingDB$pool(),
                                                     sampleIDs = input$selectMatrix,
                                                     dendrogram = proteinDendrogram$dendrogram,
                                                     brushInputs = smallProtDend,
                                                     matrixIDs = NULL,
                                                     peakPercentPresence = smallPeakSettings$percentPresence,
                                                     lowerMassCutoff = smallPeakSettings$lowerMass,
                                                     upperMassCutoff = smallPeakSettings$upperMass,
                                                     minSNR = smallPeakSettings$SNR)
        
        
        samples <- MALDIquant::binPeaks(c(matrixSample$maldiQuantPeaks, samples),
                                        tolerance = .002)
        
        
        for (i in 2:(length(samples))) {
          
          toKeep <- !samples[[i]]@mass %in% samples[[1]]@mass 
          
          samples[[i]]@mass <- samples[[i]]@mass[toKeep]
          samples[[i]]@intensity <- samples[[i]]@intensity[toKeep]
          samples[[i]]@snr <- samples[[i]]@snr[toKeep]
          
        }
        
        samples <- samples[-1]
        
        subtractedMatrixBlank$maldiQuantPeaks <- samples
        subtractedMatrixBlank$sampleIDs <- ids
        
      } else {
        
        samples <- MALDIquant::binPeaks(samples, tolerance = .002)
        subtractedMatrixBlank$maldiQuantPeaks <- samples
        subtractedMatrixBlank$sampleIDs <- ids
      }
    }
    

  })
  
  
  
  noSmallPeaks <- reactive({
    
    ind <- which(sapply(subtractedMatrixBlank$maldiQuantPeaks, function(x) length(x@mass)) == 0)
    
    subtractedMatrixBlank$sampleIDs[ind]
    
    })
  
  output$noSmallPeaksText <- renderText({
    
    paste0("Samples which contain no peaks: ",
           paste0(noSmallPeaks(), collapse=",\n"))
           
    })
  
  
  
  
  # Small mol MAN serve module ----------------------------------------------
  
  
  callModule(IDBacApp::MAN_Server,
             "smMAN",
             subtractedMatrixBlank = subtractedMatrixBlank,
             proteinDend = proteinDendrogram)
  
  
  
  
  # Small molecule data frame reactive --------------------------------------
  
  smallMolDataFrame <- reactive({
    req(MALDIquant::isMassPeaksList(subtractedMatrixBlank$maldiQuantPeaks))
    smallNetwork <- MALDIquant::intensityMatrix(subtractedMatrixBlank$maldiQuantPeaks)
    rownames(smallNetwork) <- subtractedMatrixBlank$sampleIDs
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
    p("This MAN was created by analyzing ", tags$code(length(subtractedMatrixBlank$sampleIDs)), " samples,",
      if (input$selectMatrix != "None") {
        ("subtracting a matrix blank,") 
      } else {},
      " retaining peaks with a signal to noise ratio above ", tags$code(smallPeakSettings$SNR), ", and occurring in greater than ", tags$code(smallPeakSettings$percentPresence), "% of replicate spectra.
          Peaks occuring below ", tags$code(smallPeakSettings$lowerMass), " m/z or above ", tags$code(smallPeakSettings$upperMass), " m/z were removed from the analysis. ")
  })
  
  
  
  
  # Updating IDBac ----------------------------------------------------------
  
  
  # Updating IDBac Functions
  
  observeEvent(input$updateIDBac, 
               ignoreInit = TRUE, {
                 withConsoleRedirect <- function(containerId, expr) {
                   # Change type="output" to type="message" to catch stderr
                   # (messages, warnings, and errors) instead of stdout.
                   txt <- utils::capture.output(results <- expr, type = "message")
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
                   local_version <- tryCatch(utils::packageVersion("IDBacApp"),
                                             error = function(x) paste("Installed version is latest version"),
                                             finally = function(x) utils::packageVersion("IDBacApp"))
                   
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
                       remotes::install_github("chasemc/IDBacApp",
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
                       
                     } else if (utils::compareVersion(as.character(local_version), 
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
  
  if (!base::interactive()) {
   session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
  
  
  
  
}

