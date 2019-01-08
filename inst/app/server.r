
# Setup working directories
workingDirectory <- getwd()
tempMZ <- file.path(workingDirectory, "temp_mzML")
dir.create(tempMZ)
# Cleanup mzML temp folder 
file.remove(list.files(tempMZ,
                       pattern = ".mzML",
                       recursive = FALSE,
                       full.names = TRUE))


# wq <-pool::dbPool(drv = RSQLite::SQLite(),
#              dbname = paste0("wds", ".sqlite"))
# 
# onStop(function() {
#   pool::poolClose(wq)
#   print(wq)
# }) # important!


shiny::registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data)){
    NULL
  } else {
      list(left=as.character(data$left), right=as.character(data$right))
  }}, force = TRUE)


#delete
#chase change to id

# The server portion of the Shiny app serves as the backend, 
# performing data processing and creating the visualizations 
# to be displayed as specified in the UI function(input, output,session){}

# Function to Install and Load R Packages
Install_And_Load <- function(Required_Packages)
{
  Remaining_Packages <-
    Required_Packages[!(Required_Packages %in% installed.packages()[, "Package"])]
  if (length(Remaining_Packages))
  {
    install.packages(Remaining_Packages)
  }
  for (package_name in Required_Packages)
  {
    library(package_name,
            character.only = TRUE,
            quietly = TRUE)
  }
}

# Required packages to install and load
#----
Required_Packages = c("Rcpp",
                      "devtools",
                      "svglite",
                      "shinyjs",
                      "mzR",
                      "plotly",
                      "colourpicker",
                      "shiny",
                      "MALDIquant",
                      "MALDIquantForeign",
                      "readxl",
                      "networkD3",
                      "ape",
                      "FactoMineR",
                      "dendextend",
                      "networkD3",
                      "reshape2",
                      "plyr",
                      "igraph",
                      "RSQLite",
                      "DBI",
                      "dbplyr",
                      "dplyr",
                      "rhandsontable",
                      "Rtsne",
                      "pool",
                      "magrittr",
                      "shinyBS")


# Install and Load Packages
Install_And_Load(Required_Packages)




# Reactive variable returning the user-chosen working directory as string
function(input,output,session){
  
  
  
  
  #This "observe" event creates the SQL tab UI.
  observe({
    output$rawDataUI <- renderUI({
      conversionsUI("Sd")
    })
    
    
  }) 
  
  #This "observe" event creates the SQL tab UI.
  observe({
    output$sqlUI <- renderUI({
    IDBacApp::ui_sqlUI("ssds", availableExperiments = availableExperiments())
    })
    
    
  })
  

availableExperiments <- reactive({
  
  
  tools::file_path_sans_ext(list.files(workingDirectory,
                                       pattern = ".sqlite",
                                       full.names = FALSE))
})

# Collapsers
#----  

observeEvent(input$styleSelect, {
  updateCollapse(session, "collapseSQLInstructions")
})

observeEvent(input$styleSelect, {
  updateCollapse(session, "modifySqlCollapse")
})

observeEvent(input$styleSelect, {
  isolate(
    updateCollapse(session, "collapseSQLSelector")
  )
})



  



observeEvent(input$selectExperiment,
             once = TRUE, {
  
  appendTab(inputId = "mainIDBacNav",
            tabPanel("Compare Two Samples (Protein)",
                     value = "inversePeaks",
                     uiOutput("inversepeakui")
            )
  )
})


observeEvent(input$selectExperiment,
             once = TRUE, {
  
  appendTab(inputId = "mainIDBacNav",
            tabPanel("Hierarchical Clustering (Protein)",
                     uiOutput("Heirarchicalui")
            )
  )
})



observeEvent(input$selectExperiment,
             once = TRUE, {
  
  appendTab(inputId = "mainIDBacNav",
            tabPanel("Metabolite Association Network (Small-Molecule)",
                     uiOutput("MANui")
            )
  )
})







#----

observeEvent(input$moveToAnalysis, {
  updateTabsetPanel(session, "mainIDBacNav",
                    selected = "inversePeaks")
})

observeEvent(input$moveToSelectExperiment, {
  updateNavlistPanel(session, "ExperimentNav",
                    selected = "experiment_select_tab")
})


#----
output$selectedSQLText <- renderPrint({
  fileNames <- tools::file_path_sans_ext(list.files(workingDirectory,
                                                    pattern = ".sqlite",
                                                    full.names = FALSE))
  filePaths <- list.files(workingDirectory,
                          pattern = ".sqlite",
                          full.names = TRUE)
 filePaths[which(fileNames == input$selectExperiment)]
  
})




#----
newExperimentSqlite <- reactive({
  # This pool is used when creating an entirely new "experiment" .sqlite db
  name <- base::make.names(input$newExperimentName)
  
  # maxx 100 characters with ".sqlite"
 name <-  base::substr(name, 1, 93)
  
  pool::dbPool(drv = RSQLite::SQLite(),
               dbname = paste0(name, ".sqlite"))
  
  
  
})

#---- POOLS




userDBCon <- reactive({
  IDBacApp::createPool(fileName = input$selectExperiment,
                                         filePath = workingDirectory)
})




#----
qwerty <- reactiveValues(rtab = data.frame("Strain_ID" = "Placeholder"))


#----
observeEvent(input$searchNCBI, {
 # IDBacApp::searchNCBI()
})



observeEvent(input$saven,{
  
  dbWriteTable(userDBCon(),
               "metaData",
               rhandsontable::hot_to_r(input$metaTable)[-1, ], # remove exmple row 
               overwrite = TRUE)  
  
})






#----
output$metaTable <- rhandsontable::renderRHandsontable({
  
  rhandsontable::rhandsontable(qwerty$rtab,
                               useTypes = FALSE,
                               contextMenu = TRUE ) %>%
    hot_col("Strain_ID",
            readOnly = TRUE) %>%
    hot_row(1,
            readOnly = TRUE) %>%
    hot_context_menu(allowRowEdit = FALSE,
                     allowColEdit = TRUE) %>%
    hot_cols(colWidths = 100) %>%
    hot_rows(rowHeights = 25) %>%
    hot_cols(fixedColumnsLeft = 1)
  
  
  
})



#----
observeEvent(c(input$selectExperiment),{
  
if (!is.null(userDBCon())){
  metadb <- userDBCon()
  
  if (!"metaData" %in% DBI::dbListTables(metadb)) {
   
    warning("It appears the experiment file may be corrupt, please create again.")
    qwerty$rtab <- data.frame(Strain_ID = "It appears the experiment file may be corrupt, please create the experiment again.")
    
  } else{
    
    
    dbQuery <- glue::glue_sql("SELECT *
                                  FROM ({tab*})",
                              tab = "metaData",
                              .con = metadb)
    
    conn <- pool::poolCheckout(metadb)
    dbQuery <- DBI::dbGetQuery(conn, dbQuery)

    exampleMetaData <- data.frame(      "Strain_ID"                    = "Example_Strain",
                                        "Genbank_Accession"            = "KY858228",
                                        "NCBI_TaxID"                   = "446370",
                                        "Kingdom"                      = "Bacteria",
                                        "Phylum"                       = "Firmicutes",
                                        "Class"                        = "Bacilli",
                                        "Order"                        = "Bacillales",
                                        "Family"                       = "Paenibacillaceae",
                                        "Genus"                        = "Paenibacillus",
                                        "Species"                      = "telluris",
                                        "MALDI_Matrix"                 = "CHCA",
                                        "DSM_Agar_Media"               = "1054_Fresh",
                                        "Cultivation_Temp_Celsius"     = "27",
                                        "Cultivation_Time_Days"        = "10",
                                        "Cultivation_Other"            = "",
                                        "User"                         = "Chase Clark",
                                        "User_ORCID"                   = "0000-0001-6439-9397",
                                        "PI_FirstName_LastName"        = "Brian Murphy",
                                        "PI_ORCID"                     = "0000-0002-1372-3887",
                                        "dna_16S"                      = "TCCTGCCTCAGGACGAACGCTGGCGGCGTGCCTAATACATGCAAGTCGAGCGGAGTTGATGGAGTGCTTGCACTCCTGATGCTTAGCGGCGGACGGGTGAGTAACACGTAGGTAACCTGCCCGTAAGACTGGGATAACATTCGGAAACGAATGCTAATACCGGATACACAACTTGGTCGCATGATCGGAGTTGGGAAAGACGGAGTAATCTGTCACTTACGGATGGACCTGCGGCGCATTAGCTAGTTGGTGAGGTAACGGCTCACCAAGGCGACGATGCGTAGCCGACCTGAGAGGGTGATCGGCCACACTGGGACTGAGACACGGCCCAGACTCCTACGGGAGGCAGCAGTAGGGAATCTTCCGCAATGGACGAAAGTCTGACGGAGCAACGCCGCGTGAGTGATGAAGGTTTTCGGATCGTAAAGCTCTGTTGCCAGGGAAGAACGCTAAGGAGAGTAACTGCTCCTTAGGTGACGGTACCTGAGAAGAAAGCCCCGGCTAACTACGTGCCAGCAGCCGCGGTAATACGTAGGGGGCAAGCGTTGTCCGGAATTATTGGGCGTAAAGCGCGCGCAGGCGGCCTTGTAAGTCTGTTGTTTCAGGCACAAGCTCAACTTGTGTTCGCAATGGAAACTGCAAAGCTTGAGTGCAGAAGAGGAAAGTGGAATTCCACGTGTAGCGGTGAAATGCGTAGAGATGTGGAGGAACACCAGTGGCGAAGGCGACTTTCTGGGCTGTAACTGACGCTGAGGCGCGAAAGCGTGGGGAGCAAACAGGATTAGATACCCTGGTAGTCCACGCCGTAAACGATGAATGCTAGGTGTTAGGGGTTTCGATACCCTTGGTGCCGAAGTTAACACATTAAGCATTCCGCCTGGGGAGTACGGTCGCAAGACTGAAACTCAAAGGAATTGACGGGGACCCGCACAAGCAGTGGAGTATGTGGTTTAATTCGAAGCAACGCGAAGAACCTTACCAGGTCTTGACATCCCTCTGAATCTGCTAGAGATAGCGGCGGCCTTCGGGACAGAGGAGACAGGTGGTGCATGGTTGTCGTCAGCTCGTGTCGTGAGATGTTGGGTTAAGTCCCGCAACGAGCGCAACCCTTGATCTTAGTTGCCAGCAGGTKAAGCTGGGCACTCTAGGATGACTGCCGGTGACAAACCGGAGGAAGGTGGGGATGACGTCAAATCATCATGCCCCTTATGACCTGGGCTACACACGTACTACAATGGCCGATACAACGGGAAGCGAAACCGCGAGGTGGAGCCAATCCTATCAAAGTCGGTCTCAGTTCGGATTGCAGGCTGCAACTCGCCTGCATGAAGTCGGAATTGCTAGTAATCGCGGATCAGCATGCCGCGGTGAATACGTTCCCGGGTCTTGTACACACCGCCCGTCACACCACGAGAGTTTACAACACCCGAAGCCGGTGGGGTAACCGCAAGGAGCCAGCCGTCGAAGGTGGGGTAGATGATTGGGGTGAAGTCGTAAC"
    )
    
    qwerty$rtab <- rbind(exampleMetaData, dbQuery)
    pool::poolReturn(conn)
  }

}
})








#This "observe" event creates the UI element for analyzing a single MALDI plate, based on user-input.
#----
observe({
    if (is.null(input$startingWith)){} else if (input$startingWith == 3){
    output$mzConversionUI <- renderUI({
      IDBacApp::beginWithMZ("beginWithMZ")
    })
  }
})


# Reactive variable returning the user-chosen location of the raw MALDI files as string
#----
mzmlRawFilesLocation <- reactive({
  if (input$mzmlRawFileDirectory > 0) {
    IDBacApp::choose_dir()
  }
})


# Creates text showing the user which directory they chose for raw files
#----
output$mzmlRawFileDirectory <- renderText({
  if (is.null(mzmlRawFilesLocation())) {
    return("No Folder Selected")
  } else {
    folders <- NULL
    
    findmz <- function(){
      # sets time limit outside though so dont use yet setTimeLimit(elapsed = 5, transient = FALSE)
      return(list.files(mzmlRawFilesLocation(),
                        recursive = TRUE,
                        full.names = FALSE,
                        pattern = "\\.mz"))
      # setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
      
    }
    
    
    # Get the folders contained within the chosen folder.
    foldersInFolder <- tryCatch(findmz(),
                                error = function(x) paste("Timed out"),
                                finally = function(x) x)
    
    if (foldersInFolder == "Timed out"){
      return("Timed out looking for mzML/mzXML files. This can happen if the folder you 
          selected has lots of folders within it... because IDBac looks through all 
          of them for mzML/mzXML files.")}else{
            
            for (i in 1:length(foldersInFolder)) {
              # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
              folders <- paste0(folders, 
                                "\n",
                                basename(foldersInFolder[[i]]))
            }
            return(folders)
          }}
  
  
})




# Reactive variable returning the user-chosen location of the raw delim files as string
#----
delimitedLocationP <- reactive({
  if (input$delimitedDirectoryP > 0) {
    IDBacApp::choose_dir()
  }
})


# Reactive variable returning the user-chosen location of the raw delim files as string
#----
delimitedLocationSM <- reactive({
  if (input$delimitedDirectorySM > 0) {
    IDBacApp::choose_dir()
  }
})


# Creates text showing the user which directory they chose for raw files
#----
output$delimitedLocationSMo <- renderText({
  if (is.null(delimitedLocationSM())) {
    return("No Folder Selected")} else {
      folders <- NULL
      foldersInFolder <- list.files(delimitedLocationSM(), recursive = FALSE, full.names = FALSE) # Get the folders contained directly within the chosen folder.
      for (i in 1:length(foldersInFolder)) {
        folders <- paste0(folders, "\n", foldersInFolder[[i]]) # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
      }
      folders
    }
})


# Creates text showing the user which directory they chose for raw files
#----
output$delimitedLocationPo <- renderText({
  if (is.null(delimitedLocationP())) {
    return("No Folder Selected")} else {
      folders <- NULL
      foldersInFolder <- list.files(delimitedLocationP(), recursive = FALSE, full.names = FALSE) # Get the folders contained directly within the chosen folder.
      for (i in 1:length(foldersInFolder)) {
        folders <- paste0(folders, "\n", foldersInFolder[[i]]) # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
      }
      folders
    }
})


#This "observe" event creates the UI element for analyzing a single MALDI plate, based on user-input.
#----
observeEvent(c(input$ConversionsNav,
             input$rawORreanalyze),
             {
  
  if (input$ConversionsNav == "convert_bruker_nav"){
    
    if(is.null(input$rawORreanalyze)) {
    } else if(input$rawORreanalyze == 1){
    
    output$conversionMainUI1 <- renderUI({
      IDBacApp::oneMaldiPlate("oneMaldiPlate")
    }) 
    } else if (input$rawORreanalyze == 2) {
    output$conversionMainUI1 <- renderUI({
      IDBacApp::multipleMaldiPlates("multipleMaldiPlates")
    })
    }
  }
  
  
  if (input$ConversionsNav == "convert_mzml_nav"){
    output$conversionMainUI2 <- renderUI({
      IDBacApp::beginWithMZ("beginWithMZ")
    })
  } 
  
  if (input$ConversionsNav == "convert_txt_nav"){
    output$conversionMainUI3 <- renderUI({
      IDBacApp::beginWithTXT("beginWithTXT")
    })
  } 
  
})








# Find if "IDBac" exists in selected folder and then uniquify if necessary
#----
uniquifiedIDBac <- reactive({
  req(selectedDirectory())
  uniquifiedIDBacs <- list.dirs(selectedDirectory(), 
                                recursive = F,
                                full.names = F)
  uniquifiedIDBacs <- make.unique(c(uniquifiedIDBacs, "IDBac"), 
                                  sep = "-")
  tail(uniquifiedIDBacs, 1)
})


# When ReAnalyzing data, and need to select the "IDBac" folder directly
#----
pressedidbacDirectoryButton <- reactive({
  if(is.null(input$idbacDirectoryButton)){
    return("No Folder Selected")
  } else if (input$idbacDirectoryButton > 0){
    IDBacApp::choose_dir()
  }
})


# Reactive variable returning the user-chosen location of the raw MALDI files as string
#----
rawFilesLocation <- reactive({
  if (input$rawFileDirectory > 0) {
    IDBacApp::choose_dir()
  }
})


# Creates text showing the user which directory they chose for raw files
#----
output$rawFileDirectory <- renderText({
  if (is.null(rawFilesLocation())) {
    return("No Folder Selected")
  } else {
    folders <- NULL
    # Get the folders contained within the chosen folder.
    foldersInFolder <- list.dirs(rawFilesLocation(),
                                 recursive = FALSE,
                                 full.names = FALSE) 
    for (i in 1:length(foldersInFolder)) {
      # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
      folders <- paste0(folders, 
                        "\n",
                        foldersInFolder[[i]])
    }
    return(folders)
  }
})


# Reactive variable returning the user-chosen location of the raw MALDI files as string
#----
multipleMaldiRawFileLocation <- reactive({
  if (input$multipleMaldiRawFileDirectory > 0) {
    IDBacApp::choose_dir()
  }
})


# Creates text showing the user which directory they chose for raw files
#----
output$multipleMaldiRawFileDirectory <- renderText({
  if (is.null(multipleMaldiRawFileLocation())){
    return("No Folder Selected")
  } else {
    folders <- NULL
    # Get the folders contained within the chosen folder.
    foldersInFolder <- list.dirs(multipleMaldiRawFileLocation(),
                                 recursive = FALSE, 
                                 full.names = FALSE) 
    for (i in 1:length(foldersInFolder)) {
      # Creates user feedback about which raw data folders were chosen. 
      # Individual folders displayed on a new line "\n"
      folders <- paste0(folders, "\n", foldersInFolder[[i]]) 
    }
    
    return(folders)
  }
})


# Spectra conversion
#This observe event waits for the user to select the "run" action button and then creates the folders for storing data and converts the raw data to mzML
#----
spectraConversion <- reactive({
  
  IDBacApp::excelMaptoPlateLocation(rawORreanalyze = input$rawORreanalyze,
                                    excelFileLocation = input$excelFile$datapath,
                                    rawFilesLocation = rawFilesLocation(),
                                    multipleMaldiRawFileLocation = multipleMaldiRawFileLocation())
  
})

# Returns a named character list 
  # character vector = filepaths of mzml files 
  # `names` = sample names  
#----
conversions <- reactive({
  
  if(isolate(input$ConversionsNav == "convert_mzml_nav")){
    
    paths <- list.files(mzmlRawFilesLocation(),
                        recursive = TRUE,
                        full.names = TRUE,
                        pattern = ".mz") 
    
    names(paths) <- tools::file_path_sans_ext(base::basename(paths))
  }

  popup1()
  return(paths)
  
})




# Run raw data processing on delimited-type input files
#----
observeEvent(input$runDelim,{
  
  popup1()
  
  IDBacApp::parseDelimitedMS(proteinDirectory = delimitedLocationP(),
                             smallMolDirectory = delimitedLocationSM(),
                             exportDirectory =  tempdir())
  popup2()
})


# Modal to display while converting to mzML
#----
popup1 <- reactive({
  showModal(modalDialog(
    title = "Important message",
    "When file-conversions are complete this pop-up will be replaced by a summary of the conversion.",
    br(),
    "To check what has been converted, you can navigate to:",
    easyClose = FALSE, size="l",
    footer = ""))
})


# Popup summarizing the final status of the conversion
#----
popup2 <- reactive({
  showModal(modalDialog(
    title = "Conversion Complete",
    paste0(" files were converted into open data format files."),
    br(),
    "To check what has been converted you can navigate to:",
    easyClose = TRUE,
    footer = tagList(actionButton("beginPeakProcessingModal", 
                                  "Click to continue with Peak Processing"),
                     modalButton("Close"))
  ))
})


# Call the Spectra processing function when the spectra processing button is pressed
#----
observeEvent(input$run, {
      
      
      popup3()
      
      rawDataFilePath <- conversions()
      lengthProgress <- length(rawDataFilePath)
      
      userDB <- pool::poolCheckout(newExperimentSqlite())
      withProgress(message = 'Processing in progress',
                   detail = 'This may take a while...',
                   value = 0, {
                     
                     for(i in base::seq_along(rawDataFilePath)){
                       incProgress(1/lengthProgress)
                       IDBacApp::spectraProcessingFunction(rawDataFilePath = as.vector(rawDataFilePath)[[i]],
                                                           sampleID = names(rawDataFilePath)[[i]],
                                                           userDBCon = userDB) # pool connection
                     }
                     
                   })
      
      
      pool::poolReturn(userDB)


      
      
      # aa2z <-newExperimentSqlite()
      # 
      # numCores <- parallel::detectCores()
      # cl <- parallel::makeCluster(numCores)
      # parallel::parLapply(cl,fileList, function(x)
      #                     IDBacApp::spectraProcessingFunction(rawDataFilePath = x,
      #                                     userDBCon = aa2z))
      # 
      # 
      
      #  parallel::stopCluster(cl)
      
      
      
      
      popup4()
})


# Modal displayed while speactra -> peak processing is ocurring
#----
popup3 <- reactive({
  showModal(modalDialog(
    title = "Important message",
    "When spectra processing is complete you will be able to begin with the data analysis",
    br(),
    "To check the progress, observe the progress bar at bottom right or navigate to the following directory, where four files will be created per sample ",
    easyClose = FALSE, 
    size = "l",
    footer = ""))
})


# Popup notifying user when spectra processing is complete
#----
popup4 <- reactive({
  showModal(modalDialog(
    title = "Spectra Processing is Now Complete",
    br(),
    easyClose = FALSE,
    tagList(actionButton("processToAnalysis", 
                       "Click to continue"))
  ))
  
})


  observeEvent(input$processToAnalysis, {
    updateTabsetPanel(session, "mainIDBacNav",
                      selected = "sqlUiTab")
    removeModal()
  })



#------------------------------------------------------------------------------
# Mirror Plots
#------------------------------------------------------------------------------

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
                              label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%) (Experiment/Hypothesis dependent)"),value = 70,step=10,min=0,max=100),
                 numericInput("pSNR",
                              label = h5(strong("Signal To Noise Cutoff")),
                              value = 4,
                              step= 0.5,
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
  
  db <- dplyr::tbl(userDBCon(), "IndividualSpectra")
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
  db <- dplyr::tbl(userDBCon(), "IndividualSpectra")
  conn <- pool::poolCheckout(userDBCon())
  
  # get protein peak data for the 1st mirror plot selection
  db %>%
    filter(Strain_ID %in% input$Spectra1) %>%
    filter(proteinPeaks != "NA") %>%
    select(spectrumSHA) %>%
    pull %>%
    IDBacApp::collapseProteinReplicates(fileshas = .,
                       db = userDBCon(),
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
                       db = userDBCon(),
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

  # Binpeaks for the two samples so we can color code similar peaks within the plot
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


#------------------------------------------------------------------------------
# Protein processing
#------------------------------------------------------------------------------

# Merge and trim protein replicates
#----
collapsedPeaksP <- reactive({
  req(input$myProteinchooser)
  # For each sample:
  # bin peaks and keep only the peaks that occur in input$percentPresenceP percent of replicates
  # merge into a single peak list per sample
  # trim m/z based on user input
  # connect to sql
  db <- dplyr::tbl(userDBCon(), "IndividualSpectra")

  db %>%
    filter(proteinPeaks != "NA") %>%
    select(spectrumSHA,Strain_ID) %>%
    filter(Strain_ID %in% input$myProteinchooser$right) %>%
    collect %$%
    split(spectrumSHA,Strain_ID) -> temp
  
  #TODO: Lapply might be looked at and consider replacinng with  parallel::parLapply() 
  conn <- pool::poolCheckout(userDBCon())
  
   return( lapply(temp,
                 function(x){
             IDBacApp::collapseProteinReplicates(fileshas = x,
                                                 db = userDBCon(),
                                                 proteinPercentPresence = input$percentPresenceP,
                                                 lowerMassCutoff = input$lowerMass,
                                                 upperMassCutoff = input$upperMass,
                                                 dbConnection = conn)
           }))
    
    pool::poolReturn(conn)
    
})


# Bin peaks across all protein samples and turn into intensity matrix
#----
proteinMatrix <- reactive({
 
 do.call(rbind, binnedProtein())

})


#Create the hierarchical clustering based upon the user input for distance method and clustering technique
#----
dendro <- reactive({

  shiny::callModule(IDBacApp::dendrogramCreator,
                    "prot",
                    proteinMatrix())
     
})



# Turn collapsed peak list into a distance matrix
#----

binnedProtein <- reactive({
      binvec <- lapply(collapsedPeaksP(), function(x) x@mass)
    zq <- IDBacApp::binnR(vectorList = binvec,
                        ppm = 2000, 
                        refSeqStart = 3000,
                        refSeqEnd = 15000)
      
    #  collected <- lapply(zq, function(x) S4Vectors::unique(S4Vectors::subjectHits(x)))
    collected <- lapply(zq, function(x) S4Vectors::unique(S4Vectors::subjectHits(x)))
    
    cvec <- sort(unique(unlist(collected)))
    
     lapply(collected, function(x) match(cvec, x))

      
        
})
  
  
  
  
  

proteinDistance <- reactive({
  
  validate(
    need(length(collapsedPeaksP()) != 0, "Select samples to analyze on left" )
  )
  
  
  IDBacApp::distMatrix(binnedData = binnedProtein(),
                                  method = input$distance)
  
  
  
  
  
  
  
  
})





# PCoA Calculation
#----
pcoaResults <- reactive({
  # number of samples should be greater than k
  shiny::req(nrow(as.matrix(proteinDistance())) > 10)
  bbb<<-proteinDistance()
  IDBacApp::pcoaCalculation(proteinDistance())
})


# output Plotly plot of PCoA results
#----
output$pcoaPlot <- renderPlotly({

  colorsToUse <- dendextend::leaf_colors(coloredDend())

  if(any(is.na(as.vector(colorsToUse)))){
    colorsToUse <-  dendextend::labels_colors(coloredDend())
  }

  colorsToUse <- cbind.data.frame(fac = as.vector(colorsToUse),
                                  nam = (names(colorsToUse)))
  pcaDat <<- merge(pcoaResults(),
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
  
  aaa<<-proteinMatrix()
  IDBacApp::pcaCalculation(dataMatrix = proteinMatrix(),
                 logged = TRUE,
                 scaled = TRUE, 
                 missing = .00001)
  })


# Output Plotly plot of PCA results
#----
output$pcaPlot <- renderPlotly({

  colorsToUse <- dendextend::leaf_colors(coloredDend())
  
  if(any(is.na(as.vector(colorsToUse)))){
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
output$tsnePlot <- renderPlotly({

  colorsToUse <- dendextend::leaf_colors(coloredDend())

  if(any(is.na(as.vector(colorsToUse)))){
    colorsToUse <-  dendextend::labels_colors(coloredDend())
  }

  colorsToUse <- cbind.data.frame(fac = as.vector(colorsToUse), 
                                  nam = (names(colorsToUse)))
  pcaDat <- merge(tsneResults(), 
                  colorsToUse,
                  by="nam")

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
  
  if(is.null(input$Spectra1)){
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
    


        ui_proteinClustering("proteinyo")
      }
})




# UI of paragraph explaining which variables were used
#----
output$proteinReport<-renderUI(
  p("This dendrogram was created by analyzing ",tags$code(length(labels(dendro()))), " samples,
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


# UI for coloring samples by user input metadata
#----
output$sampleGroupColoringui <- renderUI(
  
  if(input$kORheight == "3"){
    tags$div(
      p("To color samples according to user-defined groupings..."),
      p("To use this function, create a different excel file and list all of your sample names in
        different rows of a single column. In other columns you may add additional characteristics
        of your samples (eg. media type, genus, sample location), with one characteristic per column.
        You will have the option to color code your hierarchical clustering plot
        based on these characteristics, which will appear in the drop-down list below."),
      radioButtons("colDotsOrColDend", 
                   label = h5("Color dend or dots:"),
                   choices = list("dots" = 1, 
                                  "no dots" = 2),
                   selected = 1),
      p("Click the blue boxes under a factor (below) to change the color of the factor."),
      fileInput('sampleMap',
                label = "Sample Mapping",
                accept = c('.xlsx', '.xls')),
      uiOutput("sampleMapColumns1"),
      uiOutput("sampleMapColumns2"),
      fluidRow(
        uiOutput("sampleFactorMapColors"))
      )
})




#User input changes the height/length of the main dendrogram
#----
plotHeight <- reactive({
  return(as.numeric(input$hclustHeight))
  100
})


#----
sampleFactorMapColumns <- reactive({
  sampleMappings <- variable.names(read_excel(input$sampleMap$datapath,
                                              sheet = 1,
                                              range = cell_rows(1)))
})


#----
output$sampleMapColumns1 <- renderUI({
  selectInput("sampleFactorMapChosenIDColumn",
              label = h5("Select a Group Mapping"),
              choices = as.list(sampleFactorMapColumns()))
})


#----
output$sampleMapColumns2 <- renderUI({
  selectInput("sampleFactorMapChosenAttribute",
              label = h5("Select a Group Mapping"),
              choices = as.list(sampleFactorMapColumns()[!grepl(input$sampleFactorMapChosenIDColumn,
                                                                sampleFactorMapColumns(),
                                                                ignore.case = TRUE)]))
})


#----
levs <- reactive({
  sampleMappings <- read_excel(input$sampleMap$datapath, 1)
  #selected column

  sampleMappings[input$sampleFactorMapChosenAttribute] %>% 
    unique %>%
    unlist %>% 
    as.vector %>% 
    c(.,"Missing_in_Excel")
})


#----
output$sampleFactorMapColors <- renderUI({
  column(3,
         lapply(1:length(levs()),
                function(x){
                  do.call(colourInput,
                          list(paste0("factor-",
                                      gsub(" ",
                                           "",
                                           levs()[[x]])),
                               levs()[[x]],
                               value="blue",
                               allowTransparent=T))
         })
  )
})



output$hclustPlot <- shiny::renderPlot({
  
  req(dendro())
  a <- shiny::callModule(IDBacApp::colordendLines,
                         "proteinDendLines",
                         dendrogram = dendro())
  a <- shiny::callModule(IDBacApp::colordendLabels,
                         "proteinDendLabels",
                         dendrogram = a)
  
  labs <- base::strtrim(labels(a),10)
  
  labels(a) <- labs
  
  
  par(mar = c(5, 5, 5, input$dendparmar))
  plot(a, horiz = TRUE)
  
  if(!is.null(input$selectMetaColumn)){
  runDendDots(rawDendrogram = dendro(),
              trimdLabsDend = a,
              pool = userDBCon(), 
              columnID = input$selectMetaColumn) 
  }
  
  
}, height = plotHeight)






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

    if (input$kORheight == "1"){
      
      dendro() %>% 
        color_branches(k = input$kClusters) %>% 
        plot(horiz = TRUE, lwd = 8)

    } else if (input$kORheight == "2"){

      dendro() %>% 
        color_branches(h = input$cutHeight) %>%
        plot(horiz = TRUE, lwd = 8)
      abline(v = input$cutHeight,
             lty = 2)

    } else if (input$kORheight == "3"){

      par(mar = c(5, 5, 5, input$dendparmar))
      if(input$colDotsOrColDend == "1"){

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
    ape::write.tree(as.phylo(dendro()), file=file)
  }
)


# Select samples for input into dendrogram
#----
output$chooseProteinSamples <- renderUI({

  IDBacApp::chooserInput("myProteinchooser",
                         "Available frobs", 
                         "Selected frobs",
                         availableProtein(),
                         c(),
                         size = 10, 
                         multiple = TRUE
  )
})

#----
observeEvent(input$colorLines, {
  output$protLineMod <- renderUI({
    IDBacApp::colordendLinesUI("proteinDendLines")
  })
})

observeEvent(input$closeLineModification, {
  output$protLineMod <- renderUI({
    # Intentionally Blank
  })
})

#----
observeEvent(input$colorLabels, {
  output$protLabelMod <- renderUI({
    IDBacApp::colordendLabelsUI("proteinDendLabels")
  })
})

observeEvent(input$closeLabelsModification, {
  output$protLabelMod <- renderUI({
    # Intentionally Blank
  })
})

#----
observeEvent(input$protDendDots, {
  output$proteDendDots <- renderUI({
      conn <- pool::poolCheckout(userDBCon())
      a <- dbListFields(conn, "metaData")
      a <- a[-which(a == "Strain_ID")]
      ns <- session$ns  
      selectInput("selectMetaColumn",
                  "Select Category",
                  as.vector(a)
      )
    })
})










# Check which samples can be used for protein analysis, return Sample Names
#----
availableProtein <- reactive({

  combinedProteinPeaksAll <- glue::glue_sql("
                                              SELECT DISTINCT `Strain_ID`
                                              FROM `IndividualSpectra`
                                              WHERE (`proteinPeaks` IS NOT NULL)",
                                            .con = userDBCon()
  )

  conn <- pool::poolCheckout(userDBCon())
  combinedProteinPeaksAll <- DBI::dbGetQuery(conn, combinedProteinPeaksAll)
  pool::poolReturn(conn)
  return(combinedProteinPeaksAll[ , 1])

})



# Select samples for input into new databe
#----
output$chooseNewDBSamples <- renderUI({
  
  IDBacApp::chooserInput("addSampleChooser",
                         "Available samples", 
                         "Selected samples",
                         availableNewSamples(),
                         c(),
                         size = 10, 
                         multiple = TRUE
  )
})









# Check which samples are available in the databse to be moved into other databse
#----
availableNewSamples <- reactive({
  
  samples <- glue::glue_sql("SELECT DISTINCT `Strain_ID`
                            FROM `IndividualSpectra`",
                            .con = userDBCon()
                            )
  
  conn <- pool::poolCheckout(userDBCon())
  samples <- DBI::dbGetQuery(conn, samples)
  pool::poolReturn(conn)
  return(samples[ , 1])
  
})





observeEvent(input$addtoNewDB, {
  copyingDbPopup()
  newdbPath <- file.path(workingDirectory, paste0(input$nameformixNmatch, ".sqlite"))
  copyToNewDatabase(existingDBPool = userDBCon(),
                    newdbPath = newdbPath, 
                    sampleIDs = input$addSampleChooser$right)

  
  removeModal()
  })


copyingDbPopup <- reactive({
  showModal(modalDialog(
    title = "Important message",
    "When file-conversions are complete this pop-up will be replaced by a summary of the conversion.",
    br(),
    "To check what has been converted, you can navigate to:",
    easyClose = FALSE, size="l",
    footer = ""))
})









#------------------------------------------------------------------------------
# Small molecule data processing
#------------------------------------------------------------------------------

# -----------------
selectedSmallMolPeakList <- reactive({
 
  
  combinedSmallMolPeaks <- NULL
  combinedSmallMolPeaksAll <- NULL
  matrixID <- NULL
  
  if(!is.null(dendro())){  # Check if there are protein spectra if TRUE, display protein dendrogram and use to subset strains

    if(is.null(input$plot_brush$ymin)){ # If there is a protein dendrogram and user hasn't brushed:
                                        # Don't ovrwhelm the browser by displaying everthing when page loads

      if(length(labels(dendro())) >= 25){
        # If more than 25 strains present, only display 10 to start, otherwise display all
        # Get random 10 strain IDs from dendrogram
        combinedSmallMolPeaks <- labels(dendro())[1:sample.int(10, 1)]
      } else {
        combinedSmallMolPeaks <- labels(dendro())
      }
    } else {
      combinedSmallMolPeaks <- IDBacApp::networkViaBrushedDendrogram(dendrogram = dendro(),
                                                                     brushYmin = input$plot_brush$ymin,
                                                                     brushYmax = input$plot_brush$ymax)
    }
  } else {

    # retrieve all Strain_IDs in db, check for matrix.
    combinedSmallMolPeaksAll <- glue::glue_sql("
                           SELECT DISTINCT `Strain_ID`
                           FROM `IndividualSpectra`
                           WHERE (`smallMoleculePeaks` IS NOT NULL)",
                           .con = userDBCon()
    )

    conn <- pool::poolCheckout(userDBCon())
    combinedSmallMolPeaksAll <- DBI::dbGetQuery(conn, combinedSmallMolPeaksAll)[ , 1]
    pool::poolReturn(con)
      }

  # input$matrixSamplePresent (User selection of whether to subtract matrix sample)  1 = Yes, 2 = No
  if(input$matrixSamplePresent == 1){

    if(!exists("combinedSmallMolPeaksAll")){
    # retrieve all Strain_IDs in db, check for matrix.
      combinedSmallMolPeaksAll <- glue::glue_sql("
                           SELECT DISTINCT `Strain_ID`
                           FROM `IndividualSpectra`
                           WHERE (`smallMoleculePeaks` IS NOT NULL)",
                           .con = userDBCon()
      )

    conn <- pool::poolCheckout(userDBCon())
    combinedSmallMolPeaksAll <- DBI::dbGetQuery(conn, combinedSmallMolPeaksAll)[ , 1] # return as vector of strain IDs
    pool::poolReturn(conn)
    }

# Get sample IDs that begin with "matrix" (need this to search sql db)
# Also give opportunity to later add ability for letting user interactively select which sample is the blank
    matrixID <- grep("^matrix",
                     combinedSmallMolPeaksAll,
                        ignore.case = TRUE,
                        value = TRUE)


    # Check if there is a matrix sample
    validate(
      need(length(matrixID) == 0, "Matrix blank not found.  Try selecting \"No\" under \"Do you have a matrix blank\" to left." )
    )

  } else {
# Don't add matrix blank to sample ID vector (leave as NULL)
  }


  # retrieve small mol peaks, spectrumSHA, and strain_id , given Strain_ID.
  sqlQ <- glue::glue_sql("
                     SELECT `spectrumSHA`, `Strain_ID`
                     FROM (SELECT *
                     FROM `IndividualSpectra`
                     WHERE (`Strain_ID` IN ({strainIds*})))
                     WHERE (`smallMoleculePeaks` != 'NA')",
                         strainIds = c(combinedSmallMolPeaks,combinedSmallMolPeaksAll, matrixID),
                         .con = userDBCon()
  )

  
  conn <- pool::poolCheckout(userDBCon())
  
  sqlQ <- DBI::dbGetQuery(conn, sqlQ)
  pool::poolReturn(conn)
  sqlQ <- split(sqlQ$spectrumSHA, sqlQ$Strain_ID)
  sqlQ <- lapply(sqlQ, function(x){
    IDBacApp::collapseSmallMolReplicates(fileshas = x,
                                         db = userDBCon(),
                                         smallMolPercentPresence = input$percentPresenceSM,
                                         lowerMassCutoff = input$lowerMassSM,
                                         upperMassCutoff = input$upperMassSM) %>% unname
  }) 
  
  for(i in 1:length(sqlQ)){
    snr1 <-  which(MALDIquant::snr(sqlQ[[i]]) >= input$smSNR)
    sqlQ[[i]]@mass <- sqlQ[[i]]@mass[snr1]
    sqlQ[[i]]@snr <- sqlQ[[i]]@snr[snr1]
    sqlQ[[i]]@intensity <- sqlQ[[i]]@intensity[snr1]
  }
  
  
 return(sqlQ)
  


})


#----
subtractedMatrixBlank <- reactive({
  IDBacApp::subtractMatrixBlank(sampleIds = labels(selectedSmallMolPeakList()), 
                              peakList = selectedSmallMolPeakList(),
                              binTolerance = 0.002)
})


#----
smallMolNetworkDataFrame <- reactive({

  IDBacApp::smallMolDFtoNetwork(peakList = subtractedMatrixBlank())
                                
  
})



ppp <- reactive({
  
  if(length(subtractedMatrixBlank()) > 9){
  aq6 <<- subtractedMatrixBlank()
  aq2 <<-calcNetwork()
  
  zz <<- intensityMatrix(subtractedMatrixBlank())
  zz[is.na(zz)] <- 0
  zz[is.infinite(zz)] <-0
  
  
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
  
  pc<- cbind(pc, as.vector(IDBacApp::colorBlindPalette()()[calcNetwork()$wc$membership[azz], 2] ))
  colnames(pc) <- c("Dim1", "Dim2", "Dim3", "nam", "color") 
  lp3<<-pc
  pc
  }else{FALSE}
})




output$smallMolPca <- renderPlotly({
  
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
  
  
  for (i in 1:length(subtractedMatrixBlank())){
    temp <- c(temp,subtractedMatrixBlank()[[i]]@metaData$Strain)
  }
  aqww<<-smallMolNetworkDataFrame()
  
  a <- as.undirected(graph_from_data_frame(smallMolNetworkDataFrame()))
  a<-igraph::simplify(a)
  wc <<- fastgreedy.community(a)
  
  b <- igraph_to_networkD3(a, group = (wc$membership)) # zero indexed
 
  z <- b$links
  zz <- b$nodes
  
  biggerSampleNodes<-rep(1,times=length(zz[,1]))
  zz<-cbind(zz,biggerSampleNodes)
  zz$biggerSampleNodes[which(zz[,1] %in% temp)]<-50
  
  net$z <- z
  net$zz <- zz
  net$wc <- wc
  net$temp <- temp
  net
  
})


output$metaboliteAssociationNetwork <- renderSimpleNetwork({
    awq2<<-calcNetwork()
    
    
    cbp <- as.vector(IDBacApp::colorBlindPalette()()[1:100,2])
    
    
    YourColors <- paste0('d3.scaleOrdinal()
                         .domain([',paste0(shQuote(1:100), collapse = ", "),'])
                         .range([', paste0(shQuote(cbp), collapse = ", "),' ])')
    
    
    
  
  #awq2$zz$group <- rep(c(1,1,1,1,2,2,2,3,3,3),88)[1:length(awq2$zz$group)]
  
  forceNetwork(Links = awq2$z, 
               Nodes = awq2$zz, 
               Source = "source",
               Nodesize = "biggerSampleNodes",
               Target = "target",
               NodeID = "name",
               Group = "group",
               opacity = 1,
               opacityNoHover=.8, 
               zoom = TRUE,
               colourScale = JS(YourColors))

})




# -----------------
# User input changes the height of the heirarchical clustering plot within the network analysis pane
plotHeightHeirNetwork <- reactive({
  return(as.numeric(input$hclustHeightNetwork))
})

# -----------------
# Plot MAN Dendrogram

output$netheir <- renderPlot({
  par(mar = c(5, 5, 5, input$dendparmar))
  
  if (input$kORheight == "1"){
    
    coloredDend() %>%
      hang.dendrogram %>% 
      plot(horiz = TRUE, lwd = 8)
    
  } else if (input$kORheight == "2"){
    
    coloredDend()  %>%  
      hang.dendrogram %>% 
      plot(horiz = TRUE, lwd = 8)
    
    abline(v = input$cutHeight, lty = 2)
    
  } else if (input$kORheight == "3"){
    
    if(is.null(input$sampleMap$datapath)){
      # No sample mapping selected
      dendro()$dend %>%
        plot(horiz = TRUE, lwd = 8)
    } else {
      if(input$colDotsOrColDend == "1"){
        
        coloredDend() %>%  
          hang.dendrogram %>% 
          plot(.,horiz=T)
        
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
  }

}, height = plotHeightHeirNetwork)


# Create MAN UI
#----
output$MANui <-  renderUI({

  fluidPage(sidebarLayout(
    
  
             sidebarPanel(style = 'padding:30px',
                          radioButtons("matrixSamplePresent",
                                       label = h5(strong("Do you have a matrix blank?")),
                                       choices = list("Yes" = 1, 
                                                      "No (Also Turns Off Matrix Subtraction)" = 2),
                                       selected = 1),
                          numericInput("percentPresenceSM",
                                       label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%) (Experiment/Hypothesis dependent)"),
                                       value = 70,
                                       step = 10,
                                       min = 0,
                                       max = 100),
                          numericInput("smSNR",
                                       label = h5(strong("Signal To Noise Cutoff")),
                                       value = 4,
                                       step = 0.5,
                                       min = 1.5,
                                       max = 100),
                          numericInput("upperMassSM",
                                       label = h5(strong("Upper Mass Cutoff")),
                                       value = 2000,
                                       step = 20,
                                       max = 3000),
                          numericInput("lowerMassSM",
                                       label = h5(strong("Lower Mass Cutoff")),
                                       value = 200,
                                       step = 20,
                                       min = 3000),
                          numericInput("hclustHeightNetwork",
                                       label = h5(strong("Expand Tree")),
                                       value = 750,
                                       step = 50,
                                       min = 100),
                          numericInput("dendparmar2",
                                       label = h5(strong("Adjust right margin of dendrogram")),
                                       value = 5),
                          downloadButton("downloadSmallMolNetworkData",
                                         label = "Download Current Network Data",
                                         value = FALSE),
                          br(),
                          p(strong("Hint 1:"),
                            "Use mouse to select parts of the tree and display the MAN of corresponding samples."),
                          p(strong("Hint 2:"),
                            "Use mouse to click & drag parts (nodes) of the MAN if it appears congested."),
                          br(),

                          p(strong("Note 1:"), "For publication-quality networks click \"Download Current Network.\"
                            while selected- this saves a .csv file of the currently-displayed
                            network to the \"Saved_MANs\" folder in your working directory This can be easily imported into Gephi or Cytoscape.
                            While checked, any update of the network will overwrite this file. Also, an error saying: \"cannot open the connection\"
                            means this box is checked and the file is open in another program, either uncheck or close the file."),
                          br(),
                          h4("Suggestions for Reporting MAN Analysis:"),
                          uiOutput("manReport"),
                          br(),
                          h4("Suggestions for Reporting Protein Analysis"),
                          uiOutput("proteinReport2")
                          ),
mainPanel(
    column(width = 5,
               #   style ="padding: 14px 0px; margin:0%",
                  plotOutput("netheir",
                             width = "100%",
                             height = "100%",
                             click = "plot_click",
                             dblclick = "plot_dblclick",
                             hover = "plot_hover",
                             brush = "plot_brush")
           ),
    column(width=6, style ="padding: 14px 0px; margin:0%",
           absolutePanel(fixed = TRUE, width = "50%",
                         tabsetPanel(type="tabs",           
                                tabPanel(value = "smallMolMANUI","MAN",
                                simpleNetworkOutput("metaboliteAssociationNetwork", width="100%")),
                                tabPanel(value = "smallMolPCAUi","PCA",
                                         plotlyOutput("smallMolPca",
                                                      width = "100%"
                                         )
                           ))
          
                          
    
    ))
)
  ))
})


# Output a paragraph about which paramters were used to create the currently-displayed MAN
#----
output$manReport <- renderUI({
  p("This MAN was created by analyzing ", tags$code(length(subtractedMatrixBlank())), " samples,", if(input$matrixSamplePresent==1){("subtracting a matrix blank,")} else {},
    "and retaining peaks with a signal to noise ratio above ", tags$code(input$smSNR), " and occurring in greater than ", tags$code(input$percentPresenceSM), "% of replicate spectra.
    Peaks occuring below ", tags$code(input$lowerMassSM), " m/z or above ", tags$code(input$upperMassSM), " m/z were removed from the analysis. ")
})


# Output a paragraph about which parameters were used to create the currently-displayed dendrogram
#----
output$proteinReport2 <- renderUI({

  if(length(labels(dendro())) == 0){
    p("No Protein Data to Display")
  } else {
    p("This dendrogram was created by analyzing ", tags$code(length(labels(dendro()))), " samples,
      and retaining peaks with a signal to noise ratio above ", tags$code(input$pSNR)," and occurring in greater than ", tags$code(input$percentPresenceP),"% of replicate spectra.
      Peaks occuring below ", tags$code(input$lowerMass), " m/z or above ", tags$code(input$upperMass), " m/z were removed from the analyses. ",
      "For clustering spectra, ", tags$code(input$distance), " distance and ", tags$code(input$clustering), " algorithms were used.")
  }
})



#------------------------------------------------------------------------------
# Updating IDBac
#------------------------------------------------------------------------------


# Updating IDBac Functions
#----
observeEvent(input$updateIDBac,{
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

    if (internetPing == TRUE){
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

      if (class(latestStableVersion) == "try-error"){

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

        if(as.character(local_version) == "Installed version is latest version"){

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

        } else if(compareVersion(as.character(local_version), 
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




#------------------------------------------------------------------------------
# In-house library generation code:
#------------------------------------------------------------------------------


  # The UI for the library editing/creation tab
  output$libraryTab <-  renderUI({

    fluidPage(
      tabsetPanel(id= "libraryTabs", type="tabs",
                  tabPanel("Create a New Library", value="newLibPanel",
                           textInput("newDatabaseName", "Input Library Name:", value="Default Library"),
                           actionButton("saveBtn", "Save"),
                           rHandsontableOutput("hot")
                  ),
                  tabPanel("Add Isolates to an Existing Library", value="addToExistingLibPanel",
                           uiOutput("appendLibPanelRadios"),
                           actionButton("saveAppendDatabase1", "Append"),
                           rHandsontableOutput("hott"),
                           rHandsontableOutput("hot3")),

                  tabPanel("Modify an Existing Library",
                           value="modifyLibPanel",
                           uiOutput("modifyLibPanelRadios"),
                           actionButton("saveModifyDatabase1", "Update"),
                           rHandsontableOutput("hot2"))
      )
    )
  })







  #-----------  Creating a new library

  createNewLibraryTable <- reactive({

    # "Get the sample names from the protein peak files
    currentlyLoadedSamples <- list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists"),full.names = FALSE)[grep(".ProteinPeaks.", list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists")))]
    # Character vector of protein peak sample names
    currentlyLoadedSamples <- as.character(strsplit(currentlyLoadedSamples,"_ProteinPeaks.rds"))
    # Check for mzML files
    mzMLfiles <- list.files(paste0(idbacDirectory$filePath, "\\Converted_To_mzML"), full.names = FALSE)
    mzMLfiles <- unlist(strsplit(mzMLfiles, ".mzML"))
    nonMissingmzML <- which(currentlyLoadedSamples %in% mzMLfiles)
    missingmzML <- which(! currentlyLoadedSamples %in% mzMLfiles)
    currentlyLoadedSamples <- currentlyLoadedSamples[nonMissingmzML]
    # Create the data frame structure for the "database"
    currentlyLoadedSamples <- data.frame("Strain_ID" = currentlyLoadedSamples,
                                         "Genbank_Accession" = "",
                                         "NCBI_TaxID" = "",
                                         "Kingdom" = "",
                                         "Phylum"= "",
                                         "Class" = "",
                                         "Order" = "",
                                         "Family" = "",
                                         "Genus" = "",
                                         "Species" = "",
                                         "Strain" = "")
    # If interactive table exists, show it, otherwise use "currentlyLoadedSamples" created above
    if (!is.null(input$hot)) {
      rhandsontable::hot_to_r(input$hot)
    } else {
      currentlyLoadedSamples
    }

  })

  # Display the new Library as an editable table
  output$hot <- rhandsontable::renderRHandsontable({
    DF <- createNewLibraryTable()

    DF %>% select(c("Strain_ID",
                    "Genbank_Accession",
                    "Kingdom",
                    "Phylum",
                    "Class",
                    "Order",
                    "Family",
                    "Genus",
                    "Species",
                    "Strain")) %>%
      return(.) -> DF




    rhandsontable::rhandsontable(DF,
                                 useTypes = FALSE,
                                 selectCallback = TRUE,
                                 contextMenu = FALSE) %>%
      hot_col("Strain_ID",
              readOnly = TRUE)
  })

  observeEvent(input$saveBtn, {
    appDirectory <- workingDirectory # Get the location of where IDBac is installed
    if (!dir.exists(file.path(appDirectory, "SpectraLibrary"))){  # If spectra library folder doesn't exist, create it
      dir.create(file.path(appDirectory, "SpectraLibrary"))
    }
    if(!file.exists(paste0("SpectraLibrary/", isolate(input$newDatabaseName), ".sqlite"))){ # If SQL file does not exist
      isolate(
        newDatabase <- DBI::dbConnect(RSQLite::SQLite(), paste0("SpectraLibrary/", input$newDatabaseName, ".sqlite"))
      )
      isolate(
        IDBacApp::addNewLibrary(samplesToAdd = createNewLibraryTable(), newDatabase = newDatabase,  selectedIDBacDataFolder = idbacDirectory$filePath)
      )
      DBI::dbDisconnect(newDatabase)
    } else {
      print("2")
      showModal(popupDBCreation())
    }
  })



  # -----------------
  # Popup summarizing the final status of the conversion
  popupDBCreation <- function(failed = FALSE){
    modalDialog(
      title = "Are you sure?",
      p("There is already a database with this name."),
      p(paste0("Pressing save below will append to the existing database: \"", isolate(input$newDatabaseName),"\"")),
      footer = tagList(actionButton("saveNewDatabase", paste0("Append to: \"", isolate(input$newDatabaseName),"\"")), modalButton("Close"))
    )}

  observeEvent(input$saveNewDatabase, {
    removeModal()
    # After initiating the database
    newDatabase <- DBI::dbConnect(RSQLite::SQLite(), paste0("SpectraLibrary/", isolate(input$newDatabaseName),".sqlite"))
    DBI::dbRemoveTable(newDatabase, "IDBacDatabase")
    IDBacApp::addNewLibrary(samplesToAdd = createNewLibraryTable(), newDatabase = newDatabase,  selectedIDBacDataFolder = idbacDirectory$filePath)
    DBI::dbDisconnect(newDatabase)
  })

  #------------------------------------
  #------------------------------------ Modify an Existing Library
  libraries <- function(){list.files(file.path(workingDirectory, "SpectraLibrary"), pattern=".sqlite", full.names = TRUE)}

  output$modifyLibPanelRadios  <- renderUI({
    if(input$libraryTabs == "modifyLibPanel"){
      radioButtons(inputId = "modifyLibPanelRadiosSelected",
                   label= "Existing Libraries",
                   choiceNames = basename(libraries()),
                   choiceValues = as.list(libraries())
      )
    }

  })


  modifiedLibraryEnvironmentTracking <- new.env()  # This allows modifyLibraryTable() below to update correctly

  modifyLibraryTable <- reactive({
    # Open connection to chosen existing database
    modifyDatabaseConnect <- DBI::dbConnect(RSQLite::SQLite(), paste0(input$modifyLibPanelRadiosSelected))
    # Create lazy-eval tbl
    db <- dplyr::tbl(modifyDatabaseConnect, "IDBacDatabase")
    # Select only columns to be displayed in IDBac
    db <- db %>%
      dplyr::select(-c("manufacturer",
                       "model",
                       "ionisation",
                       "analyzer",
                       "detector",
                       "Protein_Replicates",
                       "Small_Molecule_Replicates",
                       "mzML",
                       "proteinPeaksRDS")) %>%
      dplyr::collect()

    if ((!is.null(input$hot2)) && modifiedLibraryEnvironmentTracking$value == input$modifyLibPanelRadiosSelected) {
      rhandsontable::hot_to_r(input$hot2)
    } else {
      modifiedLibraryEnvironmentTracking$value <-input$modifyLibPanelRadiosSelected
      db
    }

  })

  # Display the new Library as an editable table
  output$hot2 <- rhandsontable::renderRHandsontable({
    DF <- modifyLibraryTable()
    DF %>% select(c("Strain_ID",
                    "Genbank_Accession",
                    "Kingdom",
                    "Phylum",
                    "Class",
                    "Order",
                    "Family",
                    "Genus",
                    "Species",
                    "Strain")) %>%
      return(.) -> DF
    rhandsontable::rhandsontable(DF, useTypes = FALSE, selectCallback = TRUE, contextMenu = FALSE) %>%
      hot_col("Strain_ID", readOnly = TRUE)
  })

  #------------------------------------ Modify existing databse


  observeEvent(input$saveModifyDatabase1, {

    showModal(popupDBmodify())

  })


  # Popup summarizing the final status of the conversion
  popupDBmodify <- function(failed = FALSE){
    modalDialog(
      title = "Are you sure?",
      p("There is already a database with this name."),
      p(paste0("Pressing save below will append to the existing database: \"", isolate(input$modifyLibPanelRadiosSelected),"\"")),
      footer = tagList(actionButton("saveModifyDatabase2", paste0("Append to: \"", isolate(basename(input$modifyLibPanelRadiosSelected)),"\"")), modalButton("Close"))
    )}

  observeEvent(input$saveModifyDatabase2, {
    # After initiating the database

    newDatabase <- DBI::dbConnect(RSQLite::SQLite(), paste0(input$modifyLibPanelRadiosSelected))
    db          <- dplyr::tbl(newDatabase, "IDBacDatabase")

    dbIds <- db %>% select(Strain_ID) %>% collect %>% unlist() %>% as.vector()
    colsToUpdate <-  db %>%
      dplyr::select(-c("Strain_ID",
                       "manufacturer",
                       "model",
                       "ionisation",
                       "analyzer",
                       "detector",
                       "Protein_Replicates",
                       "Small_Molecule_Replicates",
                       "mzML",
                       "proteinPeaksRDS")) %>%
      colnames()



    for (i in dbIds){
      # Allows editing dynamic columns (user-added metadata column) ie- prevents overwriting instrument data or
      # MS data but can edit all other columns
      # modded is a tbl with the user-updated values from rhandsontable
      updateValues <-   modifyLibraryTable() %>% filter(Strain_ID == i) %>% select(colsToUpdate)
      a <- as.vector(unlist(updateValues))
      b <- colnames(updateValues)
      # Format for SQL multiple "SET" query
      a1 <- sapply(a, function(x) paste0("'",x,"'"))
      b1 <- sapply(b, function(x) paste0("'",x,"'"))
      all <- paste(b1, a1, sep = "=", collapse = ",")
      # Run SQL update
      DBI::dbSendQuery(newDatabase, paste("UPDATE IDBacDatabase SET ", all, " WHERE Strain_ID=",shQuote(i))) # works

    }
    removeModal()

  })


  










  #--------------------------------------



  #  The following code is necessary to stop the R backend when the user closes the browser window
  #   session$onSessionEnded(function() {
  #      stopApp()
  #      q("no")
  #    })



  }
