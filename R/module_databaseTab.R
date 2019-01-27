



databaseTabUI <- function(id) {
  ns <- shiny::NS(id)
  
  fluidPage(
    column(width = 4,
           fluidRow(
             column(width = 12,
                    align = "center",
                    uiOutput(ns("availableDB")),
                    actionButton(ns("moveToAnalysis"),
                                 "Click here to begin analysis"),
                    
                    p("Location of experiment file:", align = "center"),
                    verbatimTextOutput(ns("selectedSQLText"),
                                       placeholder = TRUE)
             )
           )
    ),
    column(width = 8,
           fluidRow(
             shinyBS::bsCollapse(id = ns("collapseSQLInstructions"), open = "Panel 1",
                                 shinyBS::bsCollapsePanel(h4("Open\\Close Instructions", align = "center"),
                                                          tags$b("What is an \"experiment\" in IDBac?"),
                                                          tags$ul(
                                                            tags$li("A user-defined group of samples that were analyzed by MALDI MS."),
                                                            tags$li("Physically, each experiment is a separate \"SQLite\" database that can be shared between colleagues
                                                                  or submitted alongside a manuscript."),
                                                            tags$li("Experiments contain the converted mzML file, instrument and data collection info (if found), 
                                                                  processed spectra, as well as any sample information input into IDBac by you.")),
                                                          tags$b("What does this mean for me?"),
                                                          
                                                          tags$ul(
                                                            tags$li("Experiments are how you organize your data."),
                                                            tags$li("Experiments should only contain the samples you want to analyze."),
                                                            tags$li("It is possible to \"mix and match\" samples from different experiments to create new experiments.")
                                                          ),
                                                          tags$b("Begin analysis by selecting an available experiment to the right."), 
                                                          br(),
                                                          br(),
                                                          tags$b("You also have the option, below, to:"),
                                                          tags$ul(
                                                            tags$li("Create new experiments using samples from the selected experiment"),
                                                            tags$li("Add information about strains (for coloring plots later, or just as a record)")
                                                            
                                                          )
                                 )
                                 
             )
           ),
           fluidRow(
             
             shinyBS::bsCollapse(id = ns("modifySqlCollapse"),
                                 shinyBS::bsCollapsePanel(h4("Click here to modify the selected experiment", align = "center"),  
                                                          
                                                          tabsetPanel(id = ns("ExperimentNav"), 
                                                                      tabPanel("Create an experiment, pulling samples from the selected experiment",
                                                                               value = "experiment_mixMatch_tab",
                                                                               column(12, align = "center",
                                                                                      style = "background-color: #7777770d",
                                                                                      offset = 1,
                                                                                      h3("Transfer samples from previous experiments to new/other experiments.", align = "center"),
                                                                                      p("Note: For data integrity, samples cannot be removed from experiments.", align = "center"),
                                                                                      p("Move strains between boxes by clicking the strain's name
                                                                               and then an arrow. Strains in the right box will be used for analysis."),
                                                                                      uiOutput(ns("chooseNewDBSamples")),
                                                                                      verbatimTextOutput(ns("selection")),
                                                                                      br(),
                                                                                      textInput(ns("nameformixNmatch"),
                                                                                                label = "Enter name for new experiment"),
                                                                                      
                                                                                      actionButton(ns("addtoNewDB"),
                                                                                                   label = "Add to new Experiment")
                                                                               )
                                                                      ),
                                                                      tabPanel("Add/modify information about samples",
                                                                               value = "experiment_metaData_tab",
                                                                               column(width = 6, 
                                                                                      p("Here is where you can add information about your sample. There are always standard
                                                                               columns like \"Genus\", but you can add your own columns as well."),
                                                                                      p("After you are finished inputting your information, press \"save\" to write the information 
                                                                               to the database."),
                                                                                      
                                                                                      actionButton(ns("saven"),
                                                                                                   label = "save")
                                                                               ), 
                                                                               column(width = 6,
                                                                                      textInput(ns("addMetaColumnName"), 
                                                                                                label = "New Column Name"),
                                                                                      actionButton(ns("insertNewMetaColumn"),
                                                                                                   label = "Insert Column")),
                                                                               rHandsontableOutput(ns("metaTable"), height = 800)
                                                                      )
                                                          ) 
                                 )
             )
           )
           
    )
  )
  
  
}





databaseTabServer <- function(input,
                              output,
                              session,
                              workingDirectory,
                              availableExperiments,
                              pool){
  
  availableExperimentsR <- reactive(availableExperiments)
  
  output$availableDB <- renderUI({
    ns <- session$ns
    selectInput(ns("selectExperiment"),
                label = h3("First, select an experiment:"),
                choices = availableExperimentsR(),
                selected = NULL,
                width = "50%"
    )
  })
  
  
  # Collapsable instruction panels
  observeEvent(input$styleSelect, 
               ignoreInit = TRUE, {
                 ns <- session$ns
                 updateCollapse(session, ns("collapseSQLInstructions"))
                 updateCollapse(session, ns("modifySqlCollapse"))
                 isolate(
                   updateCollapse(session, ns("collapseSQLSelector"))
                 )
               }
  )
  
  
  
  
  userDBCon <- eventReactive(input$selectExperiment, {
    IDBacApp::createPool(fileName = input$selectExperiment,
                         filePath = workingDirectory)[[1]]
  })
  
  
  
  
  
  
  
  
  
  
  
  output$selectedSQLText <- renderPrint({
    fileNames <- tools::file_path_sans_ext(list.files(workingDirectory,
                                                      pattern = ".sqlite",
                                                      full.names = FALSE))
    filePaths <- list.files(workingDirectory,
                            pattern = ".sqlite",
                            full.names = TRUE)
    filePaths[which(fileNames == input$selectExperiment)]
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Create analysis tabs and move the view to the mirror plots tab  
  
  observeEvent(input$moveToAnalysis,
               once = TRUE, 
               ignoreInit = TRUE, {
                 
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
                 
                 updateTabsetPanel(session, "mainIDBacNav",
                                   selected = "inversePeaks")
                 
                 updateNavlistPanel(session, "ExperimentNav",
                                    selected = "experiment_select_tab")
               }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #----
  qwerty <- reactiveValues(rtab = data.frame("Strain_ID" = "Placeholder"))
  
  
  #----
  observeEvent(input$searchNCBI, 
               ignoreInit = TRUE,  {
                 # IDBacApp::searchNCBI()
               })
  
  
  
  
  
  observeEvent(input$insertNewMetaColumn, 
               ignoreInit = TRUE, {
                 IDBacApp::insertMetadataColumns(pool = userDBCon(),
                                                 columnNames = input$addMetaColumnName)
                 
                 
               })
  
  observeEvent(input$saven, 
               ignoreInit = TRUE, {
                 
                 DBI::dbWriteTable(conn = userDBCon(),
                                   name = "metaData",
                                   value = rhandsontable::hot_to_r(input$metaTable)[-1, ], # remove example row 
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
  observeEvent(c(input$selectExperiment, input$insertNewMetaColumn), 
               ignoreInit = TRUE, {
                 
                 if (!is.null(userDBCon())) {
                   conn <- pool::poolCheckout(userDBCon())
                   
                   if (!"metaData" %in% DBI::dbListTables(conn)) {
                     
                     warning("It appears the experiment file may be corrupt, please create again.")
                     qwerty$rtab <- data.frame(Strain_ID = "It appears the experiment file may be corrupt, please create the experiment again.")
                     
                   } else{
                     
                     
                     dbQuery <- glue::glue_sql("SELECT *
                                             FROM ({tab*})",
                                               tab = "metaData",
                                               .con = conn)
                     
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
                     
                     qwerty$rtab <- merge(exampleMetaData,
                                          dbQuery,
                                          all = TRUE,
                                          sort = FALSE)
                     
                     pool::poolReturn(conn)
                   }
                   
                 }
               })
  
  
  
  
  
  
  
  
  #This "observe" event creates the UI element for analyzing a single MALDI plate, based on user-input.
  #----
  observe({
    if (is.null(input$startingWith)) { } else if (input$startingWith == 3) {
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
        setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
        
      }
      
      
      # Get the folders contained within the chosen folder.
      foldersInFolder <- tryCatch(findmz(),
                                  error = function(x) paste("Timed out"),
                                  finally = function(x) x)
      
      if (foldersInFolder == "Timed out") {
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
  
  
  
  
  return(userDBCon())
  
}













