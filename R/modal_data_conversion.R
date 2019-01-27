convertDataTabUI <- function(id) {
  ns <- shiny::NS(id)
  

navlistPanel(widths = c(3, 8), id = ns("ConversionsNav"),
             "Create an IDBac experiment",
             tabPanel(tags$ul(tags$li("Click here to convert Bruker files")),
                      value = "convert_bruker_nav",
                      mainPanel(offset = 3,
                                radioButtons(ns("rawORreanalyze"),
                                             label = h3("Begin by selecting an option below:"),
                                             choices = list("Select here to convert and analyze raw-data from a single MALDI-plate" = 1),
                                             #"Select here to convert and analyze raw-data from multiple MALDI-plates at once" = 2),
                                             selected = 0,
                                             inline = FALSE,
                                             width = "100%"),
                                uiOutput(ns("conversionMainUI1"))
                      )
             ),
             tabPanel(tags$ul(tags$li("Click here to convert mzML/mzXML files")),
                      value = ns("convert_mzml_nav"),
                      fluidRow(
                        uiOutput(ns("conversionMainUI2"))
                      )
             ),
             tabPanel(tags$ul(tags$li("Click here to convert txt files")),
                      value = ns("convert_txt_nav"),
                      mainPanel(
                        uiOutput(ns("conversionMainUI3"))
                      )
             )
)
}




convertDataTabServer <- databaseTabServer <- function(input,
                                                      output,
                                                      session){
  
  
  
  # Spectra conversion
  #This observe event waits for the user to select the "run" action button and then creates the folders for storing data and converts the raw data to mzML
  #----
  spectraConversion <- reactive({
    
    IDBacApp::excelMaptoPlateLocation(rawORreanalyze = input$rawORreanalyze,
                                      excelFileLocation = input$excelFile$datapath,
                                      rawFilesLocation = rawFilesLocation(),
                                      multipleMaldiRawFileLocation = multipleMaldiRawFileLocation())
    
  })
  
  
  
  
  
  # Run raw data processing on delimited-type input files
  #----
  observeEvent(input$runDelim, 
               ignoreInit = TRUE, {
                 
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
      easyClose = FALSE, size = "l",
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
  observeEvent(input$run, 
               ignoreInit = TRUE,  {
                
                 popup3()
                 
                 if (input$ConversionsNav == "convert_bruker_nav") {
                   if (input$rawORreanalyze == 1) {
                     validate(need(any(!is.na(sampleMapReactive$rt)), 
                                   "No samples entered into sample map, please try entering them again"))
                     aa <- sapply(1:24, function(x) paste0(LETTERS[1:16], x))
                     aa <- matrix(aa, nrow = 16, ncol = 24)
                     
                     
                     spots <-  brukerDataSpotsandPaths(brukerDataPath = rawFilesLocation())
                     s1 <- base::as.matrix(sampleMapReactive$rt)
                     sampleMap <- sapply(spots, function(x) s1[which(aa %in% x)])
                     
                     
                     
                     
                     forProcessing <- startingFromBrukerFlex(chosenDir = rawFilesLocation(), 
                                                             msconvertPath = "",
                                                             sampleMap = sampleMap,
                                                             tempDir = tempMZDir)
                     
                     
                     
                   }
                 }
                 
                 validate(need(length(forProcessing$mzFile) == length(forProcessing$sampleID), 
                               "Temp mzML files and sample ID lengths don't match."
                 ))
                 
                 lengthProgress <- length(forProcessing$mzFile)
                 
                 userDB <- pool::poolCheckout(newExperimentSqlite())
                 
                 withProgress(message = 'Processing in progress',
                              detail = 'This may take a while...',
                              value = 0, {
                                
                                for (i in base::seq_along(forProcessing$mzFile)) {
                                  incProgress(1/lengthProgress)
                                  IDBacApp::spectraProcessingFunction(rawDataFilePath = forProcessing$mzFile[[i]],
                                                                      sampleID = forProcessing$sampleID[[i]],
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}











#' oneMaldiPlate
#'
#' @param id NA
#'
#' @return NA
#' @export
#'
#' @examples NA
oneMaldiPlate <- function(id){
  ns <- shiny::NS(id)
  tagList( 
    column(width = 12,
           style = "background-color:#7777770d",
           fluidRow(
             h3("Starting with a Single MALDI Plate of Raw Data", align = "center"),
             
             br(),
             column(12, align = "center",
                    p(strong("1: Enter a Name for this New Experiment")),
                    p("Only numbers, \"_\", and A-Z. Shouldn't start with a number."),
                    textInput(ns("newExperimentName"),
                              label = ""),
                    tags$hr(size = 20)),
             br(),
             p(strong("2: Click to select the location of your RAW data"), align = "center"),
             column(12, align = "center",
                    actionButton(ns("rawFileDirectory"),
                                 label = "Raw Data Folder"),
                    verbatimTextOutput(ns("rawFileDirectory"),
                                       placeholder = TRUE),
                    tags$hr(size = 20)),
             br(),
             column(12, align = "center",
                    p(strong("3:", "Fill in the Sample-ID spreadsheet.")),
                    
                    actionButton(ns("showSampleMap", "Click to name samples")),
                    br(),
                    p(strong("Missing sample IDs for the following spots:")),
                    shiny::verbatimTextOutput(ns("missingSampleNames"), placeholder = TRUE)
             ),
             column(12, align = "center",
                    p(strong("4:","Click \"Process Data\" to begin spectra conversion.")),
                    actionButton(ns("run"),
                                 label = "Process Data"),
                    tags$hr(size = 20)),
             br(),
             br(),
             br(),
             br(),
             p(strong("Note:","If you canceled out of the popup after spectra conversion completed, you can process your converted spectra using the button below: (but only after all files have been converted) This step is not necessary otherwise."))
           )
    ))
  
}


#' oneMaldiPlateHelpUI
#'
#' @param id NA
#'
#' @return NA
#' @export
#'
#' @examples NA
oneMaldiPlateHelpUI <- function(id){
  ns <- shiny::NS(id)
  tagList(
    h3("Instructions", align = "center"),
    
    br(),
    p(strong("1: Working Directory"), " This directs where on your computer you would like to create an IDBac working directory."),
    p("In the folder you select, IDBac will create sub-folders within a main directory named \"IDBac\":"),
    img(src = "WorkingDirectory.png",
        style = "width:60%;height:60%"),
    br(),
    p(strong("2: Raw Data"), "Your RAW data is a single folder that contains: one subfolder containing protein
      data and one subfolder containing small-molecule data"),
    img(src = "Single-MALDI-Plate.png",
        style = "width:60%;height:60%"),
    br(),
    p("*Note: Sometimes the browser window won't pop up, but will still appear in the application bar. See below:"),
    img(src = "window.png",
        width = "100%")
    )
  
}



#' multipleMaldiPlates
#'
#' @param id NA
#'
#' @return NA
#' @export
#'
#' @examples NA
multipleMaldiPlates <- function(id){
  ns <- shiny::NS(id)
  fluidRow(
    column(12,
           br(),
           br(),
           fluidRow(
             column(12,offset = 3,
                    h3("Starting with Multiple MALDI Plates of Raw Data"))), br(), br(),
           column(5,
                  fluidRow(column(5,
                                  offset = 3,
                                  h3("Instructions"))),
                  br(),
                  p(strong("1:")," This directs where on your computer you would like to create an IDBac working directory."),
                  p("In the folder you select- IDBac will create folders within a main directory named \"IDBac\":"),
                  img(src = "WorkingDirectory.png",
                      style = "width:322px;height:164px"),
                  p("If there is already an \"IDBac\" folder present in the working directory,
                    files will be added into the already-present IDBac folder ", strong("and any samples with the same name will be overwritten.")),
                  br(),
                  p(strong("2:"),"The RAW data file will be one folder that contains individual folders for each
                    MALDI plate. Each MALDI plate folder will contain an Excel map and two folders: one
                    containing protein data and the other containing small molecule data:"),
                  img(src = "Multi-MALDI-Plate.png", 
                      style = "width:410px;height:319px"),
                  p("Note: Sometimes the browser window won't pop up, but will still appear in the application bar. See below:"),
                  img(src = "window.png",
                      width = "100%")
                  ),
           column(1),
           column(5,
                  style = "background-color:#7777770d",
                  fluidRow(
                    h3("Workflow Pane", align = "center")),
                  br(),
                  column(12, align = "center",
                         p(strong("1: Enter a Name for this New Experiment")),
                         p("Only numbers, \"_\", and A-Z. Shouldn't start with a number."),
                         textInput(ns("newExperimentName"),
                                   label = ""),
                         tags$hr(size = 20)),
                  fluidRow(
                    column(12,
                           verbatimTextOutput(ns("newExperimentNameText"),
                                              placeholder = TRUE))),
                  br(),
                  p(strong("2:"), "Your RAW data will be one folder that contains folders for each MALDI plate."),
                  br(),
                  p(strong("2: Click to select the location of your RAW data"), align = "center"),
                  actionButton(ns("multipleMaldiRawFileDirectory"),
                               label = "Click to select the location of your RAW data"),
                  fluidRow(column(12,
                                  verbatimTextOutput(ns("multipleMaldiRawFileDirectory"),
                                                     placeholder = TRUE))),
                  br(),
                  column(12, align = "center",
                         p(strong("4:","Click \"Process Data\" to begin spectra conversion.")),
                         actionButton(("run"),
                                      label = "Process Data"),
                         tags$hr(size = 20))
           )
           )
  )
}


#' beginWithMZ
#'
#' @param id NA
#'
#' @return NA
#' @export
#'
#' @examples NA
beginWithMZ <- function(id){
  fluidRow(
    column(width = 10, offset = 2, wellPanel(class = "intro_WellPanel", align= "center",
                                             h3("Starting with mzML or mzXML Data:"),
                                             
                                             
                                             p(strong("1: Enter a filename for this new experiment")),
                                             p("Only numbers, \"_\", and A-Z. Shouldn't start with a number."),
                                             textInput(ns("newExperimentName"),
                                                       label = ""),
                                             tags$hr(size = 20),
                                             
                                             br(),
                                             p(strong("2: Click to select the location of your mzML files"), align= "center"),
                                             actionButton(ns("mzmlRawFileDirectory"),
                                                          label = "Raw Data Folder"),
                                             verbatimTextOutput(ns("mzmlRawFileDirectory"),
                                                                placeholder = TRUE),
                                             tags$hr(size = 20),
                                             br(),
                                             p("Samples will be named according to the file name of the provided files"),
                                             br(),
                                             p(strong("4:","Click \"Process Data\" to begin spectra conversion.")),
                                             actionButton(ns("run"),
                                                          label = "Process Data"),
                                             tags$hr(size = 20)
                                             
    )
    ))
  
}

#' beginWithTXT
#'
#' @param id NA
#'
#' @return NA
#' @export
#'
#' @examples NA
beginWithTXT <- function(id){
  ns <- NS(id)
  fluidRow(
    p(".txt and .csv support coming soon!"),
    actionButton(ns("delimitedDirectoryP"),
                 label = "Raw Data P Folder"),
    actionButton(ns("delimitedDirectorySM"),
                 label = "Raw Data SM Folder"),
    actionButton(ns("runDelim"),
                 label = "Process Data"),
    verbatimTextOutput(ns("delimitedLocationPo"),
                       placeholder = TRUE),
    verbatimTextOutput(ns("delimitedLocationSMo"),
                       placeholder = TRUE)
  )
}




