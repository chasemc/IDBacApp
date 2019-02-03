convertDataTab_UI <- function(id) {
  ns <- shiny::NS(id)
  
  
  navlistPanel(widths = c(3, 8), id = ns("ConversionsNav"),
               "Create an IDBac experiment",
               tabPanel(tags$ul(tags$li("Click here to convert Bruker files")),
                        value = ns("convert_bruker_nav"),
                        fluidRow(
                          column(width = 12, 
                                 align = "center",
                                  radioButtons(ns("typeOfRawData"),
                                               label = h3("Begin by selecting an option below:"),
                                               choices = list("Select here to convert and analyze raw-data from a single MALDI-plate" = 1),
                                               #"Select here to convert and analyze raw-data from multiple MALDI-plates at once" = 2),
                                               selected = 0,
                                               inline = FALSE,
                                               width = "100%"))),
                        fluidRow(column(width = 9,
                                        offset = 1,
                                  uiOutput(ns("convertBruker"))
                        ))
                        
               ),
               tabPanel(tags$ul(tags$li("Click here to convert mzML/mzXML files")),
                        value = ns("convert_mzml_nav"),
                                 wellPanel(class = "intro_WellPanel",
                                           align = "center",
                          IDBacApp::convertMZ_UI(ns("beginWithMZ"))
               )
               ),
               tabPanel(tags$ul(tags$li("Click here to convert txt files")),
                        value = ns("convert_txt_nav"),
                        wellPanel(class = "intro_WellPanel",
                                  align = "center",
                          uiOutput(ns("conversionMainUI3"))
                        )
               )
  )
}




convertDataTab_Server <- function(input,
                                 output,
                                 session,
                                 tempMZDir,
                                 sqlDirectory){
  
  
  
  convertMZinputs <- shiny::callModule(convertMZ_Server,
                                       "beginWithMZ",
                                       sqlDirectory = sqlDirectory)
  

  convertOneBrukerInputs <- shiny::callModule(convertOneBruker_Server,
                                           "convertOneBruker")
  
    
  
  observeEvent(input$typeOfRawData, {             
    if (is.null(input$typeOfRawData)) {
      
    } else if (input$typeOfRawData == "1") {
      output$convertBruker <- renderUI({
        IDBacApp::convertOneBruker_UI("convertOneBruker")
      })
    } else if (input$typeOfRawData == "2") {
      output$convertBruker <- renderUI({
        IDBacApp::multipleMaldiPlates("multipleMaldiPlates")
      })
    }
  })
  
  
  
  
  
  
  
  
  
  # Spectra Conversion ------------------------------------------------------
  
  
  
  # Spectra conversion
  #This observe event waits for the user to select the "run" action button and then creates the folders for storing data and converts the raw data to mzML
  #----
  spectraConversion <- reactive({
    
    IDBacApp::excelMaptoPlateLocation(typeOfRawData = input$typeOfRawData,
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
  
  # Popup summarizing the final status of the conversion
  #----
  popup2 <- reactive({
    showModal(modalDialog(
      size = "m",
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
  # Modal to display while converting to mzML
  #----
  popup1 <- reactive({
    showModal(modalDialog(
      size = "m",
      title = "Important message",
      "To track the",
      "To check what has been converted, you can navigate to:",
      easyClose = FALSE, size = "l",
      footer = ""))
  })
  
  
  
  
  # Call the Spectra processing function when the spectra processing button is pressed
  #----
  observeEvent(c(convertMZinputs$runMsconvert), 
               ignoreInit = TRUE,  {
                 ns <- session$ns
                 popup3()
                 
                 if (input$ConversionsNav == ns("convert_bruker_nav")) {
                   if (input$typeOfRawData == 1) {
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
                 
                   
                   IDBacApp::processMZML(forProcessing = forProcessing,
                                         newExperimentName = input$newExperimentName,
                                         sqlDirectory = sqlDirectory)
                   
                   
                 
                 
                 
                 
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




