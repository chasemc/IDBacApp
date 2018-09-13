# The server portion of the Shiny app serves as the backend, performing data processing and creating the visualizations to be displayed as specified in the UI function(input, output,session) {

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
                      "Rtsne")


# Install and Load Packages
Install_And_Load(Required_Packages)










#  Load colored_Dots.R function



colorBlindPalette <- cbind.data.frame(fac = 1:1008,col = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", rainbow(1000)))



# Reactive variable returning the user-chosen working directory as string
function(input,output,session){





  observe({
    if (is.null(input$startingWith)){}else{

      output$arrowPNG<-renderUI({
        img(src="arrowRight.png")

      })
    }
  })

  observe({
    if (is.null(input$startingWith)){}else{
      output$startingWithUI<-renderUI({
        if(input$startingWith == 1){
          radioButtons("rawORreanalyze",
                       label = h3("Begin by selecting an option below:"),
                       choices = list("Select here to convert and analyze raw-data from a single MALDI-plate" = 1,
                                      "Select here to convert and analyze raw-data from multiple MALDI-plates at once" = 3),
                       selected = 0,
                       inline = FALSE,
                       width = "100%")
        }else if(input$startingWith == 2){
          radioButtons("rawORreanalyze",label = h3("Begin by selecting an option below:"),
                       choices = list("Select here if you want to use .txt peak list files" = 5,
                                      "Select here if you want to use .csv peak list files" = 6),
                       selected = 0,
                       inline = FALSE,
                       width = "100%")
        }else if(input$startingWith == 3){
          radioButtons("rawORreanalyze", label = h3("Begin by selecting an option below:"),
                       choices = list("Select here if you have already converted data with IDBac and want to re-analyze all of it" = 2,
                                      "Select here if you have already converted data with IDBac and want to re-analyze select files" = 4),
                       selected = 0,
                       inline = FALSE,
                       width = "100%")
        }

      })
    }
  })






  # -----------------
  #This "observe" event creates the UI element for analyzing a single MALDI plate, based on user-input.
  observe({
    if (is.null(input$startingWith)){}else if(input$startingWith == 2){
      output$ui1<-renderUI({
        fluidRow(
          p(".txt and .csv support coming soon!")
        )
      })

    }
  })



  # -----------------
  #This "observe" event creates the UI element for analyzing a single MALDI plate, based on user-input.
  observe({
    if (is.null(input$rawORreanalyze)){}else if (input$rawORreanalyze == 1){
      output$ui1 <- renderUI({
        fluidRow(
          column(12,
                 br(),
                 br(),
                 fluidRow(
                   column(12, offset = 3,
                          h3("Starting with a Single MALDI Plate of Raw Data"))), br(), br(),
                 column(5,
                        fluidRow(column(5,
                                        offset = 3,
                                        h3("Instructions"))),
                        br(),
                        p(strong("1: Working Directory")," This directs where on your computer you would like to create an IDBac working directory."),
                        p("In the folder you select, IDBac will create sub-folders within a main directory named \"IDBac\":"),
                        img(src="WorkingDirectory.png", style="width:60%;height:60%"),
                        br(),
                        p(strong("2: Raw Data"),"Your RAW data is a single folder that contains: one subfolder containing protein
                          data and one subfolder containing small-molecule data"),
                        img(src="Single-MALDI-Plate.png", style="width:60%;height:60%"),
                        br(),
                        p("*Note: Sometimes the browser window won't pop up, but will still appear in the application bar. See below:"),
                        img(src="window.png",width="100%")
                        ),
                 column(1
                 ),
                 column(5, style = "background-color:#7777770d",
                        fluidRow(
                          h3("Workflow Pane", align="center")),
                        br(),
                        p(strong("1: Working Directory")),
                        actionButton("selectedWorkingDirectory",
                                     label = "Click to select your Working Directory"),
                        fluidRow(column(12,
                                        verbatimTextOutput("selectedWorkingDirectoryText",
                                                           placeholder = TRUE))),
                        br(),
                        p(strong("2: Raw Data")),
                        actionButton("rawFileDirectory",
                                     label = "Click to select the location of your RAW data"),
                        fluidRow(column(12,
                                        verbatimTextOutput("rawFileDirectory",
                                                           placeholder = TRUE))),
                        br(),
                        p(strong("3:", "Choose  your Sample Map file, the excel sheet that IDBac will use to rename your files.")),
                        fileInput('excelFile',
                                  label = NULL ,
                                  accept =c('.xlsx','.xls')),
                        p(strong("4:","Click \"Convert to mzXML\" to begin spectra conversion.")),
                        actionButton("run",
                                     label = "Convert to mzXML"),
                        br(),
                        br(),
                        br(),
                        br(),
                        p(strong("Note:","If you canceled out of the popup after spectra conversion completed, you can process your converted spectra using the button below: (but only after all files have been converted) This step is not necessary otherwise.")),
                        actionButton("beginPeakProcessing",
                                     label = "Process mzXML spectra")

                 )
          )
        )
      })
    }
  })


  # -----------------
  #This "observe" event creates the UI element for analyzing multiple MALDI plates, based on user-input.
  observe({
    if (is.null(input$rawORreanalyze)){}else if (input$rawORreanalyze == 3){
      output$ui1<-renderUI({
        fluidRow(
          column(12,
                 br(),
                 br(),
                 fluidRow(
                   column(12,offset=3,
                          h3("Starting with Multiple MALDI Plates of Raw Data"))), br(), br(),
                 column(5,
                        fluidRow(column(5,
                                        offset = 3,
                                        h3("Instructions"))),
                        br(),
                        p(strong("1:")," This directs where on your computeryou would like to create an IDBac working directory."),
                        p("In the folder you select- IDBac will create folders within a main directory named \"IDBac\":"),
                        img(src="WorkingDirectory.png", style="width:322px;height:164px"),
                        p("If there is already an \"IDBac\" folder present in the working directory,
                          files will be added into the already-present IDBac folder ",strong("and any samples with the same name will be overwritten.")),
                        br(),
                        p(strong("2:"),"The RAW data file will be one folder that contains individual folders for each
                          MALDI plate. Each MALDI plate folder will contain an Excel map and two folders: one
                          containing protein data and the other containing small molecule data:"),
                        img(src="Multi-MALDI-Plate.png", style="width:410px;height:319px"),
                        p("Note: Sometimes the browser window won't pop up, but will still appear in the application bar. See below:"),
                        img(src="window.png",width="100%")
                        ),
                 column(1
                 ),
                 column(5,
                        style = "background-color:#7777770d",
                        fluidRow(
                          h3("Workflow Pane", align="center")),
                        br(),
                        p(strong("1:"), " Your Working Directory is where files will be created."),
                        actionButton("selectedWorkingDirectory",
                                     label = "Click to select your Working Directory"),
                        fluidRow(column(12,
                                        verbatimTextOutput("selectedWorkingDirectoryText",
                                                           placeholder = TRUE))),
                        br(),
                        p(strong("2:"), "Your RAW data will be one folder that contains folders for each MALDI plate."),
                        actionButton("multipleMaldiRawFileDirectory",
                                     label = "Click to select the location of your RAW data"),
                        fluidRow(column(12,
                                        verbatimTextOutput("multipleMaldiRawFileDirectory",
                                                           placeholder = TRUE))),
                        br(),
                        actionButton("run",
                                     label = "Convert to mzXML"),
                        actionButton("beginPeakProcessing2",
                                     label = "Process mzXML")
                 )
                        )
        )
      })
    }
  })

  # -----------------
  #This "observe" event creates the UI element for re-analyzing data
  observe({
    if (is.null(input$rawORreanalyze)){}else if (input$rawORreanalyze == 2){
      output$ui1<-renderUI({
        fluidRow(
          column(12,
                 br(),
                 br(),
                 fluidRow(
                   column(12,offset=3,
                          h3("Re-Analyze Data That You Already Converted with IDBac"))), br(), br(),
                 column(12,
                        br(),
                        column(5,
                               fluidRow(column(5,
                                               offset = 3,
                                               h3("Instructions"))),
                               br(),
                               p("Left-click the button to the right to select your previously-analyzed data."),
                               p("This will be the folder, originally named \"IDBac\", that was created when you analyzed data the first time."),
                               p("It contains the folders:"),
                               tags$ul(
                                 tags$li("Converted_To_mzXML"),
                                 tags$li("Peak_Lists"),
                                 tags$li("Saved_MANs")
                               ),
                               br(),
                               tags$b("Example:"), br(),
                               img(src="WorkingDirectory_ReAnalysis.png", style="width:322px;height:164px"),
                               br(),br(),
                               p("Note: Sometimes the browser window won't pop up, but will still appear in the application bar. See below:"),
                               img(src="window.png",width="100%")
                        ),
                        column(1
                        ),
                        column(5,
                               style = "background-color:#7777770d",
                               fluidRow(
                                 h3("Workflow Pane", align="center")),
                               p(strong("1:"), "Select the folder containing your data"),
                               actionButton("idbacDirectoryButton",
                                            label = "Click to select the data directory"),
                               fluidRow(column(12,
                                 verbatimTextOutput("idbacDirectoryOut",
                                                    placeholder = TRUE))),
                               br(),
                               p(strong("2:"), "You can now reanalyze your data by proceeding through the tabs at the top of the page. (\"Inverse Peak Comparison\", etc)      ")
                        )
                 )

          )
        )
      })
    }
  })



  # -----------------
  observe({
    if (is.null(input$rawORreanalyze)){}else if (input$rawORreanalyze == 4){
      output$ui1<-renderUI({
        fluidRow(
          column(12,
                 br(),
                 br(),
                 fluidRow(
                   column(width = 12,
                          h3("Customizing which samples to analyze", align = "center"))), br(), br(),
                 column(width = 4),
                 column(width = 4,
                        style = "background-color:#7777770d",
                        h3("Workflow Pane", align = "center"),
                        br(),
                        p(strong("1: "), actionButton("selectedWorkingDirectory",
                                                      label = "Click to select where to create a working directory")),
                        p("Selected Location:"),
                        fluidRow(column(12,
                                        verbatimTextOutput("selectedWorkingDirectoryText",
                                                           placeholder = TRUE))),
                        br(),
                        p(strong("2: "), actionButton("createBlankSelectedWorkingDirectoryFolders",
                                                      label = "Click to create a blank working directory")),
                        br(),
                        p(strong("3:"), "Place the mzXML files that you wish to analyze into:"),
                        p(verbatimTextOutput("whereConvert")),
                        p(strong("4:"), "Select \"Process mzXML\" to process mzXML files for analysis"),
                        actionButton("beginPeakProcessingAgain",
                                     label = "Process mzXML spectra")
                 )
          )
        )
      })
    }
  })


  # -----------------
  # Lets the user choose the directory which the IDBac working directory will be located in
  selectedDirectory <- reactive({
    if(is.null(input$selectedWorkingDirectory)){
      return("No Folder Selected")
    }else if (input$selectedWorkingDirectory > 0){
      choose.dir()
    }
  })


  # -----------------
  # Shows the user which directory they chose for the IDBac working directory
  output$selectedWorkingDirectoryText <- renderPrint(selectedDirectory())


  # -----------------
  # Find if "IDBac" exists in selected folder and then uniquify if necessary
  uniquifiedIDBac <- reactive({
    req(selectedDirectory())
    uniquifiedIDBacs <- list.dirs(selectedDirectory(), recursive = F, full.names = F)
    uniquifiedIDBacs <- make.unique(c(uniquifiedIDBacs, "IDBac"), sep = "-")
    tail(uniquifiedIDBacs, 1)
  })

  # -----------------
  # Creates the IDBac Directory structure, Uniquifies the  "IDBac" folder according to what folders are present in the selected directory
  idbacuniquedir <- eventReactive(input$createBlankSelectedWorkingDirectoryFolders, {
    dir.create(paste0(selectedDirectory(), "\\", uniquifiedIDBac()))
    dir.create(paste0(selectedDirectory(), "\\", uniquifiedIDBac(), "\\Converted_To_mzXML"))
    dir.create(paste0(selectedDirectory(), "\\", uniquifiedIDBac(), "\\Sample_Spreadsheet_Map"))
    dir.create(paste0(selectedDirectory(), "\\", uniquifiedIDBac(), "\\Peak_Lists"))
    dir.create(paste0(selectedDirectory(), "\\", uniquifiedIDBac(), "\\Saved_MANs"))
    return(paste0(selectedDirectory(), "\\", uniquifiedIDBac()))
  })


  # -----------------
  # Shows the user where the created, uniquified, IDBac folder was created
  observeEvent(input$createBlankSelectedWorkingDirectoryFolders,{
    output$whereConvert <- renderText({
      paste0(idbacuniquedir(), "\\Converted_To_mzXML")
    })
    # save path of where mzxml files will be
    idbacDirectory$filePath <- idbacuniquedir()
  })


  # -----------------
  # When ReAnalyzing data, and need to select the "IDBac" folder directly
  pressedidbacDirectoryButton <- reactive({
    if(is.null(input$idbacDirectoryButton)){
      return("No Folder Selected")
    }else if (input$idbacDirectoryButton > 0){
      choose.dir()
    }
  })


  # -----------------
  # Display which directory was selected
  output$idbacDirectoryOut <- renderPrint(pressedidbacDirectoryButton())


  # Create NULL instance
  idbacDirectory <- reactiveValues(filePath = NULL)


  # Reactive events to trigger the creation of the "idbacDirectory" reactive variable
  # This variable is used in lieu of setting a working directory, therefore should point to the main working folder
  observeEvent(input$createBlankSelectedWorkingDirectoryFolders, {
    idbacDirectory$filePath <- idbacuniquedir()
  })


  observeEvent(input$selectedWorkingDirectory, {
    idbacDirectory$filePath <- paste0(selectedDirectory(), "/",uniquifiedIDBac())
  })


  observeEvent(input$idbacDirectoryButton, {
    idbacDirectory$filePath <- pressedidbacDirectoryButton()
  })



  # -----------------
  # This function revereses a provided string
  strReverse <- function(x){
    sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
  }


  # -----------------
  # Reactive variable returning the user-chosen location of the raw MALDI files as string
  rawFilesLocation <- reactive({
    if (input$rawFileDirectory > 0) {
      choose.dir()
    }
  })


  # -----------------
  # Creates text showing the user which directory they chose for raw files
  output$rawFileDirectory <- renderText({
    if (is.null(rawFilesLocation())) {
      return("No Folder Selected")} else{
        folders <- NULL
        foldersInFolder <- list.dirs(rawFilesLocation(), recursive = FALSE, full.names = FALSE) # Get the folders contained directly within the chosen folder.
        for (i in 1:length(foldersInFolder)) {
          folders <- paste0(folders, "\n", foldersInFolder[[i]]) # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
        }
        folders
      }
  })


  # -----------------
  # Reactive variable returning the user-chosen location of the raw MALDI files as string
  multipleMaldiRawFileLocation <- reactive({
    if (input$multipleMaldiRawFileDirectory > 0) {
      choose.dir()
    }
  })


  # -----------------
  # Creates text showing the user which directory they chose for raw files
  output$multipleMaldiRawFileDirectory <- renderText({
    if (is.null(multipleMaldiRawFileLocation())){
      return("No Folder Selected")
    }else{
      folders <- NULL
      foldersInFolder <- list.dirs(multipleMaldiRawFileLocation(), recursive = FALSE, full.names = FALSE) # Get the folders contained directly within the chosen folder.
      for (i in 1:length(foldersInFolder)) {
        folders <- paste0(folders, "\n", foldersInFolder[[i]]) # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
      }

      folders
    }
  })



  # -----------------
  # Spectra conversion
  #This observe event waits for the user to select the "run" action button and then creates the folders for storing data and converts the raw data to mzXML
  spectraConversion<-reactive({
    if(input$rawORreanalyze == 1){
      # When only analyzing one maldi plate this handles finding the raw data directories and the excel map
      # excelMap is a dataframe (it's "Sheet1" of the excel template)
      excelTable <- as.data.frame(read_excel(paste0(input$excelFile$datapath), 2))
      # excelTable takes the sample location and name from excelTable, and also converts the location to the same name-format as Bruker (A1 -> 0_A1)
      excelTable <- cbind.data.frame(paste0("0_", excelTable$Key), excelTable$Value)
      # List the raw data files (for Bruker MALDI files this means pointing to a directory, not an individual file)
      fullZ <- list.dirs(list.dirs(rawFilesLocation(), recursive = FALSE), recursive = FALSE)
      # Get folder name from fullZ for merging with excel table names
      fullZ <- cbind.data.frame(fullZ, unlist(lapply(fullZ, function(x) strsplit(x, "/")[[1]][[3]])))
      colnames(fullZ) <- c("UserInput", "ExcelCell")
      colnames(excelTable) <- c("ExcelCell", "UserInput")
      # Merge to connect filenames in excel sheet to file paths
      fullZ <- merge(excelTable, fullZ, by = c("ExcelCell"))
      fullZ[,3] <- normalizePath(as.character(fullZ[ , 3]))
    }else if(input$rawORreanalyze == 3){
      # When analyzing more han one MALDI plate this handles finding the raw data directories and the excel map
      mainDirectory <- list.dirs(multipleMaldiRawFileLocation(), recursive = F)
      lapped <- lapply(mainDirectory, function(x) list.files(x, recursive = F, full.names = T))
      collectfullZ <- NULL
      # For annotation, look at the single-plate conversion above, the below is basically the same, but iterates over multiple plates, each plate must reside in its own directory.
      for (i in 1:length(lapped)){
        excelTable <- as.data.frame(read_excel(lapped[[i]][grep(".xls",lapped[[i]])], 2))
        excelTable <- cbind.data.frame(paste0("0_", excelTable$Key), excelTable$Value)
        fullZ <- list.dirs(lapped[[i]], recursive = F)
        fullZ <- cbind.data.frame(fullZ, unlist(lapply(fullZ, function(x) strsplit(x, "/")[[1]][[4]])))
        colnames(fullZ) <- c("UserInput", "ExcelCell")
        colnames(excelTable) <- c("ExcelCell", "UserInput")
        fullZ <- merge(excelTable, fullZ, by = c("ExcelCell"))
        fullZ[ , 3] <- normalizePath(as.character(fullZ[ , 3]))
        collectfullZ <- c(collectfullZ, list(fullZ))
      }
      fullZ <- ldply(collectfullZ, data.frame)
    }

    fullZ <- dlply(fullZ, .(UserInput.x))
    # Allow spaces in filepathe
    ww <<-fullZ

    for (i in 1:length(fullZ)){
      fullZ[[i]]$UserInput.y <- shQuote(fullZ[[i]]$UserInput.y)
    }
    # return fullz to the "spectraConversion" reactive variable, this is  is a named list, where each element represents a sample and the element name is the sample name;
    # contents of each element are file paths to the raw data for that sample
    # This will be used by the spectra conversion observe function/event
    fullZ
  })


  # -----------------
  observe({
    # If user chooses to convert files...
    if (is.null(input$run)){}else if(input$run > 0) {


      # Create the proper directory structure, and make sure main directory name is unique so there is no overwriting
      dir.create(paste0(selectedDirectory(), "/", uniquifiedIDBac()))
      dir.create(paste0(selectedDirectory(), "/", uniquifiedIDBac(), "/Converted_To_mzXML"))
      dir.create(paste0(selectedDirectory(), "/", uniquifiedIDBac(), "/Sample_Spreadsheet_Map"))
      dir.create(paste0(selectedDirectory(), "/", uniquifiedIDBac(), "/Peak_Lists"))
      dir.create(paste0(selectedDirectory(), "/", uniquifiedIDBac(), "/Saved_MANs"))


      # spectraConversion() is a named list, where each element represents a sample and the element name is the sample name;
      # contents of each element are file paths to the raw data for that sample
      fullZ <- spectraConversion()
      # fullZ$UserInput.x = sample name
      # fullZ$UserInput.y = file locations

      # outp is the filepath of where to save the created mzXML files
      outp <- file.path(paste0(selectedDirectory(), "\\", uniquifiedIDBac(), "\\Converted_To_mzXML"))



      # Find the location of the proteowizard libraries
      appwd <- getwd()
      applibpath <- file.path(appwd, "library")
      pwizFolderLocation <- installed.packages(c(.libPaths(), applibpath))
      pwizFolderLocation <- as.list(pwizFolderLocation[grep("proteowizardinstallation", pwizFolderLocation), ])
      pwizFolderLocation <- file.path(pwizFolderLocation$LibPath, "proteowizardinstallation", "pwiz")

      #Command-line MSConvert, converts from proprietary vendor data to open mzXML
      msconvertCmdLineCommands <- lapply(fullZ, function(x){
        #Finds the msconvert.exe program which is located the in pwiz folder which is two folders up ("..\\..\\") from the directory in which the IDBac shiny app initiates from
        paste0(shQuote(file.path(pwizFolderLocation, "msconvert.exe")),
               # sets up the command to pass to MSConvert in commandline, with variables for the input files (x$UserInput.y) and for where the newly created mzXML files will be saved
               " ",
               paste0(x$UserInput.y, collapse = "", sep=" "),
              # "--noindex --mzXML --merge -z",
               "--noindex --mzXML --merge -z",
               " -o ",
               shQuote(outp),
               " --outfile ",
               shQuote(paste0(x$UserInput.x[1],".mzXML"))
        )
      }
      )


      functionTOrunMSCONVERTonCMDline<-function(x){

        system(command = as.character(x))
      }



      popup1()


      lengthProgress <- length(msconvertCmdLineCommands)

      withProgress(message = 'Conversion in progress',
                   detail = 'This may take a while...', value = 0, {

                     for(i in 1:lengthProgress){
                       incProgress(1/lengthProgress)

                       functionTOrunMSCONVERTonCMDline(msconvertCmdLineCommands[i])

                     }

                   })

      popup2()


    }
  })



  popup1<-reactive({
    showModal(modalDialog(
      title = "Important message",
      "When file-conversions are complete this pop-up will be replaced by a summary of the conversion.", br(),
      "To check what has been converted, you can navigate to:",
      paste0(selectedDirectory(), "/", uniquifiedIDBac(), "/Converted_To_mzXML"),
      easyClose = FALSE, size="l",
      footer = ""
    ))
  })



  # -----------------
  # Popup summarizing the final status of the conversion
  popup2<-reactive({
    showModal(modalDialog(
      title = "Conversion Complete",
      paste0(nrow(ldply(spectraConversion()))," files were converted into ", length(list.files(paste0(selectedDirectory(), "/", uniquifiedIDBac(), "/Converted_To_mzXML"))),
             " open data format files."), br(),
      "To check what has been converted you can navigate to:",
      paste0(selectedDirectory(), "/", uniquifiedIDBac(), "/Converted_To_mzXML"),
      easyClose = TRUE,
      footer = tagList(actionButton("beginPeakProcessingModal", "Click to continue with Peak Processing"), modalButton("Close"))
    ))
  })





  # -----------------
  # Call the Spectra processing function when the spectra processing button is pressed
  observeEvent({
    c(input$beginPeakProcessing,
      input$beginPeakProcessing2,
      input$beginPeakProcessingModal,
      input$beginPeakProcessingAgain)},{

        fileList <- normalizePath(list.files(list.dirs(paste0(idbacDirectory$filePath,"/Converted_To_mzXML")),pattern = ".mz", full.names = TRUE,ignore.case = TRUE))


        popup3()

        #   numCores <- detectCores()
        #   cl <- makeCluster(numCores)
        #   parSapply(cl,fileList,spectraProcessingFunction)
        #   stopCluster(cl)

        #Single process with sapply instead of parsapply
        #sapply(fileList,function(x)spectraProcessingFunction(x,idbacDirectory$filePath))

        lengthProgress <- length(fileList)

        withProgress(message = 'Processing in progress',
                     detail = 'This may take a while...', value = 0, {

                       for(i in 1:lengthProgress){
                         incProgress(1/lengthProgress)

                         IDBacApp::spectraProcessingFunction(fileList[i],idbacDirectory$filePath)

                       }

                     })


        popup4()


      })



  # -----------------
  # Popup displaying where to find files being created during spectra processing. No status bar -parallel
  # popup3<-reactive({
  #   showModal(modalDialog(
  #     title = "Important message",
  #     "When spectra processing is complete you will be able to begin with the data analysis",br(),
  #     "IDBac uses parallel processing to make these computations faster, unfortunately this means we can't show a progress bar.",br(),
  #     "This also means your computer might be slow during the computations.",br(),
  #     "The step allows for fast interaction during the various data analysis",
  #     "To check the progress, you can navigate to the following directory, where four files will be created per sample ",
  #     paste0(selectedDirectory(), "/",uniquifiedIDBac(),"/Peak_Lists"),
  #
  #
  #     easyClose = FALSE, size="l",footer=""
  #   ))
  # })


  popup3<-reactive({
    showModal(modalDialog(
      title = "Important message",
      "When spectra processing is complete you will be able to begin with the data analysis",br(),
      "To check the progress, observe the progress bar at bottom right or navigate to the following directory, where four files will be created per sample ",
      paste0(selectedDirectory(), "/",uniquifiedIDBac(),"/Peak_Lists"),


      easyClose = FALSE, size="l",footer=""
    ))
  })



  # -----------------
  # Popup notifying user when spectra processing is complete
  popup4<-reactive({
    showModal(modalDialog(
      title = "Spectra Processing is Now Complete",
      br(),
      easyClose = TRUE,
      footer = modalButton("Continue to Data Analysis by visiting consecutive tabs at the top of the page.")))
  })

  # -----------------
  # Read into R, the summed Protein Spectra (subset this (only need two at a time! monkeys)
  # spectra <- reactive({
  #   unlist(sapply(list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists"),full.names=TRUE)[grep(".SummedProteinSpectra.", list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists")))], readRDS))
  # })

  spectra <- reactive({

    # Return Full File Path of all averaged protein spectra RDS
    vectorFilePaths <- list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists"), full.names=TRUE)[grep(".SummedProteinSpectra.", list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists")))]
    # Return only file name of all averaged protein spectra RDS
    vectorFileNames <- list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists"), full.names=FALSE)[grep(".SummedProteinSpectra.", list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists")))]
    # Keep only the sample name from file names
    vectorFileNames <- unlist(strsplit(vectorFileNames, "_SummedProteinSpectra.rds"))
    temp            <- cbind.data.frame(paths = vectorFilePaths, names = vectorFileNames)

    lapply(temp,as.vector)


    # The output is list of length 2: file paths of protein spectra rds files (full spectra, not just peaks) and the samples names
    #
    # > spectra()
    #                                                                               paths   names
    #   C:\\Users\\chase\\Desktop\\New folder\\Peak_Lists/114A-1_SummedProteinSpectra.rds  114A-1
    #   C:\\Users\\chase\\Desktop\\New folder\\Peak_Lists/114A-2_SummedProteinSpectra.rds  114A-2
    #   C:\\Users\\chase\\Desktop\\New folder\\Peak_Lists/114A-3_SummedProteinSpectra.rds  114A-3
    #   C:\\Users\\chase\\Desktop\\New folder\\Peak_Lists/114A-4_SummedProteinSpectra.rds  114A-4
    #   C:\\Users\\chase\\Desktop\\New folder\\Peak_Lists/114B-1_SummedProteinSpectra.rds  114B-1
    #   C:\\Users\\chase\\Desktop\\New folder\\Peak_Lists/114B-10_SummedProteinSpectra.rds 114B-10
  })








  # -----------------
  # Read into R, Protein peak lists (subset this! monkeys)
  # Also, bin then trim
  # Return Peak Intensity Matrix

  trimmedP <- reactive({
# Trims and bins protein peak lists
# If DB insertion is selected, will trim and bin user samples along with DB samples
    IDBacApp::trimBinProtein(injectLibrary = input$libraryInjection,
                       idsToInject = libSearchResultIDsForDendro(),
                       addToLibraryDendroLabel = input$libraryMetadataColumns,
                       spectraPath = idbacDirectory$filePath,
                       lowerMassCutoff = input$lowerMass,
                       upperMassCutoff = input$upperMass,
                       massTolerance = 0.002
                       )


  })



  # -----------------
  # Only include peaks occurring in specified percentage of replicates (groups determined by sample names)
  collapsedPeaksP <- reactive({

    IDBacApp::collapseProteinReplicates(trimmedProteinPeakList = trimmedP(),
                              proteinPercentPresence = input$percentPresenceP)


  })



  # -----------------
  proteinMatrix <- reactive({



    IDBacApp::proteinPeaksToMatrix(bool = input$booled,
                         proteinPeaks = collapsedPeaksP())


  })


  # -----------------
  ################################################
  #This creates the Inverse Peak Comparison plot that compares two user-selected spectra() and the calculation required for such.
  listOfDataframesForInversePeakComparisonPlot <- reactive({
    ww<<- session$ns
    ert <<- collapsedPeaksP()
    #Selects the peaks to plot based on user-input
    peaksSampleOne<-collapsedPeaksP()[[grep(paste0("^",input$Spectra1,"$"),sapply(seq(1,length(collapsedPeaksP()),by=1),function(x)metaData(collapsedPeaksP()[[x]])$Strain))]]
    peaksSampleTwo<-collapsedPeaksP()[[grep(paste0("^",input$Spectra2,"$"),sapply(seq(1,length(collapsedPeaksP()),by=1),function(x)metaData(collapsedPeaksP()[[x]])$Strain))]]


    #pSNR= the User-Selected Signal to Noise Ratio for protein

    #Create a MALDIquant massObject from the selected peaks
    peaksSampleOne@mass<-peaksSampleOne@mass[which(peaksSampleOne@snr>input$pSNR)]
    peaksSampleOne@intensity<-peaksSampleOne@intensity[which(peaksSampleOne@snr>input$pSNR)]
    peaksSampleOne@snr<-peaksSampleOne@snr[which(peaksSampleOne@snr>input$pSNR)]
    peaksSampleTwo@mass<-peaksSampleTwo@mass[which(peaksSampleTwo@snr>input$pSNR)]
    peaksSampleTwo@intensity<-peaksSampleTwo@intensity[which(peaksSampleTwo@snr>input$pSNR)]
    peaksSampleTwo@snr<-peaksSampleTwo@snr[which(peaksSampleTwo@snr>input$pSNR)]

    #Selects the spectra to plot based on user-input
    # meanSpectrumSampleOne<-spectra()[[grep(paste0(input$Spectra1,"$"),sapply(seq(1,length(spectra()),by=1),function(x)metaData(spectra()[[x]])$Strain))]]
    # meanSpectrumSampleTwo<-spectra()[[grep(paste0(input$Spectra2,"$"),sapply(seq(1,length(spectra()),by=1),function(x)metaData(spectra()[[x]])$Strain))]]

    meanSpectrumSampleOne<-readRDS(spectra()$paths[[which(input$Spectra1 ==  spectra()$names)]])
    meanSpectrumSampleTwo<-readRDS(spectra()$paths[[which(input$Spectra2 == spectra()$names)]])


    #Create dataframes for peak plots and color each peak according to whether it occurs in the other spectrum
    p1b<-as.data.frame(cbind(peaksSampleOne@mass,peaksSampleOne@intensity))
    p1b<-as.data.frame(cbind(peaksSampleOne@mass,peaksSampleOne@intensity))
    p2b<-as.data.frame(cbind(peaksSampleTwo@mass,peaksSampleTwo@intensity))
    p2b<-as.data.frame(cbind(peaksSampleTwo@mass,peaksSampleTwo@intensity))


    p3b<-data.frame(p1b,rep("red",length=length(p1b$V1)),stringsAsFactors = F)
    colnames(p3b)<-c("Mass","Intensity","Color")

    p4b<-data.frame(p2b,rep("grey",length=length(p2b$V1)),stringsAsFactors = F)
    colnames(p4b)<-c("Mass","Intensity","Color")
    p3b$Color[which(p3b$Mass %in% intersect(p3b$Mass,p4b$Mass))]<-"blue"


    a<-(list(meanSpectrumSampleOne,meanSpectrumSampleTwo,p1b,p2b,p3b,p4b))
    names(a)<-c("meanSpectrumSampleOne","meanSpectrumSampleTwo","p1b","p2b","p3b","p4b")
    return(a)


  })

  # -----------------
  #Used in the the inverse-peak plot for zooming
  ranges2 <- reactiveValues(x = NULL, y = NULL)

  # -----------------
  output$inversePeakComparisonPlot <- renderPlot({

    temp <<- listOfDataframesForInversePeakComparisonPlot()
    meanSpectrumSampleOne <-temp$meanSpectrumSampleOne
    meanSpectrumSampleTwo <-temp$meanSpectrumSampleTwo
    p3b <<-temp$p3b
    p4b <<-temp$p4b

    remove(temp)

    #Create peak plots and color each peak according to whether it occurs in the other spectrum
    plot(x = meanSpectrumSampleOne@mass,
         y = meanSpectrumSampleOne@intensity,
         ylim = c(-max(meanSpectrumSampleTwo@intensity),
                  max(meanSpectrumSampleOne@intensity)),
         type = "l",
         col = adjustcolor("Black", alpha=0.3),
         xlab = "m/z",
         ylab = "Intensity")
    lines(x = meanSpectrumSampleTwo@mass,
          y = -meanSpectrumSampleTwo@intensity)
    rect(xleft = p3b$Mass - 0.5,
         ybottom = 0,
         xright = p3b$Mass + 0.5,
         ytop = ((p3b$Intensity) * max(meanSpectrumSampleOne@intensity) / max(p3b$Intensity)),
         border = p3b$Color)
    rect(xleft = p4b$Mass - 0.5,
         ybottom = 0,
         xright = p4b$Mass + 0.5,
         ytop = -((p4b$Intensity) * max(meanSpectrumSampleTwo@intensity) / max(p4b$Intensity)),
         border = p4b$Color)

    observe({
      brush <- input$plot2_brush
      if (!is.null(brush)) {
        ranges2$x <- c(brush$xmin, brush$xmax)
        ranges2$y <- c(brush$ymin, brush$ymax)
      }else{
        ranges2$x <- NULL
        ranges2$y <- c(-max(meanSpectrumSampleTwo@intensity),
                       max(meanSpectrumSampleOne@intensity))
      }
    })
  })

  # -----------------
  output$inversePeakComparisonPlotZoom <- renderPlot({
    temp<- listOfDataframesForInversePeakComparisonPlot()
    meanSpectrumSampleOne <-temp$meanSpectrumSampleOne
    meanSpectrumSampleTwo <-temp$meanSpectrumSampleTwo
    p1b <-temp$p1b
    p2b <-temp$p2b
    p3b <-temp$p3b
    p4b <-temp$p4b
    remove(temp)
    plot(meanSpectrumSampleOne@mass,meanSpectrumSampleOne@intensity,type="l",col=adjustcolor("Black", alpha=0.3), xlim = ranges2$x, ylim = ranges2$y,xlab="m/z",ylab="Intensity")
    lines(meanSpectrumSampleTwo@mass,-meanSpectrumSampleTwo@intensity)
    rect(xleft=p3b$Mass-.5, ybottom=0, xright=p3b$Mass+.5, ytop=((p3b$Intensity)*max(meanSpectrumSampleOne@intensity)/max(p3b$Intensity)),border=p3b$Color)
    rect(xleft=p4b$Mass-.5, ybottom=0, xright=p4b$Mass+.5, ytop=-((p4b$Intensity)*max(meanSpectrumSampleTwo@intensity)/max(p4b$Intensity)),border=p4b$Color)

  })

  # -----------------
  #observeEvent(input$downloadInverse,{
  output$downloadInverse <- downloadHandler(
    filename = function(){paste0("top-",input$Spectra1,"_","bottom-",input$Spectra2,".svg")},
    content = function(file1){
      temp<- listOfDataframesForInversePeakComparisonPlot()
      meanSpectrumSampleOne <-temp$meanSpectrumSampleOne
      meanSpectrumSampleTwo <-temp$meanSpectrumSampleTwo

      p3b <-temp$p3b
      p4b <-temp$p4b

      remove(temp)


      #svg(filename=paste0(input$Spectra1,"_",input$Spectra1,".svg"))
      svglite::svglite(file1, width = 10, height = 8, bg = "white",
                       pointsize = 12, standalone = TRUE)
      #Create peak plots and color each peak according to whether it occurs in the other spectrum
      plot(meanSpectrumSampleOne@mass,meanSpectrumSampleOne@intensity,ylim=c(-max(meanSpectrumSampleTwo@intensity),max(meanSpectrumSampleOne@intensity)),type="l",col=adjustcolor("Black", alpha=0.3),xlab="m/z",ylab="Intensity")
      lines(meanSpectrumSampleTwo@mass,-meanSpectrumSampleTwo@intensity)
      rect(xleft=p3b$Mass-.5, ybottom=0, xright=p3b$Mass+.5, ytop=((p3b$Intensity)*max(meanSpectrumSampleOne@intensity)/max(p3b$Intensity)),border=p3b$Color)
      rect(xleft=p4b$Mass-.5, ybottom=0, xright=p4b$Mass+.5, ytop=-((p4b$Intensity)*max(meanSpectrumSampleTwo@intensity)/max(p4b$Intensity)),border=p4b$Color)
      legend(max(meanSpectrumSampleOne@mass)*.6,max(meanSpectrumSampleOne@intensity)*.7, legend=c(paste0("Top: ",input$Spectra1), paste0("Bottom: ",input$Spectra2)),
             col=c("black", "black"), lty=1:1, cex=1)

      dev.off()
      if (file.exists(paste0(file1, ".svg")))
        file.rename(paste0(file1, ".svg"), file1)

    })

  # -----------------
  #observeEvent(input$downloadInverse,{
  output$downloadInverseZoom <- downloadHandler(
    filename = function(){paste0("top-",input$Spectra1,"_","bottom-",input$Spectra2,"-Zoom.svg")},
    content = function(file1){

      temp<- listOfDataframesForInversePeakComparisonPlot()
      meanSpectrumSampleOne <-temp$meanSpectrumSampleOne
      meanSpectrumSampleTwo <-temp$meanSpectrumSampleTwo
      p1b <-temp$p1b
      p2b <-temp$p2b
      p3b <-temp$p3b
      p4b <-temp$p4b
      remove(temp)


      svglite::svglite(file1, width = 10, height = 8, bg = "white",
                       pointsize = 12, standalone = TRUE)

      plot(meanSpectrumSampleOne@mass,meanSpectrumSampleOne@intensity,type="l",col=adjustcolor("Black", alpha=0.3), xlim = ranges2$x, ylim = ranges2$y,xlab="m/z",ylab="Intensity")
      lines(meanSpectrumSampleTwo@mass,-meanSpectrumSampleTwo@intensity)
      rect(xleft=p3b$Mass-.5, ybottom=0, xright=p3b$Mass+.5, ytop=((p3b$Intensity)*max(meanSpectrumSampleOne@intensity)/max(p3b$Intensity)),border=p3b$Color)
      rect(xleft=p4b$Mass-.5, ybottom=0, xright=p4b$Mass+.5, ytop=-((p4b$Intensity)*max(meanSpectrumSampleTwo@intensity)/max(p4b$Intensity)),border=p4b$Color)


      legend(max(meanSpectrumSampleOne@mass)*.6,max(meanSpectrumSampleOne@intensity)*.7, legend=c(paste0("Top: ",input$Spectra1), paste0("Bottom: ",input$Spectra2)),
             col=c("black", "black"), lty=1:1, cex=1)

      dev.off()
      if (file.exists(paste0(file1, ".svg")))
        file.rename(paste0(file1, ".svg"), file1)

    })

  # -----------------
  # Create peak comparison ui
  output$inversepeakui <-  renderUI({

    if(is.null(input$rawORreanalyze)){
      fluidPage(

        h1(" There is no data to display",img(src="errors/hit3.gif",width="200" ,height="100")),
        br(),
        h4("Troubleshooting:"),
        tags$ul(
          tags$li("Please ensure you have followed the instructions in the \"PreProcessing\" tab"),
          tags$li("If you have already tried that, make sure there are \".rds\" files in your IDBac folder, within a folder
                  named \"Peak_Lists\""),
          tags$li("If it seems there is a bug in the software, this can be reported on the" , a(href="https://github.com/chasemc/IDBacApp/issues",target="_blank","IDBac Issues Page at GitHub.", img(border="0", title="https://github.com/chasemc/IDBacApp/issues", src="GitHub.png", width="25" ,height="25")))
        )

      )

    }else{



      sidebarLayout(
        sidebarPanel(width = 3, style = "background-color:#7777770d",
                     selectInput("Spectra1", label=h5(strong("Spectrum 1 (up)"), br(), "(Peak matches to bottom spectrum are blue, non-matches are red)"),
                                 choices = spectra()$names),
                     selectInput("Spectra2", label=h5(strong("Spectrum 2 (down)")),
                                 choices = spectra()$names),
                     downloadButton("downloadInverse",label="Download Main Plot"),
                     downloadButton("downloadInverseZoom",label="Download Zoomed Plot"),
                     numericInput("percentPresenceP", label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%) (Experiment/Hypothesis dependent)"),value = 70,step=10,min=0,max=100),
                     numericInput("pSNR", label = h5(strong("Signal To Noise Cutoff")),value = 4,step=.5,min=1.5,max=100),
                     numericInput("lowerMass", label = h5(strong("Lower Mass Cutoff")),value = 3000,step=50),
                     numericInput("upperMass", label = h5(strong("Upper Mass Cutoff")),value = 15000,step=50),
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

    }
  })


  # -----------------
  ################################################
  # This creates the Plotly PCA plot and the calculation required for such.

  pcoaCalculation <- reactive({
    if(req(input$distance)=="cosineD"){


      #Cosine Distance Matrix Function
      cosineD <- function(x) {
        as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
      }
      # Perform cosine similarity function
      dend <- proteinMatrix() %>% cosineD
      # Convert NA to 1
      dend[which(is.na(dend))] <- 1
      # Hierarchical clustering using the chosen agglomeration method, convert to as.dendrogram object for dendextend functionality

    }else{
      dend <- proteinMatrix() %>% dist(method=input$distance)
      dend[which(is.na(dend))] <- 1

    }

    pc <- as.data.frame(cmdscale(dend, k=10))
    pc<-pc[,1:3]
    colnames(pc) <- c("Dim.1", "Dim.2", "Dim.3")
    pc["nam"] <- row.names(pc)
    pc
  })

  output$pcoaPlot <- renderPlotly({
    pcaDat <- pcoaCalculation()

    colorsToUse <- dendextend::leaf_colors(coloredDend()$dend)

    if(any(is.na(as.vector(colorsToUse)))){
      colorsToUse <-  dendextend::labels_colors(coloredDend()$dend)
    }

    colorsToUse <- cbind.data.frame(fac = as.vector(colorsToUse), nam = (names(colorsToUse)))
    pcaDat <- merge(pcaDat, colorsToUse, by="nam")



    plot_ly(data = pcaDat,
            x = ~Dim.1,
            y = ~Dim.2,
            z = ~Dim.3,
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


  #PCA Calculation

  pcaCalculation <- reactive({


    pc <- log(proteinMatrix())
    pc[is.infinite(pc)]<-.000001

    pc <- PCA(pc, graph=FALSE, ncp = 50)
    pc <- pc$ind$coord
    pc <- as.data.frame(pc)
    nam <- row.names(pc)
    pc <- cbind(pc,nam)
  })


  colorMatch <- reactive({
    pc <- pcaCalculation()


    # Based on user selection, color PCA based on dendrogram groupings
    if(!is.null(isolate(input$kORheight))){
      if(input$kORheight=="2"){
        # Colors chosen by cutting dendrogram at user-chosen height
        fac <- cutree(dendro(),h=input$height)
      }else if (input$kORheight=="1"){
        # Colors chosen by user-selected "k" of k-means
        fac <- cutree(dendro(),k=input$kClusters)
      }else{
        # No factors, everthing colored black
        fac <- rep(1,length(labels(dendro())))
        names(fac)<-labels(dendro())
      }

      fac


    }

  })

  output$pcaPlot <- renderPlotly({

    pcaDat <- pcaCalculation()
    colorsToUse <- dendextend::leaf_colors(coloredDend()$dend)

    if(any(is.na(as.vector(colorsToUse)))){
      colorsToUse <-  dendextend::labels_colors(coloredDend()$dend)
    }

    colorsToUse <- cbind.data.frame(fac = as.vector(colorsToUse), nam = (names(colorsToUse)))
    pcaDat <- merge(pcaDat, colorsToUse, by="nam")



plot_ly(data = pcaDat,
        x = ~Dim.1,
        y = ~Dim.2,
        z = ~Dim.3,
        type = "scatter3d",
        mode = "markers",
        marker = list(color = ~fac),
        hoverinfo = 'text',
        text = ~nam)
  })





  tsneCalculation <- reactive({
    d<- Rtsne(pcaCalculation(), pca=FALSE, dims=3, perplexity = input$tsnePerplexity,theta = input$tsneTheta, max_iter = input$tsneIterations)
    d <- as.data.frame(d$Y)
    d <- cbind.data.frame(as.vector(pcaCalculation()$nam),d)
    colnames(d) <- c("nam","Dim.1","Dim.2","Dim.3")

    as.data.frame(d)

  })

  output$tsnePlot <- renderPlotly({
    pcaDat <- tsneCalculation()
    colorsToUse <- dendextend::leaf_colors(coloredDend()$dend)

    if(any(is.na(as.vector(colorsToUse)))){
      colorsToUse <-  dendextend::labels_colors(coloredDend()$dend)
    }

    colorsToUse <- cbind.data.frame(fac = as.vector(colorsToUse), nam = (names(colorsToUse)))
    pcaDat <- merge(pcaDat, colorsToUse, by="nam")



    plot_ly(data = pcaDat,
            x = ~Dim.1,
            y = ~Dim.2,
            z = ~Dim.3,
            type = "scatter3d",
            mode = "markers",
            marker = list(color = ~fac),
            hoverinfo = 'text',
            text = ~nam)
  })





  # -----------------
  # Create PCA ui
  output$PCAui <-  renderUI({

    if(is.null(input$Spectra1)){
      fluidPage(

        h1(" There is no data to display",img(src="errors/hit3.gif",width="200" ,height="100")),

        br(),
        h4("Troubleshooting:"),
        tags$ul(
          tags$li("Please ensure you have followed the instructions in the \"PreProcessing\" tab, and then visit the
                  \"Compare Two Samples\" tab."),
          tags$li("If you have already tried that, make sure there are \".rds\" files in your IDBac folder, within a folder
                  named \"Peak_Lists\""),
          tags$li("If it seems there is a bug in the software, this can be reported on the" , a(href="https://github.com/chasemc/IDBacApp/issues",target="_blank","IDBac Issues Page at GitHub.", img(border="0", title="https://github.com/chasemc/IDBacApp/issues", src="GitHub.png", width="25" ,height="25")))
        )

      )

    }else{
      mainPanel(width=12,


                fluidRow(
                  column(width=6,
                         p("PCOA: Provides Three-Dimensional View of Distances (Based upon Distance Algorithm Chosen)"),
                         plotlyOutput("pcoaPlot",width="100%",height="800px")),
                  column(width=6,
                         p("Principle Components Analysis: Provides a dimension reduction of the peak intensity/presence matrix"),
                         plotlyOutput("pcaPlot",width="100%",height="800px"))
                ),
                p("t-SNE"),
                numericInput("tsnePerplexity", label = h5(strong("t-SNE Perplexity")), value = 30, step=10, min = 0, max = 300),
                numericInput("tsneTheta", label = h5(strong("t-SNE Theta")), value = .5, step=.1, max = 1, min=0),
                numericInput("tsneIterations", label = h5(strong("t-SNE Iterations")), value = 1000, step = 50),
                fluidRow( plotlyOutput("tsnePlot",width="100%",height="800px"))

                # br(),
                # fluidRow(      rglwidgetOutput("pcaplot3d"))
      )
    }

  })

  #Create the hierarchical clustering based upon the user input for distance method and clustering technique
  dendro <- reactive({


    if (input$booled == "1") {
      booled<-"_UsedIntenstites"
    }
    else{
      booled<-"_UsedPresenceAbsence"
    }


    cacheDir<-paste0(idbacDirectory$filePath,"\\Dendrogram_Cache\\")
    cacheFile<-paste0(idbacDirectory$filePath,"\\Dendrogram_Cache\\","Distance-",input$distance,"_Clustering-",input$clustering, booled,
                      "_SNR-",input$pSNR,"_PercentPresence-",input$percentPresenceP,"_LowCut-",input$lowerMass,"_HighCut-",input$upperMass,".rds")

    # Create the cache directory if it doesn't exist
    if(!dir.exists(cacheDir)){
      dir.create(cacheDir)
    }

    if(req(input$distance)=="cosineD"){

      if(!file.exists(cacheFile) || input$initateInjection == "TRUE"){

        #Cosine Distance Matrix Function
        cosineD <- function(x) {
          as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
        }
        # Perform cosine similarity function
        dend <- proteinMatrix() %>% cosineD
        # Convert NA to 1
        dend[which(is.na(dend))]<-1
        # Hierarchical clustering using the chosen agglomeration method, convert to as.dendrogram object for dendextend functionality
        dend<- dend %>% hclust(method=input$clustering) %>% as.dendrogram
        # Cache results (insignificant for small trees, helpful for larger trees)
        saveRDS(dend,cacheFile)

      }else{
        dend<-  readRDS(cacheFile)
      }
    }else{
      if(!file.exists(cacheFile) || input$initateInjection == "TRUE"){
        dend <- proteinMatrix() %>% dist(method=input$distance) %>% hclust(method=input$clustering) %>% as.dendrogram
        saveRDS(dend,cacheFile)
      }else{
        dend<-  readRDS(cacheFile)
      }
    }
u <<- dend
    dend
  })

  #User input changes the height of the main hierarchical clustering plot
  plotHeight <- reactive({
    return(as.numeric(input$hclustHeight))
  })


  # -----------------
  output$hclustui <-  renderUI({
    if(input$kORheight!="2"){return(NULL)}else{
      numericInput("height", label = h5(strong("Cut Tree at Height")),value = .5,step=.1,min=0)}
  })

  # -----------------
  output$groupui <-  renderUI({
    if(input$kORheight=="1"){
      numericInput("kClusters", label = h5(strong("Number of Groups")),value = 1,step=1,min=1)}
  })

  # -----------------
  sampleFactorMapColumns<-reactive({
    sampleMappings<-variable.names(read_excel(input$sampleMap$datapath,sheet=1,range=cell_rows(1)))

  })


  # -----------------
  output$sampleMapColumns1<-renderUI({
    selectInput("sampleFactorMapChosenIDColumn", label = h5("Select a Group Mapping"),
                choices = as.list(sampleFactorMapColumns()))
  })


  # -----------------
  output$sampleMapColumns2<-renderUI({


    selectInput("sampleFactorMapChosenAttribute", label = h5("Select a Group Mapping"),
                choices = as.list(sampleFactorMapColumns()[!grepl(input$sampleFactorMapChosenIDColumn,sampleFactorMapColumns(),ignore.case = TRUE)]))
  })


  # -----------------
  levs<-reactive({
    sampleMappings<-read_excel(input$sampleMap$datapath,1)
    #selected column
    # if(any(is.na(sampleMappings[input$sampleFactorMapChosenAttribute]))){
    sampleMappings[input$sampleFactorMapChosenAttribute] %>% unique %>% unlist %>%  as.vector %>% c(.,"Missing_in_Excel")
    # }else{
    #   sampleMappings[input$sampleFactorMapChosenAttribute] %>% unique %>% unlist %>%  as.vector
    # }
  })

  # -----------------
  output$sampleFactorMapColors<-renderUI({

    column(3,
           lapply(1:length(levs()),function(x){
             do.call(colourInput,list(paste0("factor-",gsub(" ","",levs()[[x]])),levs()[[x]],value="blue",allowTransparent=T))
           })
    )
  })


  # -----------------
# Color the Protein Dendrogram
  coloredDend <- reactive({

#
#     # if user selects to customize group samples
#     if (input$kORheight =="3"){
#       if(!is.null(input$sampleMap$datapath)){
#         if(input$colDotsOrColDend == "1"){

    if (input$kORheight =="3"){
      colorsChosen <- sapply(1:length(levs()), function(x) input[[paste0("factor-", gsub(" ", "", levs()[[x]]))]])
    }

    IDBacApp::coloringDendrogram(
        useDots          = if(input$colDotsOrColDend == "1"){TRUE}else{FALSE},
        useKMeans        = if(input$kORheight=="1"){TRUE}else{FALSE},
        cutByHeight      = if(input$kORheight=="2"){TRUE}else{FALSE},
        userColor        = if(input$kORheight=="3"){TRUE}else{FALSE},
        excelFilePath    = input$sampleMap$datapath,
        chosenIdColumn   = input$sampleFactorMapChosenIDColumn,
        chosenMetaColumn = input$sampleFactorMapChosenAttribute,
        dendrogram       = dendro(),
        cutHeight        = input$height,
        cutK             = input$kClusters,
        chosenColorsMeta = levs(),
        colorsChosen     = colorsChosen
    )




  })


  # -----------------
  #Create the hierarchical clustering plot as well as the calculations needed for such.

  output$hclustPlot <- renderPlot({

aws <<- coloredDend()
    par(mar=c(5,5,5,input$dendparmar))

    if (input$kORheight=="1"){
      coloredDend()$dend %>% plot(horiz=TRUE,lwd=8)
    }else if (input$kORheight=="2"){
      coloredDend()$dend  %>%  plot(horiz=TRUE,lwd=8)
      abline(v=input$height,lty=2)
    }else if (input$kORheight=="3"){

      if(is.null(input$sampleMap$datapath)){
        # No sample mapping selected
        dendro()$dend %>% plot(horiz = TRUE,lwd = 8)}else{
          if(input$colDotsOrColDend == "1"){

            coloredDend()$dend  %>%  plot(.,horiz=T)
            IDBacApp::colored_dots(coloredDend()$bigMatrix, coloredDend()$shortenedNames,
                         rowLabels = names(coloredDend()$bigMatrix), horiz=T, sort_by_labels_order = FALSE)
          }else{

            coloredDend()$dend  %>%  plot(., horiz=T)
          }
        }
    }
  }, height=plotHeight)

  # -----------------
  #observeEvent(input$downloadInverse,{
  output$downloadHeirSVG <- downloadHandler(
    filename = function(){paste0("Dendrogram.svg")},
    content = function(file1){
      # svg(filename=paste0(input$Spectra1,"_",input$Spectra1,".svg"))
      svglite::svglite(file1, width = 10, height = plotHeight()/100, bg = "white",
                       pointsize = 12, standalone = TRUE)

      par(mar=c(5,5,5,input$dendparmar))

      if (input$kORheight=="1"){
        dendro() %>% color_branches(k=input$kClusters) %>% plot(horiz=TRUE,lwd=8)

      } else if (input$kORheight=="2"){

        dendro() %>% color_branches(h=input$height)  %>% plot(horiz=TRUE,lwd=8)
        abline(v=input$height,lty=2)

      } else if (input$kORheight=="3"){

        par(mar=c(5,5,5,input$dendparmar))
        if(input$colDotsOrColDend == "1"){

          coloredDend()$dend  %>%  plot(.,horiz=T)
          IDBacApp::colored_dots(coloredDend()$bigMatrix, coloredDend()$shortenedNames,
                       rowLabels = names(coloredDend()$bigMatrix),horiz=T,sort_by_labels_order = FALSE)
        }else{
          coloredDend()$dend  %>%  plot(.,horiz=T)
        }
      }
      dev.off()
      if (file.exists(paste0(file1, ".svg")))
        file.rename(paste0(file1, ".svg"), file1)
    })

  # -----------------
  output$downloadHierarchical <- downloadHandler(


    filename = function() {
      paste0(Sys.Date(), ".newick")
    },
    content = function(file) {
      ape::write.tree(as.phylo(dendro()), file=file)
    }

  )




  # Searching against Databases







  # -----------------
  # Create Heir ui
  output$Heirarchicalui <-  renderUI({

    if(is.null(input$Spectra1)){
      fluidPage(

        h1(" There is no data to display",img(src="errors/hit3.gif",width="200" ,height="100")),

        br(),
        h4("Troubleshooting:"),
        tags$ul(
          tags$li("Please ensure you have followed the instructions in the \"PreProcessing\" tab, and then visited the
                  \"Compare Two Samples\" tab."),
          tags$li("If you have already tried that, make sure there are \".rds\" files in your IDBac folder, within a folder
                  named \"Peak_Lists\""),
          tags$li("If it seems there is a bug in the software, this can be reported on the" , a(href="https://github.com/chasemc/IDBacApp/issues",target="_blank","IDBac Issues Page at GitHub.", img(border="0", title="https://github.com/chasemc/IDBacApp/issues", src="GitHub.png", width="25" ,height="25")))
        )

      )

    }else{

      sidebarLayout(
        sidebarPanel(style = "background-color:#7777770d",

                     tabsetPanel(id= "HierarchicalSidebarTabs", type="tabs",
                                 tabPanel("Hierarchical Clustering Settings", value="hierSettings",
                                          #checkboxGroupInput("Library", label=h5("Inject Library Phylum"),
                                          #                    choices = levels(phyla)),
                                          selectInput("distance", label = h5(strong("Distance Algorithm")),
                                                      choices = list("cosine"="cosineD","euclidean"="euclidean","maximum"="maximum","manhattan"="manhattan","canberra"="canberra", "binary"="binary","minkowski"="minkowski"),
                                                      selected = "euclidean"),
                                          selectInput("clustering", label = h5(strong("Clustering Algorithm")),
                                                      choices = list("ward.D"="ward.D","ward.D2"="ward.D2", "single"="single", "complete"="complete", "average (UPGMA)"="average", "mcquitty (WPGMA)"="mcquitty", "median (WPGMC)"="median","centroid (UPGMC)"="centroid"),
                                                      selected = "ward.D2"),

                                          radioButtons("booled", label = h5(strong("Include peak intensities, or use presence/absence?")),
                                                       choices = list("Presence/Absence" = 1, "Intensities" = 2),
                                                       selected = 2),
                                          numericInput("hclustHeight", label = h5(strong("Expand Tree")),value = 750,step=50,min=100),
                                          numericInput("dendparmar",label=h5(strong("Adjust right margin of dendrogram")),value=20),

                                          radioButtons("kORheight", label = h5(strong("Color clusters based on:")),
                                                       choices = list("Specified Number of Groups" = 1, "Height (x-axis value)" = 2, "User-Defined Categories in Excel Sheet" = 3),
                                                       selected = 1),

                                          uiOutput("groupui"),
                                          uiOutput("hclustui"),
                                          uiOutput("sampleGroupColoringui"),

                                          br(),
                                          h4("Suggestions for Reporting Protein Analysis:"),
                                          uiOutput("proteinReport"),
                                          br(),
                                          downloadButton("downloadHeirSVG",label="Save Dendrogram as SVG"),
                                          br(),
                                          br(),
                                          downloadButton("downloadHierarchical","Save as Newick File")
                                 ),
                                 tabPanel("Library Search", value="hierLibrarySearch",
                                          p("This is for searching against user-created libraries"),

                                          uiOutput("libraryInjectionLibrarySelect"),
                                          uiOutput("libraryMetadataColumnsSelection"),

                                          radioButtons("initateInjection", label = h3("Start Injection"),
                                                       choices = list("Yes" = "TRUE", "No" = "FALSE"),
                                                       selected = "FALSE")

                                 )

                     )),
        mainPanel("Hierarchical Clustering",
                  column(8,
                         plotOutput("hclustPlot")))

      )
    }
  })


  # -----------------
  output$proteinReport<-renderUI(
    p("This dendrogram was created by analyzing ",tags$code(length(labels(dendro()))), " samples,
      and retaining peaks with a signal to noise ratio above ",tags$code(input$pSNR)," and occurring in greater than ",tags$code(input$percentPresenceP),"% of replicate spectra.
      Peaks occuring below ",tags$code(input$lowerMass)," m/z or above ",tags$code(input$upperMass)," m/z were removed from the analyses. ",
      "For clustering spectra, ",tags$code(input$distance), " distance and ",tags$code(input$clustering), " algorithms were used.")
  )





  # -----------------
  output$sampleGroupColoringui <-   renderUI(

    if(input$kORheight == "3"){
      tags$div(
        p("To color samples according to user-defined groupings..."),
        p("To use this function, create a different excel file and list all of your sample names in
          different rows of a single column. In other columns you may add additional characteristics
          of your samples (eg. media type, genus, sample location), with one characteristic per column.
          You will have the option to color code your hierarchical clustering plot
          based on these characteristics, which will appear in the drop-down list below."),
        radioButtons("colDotsOrColDend", label = h5("Color dend or dots:"),
                     choices = list("dots" = 1, "no dots" = 2),
                     selected = 1),
        p("Click the blue boxes under a factor (below) to change the color of the factor."),
        fileInput('sampleMap', label = "Sample Mapping" , accept =c('.xlsx','.xls')),
        uiOutput("sampleMapColumns1"),
        uiOutput("sampleMapColumns2"),
        fluidRow(
          uiOutput("sampleFactorMapColors"))
        )
    })


  # -----------------
  smallPeaks <- reactive({
    unlist(sapply(list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists"),full.names=TRUE)[grep(".SmallMoleculePeaks.", list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists")))], readRDS))
  })


  # -----------------
  small_Binned_Matrix<-reactive({


    if(!is.null(input$Spectra1)){  # Check if there are protein spectra if TRUE, display dendro and use in analysis

      labs <- sapply(smallPeaks(), function(x)metaData(x)$Strain)

      if(is.null(input$plot_brush$ymin)){
        if(length(smallPeaks()) >= 100){
          combinedSmallMolPeaks <- smallPeaks()[1:sample.int(10,1)]
          # Also get matrix sample for subtraction
          combinedSmallMolPeaks <- c(combinedSmallMolPeaks,smallPeaks()[grep(paste0("Matrix",collapse="|"), labs,ignore.case=TRUE)])

        }else{
          combinedSmallMolPeaks <- smallPeaks()
        }
      }else{
        #This takes a brush selection over the heirarchical clustering plot within the MAN tab and uses this selection of samples for MAN analysis
        location_of_Heirarchical_Leaves<-get_nodes_xy(dendro())
        minLoc<-input$plot_brush$ymin
        maxLoc<-input$plot_brush$ymax
        # See undernath for explanation of each column
        threeColTable <- data.frame(seq(1:length(labels(dendro()))), rep(1:length(labels(dendro()))) ,labels(dendro()))
        #note: because rotated tree, x is actually y, y is actually x
        #column 1= y-values of dendrogram leaves
        #column 2= node x-values we selected for only leaves by only returning nodes with x-values of 0
        #column 3= leaf labels

        # w = pull out the selected sample(s) indices based on the brush
        w<- which(threeColTable[,1] > minLoc & threeColTable[,1] < maxLoc)
        # w = pull out the selected brushed sample(s)
        brushed<-as.vector(threeColTable[,3][w])
        # get indices of all sample names for small molecule peak lists
        labs<-as.vector(sapply(smallPeaks(),function(x)metaData(x)$Strain))
        # only return small moleule peak lists which were brushed, strict grep

        combinedSmallMolPeaks<-smallPeaks()[grep(paste0(c(paste0("^",brushed,"$"),"Matrix"),collapse="|"), labs,ignore.case=TRUE)]

      }
      #if(length(grep("Matrix",sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain),ignore.case=TRUE))==0){"No Matrix Blank!!!!!!!"}else{
      #find matrix spectra

    }else{ # If !is.null(input$Spectra1 == TRUE then there are no protein spectra, run only MAN analysis
      combinedSmallMolPeaks<-smallPeaks()


    }

    # input$matrixSamplePresent (Whether there is a matrix sample)  1=Yes   2=No
    if(input$matrixSamplePresent ==1){
      # combinedsmallMolPeaksm  will contain all samples containing word matrix
      combinedSmallMolPeaksm<-combinedSmallMolPeaks[grep(paste0("Matrix",collapse="|"),sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain),ignore.case=TRUE)]

      # Check if there is a matrix sample
      validate(
        need(combinedSmallMolPeaksm != "", "It seems that you don't have a sample containing \"Matrix\" in its name to use for a matrix blank.  Try selecting \"No\" under \"Do you have a matrix blank\" to left, or checking your sample names/data." )
      )

      # At least for now, replicate matrix samples are merged into a consensus peak list.
      # Make sure we haven't reduced the mass object down to S4
      if(typeof(combinedSmallMolPeaksm)=="S4"){combinedSmallMolPeaksm<- list(combinedSmallMolPeaksm)}
      #
      combinedSmallMolPeaksm<-mergeMassPeaks(combinedSmallMolPeaksm)
      #For now, matrix peaks are all picked at SNR > 6
      combinedSmallMolPeaksm@mass<-combinedSmallMolPeaksm@mass[which(combinedSmallMolPeaksm@snr>6)]
      combinedSmallMolPeaksm@intensity<-combinedSmallMolPeaksm@intensity[which(combinedSmallMolPeaksm@snr>6)]
      combinedSmallMolPeaksm@snr<-combinedSmallMolPeaksm@snr[which(combinedSmallMolPeaksm@snr>6)]
      combinedSmallMolPeaks<-combinedSmallMolPeaks[which(!grepl(paste0("Matrix",collapse="|"),sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain),ignore.case=TRUE))]
      combinedSmallMolPeaks<-c(combinedSmallMolPeaksm,combinedSmallMolPeaks)

    }else{
      combinedSmallMolPeaks<-combinedSmallMolPeaks[which(!grepl(paste0("Matrix",collapse="|"),sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain),ignore.case=TRUE))]
    }


    for (i in 1:length(combinedSmallMolPeaks)){
      combinedSmallMolPeaks[[i]]@mass<-combinedSmallMolPeaks[[i]]@mass[which(combinedSmallMolPeaks[[i]]@snr>input$smSNR)]
      combinedSmallMolPeaks[[i]]@intensity<-combinedSmallMolPeaks[[i]]@intensity[which(combinedSmallMolPeaks[[i]]@snr>input$smSNR)]
      combinedSmallMolPeaks[[i]]@snr<-combinedSmallMolPeaks[[i]]@snr[which(combinedSmallMolPeaks[[i]]@snr>input$smSNR)]
    }
    binPeaks(combinedSmallMolPeaks[which(sapply(combinedSmallMolPeaks,function(x)length(mass(x)))!=0)],tolerance=.002)
  })


  # -----------------
  trimmedSM <- reactive({
   trim(small_Binned_Matrix(),c(input$lowerMassSM,input$upperMassSM))
  })

  # -----------------
  subSelect<-reactive({

    # process for MAN creation

    #Get sample IDs from MALDIquant spectra
    labs <- sapply(trimmedSM(), function(x)metaData(x)$Strain)
    labs <- factor(labs)
    new2 <- NULL
    newPeaks <- NULL

    #Merge specctra based on frequency of presence.  For peaks above threshold, keep the mean intensity
    for (i in seq_along(levels(labs))) {
      specSubset <- which(labs == levels(labs)[[i]])
      if (length(specSubset) > 1) {
        new <-suppressWarnings(filterPeaks(trimmedSM()[specSubset],minFrequency=input$percentPresenceSM/100))
        new<-mergeMassPeaks(new,method="mean")
        new2 <- c(new2, new)
      } else{
        new2 <- c(new2, trimmedSM()[specSubset])
      }

    }

    combinedSmallMolPeaks <- new2


    #This section deals with whether to remove a matrix blank or not, as decided by user, defaults to remove blank
    if(input$matrixSamplePresent ==1){

      #Removing Peaks which share the m/z as peaks that are in the Matrix Blank

      #----------------------------------------------------------------------------
      #Find the matrix sample index
      #First, get all sample IDs
      labs <- sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain)
      #Next, find which ID contains "matrix", in any capitalization
      matrixIndex <- grep(paste0("Matrix",collapse="|"),labs,ignore.case=TRUE)

      #----------------------------------------------------------------------------
      #peaksa = all samples but remove the matrix sample from the list
      peaksa <- combinedSmallMolPeaks[-matrixIndex]
      #peaksb = matrix blank sample
      peaksb <- combinedSmallMolPeaks[[matrixIndex]]

      for (i in 1:length(peaksa)){

        # setdiff to find which peaks are
        commonIons <- which(!peaksa[[i]]@mass %in% setdiff(peaksa[[i]]@mass,peaksb@mass))
        if(length(commonIons)!=0){   # Without this if statement, peaksa values will be set to 0 if no matrix matches are found == BAD
          peaksa[[i]]@mass <- peaksa[[i]]@mass[-commonIons]
          peaksa[[i]]@intensity <- peaksa[[i]]@intensity[-commonIons]
          peaksa[[i]]@snr <- peaksa[[i]]@snr[-commonIons]
        }
      }

    }else{
      peaksa<-combinedSmallMolPeaks
    }

    peaksa
  })

  # -----------------

  smallMolNetworkDataFrame <- reactive({

    smallNetwork <- intensityMatrix(subSelect())
    temp <- NULL
    for (i in 1:length(subSelect())){
      temp <- c(temp,subSelect()[[i]]@metaData$Strain)
    }


    peaksaNames <- factor(temp)

    rownames(smallNetwork) <- paste(peaksaNames)

    www <<-smallNetwork
    #---- Note for Chase: attributes(smallNetwork) contain vectors of masses, this will cause slow-downs

    bool <- smallNetwork
    bool[is.na(bool)] <- 0
    bool <- as.data.frame(bool)


    bool <-  ifelse(bool > 0,1,0)
    bool <- bool
    bool <- as.data.frame(bool)
    #The network is weighted by the inverse of percentage of presence of the peak, this de-emphasizes commonly occuring peaks and "pulls" rarer peaks closer to their associated sample
    bool[,colnames(bool)] <- sapply(bool[,colnames(bool)],function(x) ifelse(x==1,1/sum(x),x))
    #Create a matrix readable by Gephi as an edges file
    bool <- cbind(rownames(bool),bool)
    bool <- melt(bool)
    # Removeself
    bool <- subset(bool, value!=0)
    colnames(bool) <- c("Source","Target","Weight")
    # Round m/z values to two decimals, use sprintf to preserve trailing zeros
    bool$Target <- sprintf(as.numeric(as.matrix(bool$Target)),fmt='%#.2f')

    bool$Source <- as.character(bool$Source)
    bool$Target <- as.numeric(bool$Target)
    bool$Weight <- as.numeric(bool$Weight)

    bool

  })

  # -----------------

  output$downloadSmallMolNetworkData <- downloadHandler(
    filename = function(){"SmallMolecule_Network.csv"},
    content = function(file){

      write.csv(as.matrix(smallMolNetworkDataFrame()),file,row.names = FALSE)
    }
  )


  # -----------------
  #This creates the network plot and calculations needed for such.
  output$metaboliteAssociationNetwork <- renderSimpleNetwork({
    temp <- NULL


    for (i in 1:length(subSelect())){
      temp <- c(temp,subSelect()[[i]]@metaData$Strain)
    }

    a <- as.undirected(graph_from_data_frame(smallMolNetworkDataFrame()))
    a<-igraph::simplify(a)
    wc <- fastgreedy.community(a)
    b <- igraph_to_networkD3(a, group = (wc$membership + 1))
    z <- b$links
    zz <- b$nodes

    biggerSampleNodes<-rep(1,times=length(zz[,1]))
    zz<-cbind(zz,biggerSampleNodes)
    zz$biggerSampleNodes[which(zz[,1] %in% temp)]<-50
    forceNetwork(Links = z, Nodes = zz, Source = "source",Nodesize = "biggerSampleNodes",
                 Target = "target", NodeID = "name",
                 Group = "group", opacity = 1,opacityNoHover=.8, zoom = TRUE)

  })


  # # -----------------
  # #This displays the number of clusters created on the hierarchical clustering tab- displays text at top of the clustering page.
  # output$Clusters <- renderText({
  #
  #   # Display text of how many clusters were created.
  #   if (is.null(input$kORheight)){
  #   }else if (input$kORheight=="2"){
  #     isolate(   paste("You have Created ", length(unique(cutree(dendro(),h=input$height)))," Cluster(s)"))
  #   }
  #   else if (input$kORheight=="1"){
  #     isolate( paste("You have Created ", length(unique(cutree(dendro(),k=input$kClusters)))," Cluster(s)"))
  #   }
  # })


  # -----------------
  # ##This displays the number of clusters created on the hierarchical clustering tab- displays text at top of the networking page.
  # output$Clusters2 <- renderText({
  #   # Display text of how many clusters were created.
  #
  #   if(!is.null(input$Spectra1)){
  #
  #     if(input$kORheight=="2"){
  #       paste("You have ", length(unique(cutree(dendro(),h=input$height)))," Cluster(s)")
  #     }
  #     else{
  #       paste("You have ", length(unique(cutree(dendro(),k=input$kClusters)))," Cluster(s)")
  #     }
  #
  #   }
  # })


  # -----------------
  # User input changes the height of the heirarchical clustering plot within the network analysis pane
  plotHeightHeirNetwork <- reactive({
    return(as.numeric(input$hclustHeightNetwork))
  })

  # -----------------
  # Plot MAN Dendrogram

  output$netheir <- renderPlot({
    par(mar=c(5,5,5,input$dendparmar2))
    if (input$kORheight=="1"){
      isolate(dendro() %>% color_branches(k=input$kClusters) %>% plot(horiz=TRUE,lwd=8))
    } else if (input$kORheight=="2"){
      isolate(dendro() %>% color_branches(h=input$height)  %>% plot(horiz=TRUE,lwd=8))
      isolate(abline(v=input$height,lty=2))
    } else if (input$kORheight=="3"){
      if(input$colDotsOrColDend == "1"){
        IDBacApp::colored_dots(coloredDend()$bigMatrix, coloredDend()$shortenedNames,
                     rowLabels = names(coloredDend()$bigMatrix),horiz=T,sort_by_labels_order = FALSE)
      }else{
        coloredDend()$dend  %>%  plot(.,horiz=T)
      }
    }
  },height=plotHeightHeirNetwork)

  # -----------------
  # Create MAN ui
  output$MANui <-  renderUI({

    fluidPage(
      column(width=3,
             fluidRow(
               sidebarPanel(style='padding:30px',width="100%",
                            radioButtons("matrixSamplePresent", label = h5(strong("Do you have a matrix blank?")),
                                         choices = list("Yes" = 1, "No (Also Turns Off Matrix Subtraction)" = 2),
                                         selected = 1),
                            numericInput("percentPresenceSM", label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%) (Experiment/Hypothesis dependent)"),value = 70,step=10,min=0,max=100),
                            numericInput("smSNR", label = h5(strong("Signal To Noise Cutoff")),value = 4,step=.5,min=1.5,max=100),


                            numericInput("upperMassSM", label = h5(strong("Upper Mass Cutoff")),value = 2000,step=20,max=round(max(sapply(smallPeaks(),function(x)max(mass(x)))),digits=-1)),
                            numericInput("lowerMassSM", label = h5(strong("Lower Mass Cutoff")),value = 200,step=20,min=round(min(sapply(smallPeaks(),function(x)min(mass(x)))),digits=-1)),
                            numericInput("hclustHeightNetwork", label = h5(strong("Expand Tree")),value = 750,step=50,min=100),
                            numericInput("dendparmar2",label=h5(strong("Adjust right margin of dendrogram")),value=5),
                            downloadButton("downloadSmallMolNetworkData", label = "Download Current Network Data", value = FALSE),
                            br(),
                            p(strong("Hint 1:"), "Use mouse to select parts of the tree and display the MAN of corresponding samples."),
                            p(strong("Hint 2:"), "Use mouse to click & drag parts (nodes) of the MAN if it appears congested."),br()

               )),


             fluidRow(
               sidebarPanel(width="100%",
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
                            )
             )),


      column(width=9,
             column(width=5,style ="padding: 14px 0px; margin:0%",
                    plotOutput("netheir",width="100%",height="100%",
                               click = "plot_click",
                               dblclick = "plot_dblclick",
                               hover = "plot_hover",
                               brush = "plot_brush")
             ),column(width=7,style ="padding: 14px 0px; margin:0%",
                      absolutePanel(fixed=TRUE,width ="50%",

                      simpleNetworkOutput("metaboliteAssociationNetwork"))
             )))
  })

  # -----------------
  # Output a paragraph about which paramters were used to create the currently-displayed MAN
  output$manReport<-renderUI({
    p("This MAN was created by analyzing ",tags$code(length(subSelect())), " samples,",if(input$matrixSamplePresent==1){("subtracting a matrix blank,")}else{},
      "and retaining peaks with a signal to noise ratio above ",tags$code(input$smSNR)," and occurring in greater than ",tags$code(input$percentPresenceSM),"% of replicate spectra.
      Peaks occuring below ",tags$code(input$lowerMassSM)," m/z or above ",tags$code(input$upperMassSM)," m/z were removed from the analysis. ")
  })

  # -----------------
  # Output a paragraph about which parameters were used to create the currently-displayed dendrogram
  output$proteinReport2<-renderUI({

    if(length(labels(dendro()))==0){
      p("No Protein Data to Display")
    }else{
      p("This dendrogram was created by analyzing ",tags$code(length(labels(dendro()))), " samples,
        and retaining peaks with a signal to noise ratio above ",tags$code(input$pSNR)," and occurring in greater than ",tags$code(input$percentPresenceP),"% of replicate spectra.
        Peaks occuring below ",tags$code(input$lowerMass)," m/z or above ",tags$code(input$upperMass)," m/z were removed from the analyses. ",
        "For clustering spectra, ",tags$code(input$distance), " distance and ",tags$code(input$clustering), " algorithms were used.")
    }

  })


  # Updating IDBac Functions
  #--------------------------------------

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
      tags$li(paste0("Checking for Internet Connection: ")),
      tags$li(paste0("Installed Version: ")),
      tags$li(paste0("Latest Stable Release: ")),
      easyClose = FALSE, size="l",footer="",fade=FALSE
    ))



    internetPing <-  !suppressWarnings(system(paste("ping -n 1", "www.google.com")))

    if (internetPing == TRUE){
      internetPingResponse <- "Successful"
      showModal(modalDialog(
        title = "IDBac Update",
        tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
        tags$li(paste0("Installed Version: ")),
        tags$li(paste0("Latest Stable Release: ")),
        easyClose = FALSE, size="l",footer="",fade=FALSE
      ))

      Sys.sleep(.75)

      # Currently installed version
      local_version <- tryCatch(packageVersion("IDBacApp"),
                                error = function(x) paste("Installed version is latest version"),
                                finally = function(x)packageVersion("IDBacApp"))

      showModal(modalDialog(
        title = "IDBac Update",
        tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
        tags$li(paste0("Installed Version: ", local_version)),
        tags$li(paste0("Latest Stable Release: ")),
        easyClose = FALSE, size="l",footer="",fade=FALSE
      ))

      Sys.sleep(.75)

      showModal(modalDialog(
        title = "IDBac Update",
        tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
        tags$li(paste0("Installed Version: ", local_version)),
        tags$li(paste0("Latest Stable Release: ")),
        easyClose = FALSE, size="l",footer="",fade=FALSE
      ))

      Sys.sleep(.75)

      # Latest GitHub Release
      getLatestStableVersion <- function(){
        base_url <- "https://api.github.com/repos/chasemc/IDBacApp/releases/latest"
        response <- httr::GET(base_url)
        parsed_response <- httr::content(response, "parsed", encoding = "utf-8")
        parsed_response$tag_name
      }

      latestStableVersion <- try(getLatestStableVersion())

      showModal(modalDialog(
        title = "IDBac Update",
        tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
        tags$li(paste0("Installed Version: ", local_version)),
        tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
        easyClose = FALSE, size="l",footer="",fade=FALSE
      ))

      if (class(latestStableVersion) == "try-error"){

        showModal(modalDialog(
          title = "IDBac Update",
          tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
          tags$li(paste0("Installed Version: ", local_version)),
          tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
          tags$li("Unable to connect to IDBac GitHub repository"),
          easyClose = TRUE, size="l",footer="",fade=FALSE
        ))

      }else{
        # Check current version # and the latest github version. If github v is higher, download and install
        # For more info on version comparison see: https://community.rstudio.com/t/comparing-string-version-numbers/6057/6
        downFunc <- function() {
          devtools::install_github(paste0("chasemc/IDBacApp@",latestStableVersion), force=TRUE, quiet = F, quick=T)
          message(tags$span(style="color:red;font-size:36px;", "Finished. Please Exit and Restart IDBac."))
        }

        if(as.character(local_version) == "Installed version is latest version"){

          showModal(modalDialog(
            title = "IDBac Update",
            tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
            tags$li(paste0("Installed Version: ", local_version)),
            tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
            tags$li("Updating to latest version... (please be patient)"),
            pre(id = "console"),
            easyClose = FALSE, size="l",footer="",fade=FALSE
          ))

          withCallingHandlers(
            downFunc(),
            message = function(m) {
              shinyjs::html("console", m$message, TRUE)
            }
          )

        }else if(compareVersion(as.character(local_version), as.character(latestStableVersion)) == -1) {

          showModal(modalDialog(
            title = "IDBac Update",
            tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
            tags$li(paste0("Installed Version: ", local_version)),
            tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
            tags$li("Updating to latest version... (please be patient)"),
            pre(id = "console"),
            easyClose = FALSE, size="l",footer="",fade=FALSE
          ))

          withCallingHandlers(
            downFunc(),
            message = function(m) {
              shinyjs::html("console", m$message, TRUE)
            }
          )

        }else{

          showModal(modalDialog(
            title = "IDBac Update",
            tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
            tags$li(paste0("Installed Version: ", local_version)),
            tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
            tags$li("Latest Version is Already Installed"),
            easyClose = TRUE, size="l",fade=FALSE,footer = modalButton("Close")
          ))
        }
      }

    }else{
      # if internet ping is false:

      internetPingResponse <- "Unable to Connect"
      showModal(modalDialog(
        title = "IDBac Update",
        tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
        tags$li(paste0("Installed Version: ")),
        tags$li(paste0("Latest Stable Release: ")),
        easyClose = FALSE, size="l",footer="",fade=FALSE
      ))

    }
  })






  #------------------------------------------------------------------------------------------------------------

  #-------------------------------------- In-house library generation code:


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
    # Check for mzXML files
    mzXMLfiles <- list.files(paste0(idbacDirectory$filePath, "\\Converted_To_mzXML"), full.names = FALSE)
    mzXMLfiles <- unlist(strsplit(mzXMLfiles, ".mzXML"))
    nonMissingmzXML <- which(currentlyLoadedSamples %in% mzXMLfiles)
    missingmzXML <- which(! currentlyLoadedSamples %in% mzXMLfiles)
    currentlyLoadedSamples <- currentlyLoadedSamples[nonMissingmzXML]
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




    rhandsontable::rhandsontable(DF, useTypes = FALSE, selectCallback = TRUE, contextMenu = FALSE) %>%
      hot_col("Strain_ID", readOnly = TRUE)
  })

  observeEvent(input$saveBtn, {
    appDirectory <- getwd() # Get the location of where IDBac is installed
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
    }else{
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
  libraries <- function(){list.files(file.path(getwd(), "SpectraLibrary"), pattern=".sqlite", full.names = TRUE)}

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
                       "mzXML",
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
                       "mzXML",
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


  #------------------------------------
  #------------------------------------ append an Existing Library


  output$appendLibPanelRadios  <- renderUI({
    if(input$libraryTabs == "addToExistingLibPanel"){
      radioButtons(inputId = "appendLibPanelRadiosSelected",
                   label= "Existing Libraries",
                   choiceNames = basename(libraries()),
                   choiceValues = as.list(libraries())
      )
    }

  })


  appendLibraryEnvironmentTracking <- new.env()  # This allows modifyLibraryTable() below to update correctly

  appendToLibraryTable <- reactive({
    # Open connection to chosen existing database
    appendDatabaseConnect <- DBI::dbConnect(RSQLite::SQLite(), paste0(input$appendLibPanelRadiosSelected))
    # Create lazy-eval tbl
    db <- dplyr::tbl(appendDatabaseConnect, "IDBacDatabase")
    # Select only columns to be displayed in IDBac
    db <- db %>%
      dplyr::select(-c("manufacturer",
                       "model",
                       "ionisation",
                       "analyzer",
                       "detector",
                       "Protein_Replicates",
                       "Small_Molecule_Replicates",
                       "mzXML",
                       "proteinPeaksRDS")) %>%
      dplyr::collect()


    if ((!is.null(input$hot2)) && appendLibraryEnvironmentTracking$value == input$appendLibPanelRadiosSelected) {
      rhandsontable::hot_to_r(input$hot3)
    } else {
      appendLibraryEnvironmentTracking$value <-input$appendLibPanelRadiosSelected
      db
    }

  })

  # Display the new Library as an editable table
  output$hot3 <- rhandsontable::renderRHandsontable({
    DF <- appendToLibraryTable()
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





  # ----------------- Append data to selected database

  observeEvent(input$saveAppendDatabase1, {

    showModal(popupDBappend())

  })


  # Popup summarizing the final status of the conversion
  popupDBappend <- function(failed = FALSE){
    modalDialog(
      title = "Are you sure?",
      p("There is already a database with this name."),
      p(paste0("Pressing save below will append to the existing database: \"", isolate(input$appendLibPanelRadiosSelected),"\"")),
      footer = tagList(actionButton("saveAppendDatabase2", paste0("Append to: \"", isolate(basename(input$appendLibPanelRadiosSelected)),"\"")), modalButton("Close"))
    )}

  observeEvent(input$saveAppendDatabase2, {
    # After initiating the database
    newDatabase <- DBI::dbConnect(RSQLite::SQLite(), paste0(input$appendLibPanelRadiosSelected))
    IDBacApp::addNewLibrary(samplesToAdd = createNewLibraryTable2(), newDatabase = newDatabase,  selectedIDBacDataFolder = idbacDirectory$filePath)
  })





  createNewLibraryTable2 <- reactive({

    # "Get the sample names from the protein peak files
    currentlyLoadedSamples <- list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists"),full.names = FALSE)[grep(".ProteinPeaks.", list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists")))]
    # Character vector of protein peak sample names
    currentlyLoadedSamples <- as.character(strsplit(currentlyLoadedSamples,"_ProteinPeaks.rds"))
    # Check for mzXML files
    mzXMLfiles <- list.files(paste0(idbacDirectory$filePath, "\\Converted_To_mzXML"), full.names = FALSE)
    mzXMLfiles <- unlist(strsplit(mzXMLfiles, ".mzXML"))
    nonMissingmzXML <- which(currentlyLoadedSamples %in% mzXMLfiles)
    missingmzXML <- which(! currentlyLoadedSamples %in% mzXMLfiles)
    currentlyLoadedSamples <- currentlyLoadedSamples[nonMissingmzXML]
    # Create the data frame structure for the "database"
    currentlyLoadedSamples <- data.frame("Strain_ID" = currentlyLoadedSamples,
                                         "Genbank_Accession" = "",
                                         "Kingdom" = "",
                                         "Phylum"= "",
                                         "Class" = "",
                                         "Order" = "",
                                         "Family" = "",
                                         "Genus" = "",
                                         "Species" = "",
                                         "Strain" = "")
    # If interactive table exists, show it, otherwise use "currentlyLoadedSamples" created above
    if (!is.null(input$hott)) {
      rhandsontable::hot_to_r(input$hott)
    } else {
      currentlyLoadedSamples
    }

  })

  # Display the new Library as an editable table
  output$hott <- rhandsontable::renderRHandsontable({
    DF <- createNewLibraryTable2()
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






  #------------------------------------------------------------------------------------------------------------

  #-------------------------------------- Library Search


  # Load library search function:
  #source("librarySearch.r")




  librarySearchResults <- reactive({
    input$libraryInjection
    input$initateInjection
    IDBacApp::databaseSearch(idbacPath = idbacDirectory$filePath,
                   databasePath = input$libraryInjection,
                   wantReport = FALSE)


  })





  libSearchResultIDsForDendro <-  reactive({
    a <- input$librarySearch


   ffw <<- do.call(rbind.data.frame,unlist(librarySearchResults(), recursive = FALSE))



       # only keep top hits and no duplicate library IDs
       librarySearchResults <- ffw %>%
         filter(Score < 30) %>%
         distinct(Lib_ID, Unknown, .keep_all = TRUE)


      # only keep top hits and no duplicate library IDs
#      librarySearchResults <- librarySearchResults()

            # return vector of Library IDs to Injecct
      as.vector(unlist(librarySearchResults[,1]))

  })






  #---------------------------------------
  # Library Injection



  output$libraryInjectionLibrarySelect  <- renderUI({
    if(input$HierarchicalSidebarTabs == "hierLibrarySearch"){
      checkboxGroupInput(inputId = "libraryInjection",
                         label= "Select Library to Inject",
                         choiceNames = basename(libraries()),
                         choiceValues = as.list(libraries())
      )
    }

  })






  output$libraryMetadataColumnsSelection  <- renderUI({
    if(input$HierarchicalSidebarTabs == "hierLibrarySearch"){


      checkboxGroupInput(inputId = "libraryMetadataColumns",
                         label= "Select Library Columns",
                         choiceNames =  DBI::dbConnect(RSQLite::SQLite(), input$libraryInjection) %>%
                           dplyr::tbl(., "IDBacDatabase") %>%
                           select(-c("Strain_ID",
                                     "manufacturer",
                                     "model",
                                     "ionisation",
                                     "analyzer",
                                     "detector",
                                     "Protein_Replicates",
                                     "Small_Molecule_Replicates",
                                     "mzXML",
                                     "proteinPeaksRDS",
                                     "proteinSummedSpectrumRDS",
                                     "smallMoleculePeaksRDS",
                                     "mzXMLhash",
                                     "proteinPeaksRDShash",
                                     "proteinSummedSpectrumRDShash",
                                     "smallMoleculePeaksRDShash")) %>%  colnames(),
                         choiceValues = as.list( DBI::dbConnect(RSQLite::SQLite(), input$libraryInjection) %>%
                                                   dplyr::tbl(., "IDBacDatabase") %>%
                                                   select(-c("Strain_ID",
                                                             "manufacturer",
                                                             "model",
                                                             "ionisation",
                                                             "analyzer",
                                                             "detector",
                                                             "Protein_Replicates",
                                                             "Small_Molecule_Replicates",
                                                             "mzXML",
                                                             "proteinPeaksRDS",
                                                             "smallMoleculePeaksRDS",
                                                             "proteinSummedSpectrumRDS",
                                                             "mzXMLhash",
                                                             "proteinPeaksRDShash",
                                                             "proteinSummedSpectrumRDShash",
                                                             "smallMoleculePeaksRDShash")) %>%  colnames())
      )
    }

  })















  #--------------------------------------



  #  The following code is necessary to stop the R backend when the user closes the browser window
  #   session$onSessionEnded(function() {
  #      stopApp()
  #      q("no")
  #    })



  }
