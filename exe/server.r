# The server portion of the Shiny app serves as the backend, performing data processing and creating the visualizations to be displayed as specified in the U function(input, output,session) {




# Reactive variable returning the user-chosen working directory as string
function(input,output,session){
  
  
  
  
  functionA <- function(z,idbacDir) {
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
    Required_Packages = c("MALDIquant", "MALDIquantForeign", "mzR", "readxl")
    
    
    # Install and Load Packages
    Install_And_Load(Required_Packages)
    
    
    
    strReverse <- function(x) {
      sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
    }
    
    if ( length(mzR::header(mzR::openMSfile(file = z))$seqNum) > 1) {
      spectraImport <- sapply(z, function(x)mzR::peaks(mzR::openMSfile(file = x)))
      spectraList <- lapply(z, function(x)(mzR::openMSfile(file = x)))
      names <- strReverse(unlist(lapply(strReverse(sapply(spectraList, fileName)), function(x)strsplit(x, "/")[[1]][1])))[[1]]
      spectraImport <- lapply(1:length(spectraImport), function(x)createMassSpectrum(mass = spectraImport[[x]][, 1],intensity = spectraImport[[x]][, 2],metaData = list(File = names)))
    } else{
      spectraImport <- lapply(z, function(x)mzR::peaks(mzR::openMSfile(file = x)))
      spectraList <- lapply(z, function(x)(mzR::openMSfile(file = x)))
      names <- strReverse(unlist(lapply(strReverse(sapply(spectraList, fileName)), function(x)strsplit(x, "/")[[1]][1])))[[1]]
      spectraImport <- createMassSpectrum(mass = spectraImport[[1]][, 1],intensity = spectraImport[[1]][, 2],metaData = list(File = names))
      spectraImport<- list(spectraImport)
    }
    
    sampleNames <- strsplit(names, ".mzXML")[[1]][1]
    for (i in 1:length(spectraImport)) {
      spectraImport[[i]]@metaData$Strain <- sampleNames
    }
    labs <- sapply(spectraImport, function(x) metaData(x)$Strain)[[1]]
    proteinSpectra <- spectraImport[which(sapply(spectraImport, function(x)max(mass(x))) > 10000)]
    smallSpectra <- spectraImport[which(!sapply(spectraImport, function(x) max(mass(x))) > 10000)]
    if(length(proteinSpectra) > 0){
      averaged <- averageMassSpectra(proteinSpectra, method = "mean")
      saveRDS(averaged, paste0(idbacDir,"/Peak_Lists/", averaged@metaData$Strain[[1]], "_", "SummedProteinSpectra.rds"))
      remove(averaged)
      gc()
      # Why square root transformation and not log:
      #  Anal Bioanal Chem. 2011 Jul; 401(1): 167–181.
      # Published online 2011 Apr 12. doi:  10.1007/s00216-011-4929-z
      #"Especially for multivariate treatment of MALDI imaging data, the square root transformation can be considered for the data preparation
      #because for all three intensity groups in Table 1, the variance is approximately constant."
      
      proteinSpectra <- transformIntensity(proteinSpectra, method = "sqrt")
      proteinSpectra <- smoothIntensity(proteinSpectra, method = "SavitzkyGolay", halfWindowSize = 20)
      proteinSpectra <- removeBaseline(proteinSpectra, method = "TopHat")
      proteinSpectra <- detectPeaks(proteinSpectra, method = "MAD", halfWindowSize = 20, SNR = 4)
      saveRDS(proteinSpectra, paste0(idbacDir, "/Peak_Lists/", labs, "_", "ProteinPeaks.rds"))
    }
    
    if(length(smallSpectra) > 0){
      ############
      #Spectra Preprocessing, Peak Picking
      averaged <- averageMassSpectra(smallSpectra, method = "mean")
      saveRDS(averaged, paste0(idbacDir,"/Peak_Lists/", averaged@metaData$Strain[[1]], "_", "SummedSmallMoleculeSpectra.rds"))
      remove(averaged)
      gc()
      smallSpectra <- smoothIntensity(smallSpectra, method = "SavitzkyGolay", halfWindowSize = 20)
      smallSpectra <- removeBaseline(smallSpectra, method = "TopHat")
      #Find all peaks with SNR >1, this will allow us to filter by SNR later, doesn't effect the peak-picking algorithm, just makes files bigger
      smallSpectra <- detectPeaks(smallSpectra, method = "SuperSmoother", halfWindowSize = 20, SNR = 1)
      saveRDS(smallSpectra, paste0(idbacDir, "/Peak_Lists/", labs, "_", "SmallMoleculePeaks.rds"))
      
    }
  }
  
  # "appLocation" will be assigned to the directory of where IDBac was installed to.  This will be used later in order to access MSConvert
  # which is used for converting raw MALDI files to mzML via command line.
  #appLocation <- getwd()
  #####################################################################################################################################################
  
  ################################################
  #Cosine Distance Matrix Function
  cosineD <- function(x) {
    as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
  }
  ################################################
  
  # This function revereses a provided string
  strReverse <- function(x) {
    sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
  }
  
  
  
  output$value <- renderPrint({ input$radio })
  
  #This "observe" event creates the UI element for analyzing a single MALDI plate, based on user-input.
  
  observe({
    
    if (is.null(input$rawORreanalyze)){}else if (input$rawORreanalyze == 1){
      output$ui1<-renderUI({
        fluidRow(
          column(12,
                 br(),
                 br(),
                 fluidRow(
                   column(12,offset=3,
                          h3("Starting with a Single MALDI Plate of Raw Data"))),br(),br(),
                 column(5,
                        fluidRow(column(5,offset=3,h3("Instructions"))),
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
                        div(img(src="window.png",style="width:750px;height:40px"))
                        ),
                 column(1
                 ),
                 column(5,style = "background-color:#F5F5F5",
                        fluidRow(column(5,offset=3,h3("Workflow Pane"))),
                        br(),
                        p(strong("1: Working Directory")),
                        actionButton("selectedWorkingDirectory", label = "Click to select your Working Directory"),
                        fluidRow(column(12, verbatimTextOutput("selectedWorkingDirectoryText", placeholder = TRUE))),
                        br(),
                        p(strong("2: Raw Data")),
                        actionButton("rawFileDirectory", label = "Click to select the location of your RAW data"),
                        fluidRow(column(12, verbatimTextOutput("rawFileDirectory", placeholder = TRUE))),
                        br(),
                        p(strong("3:"), "Choose  your Sample Map file, the excel sheet that IDBac will use to rename your files."),
                        fileInput('excelFile', label = NULL , accept =c('.xlsx','.xls')),
                        p(strong("4:"),"Click \"Convert to mzXML\" to begin spectra conversion."),
                        actionButton("run", label = "Convert to mzXML"),
                        br(),
                        br(),
                        br(),
                        br(),
                        p(strong("Note:"),"If you canceled out of the popup after spectra conversion completed, you can process your converted spectra using the button below: (but only after all files have been converted) This step is not necessary otherwise."),
                        actionButton("mbeginPeakProcessing", label = "Process mzXML spectra")
                        
                 )
          )
        )
      })
    }
  })
  
  
  
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
                          h3("Starting with Multiple MALDI Plates of Raw Data"))),br(),br(),
                 column(5,
                        fluidRow(column(5,offset=3,h3("Instructions"))),
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
                        img(src="window.png",style="width:750px;height:40px")
                        ),
                 column(1
                 ),
                 column(5,style = "background-color:#F5F5F5",
                        fluidRow(column(5,offset=3,h3("Workflow Pane"))),
                        br(),
                        p(strong("1:"), " Your Working Directory is where files will be created."),
                        actionButton("selectedWorkingDirectory", label = "Click to select your Working Directory"),
                        fluidRow(column(12, verbatimTextOutput("selectedWorkingDirectoryText", placeholder = TRUE))),
                        br(),
                        p(strong("2:"), "Your RAW data will be one folder that contains folders for each MALDI plate."),
                        actionButton("multipleMaldiRawFileDirectory", label = "Click to select the location of your RAW data"),
                        fluidRow(column(12, verbatimTextOutput("multipleMaldiRawFileDirectory", placeholder = TRUE))),
                        br(),
                        actionButton("run", label = "Convert to mzXML"),
                        actionButton("mbeginPeakProcessing", label = "Process mzXML")
                 )
                        )
        )
      })
    }
  })
  
  
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
                          h3("Re-Analyze Data That You Already Converted with IDBac"))),br(),br(),
                 column(12,
                        br(),
                        column(5,
                               fluidRow(column(5,offset=3,h3("Instructions"))),
                               br(),
                               p("Left-click the button to the right to select your previously-analyzed data."),
                               p("This will be the folder, originally named \"IDBac\", that was created when you analyzed data the first time."),
                               p("It contains the folders:"),
                               tags$ul(
                                 tags$li("Converted_To_mzML"),
                                 tags$li("Peak_Lists"),
                                 tags$li("Saved_MANs")
                               ),
                               br(),
                               tags$b("Example:"),br(),
                               img(src="WorkingDirectory_ReAnalysis.png", style="width:322px;height:164px"),
                               br(),br(),
                               p("Note: Sometimes the browser window won't pop up, but will still appear in the application bar. See below:"),
                               div(img(src="window.png",style="width:750px;height:40px"))
                        ),
                        column(1
                        ),
                        column(5,style = "background-color:#F5F5F5",
                               fluidRow(column(5,offset=3,h3("Workflow Pane"))),
                               p(strong("1:"), "Select the folder containing your data"),
                               actionButton("idbacDirectoryButton", label = "Click to select the data directory"),
                               fluidRow(column(
                                 12,
                                 verbatimTextOutput("idbacDirectoryOut", placeholder = TRUE))),
                               br(),
                               p(strong("2:"), "You can now reanalyze your data by proceeding through the tabs at the top of the page. (\"Inverse Peak Comparison\", etc)      ")
                        )
                 )
                 
          )
        )
      })
    }
  })
  
  
  
  
  observe({
    
    if (is.null(input$rawORreanalyze)){}else if (input$rawORreanalyze == 4){
      output$ui1<-renderUI({
        fluidRow(
          column(12,
                 br(),
                 br(),
                 fluidRow(
                   column(12,offset=3,
                          h3("Customizing which samples to analyze"))),br(),br(),
                 column(5,
                        fluidRow(column(5,offset=3,h3("Instructions"))),
                        br(),
                        p("Begin by creating a blank working directory.")
                 ),
                 column(1
                 ),
                 column(5,style = "background-color:#F5F5F5",
                        fluidRow(column(5,offset=3,h3("Workflow Pane"))),
                        br(), 
                        p(strong("1: "), actionButton("selectedWorkingDirectory", label = "Click to select where to create a working directory")),
                        p("Selected Location:"),
                        fluidRow(column(12, verbatimTextOutput("selectedWorkingDirectoryText", placeholder = TRUE))),
                        
                        br(),
                        p(strong("2: "), actionButton("createBlankSelectedWorkingDirectoryFolders", label = "Click to create a blank working directory")),   
                        br(),
                        p(strong("3:"), "Place the mzXML files that you wish to analyze into:"),
                        p(verbatimTextOutput("whereConvert")),
                        p(strong("4:"), "Select \"Process mzXML\" to process mzXML files for analysis"),
                        actionButton("mbeginPeakProcessing", label = "Process mzXML spectra")
                        
                 )
          )
        )
      })
    }
  })
  
  
  
  
  
  
  
  
  
  # Lets the user choose the directory which the IDBac working directory will be located in 
  selectedDirectory <- reactive({
    if(is.null(input$selectedWorkingDirectory)){
      return("No Folder Selected")
    }else if (input$selectedWorkingDirectory > 0){
      choose.dir()
    }
  })
  # Shows the user which directory they chose for the IDBac working directory   
  output$selectedWorkingDirectoryText <- renderPrint(selectedDirectory())
  
  
  
  # Find if "IDBac" exists in selected folder and then uniquify if necessary
  uniquifiedIDBac<-reactive({
    
    
    req(selectedDirectory())
    uniquifiedIDBacs<-list.dirs(selectedDirectory(),recursive = F,full.names = F)
    uniquifiedIDBacs<-make.unique(c(uniquifiedIDBacs,"IDBac"),sep="-")
    last(uniquifiedIDBacs)
  })
  
  
  # Creates the IDBac Directory structure, Uniquifies the  "IDBac" folder according to what folders are present in the selected directory
  idbacuniquedir <-reactive({
    if (input$createBlankSelectedWorkingDirectoryFolders>0 ) {
      
      dir.create(paste0(selectedDirectory(), "\\",uniquifiedIDBac()))
      dir.create(paste0(selectedDirectory(), "\\",uniquifiedIDBac(),"\\Converted_To_mzML"))
      dir.create(paste0(selectedDirectory(), "\\",uniquifiedIDBac(),"\\Sample_Spreadsheet_Map"))
      dir.create(paste0(selectedDirectory(), "\\",uniquifiedIDBac(),"\\Peak_Lists"))
      dir.create(paste0(selectedDirectory(), "\\",uniquifiedIDBac(),"\\Saved_MANs"))
      
      return(paste0(selectedDirectory(), "\\",uniquifiedIDBac()))
    }
    
  })
  
  
  # Shows the user where the created, uniquified, IDBac folder was created    
  
  observeEvent(input$createBlankSelectedWorkingDirectoryFolders,{
    
    output$whereConvert<-renderText({
      
      paste0(idbacuniquedir(),"\\Converted_To_mzXML")
      
    })
    
  })
  
  
  
  #When ReAnalyzing data, and need to select the "IDBac" folder directly
  pressedidbacDirectoryButton <- reactive({
    if(is.null(input$idbacDirectoryButton)){
      return("No Folder Selected")
    }else if (input$idbacDirectoryButton > 0){
      choose.dir()
    }
  })
  
  
  output$idbacDirectoryOut <- renderPrint(pressedidbacDirectoryButton())
  
  
  
  idbacDirectory<-reactiveValues(filePath = NULL)
  
  
  
  
  #Reactive events to trigger the creation of the "idbacDirectory" reactive variable
  
  observeEvent(input$createBlankSelectedWorkingDirectoryFolders,{
    idbacDirectory$filePath <- idbacuniquedir()
  })
  
  observeEvent(input$selectedWorkingDirectory,{
    idbacDirectory$filePath <- paste0(selectedDirectory(), "/",uniquifiedIDBac())
  })
  
  observeEvent(input$idbacDirectoryButton,{
    idbacDirectory$filePath <- pressedidbacDirectoryButton()
  })
  
  
  
  
  
  
  
  
  
  # This function revereses a provided string
  strReverse <- function(x){
    sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
  }
  
  # Reactive variable returning the user-chosen location of the raw MALDI files as string
  rawFilesLocation <- reactive({
    if (input$rawFileDirectory > 0) {
      choose.dir()
    }
  })
  
  
  # Creates text showing the user which directory they chose for raw files
  output$rawFileDirectory <- renderText(if (is.null(rawFilesLocation())) {
    return("No Folder Selected")
  } else{
    folders <- NULL
    foldersInFolder <-list.dirs(rawFilesLocation(), recursive = FALSE, full.names = FALSE) # Get the folders contained directly within the chosen folder.
    for (i in 1:length(foldersInFolder)) {
      folders <- paste0(folders, "\n", foldersInFolder[[i]]) # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
    }
    folders #output$rawFileDirectory == folders
    
  })
  
  
  
  multipleMaldiRawFileLocation <- reactive({
    if (input$multipleMaldiRawFileDirectory > 0) {
      choose.dir()
    }
  })
  
  # Creates text showing the user which directory they chose for raw files
  output$multipleMaldiRawFileDirectory <- renderText({
    if (is.null(multipleMaldiRawFileLocation())) {
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
  
  
  
  
  # Spectra conversion
  #This observe event waits for the user to select the "run" action button and then creates the folders for storing data and converts the raw data to mzML
  
  spectraConversion<-reactive({
    if(input$rawORreanalyze == 1){
      #When only analyzing one maldi plate this handles finding the raw data directories and the excel map
      #excelMap is a dataframe (it's "Sheet1" of the excel template)
      excelTable <- as.data.frame(read_excel(paste0(input$excelFile$datapath), 2))
      #excelTable takes the sample location and name from excelTable, and also converts the location to the same name-format as Bruker (A1 -> 0-A1)
      excelTable <- cbind.data.frame(paste0("0_", excelTable$Key), excelTable$Value)
      #List the raw data files (for Bruker MALDI files this means pointing to a directory, not an individual file)
      fullZ <- list.dirs(list.dirs(rawFilesLocation(), recursive = FALSE), recursive = FALSE)
      #Get folder name from fullZ for merging with excel table names
      fullZ<-cbind.data.frame(fullZ,unlist(lapply(fullZ,function(x)strsplit(x,"/")[[1]][[3]])))
      colnames(fullZ)<-c("UserInput","ExcelCell")
      colnames(excelTable)<-c("ExcelCell","UserInput")
      #merge to connect filenames in excel sheet to file paths
      fullZ<-merge(excelTable,fullZ,by=c("ExcelCell"))
      fullZ[,3]<-normalizePath(as.character(fullZ[,3]))
    }else if(input$rawORreanalyze == 3){
      
      #When analyzing more han one MALDI plate this handles fiding the raw data directories and the excel map
      mainDirectory<-list.dirs(multipleMaldiRawFileLocation(),recursive = F)
      lapped<-lapply(mainDirectory,function(x)list.files(x,recursive = F,full.names = T))
      collectfullZ<-NULL
      #For annotation, look at the single-plate conversion above, the below is basically the same, but iterates over multiple plates, each plate must reside in its own directory.
      lll<<-lapped
      for (i in 1:length(lapped)){
        excelTable <- as.data.frame(read_excel(lapped[[i]][grep(".xls",lapped[[i]])], 2))
        excelTable <- cbind.data.frame(paste0("0_", excelTable$Key), excelTable$Value)
        fullZ<-list.dirs(lapped[[i]],recursive = F)
        fullZ<-cbind.data.frame(fullZ,unlist(lapply(fullZ,function(x)strsplit(x,"/")[[1]][[4]])))
        colnames(fullZ)<-c("UserInput","ExcelCell")
        colnames(excelTable)<-c("ExcelCell","UserInput")
        fullZ<-merge(excelTable,fullZ,by=c("ExcelCell"))
        fullZ[,3]<-normalizePath(as.character(fullZ[,3]))
        collectfullZ<-c(collectfullZ,list(fullZ))
      }
      
      fullZ<-ldply(collectfullZ,data.frame)    
      
    }
    
    
    
    aaa<<-fullZ
    
    
    
    
    
    fullZ<-dlply(fullZ,.(UserInput.x))
    #return fullz to the "spectraConversion" reactive variable, this is a list of samples; contents of each sample are file paths to the raw data for that samples
    #This will be used by the spectra conversion observe function/event
    fullZ
  })
  
  
  observe({
    
    if (is.null(input$run)){}else if(input$run > 0) {
      
      
      dir.create(paste0(selectedDirectory(), "/",uniquifiedIDBac()))
      dir.create(paste0(selectedDirectory(), "/",uniquifiedIDBac(),"/Converted_To_mzML"))
      dir.create(paste0(selectedDirectory(), "/",uniquifiedIDBac(),"/Sample_Spreadsheet_Map"))
      dir.create(paste0(selectedDirectory(), "/",uniquifiedIDBac(),"/Peak_Lists"))
      dir.create(paste0(selectedDirectory(), "/",uniquifiedIDBac(),"/Saved_MANs"))
      
      fullZ<-spectraConversion()
      
      
      outp <- file.path(paste0(selectedDirectory(), "\\",uniquifiedIDBac(),"\\Converted_To_mzML"))
      
      
      #fullZ$UserInput.x = sample name
      #fullZ$UserInput.y = file locations
      zxcv<<-fullZ
      oooo<<-outp
      
      
      #Command-line MSConvert, converts from proprietary vendor data to open mzXML
      msconvertCmdLineCommands<-lapply(fullZ,function(x){
        #Finds the msconvert.exe program which is located the in pwiz folder which is two folders up ("..\\..\\") from the directory in which the IDBac shiny app initiates from
        paste0("pwiz\\msconvert.exe",
               #sets up the command to pass to MSConvert in CMD, with variables for the input files (x$UserInput.y) and for where the newly created mzXML files will be saved
               " ",
               paste0(x$UserInput.y,collapse = "",sep=" "),
               "--noindex --mzXML --merge -z",
               " -o ",
               outp,
               " --outfile ",
               paste0(x$UserInput.x[1],".mzXML")
        )
      }
      )
      functionTOrunMSCONVERTonCMDline<-function(x){
        system(command =
                 "cmd.exe",
               input = as.character(x))
      }
      
      
      
      popup1()
      #sapply(msconvertCmdLineCommands,functionTOrunMSCONVERTonCMDline)  #No parallel processing
      numCores <- detectCores()
      numCores <- makeCluster(numCores-1)
      parSapply(numCores,msconvertCmdLineCommands,functionTOrunMSCONVERTonCMDline)
      stopCluster(numCores)
      popup2()
      
      
    }
  })
  
  
  popup1<-reactive({
    
    
    showModal(modalDialog(
      title = "Important message",
      "When file-conversions are complete this pop-up will be replaced by a summary of the conversion.",br(),
      "IDBac uses parallel processing to make these computations faster, unfortunately this means we can't show a progress bar.",br(),
      "This also means your computer might be slow during these file conversions.",br(),
      "To check what has been converted, you can navigate to:",
      paste0(selectedDirectory(), "/",uniquifiedIDBac(),"/Converted_To_mzML"),
      easyClose = FALSE, size="l",footer=""
    ))
  })
  
  
  popup2<-reactive({
    showModal(modalDialog(
      title = "Conversion Complete",
      
      paste0(nrow(ldply(spectraConversion()))," files were converted into ",length(list.files(paste0(selectedDirectory(), "/",uniquifiedIDBac(),"/Converted_To_mzML"))),
             " open data format files."),br(),
      "To check what has been converted you can navigate to:",
      
      paste0(selectedDirectory(), "/",uniquifiedIDBac(),"/Converted_To_mzML"),
      easyClose = TRUE,
      footer = tagList(actionButton("beginPeakProcessing","Click to continue with Peak Processing"), modalButton("Close"))
    ))
  })
  
  
  observeEvent(input$beginPeakProcessing, {
    removeModal()
  })
  
  
  
  
  
  
  
  
  
  # Spectra processing
  
  observeEvent(input$beginPeakProcessing,{
    if (is.null(input$beginPeakProcessing)){}else if(input$beginPeakProcessing > 0) {
      fileList <- list.files(list.dirs(paste0(idbacDirectory$filePath,"/Converted_To_mzML")),pattern = ".mzXML", full.names = TRUE)
      popup3()
      
      #   numCores <- detectCores()
      #   cl <- makeCluster(numCores)
      #   parSapply(cl,fileList,functionA)
      #   stopCluster(cl)
      
      #Single process with sapply instead of parsapply
      sapply(fileList,function(x)functionA(x,idbacDirectory$filePath))
      popup4()
    }
    
  })
  # Spectra processing
  
  observeEvent(input$mbeginPeakProcessing,{
    if (is.null(input$mbeginPeakProcessing) ){}else if(input$mbeginPeakProcessing > 0) {
      fileList <-list.files(list.dirs(paste0(idbacDirectory$filePath,"/Converted_To_mzML")),pattern = ".mzXML", full.names = TRUE)
      popup3()
      
      #      numCores <- detectCores()
      #     cl <- makeCluster(numCores)
      #    parSapply(cl,fileList,function(x)functionA(x,idbacDirectory$filePath))
      #   stopCluster(cl)
      
      #Single process with sapply instead of parsapply
      sapply(fileList,function(x)functionA(x,idbacDirectory$filePath))
      popup4()
    }
    
  })
  
  
  popup3<-reactive({
    showModal(modalDialog(
      title = "Important message",
      "When spectra processing is complete you will be able to begin with the data analysis",br(),
      "IDBac uses parallel processing to make these computations faster, unfortunately this means we can't show a progress bar.",br(),
      "This also means your computer might be slow during the computations.",br(),
      "The step allows for fast interaction during the various data analysis",
      "To check the progress, you can navigate to the following directory, where four files will be created per sample ",
      paste0(selectedDirectory(), "/",uniquifiedIDBac(),"/Peak_Lists"), 
      
      
      easyClose = FALSE, size="l",footer=""
    ))
  })
  
  
  popup4<-reactive({
    showModal(modalDialog(
      title = "Spectra Processing is Now Complete",
      br(),
      easyClose = TRUE,
      footer = modalButton("Continue to Data Analysis by visiting consecutive tabs at the top of the page.")))
  })
  
  
  spectra <- reactive({
    unlist(sapply(list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists"),full.names=TRUE)[grep(".SummedProteinSpectra.", list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists")))], readRDS))
  })
  
  
  trimmedP <- reactive({
    all <-unlist(sapply(list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists"),full.names = TRUE)[grep(".ProteinPeaks.", list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists")))], readRDS))
    all<-binPeaks(all, tolerance =.02)
    
    validate(
      need(try(trim(all, c(input$lowerMass, input$upperMass))),"The hierarchical clustering and PCA analyses require you to first visit the \"Compare Two Samples (Protein)\"
           tab at the top of the page.")
      )
    trim(all, c(input$lowerMass, input$upperMass))
    
    
    
    
  })
  
  
  collapsedPeaksP <- reactive({
    labs <- sapply(trimmedP(), function(x)metaData(x)$Strain)
    labs <- factor(labs)
    new2 <- NULL
    newPeaks <- NULL
    for (i in seq_along(levels(labs))) {
      specSubset <- (which(labs == levels(labs)[[i]]))
      if (length(specSubset) > 1) {
        new <- filterPeaks(trimmedP()[specSubset],minFrequency=input$percentPresenceP/100)
        new<-mergeMassPeaks(new,method="mean")
        new2 <- c(new2, new)
      } else{
        new2 <- c(new2, trimmedP()[specSubset])
      }
      
    }
    
    
    new2   #collapsedPeaksP() == new2
    
    
  })
  
  
  proteinMatrix <- reactive({
    temp <- NULL
    for (i in 1:length(collapsedPeaksP())) {
      temp <- c(temp, collapsedPeaksP()[[i]]@metaData$Strain)
    }
    proteinSamples <- factor(temp)
    proteinMatrixInnard <- intensityMatrix(collapsedPeaksP())
    rownames(proteinMatrixInnard) <- paste(proteinSamples)
    proteinMatrixInnard[is.na(proteinMatrixInnard)] <- 0
    
    if (input$booled == "1") {
      ifelse(proteinMatrixInnard > 0, 1, 0)
    }
    else{
      proteinMatrixInnard
    }
    
  })
  
  
  
  ################################################
  #This creates the Inverse Peak Comparison plot that compares two user-selected spectra() and the calculation required for such.
  
  
  listOfDataframesForInversePeakComparisonPlot <- reactive({
    
    #Selects the peaks to plot based on user-input
    peaksSampleOne<-collapsedPeaksP()[[grep(paste0(input$Spectra1,"$"),sapply(seq(1,length(collapsedPeaksP()),by=1),function(x)metaData(collapsedPeaksP()[[x]])$Strain))]]
    peaksSampleTwo<-collapsedPeaksP()[[grep(paste0(input$Spectra2,"$"),sapply(seq(1,length(collapsedPeaksP()),by=1),function(x)metaData(collapsedPeaksP()[[x]])$Strain))]]
    
    
    #pSNR= the User-Selected Signal to Noise Ratio for protein
    
    #Create a MALDIquant massObject from the selected peaks
    peaksSampleOne@mass<-peaksSampleOne@mass[which(peaksSampleOne@snr>input$pSNR)]
    peaksSampleOne@intensity<-peaksSampleOne@intensity[which(peaksSampleOne@snr>input$pSNR)]
    peaksSampleOne@snr<-peaksSampleOne@snr[which(peaksSampleOne@snr>input$pSNR)]
    peaksSampleTwo@mass<-peaksSampleTwo@mass[which(peaksSampleTwo@snr>input$pSNR)]
    peaksSampleTwo@intensity<-peaksSampleTwo@intensity[which(peaksSampleTwo@snr>input$pSNR)]
    peaksSampleTwo@snr<-peaksSampleTwo@snr[which(peaksSampleTwo@snr>input$pSNR)]
    
    #Selects the spectra to plot based on user-input
    meanSpectrumSampleOne<-spectra()[[grep(paste0(input$Spectra1,"$"),sapply(seq(1,length(spectra()),by=1),function(x)metaData(spectra()[[x]])$Strain))]]
    meanSpectrumSampleTwo<-spectra()[[grep(paste0(input$Spectra2,"$"),sapply(seq(1,length(spectra()),by=1),function(x)metaData(spectra()[[x]])$Strain))]]
    
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
  
  #Used in the the inverse-peak plot for zooming
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  
  output$inversePeakComparisonPlot <- renderPlot({
    
    temp<- listOfDataframesForInversePeakComparisonPlot()
    meanSpectrumSampleOne <-temp$meanSpectrumSampleOne
    meanSpectrumSampleTwo <-temp$meanSpectrumSampleTwo
    p1b <-temp$p1b
    p2b <-temp$p2b
    p3b <-temp$p3b
    p4b <-temp$p4b
    
    remove(temp)
    
    #Create peak plots and color each peak according to whether it occurs in the other spectrum
    plot(meanSpectrumSampleOne@mass,meanSpectrumSampleOne@intensity,ylim=c(-max(meanSpectrumSampleTwo@intensity),max(meanSpectrumSampleOne@intensity)),type="l",col=adjustcolor("Black", alpha=0.3),xlab="m/z",ylab="Intensity")
    lines(meanSpectrumSampleTwo@mass,-meanSpectrumSampleTwo@intensity)
    rect(xleft=p3b$Mass-.5, ybottom=0, xright=p3b$Mass+.5, ytop=((p3b$Intensity)*max(meanSpectrumSampleOne@intensity)/max(p3b$Intensity)),border=p3b$Color)
    rect(xleft=p4b$Mass-.5, ybottom=0, xright=p4b$Mass+.5, ytop=-((p4b$Intensity)*max(meanSpectrumSampleTwo@intensity)/max(p4b$Intensity)),border=p4b$Color)
    
    observe({
      brush <- input$plot2_brush
      if (!is.null(brush)) {
        ranges2$x <- c(brush$xmin, brush$xmax)
        ranges2$y <- c(brush$ymin, brush$ymax)
      }else{
        ranges2$x <- NULL
        ranges2$y <- c(-max(meanSpectrumSampleTwo@intensity),max(meanSpectrumSampleOne@intensity))
      }
    })
  })
  
  
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
  
  
  
  # Create peak comparison ui
  output$inversepeakui <-  renderUI({
    sidebarLayout(
      sidebarPanel(
        selectInput("Spectra1", label=h5("Spectrum 1 (up; matches to bottom spectrum are blue, non-matches are red)"),
                    choices = sapply(seq(1,length(spectra()),by=1),function(x)metaData(spectra()[[x]])$Strain)),
        selectInput("Spectra2", label=h5("Spectrum 2 (down)"),
                    choices = sapply(seq(1,length(spectra()),by=1),function(x)metaData(spectra()[[x]])$Strain)),
        numericInput("percentPresenceP", label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%) (Experiment/Hypothesis dependent)"),value = 70,step=10,min=70,max=70),
        numericInput("pSNR", label = h5("Signal To Noise Cutoff"),value = 4,step=.5,min=1.5,max=100),
        numericInput("lowerMass", label = h5("Lower Mass Cutoff"),value = 3000,step=50),
        numericInput("upperMass", label = h5("Upper Mass Cutoff"),value = 15000,step=50),
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
  
  
  ################################################
  # This creates the Plotly PCA plot and the calculation required for such.
  output$pcaplot <- renderPlotly({
    c <- PCA(proteinMatrix(),graph=FALSE)
    a <- c$ind$coord
    a <- as.data.frame(a)
    nam <- rownames(a)
    a <- cbind(a,nam)
    if(input$kORheight=="2"){
      d <- cutree(dendro(),h=input$height)
    }else{
      d <- cutree(dendro(),k=input$kClusters)
    }
    e <- as.data.frame(cbind(a,d))
    if(input$PCA3d==1){
      plot3d(x=e$Dim.1,y=e$Dim.2,z=e$Dim.3,xlab="", ylab="", zlab="")
      text3d(x=e$Dim.1,y=e$Dim.2,z=e$Dim.3,text=e$nam,col=factor(d))}
    p <- (ggplot(e,aes(Dim.1,Dim.2,label=nam)))
    output$pcaplot2<-renderPlot(p+geom_text(aes(col=factor(d)),size=6)+xlab("Dimension 1")+ylab("Dimension 2")+ggtitle("PCA of Protein MALDI Spectra (colors based on clusters from hiearchical clustering)")+theme(plot.title=element_text(size=15)) + scale_color_discrete(name="Clusters from hierarchical clustering")+theme(legend.position="none"))
    ggplotly(p + geom_text()+xlab("Dimension 1")+ylab("Dimension 2")+ ggtitle("Zoomable PCA of Protein MALDI Spectra")+theme(plot.title=element_text(size=15),legend.position="none"))
  })
  #  },height=750)
  
  
  # Create PCA ui
  output$PCAui <-  renderUI({
    sidebarLayout	(
      sidebarPanel(
        radioButtons("PCA3d", label = h4("PCA 3D Plot"),
                     choices = list("Show" = 1, "Don't Show" = 2),selected = 2)
      ),
      mainPanel(plotOutput("pcaplot2"),
                h5("Same PCA as above, no colors, but with interaction"),
                plotlyOutput("pcaplot"))
    )
    
  })
  
  
  
  
  
  ################################################
  #Create the hierarchical clustering based upon the user input for distance method and clustering technique
  dendro <- reactive({
    
    ret<-if(req(input$distance)=="cosineD"){
      a<-as.function(get(input$distance))
      
      
      
      
      dend <- proteinMatrix() %>% a
      
      
      dend[which(is.na(dend))]<-1  
      
      dend %>% hclust(method=input$clustering) %>% as.dendrogram
      
      
      
    }
    else{
      dend <- proteinMatrix() %>% dist(method=input$distance) %>% hclust(method=input$clustering) %>% as.dendrogram
    }
    
    
    
    ret
  })
  
  
  #User input changes the height of the main hierarchical clustering plot
  plotHeight <- reactive({
    return(as.numeric(input$hclustHeight))
  })
  
  
  
  
  
  
  
  output$hclustui <-  renderUI({
    if(input$kORheight!="2"){return(NULL)}else{
      numericInput("height", label = h5("Cut Tree at Height"),value = .5,step=.1,min=0)}
  })
  
  
  output$groupui <-  renderUI({
    if(input$kORheight=="1"){
      numericInput("kClusters", label = h5("Number of Groups"),value = 1,step=1,min=1)}
  })
  
  
  
  sampleFactorMapColumns<-reactive({
    sampleMappings<-as.data.frame(read_excel(input$sampleMap$datapath,1))
    sampleMappings<-colnames(sampleMappings)
    
  })
  
  
  output$sampleMapColumns1<-renderUI({
    selectInput("sampleFactorMapChosenIDColumn", label = h5("Select a Group Mapping"),
                choices = as.list(sampleFactorMapColumns()))
  })
  
  
  
  output$sampleMapColumns2<-renderUI({
    
    selectInput("sampleFactorMapChosenAttribute", label = h5("Select a Group Mapping"),
                choices = as.list(sampleFactorMapColumns()[!grepl(input$sampleFactorMapChosenIDColumn,sampleFactorMapColumns(),ignore.case = TRUE)]))
  })
  
  
  
  levs<-reactive({
    sampleMappings<-as.data.frame(read_excel(input$sampleMap$datapath,1))
    #selected column
    sampleMappings<-factor(sampleMappings[[input$sampleFactorMapChosenAttribute]])
    levs<-levels(sampleMappings)
  })
  
  
  output$sampleFactorMapColors<-renderUI({
    column(3,
           lapply(1:length(levs()),function(x){
             do.call(colourInput,list(paste0("factor-",x,"_",levs()[[x]]),levs()[[x]],value="blue"))
           })
    )
  })
  
  
  
  ################################################
  #Create the hierarchical clustering plot as well as the calculations needed for such.
  output$hclustPlot <- renderPlot({
    
    
    if (input$kORheight =="3"){
      sampleMappings<-as.data.frame(read_excel(input$sampleMap$datapath,1))
      sampleFactors<-levels(factor(sampleMappings[[input$sampleFactorMapChosenAttribute]]))
      
      
      
      sampleIDs1<-cbind.data.frame(sampleMappings[[input$sampleFactorMapChosenIDColumn]],sampleMappings[[input$sampleFactorMapChosenAttribute]])
      
      
      
      #get colors chosen
      colorsChosen<- sapply(1:length(sampleFactors),function(x)input[[paste0("factor-",x,"_",sampleFactors[[x]])]])
      
      
      zz<-cbind.data.frame(colorsChosen,sampleFactors)
      zz$sampleFactors<-as.vector(zz$sampleFactors)
      zz$asew<-as.vector(zz$colorsChosen)
      colnames(zz)<-c("colors","chosenFactor")
      colnames(sampleIDs1)<-c(input$sampleFactorMapChosenIDColumn,"chosenFactor")
      sampleIDs1$chosenFactor<- as.character(sampleIDs1$chosenFactor)
      zz$chosenFactor<- as.character(zz$chosenFactor)
      
      matchedColors<-merge(zz,sampleIDs1)
      
      # fcol<-grep(paste0(as.character(unlist(matchedColors["ID"])),collapse="|"), labels(dendro()),ignore.case=TRUE)
      fcol<-match(as.character(unlist(matchedColors[input$sampleFactorMapChosenIDColumn])),labels(dendro()))
      fcol2<-c(as.character(unlist(matchedColors["colors"])))
      aaa<-rep("#000000",length(labels(dendro())))
      aaa[fcol]<-fcol2
      
      dendro() %>% color_labels(labels=labels(dendro()),col=aaa) %>% plot(horiz=TRUE,lwd=8)
      #dendro %>% set("labels_cex", c(2,1)) %>% plot
      
      
      #If no sample map is selected, run this:
    }else if (input$kORheight=="2"){
      par(mar=c(5,5,5,10))
      dendro() %>% color_branches(h=input$height) %>% plot(horiz=TRUE,lwd=8)
      abline(v=input$height,lty=2)
      
      
      
    }
    else{
      par(mar=c(5,5,5,10))
      s<-dendro()
      dendro() %>% color_branches(k=input$kClusters)   %>% plot(horiz=TRUE,lwd=8)
    }
  },height=plotHeight)
  
  
  
  # Create Heir ui
  output$Heirarchicalui <-  renderUI({
    fluidPage(
      sidebarPanel(
        #checkboxGroupInput("Library", label=h5("Inject Library Phylum"),
        #                    choices = levels(phyla)),
        selectInput("distance", label = h5("Distance Algorithm"),
                    choices = list("cosine"="cosineD","euclidean"="euclidean","maximum"="maximum","manhattan"="manhattan","canberra"="canberra", "binary"="binary","minkowski"="minkowski"),
                    selected = "euclidean"),
        selectInput("clustering", label = h5("Clustering Algorithm"),
                    choices = list("ward.D"="ward.D","ward.D2"="ward.D2", "single"="single", "complete"="complete", "average (UPGMA)"="average", "mcquitty (WPGMA)"="mcquitty", "median (WPGMC)"="median","centroid (UPGMC)"="centroid"),
                    selected = "ward.D2"),
        radioButtons("booled", label = h5("Include peak intensities, or use presence/absence?"),
                     choices = list("Presence/Absence" = 1, "Intensities" = 2),
                     selected = 2),
        radioButtons("kORheight", label = h5("Color clusters based on:"),
                     choices = list("Specified Number of Groups" = 1, "Height (x-axis value)" = 2, "User-defined Categories in Excel Sheet" = 3),
                     selected = 1),
        uiOutput("hclustui"),
        uiOutput("groupui"),
        numericInput("hclustHeight", label = h5("Expand Tree"),value = 750,step=50,min=100),
        p("To color samples according to user-defined groupings..."),
        p("To use this function, create a different excel file and list all of your sample names in 
          different rows of a single column. In other columns you may add additional characteristics 
          of your samples (eg. media type, genus, sample location), with one characteristic per column. 
          You will have the option to color code your hierarchical clustering plot
          based on these characteristics, which will appear in the drop-down list below."),
        p("Click the blue boxes under a factor (below) to change the color of the factor."),
        fileInput('sampleMap', label = "Sample Mapping" , accept =c('.xlsx','.xls')),
        uiOutput("sampleMapColumns1"),
        uiOutput("sampleMapColumns2"),
        fluidRow(       
          uiOutput("sampleFactorMapColors")),
        br(),
        h4(" Reporting Settings"),
        p("This dendrogram was created by analyzing ",length(labels(a)), " samples,
          and retaining peaks with a signal to noise ratio above ",input$pSNR," and occurring in greater than ",input$percentPresenceP,"% of replicate spectra. 
          Peaks occuring below ",input$lowerMass," m/z or above ",input$upperMass," m/z were removed from the analysis. ",
          "For clustering spectra, ",input$distance, " distance and ",input$clustering, " clustering algorithms were used."
          
          
        )),	
      mainPanel("Hierarchical Clustering",textOutput("Clusters"),plotOutput("hclustPlot"))		
      
      )
  })
  
  
  
  
  
  
  
  
  smallPeaks <- reactive({
    unlist(sapply(list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists"),full.names=TRUE)[grep(".SmallMoleculePeaks.", list.files(paste0(idbacDirectory$filePath, "\\Peak_Lists")))], readRDS))
  })
  
  
  
  
  
  
  ################################################
  #This creates the network plot and calculations needed for such.
  output$metaboliteAssociationNetwork <- renderSimpleNetwork({
    
    
    if(!is.null(input$Spectra1)){
      
      labs <- sapply(smallPeaks(), function(x)metaData(x)$Strain)
      
      if(is.null(input$plot_brush$ymin)){
        #This takes the cluster # selection from the left selection pane and passes that cluster of samples for MAN analysis
        if(input$kORheight=="2"){
          combinedSmallMolPeaks<-smallPeaks()[grep(paste0(c(labels(which(cutree(dendro(),h=input$height)==input$Group)),"Matrix"),collapse="|"), labs,ignore.case=TRUE)]
        }else{
          combinedSmallMolPeaks<-smallPeaks()[grep(paste0(c(labels(which(cutree(dendro(),k=input$kClusters)==input$Group)),"Matrix"),collapse="|"), labs,ignore.case=TRUE)]
        }
      }else{
        #This takes a brush selection over the heirarchical clustering plot within the MAN tab and uses this selection of samples for MAN analysis
        location_of_Heirarchical_Leaves<-get_nodes_xy(dendro())
        minLoc<-input$plot_brush$ymin
        maxLoc<-input$plot_brush$ymax
        threeColTable<-data.frame(location_of_Heirarchical_Leaves[location_of_Heirarchical_Leaves[,2]==0,],labels(dendro()))
        #column 1= y-values of dendrogram leaves
        #column 2= node x-values we selected for only leaves by only returning nodes with x-values of 0
        #column 3= leaf labels
        w<- which(threeColTable[,1] > minLoc & threeColTable[,1] < maxLoc)
        brushed<-as.vector(threeColTable[,3][w])
        labs<-as.vector(sapply(smallPeaks(),function(x)metaData(x)$Strain))
        
        combinedSmallMolPeaks<-smallPeaks()[grep(paste0(c(brushed,"Matrix"),collapse="|"), labs,ignore.case=TRUE)]
      }
      #if(length(grep("Matrix",sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain),ignore.case=TRUE))==0){"No Matrix Blank!!!!!!!"}else{
      #find matrix spectra
      
    }else{
      combinedSmallMolPeaks<-smallPeaks()
      
      
    }  
    
    # input$matrixSamplePresent (Whether there is a matrix sample)  1=Yes   2=No
    if(input$matrixSamplePresent ==1){    
      combinedSmallMolPeaksm<-combinedSmallMolPeaks[grep(paste0("Matrix",collapse="|"),sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain),ignore.case=TRUE)]
      #For now, replicate matrix samples are merged into a consensus peak list.
      
      validate(
        need(try(mergeMassPeaks(combinedSmallMolPeaksm)),"It seems that you don't have a sample containing \"Matrix\" in its name to use for a matrix blank.  Try selecting \"No\" under \"Do you have a matrix blank\" to left, or checking your sample names/data." )
      )
      
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
    
    
    allS<-binPeaks(combinedSmallMolPeaks[which(sapply(combinedSmallMolPeaks,function(x)length(mass(x)))!=0)],tolerance=.002)
    
    trimmedSM <-  trim(allS,c(input$lowerMassSM,input$upperMassSM))
    
    labs <- sapply(trimmedSM, function(x)metaData(x)$Strain)
    labs <- factor(labs)
    new2 <- NULL
    newPeaks <- NULL
    
    for (i in seq_along(levels(labs))) {
      specSubset <- (which(labs == levels(labs)[[i]]))
      if (length(specSubset) > 1) {
        new <- filterPeaks(trimmedSM[specSubset],minFrequency=input$percentPresenceSM/100)
        new<-mergeMassPeaks(new,method="mean")
        new2 <- c(new2, new)
      } else{
        new2 <- c(new2, trimmedSM[specSubset])
      }
      
    }
    
    combinedSmallMolPeaks <- new2
    
    
    
    if(input$matrixSamplePresent ==1){    
      
      #Removing Peaks which share the m/z as peaks that are in the Matrix Blank
      
      ############
      #Find the matrix sample index
      labs <- sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain)
      matrixIndex <- grep(paste0("Matrix",collapse="|"),labs,ignore.case=TRUE)
      ############
      #peaksa = samples
      #peaksb = matrix blank
      peaksa <- combinedSmallMolPeaks[-matrixIndex]
      peaksb <- combinedSmallMolPeaks[[matrixIndex]]
      
      
      
      for (i in 1:length(peaksa)){
        
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
    
    ############
    # Turn Peak-List into a table, add names, change from wide to long, and export as Gephi-compatible edge list
    smallNetwork <- intensityMatrix(peaksa)
    temp <- NULL
    for (i in 1:length(peaksa)){
      temp <- c(temp,peaksa[[i]]@metaData$Strain)
    }
    
    
    peaksaNames <- factor(temp)
    
    rownames(smallNetwork) <- paste(peaksaNames)
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
    bool <- subset(bool, value!=0)
    colnames(bool) <- c("Source","Target","Weight")
    #Round m/z values to a single decimal
    bool$Target <- round(as.numeric(as.matrix(bool$Target)),digits=1)
    if(input$save=="FALSE"){
    }
    else{
      workdir <- idbacDirectory$filePath
      write.csv(as.matrix(bool),paste0(workdir, "\\Saved_MANs\\Current_Network.csv"))
    }
    
    
    a <- as.undirected(graph_from_data_frame(bool))
    a<-simplify(a)
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
  
  
  ################################################
  #This displays the number of clusters created on the hierarchical clustering tab- displays text at top of the clustering page.
  output$Clusters <- renderText({
    
    # Display text of how many clusters were created.
    if (is.null(input$kORheight)){
    }else if (input$kORheight=="2"){
      paste("You have Created ", length(unique(cutree(dendro(),h=input$height)))," Cluster(s)")
    }
    else if (input$kORheight=="1"){
      paste("You have Created ", length(unique(cutree(dendro(),k=input$kClusters)))," Cluster(s)")
    }
  })
  
  
  ################################################
  ##This displays the number of clusters created on the hierarchical clustering tab- displays text at top of the networking page.
  output$Clusters2 <- renderText({
    # Display text of how many clusters were created.
    
    if(!is.null(input$Spectra1)){
      
      if(input$kORheight=="2"){
        paste("You have ", length(unique(cutree(dendro(),h=input$height)))," Cluster(s)")
      }
      else{
        paste("You have ", length(unique(cutree(dendro(),k=input$kClusters)))," Cluster(s)")
      }
      
    }
  })
  
  
  output$info <- renderText({
    
    if(!is.null(input$Spectra1)){
      
      
      xy_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
      }
      xy_range_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
               " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
      }
      paste0(
        "click: ", xy_str(input$plot_click),
        "dblclick: ", xy_str(input$plot_dblclick),
        "hover: ", xy_str(input$plot_hover),
        "brush: ", xy_range_str(input$plot_brush)
      )
      
    }
  })
  
  
  #User input changes the height of the heirarchical clustering plot within the network analysis pane
  plotHeightHeirNetwork <- reactive({
    
    
    return(as.numeric(input$hclustHeightNetwork))
    
    
  })
  
  
  output$netheir <- renderPlot({
    
    
    if(input$kORheight=="2"){
      par(mar=c(5,5,5,10))
      dendro() %>% color_branches(h=input$height) %>% plot(horiz=TRUE,lwd=8)
      abline(v=input$height,lty=2)
    }
    else{
      par(mar=c(5,5,5,10))
      dendro() %>% color_branches(k=input$kClusters)   %>% plot(horiz=TRUE,lwd=8)
    }
    
    
    
  },height=plotHeightHeirNetwork)
  
  
  # Create MAN ui
  output$MANui <-  renderUI({
    sidebarLayout(
      sidebarPanel(
        radioButtons("matrixSamplePresent", label = h5("Do you have a matrix blank?"),
                     choices = list("Yes" = 1, "No (Also Turns Off Matrix Subtraction)" = 2),
                     selected = 1),
        numericInput("percentPresenceSM", label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%) (Experiment/Hyp+othesis dependent)"),value = 70,step=10,min=0,max=100),
        numericInput("smSNR", label = h5("Signal To Noise Cutoff"),value = 4,step=.5,min=1.5,max=100),
        numericInput("Group", label = h5("View Small-Molecule Network for Cluster #:"),value = 1,step=1,min=1),
        numericInput("upperMassSM", label = h5("Upper Mass Cutoff"),value = 2000,step=20,max=round(max(sapply(smallPeaks(),function(x)max(mass(x)))),digits=-1)),
        numericInput("lowerMassSM", label = h5("Lower Mass Cutoff"),value = 200,step=20,min=round(min(sapply(smallPeaks(),function(x)min(mass(x)))),digits=-1)),
        numericInput("hclustHeightNetwork", label = h5("Expand Tree"),value = 750,step=50,min=100),
        checkboxInput("save", label = "Save Current Network?", value = FALSE),
        br(),
        p(strong("Hint 1:"), "Use mouse to select parts of the tree and display the MAN of corresponding samples."),
        p(strong("Hint 2:"), "Use mouse to click & drag parts (nodes) of the MAN if it appears congested."),br(),
        p(strong("Note 1:"), "For publication-quality networks click the box next to \"Save Current Network\",
          while selected- this saves a .csv file of the currently-displayed
          network to the \"Saved_MANs\" folder in your working directory This can be easily imported into Gephi or Cytoscape.
          While checked, any update of the network will overwrite this file. Also, an error saying: \"cannot open the connection\"
          means this box is checked and the file is open in another program, either uncheck or close the file."
          
        )
      ),
      mainPanel(textOutput("Clusters2"),
                simpleNetworkOutput("metaboliteAssociationNetwork"),
                plotOutput("netheir",
                           click = "plot_click",
                           dblclick = "plot_dblclick",
                           hover = "plot_hover",
                           brush = "plot_brush")

                
                
                
      ))
  })
  
  
  
#  The following code is necessary to stop the R backend when the user closes the browser window
  
#   session$onSessionEnded(function() {
#      stopApp()
#      q("no")
#    })
  
  
}