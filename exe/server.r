# The server portion of the Shiny app serves as the backend, performing data processing and creating the visualizations to be displayed as specified in the U function(input, output,session) {

# Reactive variable returning the user-chosen working directory as string
function(input,output,session){


  # "appLocation" will be assigned to the directory of where IDBac was installed to.  This will be used later in order to access MSConvert
  # which is used for converting raw MALDI files to mzML via command line.
  #appLocation <- getwd()
  #####################################################################################################################################################

  ################################################
  #Cosine Distance Matrix Function
  cosineD <- function(x){
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

        column(12,
               br(),
               br(),
               column(5,
                      h3("Instructions:"),
                      br(),
                      p(strong("1:")," This directs where you would like to create an IDBac working directory."),
                      p("In the folder you select- IDBac will create folders within a main directory named \"IDBac\":"),
                      img(src="WorkingDirectory.png", style="width:322px;height:164px"),
                      p("If there is already an \"IDBac\" folder present in the working directory,
                        files will be added into the already-present IDBac folder ",strong("and any samples with the same name will be overwritten.")),
                      br(),
                      p("Note: Sometimes the browser window won't pop up, but will still appear in the application bar. See below:"),
                      div(img(src="window.png",style="width:750px;height:40px"))),
               column(5,style = "background-color:#F5F5F5",
                      h3("Starting with 1 MALDI-Plate of Raw Data"),
                      br(),
                      p(strong("1:"), " Your Working Directory is where files will be created"),
                      actionButton("selectedWorkingDirectory", label = "Click to select your Working Directory"),
                      fluidRow(column(12, verbatimTextOutput("selectedWorkingDirectory", placeholder = TRUE))),
                      br(),
                      p(strong("2:"), "Your RAW data should be one folder that contains: a folder containing protein data and folder containing small-molecule data"),
                      actionButton("rawFileDirectory", label = "Click to select the location of your RAW data"),
                      fluidRow(column(12, verbatimTextOutput("rawFileDirectory", placeholder = TRUE))),
                      br(),
                      p(strong("3:"), "Choose  your Sample Map file, the excel sheet which IDBac will use to rename your files."),
                      fileInput('excelFile', label = NULL , accept =c('.xlsx','.xls')),
                      actionButton("run", label = "Convert to mzXML"),
                      br(),
                      actionButton("beginPeakProcessing", label = "Process mzXML"),
                      br(),
                      p("Spectra Processing Progress"),
                      textOutput("mzXMLProcessingProgress")

               )
               )
      })
    }
  })




  #This "observe" event creates the UI element for analyzing multiple MALDI plates, based on user-input.

  observe({

    if (is.null(input$rawORreanalyze)){}else if (input$rawORreanalyze == 3){
      output$ui1<-renderUI({

        column(12,
               br(),
               br(),
               column(5,
                      h3("Instructions:"),
                      br(),
                      p(strong("1:")," This directs where you would like to create an IDBac working directory."),
                      p("In the folder you select- IDBac will create folders within a main directory named \"IDBac\":"),
                      img(src="WorkingDirectory.png", style="width:322px;height:164px"),
                      p("If there is already an \"IDBac\" folder present in the working directory,
                        files will be added into the already-present IDBac folder ",strong("and any samples with the same name will be overwritten.")),
                      br(),
                      p(strong("2:"),"The RAW data file should be one folder which will contain individual folders for each
                        MALDI plate. Each MALDI plate folder will contain an Excel map and two folders: one
                        containing protein data and the other containing small molecule data:"),
                      img(src="Multi-MALDI-Plate.png", style="width:410px;height:319px"),

                      p("Note: Sometimes the browser window won't pop up, but will still appear in the application bar. See below:"),
                      div(img(src="window.png",style="width:750px;height:40px"))),


               column(5,style = "background-color:#F5F5F5",
                      h3("Starting With >1 MALDI-Plate of Raw Data"),
                      br(),
                      p(strong("1:"), " Your Working Directory is where files will be created."),
                      actionButton("selectedWorkingDirectory", label = "Click to select your Working Directory"),
                      fluidRow(column(12, verbatimTextOutput("selectedWorkingDirectory", placeholder = TRUE))),
                      br(),
                      p(strong("2:"), "Your RAW data should be one folder that contains folders for each MALDI plate."),
                      actionButton("multipleMaldiRawFileDirectory", label = "Click to select the location of your RAW data"),
                      fluidRow(column(12, verbatimTextOutput("multipleMaldiRawFileDirectory", placeholder = TRUE))),
                      br(),



                      actionButton("run", label = "Convert to mzXML"),
                      actionButton("beginPeakProcessing", label = "Process mzXML")
               )
                      )
      })
    }
  })




  #This "observe" event creates the UI element for re-analyzing data
  observe({

    if (is.null(input$rawORreanalyze)){}else if (input$rawORreanalyze == 2){
      output$ui1<-renderUI({

        column(12,

               br(),
               column(5,
                      h2("Instructions:"),
                      br(),
                      p("Left-click the button to the right to select a working directory."),
                      p("Your working directory is the folder (originally named \"IDBac\"), which contains the folders:"),
                      tags$ul(
                        tags$li("Converted_To_mzML"),
                        tags$li("Peak_Lists"),
                        tags$li("Saved_MANs")
                      ),
                      br(),
                      tags$b("Example:"),br(),
                      img(src="WorkingDirectory.png", style="width:322px;height:164px"),
                      br(),br(),
                      p("Note: Sometimes the browser window won't pop up, but will still appear in the application bar. See below:"),
                      div(img(src="window.png",style="width:750px;height:40px"))


               ),


               column(5,style = "background-color:#F5F5F5",
                      h2("ReAnalyze Data:"),
                      br(),
                      p(
                        strong("1:"),
                        "Your Working Directory is where files will be created"
                      ),
                      actionButton("idbacDirectoryButton", label = "Click to select your Working Directory"),
                      fluidRow(column(
                        12,
                        verbatimTextOutput("idbacDirectoryOut", placeholder = TRUE)
                      ))
               )
        )
      })
    }
  })








  selectedDirectory <- reactive({
    if(input$selectedWorkingDirectory==0){
      return("No Folder Selected")
    }else if (input$selectedWorkingDirectory > 0) {
      choose.dir()
    }
  })


  output$selectedWorkingDirectory <-
    renderPrint(selectedDirectory())



  idbacDirectory<-reactive({

    if(is.null(input$idbacDirectoryButton)){
      return("No Folder Selected")
    } else if(input$idbacDirectoryButton==0){
      return("No Folder Selected")
    }else if (input$idbacDirectoryButton > 0){
      choose.dir()
    }else if (input$selectedWorkingDirectory > 0){
      paste0(selectedDirectory(),"/IDBac")}
  })


  output$idbacDirectoryOut <-
    renderPrint(idbacDirectory())








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
    foldersInFolder <-
      list.dirs(rawFilesLocation(),
                recursive = FALSE,
                full.names = FALSE) # Get the folders contained directly within the chosen folder.
    for (i in 1:length(foldersInFolder)) {
      folders <-
        paste0(folders, "\n", foldersInFolder[[i]]) # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
    }

    folders
  })



  multipleMaldiRawFileLocation <- reactive({

    if (input$multipleMaldiRawFileDirectory > 0) {
      choose.dir()
    }
  })




  # Creates text showing the user which directory they chose for raw files
  output$multipleMaldiRawFileDirectory <- renderText(if (is.null(multipleMaldiRawFileLocation())) {
    return("No Folder Selected")
  } else{
    folders <- NULL
    foldersInFolder <-
      list.dirs(multipleMaldiRawFileLocation(),
                recursive = FALSE,
                full.names = FALSE) # Get the folders contained directly within the chosen folder.
    for (i in 1:length(foldersInFolder)) {
      folders <-
        paste0(folders, "\n", foldersInFolder[[i]]) # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
    }

    folders
  })









  # Spectra conversion
  #This observe event waits for the user to select the "run" action button and then creates the folders for storing data and converts the raw data to mzML

spectraConversion<-reactive({


      if(is.null(input$multipleMaldiRawFileDirectory)){

        #When only analyzing one maldi plate this handles finding the raw data directories and the excel map
        excelMap <- as.data.frame(read_excel(paste0(input$excelFile$datapath), 2))
        lookupExcel <- as.data.frame(cbind(sapply(excelMap$Key, function(x)paste0("0_", x)), excelMap$Value, make.unique(as.character(excelMap$Value), sep = "_replicate-")))
        lookupExcel <- split(lookupExcel$V1, lookupExcel$V2)
        fullZ <- list.dirs(list.dirs(rawFilesLocation(), recursive = FALSE), recursive = FALSE)
        excelTable<-ldply(lookupExcel, data.frame)
        fullZ<-cbind.data.frame(fullZ,unlist(lapply(fullZ,function(x)strsplit(x,"/")[[1]][[3]])))
        colnames(fullZ)<-c("UserInput","ExcelCell")
        colnames(excelTable)<-c("UserInput","ExcelCell")
        fullZ<-merge(excelTable,fullZ,by=c("ExcelCell"))
        fullZ[,3]<-normalizePath(as.character(fullZ[,3]))

      }else{

        #When analyzing more han one MALDI plate this handles fiding the raw data directories and the excel map
        mainDirectory<-list.dirs(multipleMaldiRawFileLocation(),recursive = F)
        lapped<-lapply(mainDirectory,function(x)list.files(x,recursive = F,full.names = T))
        collectfullZ<-NULL
        for (i in 1:length(lapped)){

          fullZ<-list.dirs(lapped[[i]],recursive = F)
          excelMap <- as.data.frame(read_excel(lapped[[i]][grep(".xls",lapped[[i]])], 2))
          lookupExcel <- as.data.frame(cbind(sapply(excelMap$Key, function(x)paste0("0_", x)), excelMap$Value, make.unique(excelMap$Value, sep = "_replicate-")))
          lookupExcel <- split(lookupExcel$V1, lookupExcel$V2)



          excelTable<-ldply(lookupExcel, data.frame)
          fullZ<-cbind.data.frame(fullZ,unlist(lapply(fullZ,function(x)strsplit(x,"/")[[1]][[4]])))
          colnames(fullZ)<-c("UserInput","ExcelCell")
          colnames(excelTable)<-c("UserInput","ExcelCell")
          fullZ<-merge(excelTable,fullZ,by=c("ExcelCell"))
          fullZ[,3]<-normalizePath(as.character(fullZ[,3]))

          collectfullZ<-c(collectfullZ,list(fullZ))


        }
        fullZ<-ldply(collectfullZ,data.frame)

      }



      fullZ<-dlply(fullZ,.(UserInput.x))



fullZ


})





  observe({
    if (is.null(input$run)){}else if(input$run > 0) {

      dir.create(paste0(selectedDirectory(), "/IDBac"))
      dir.create(paste0(selectedDirectory(), "/IDBac/Converted_To_mzML"))
      dir.create(paste0(selectedDirectory(), "/IDBac/Sample_Spreadsheet_Map"))
      dir.create(paste0(selectedDirectory(), "/IDBac/Peak_Lists"))
      dir.create(paste0(selectedDirectory(), "/IDBac/Saved_MANs"))


      fullZ<-spectraConversion()
      workdir <- selectedDirectory()
      outp <- file.path(workdir, "IDBac/Converted_To_mzML")

      #fullZ$UserInput.x = sample name
      #fullZ$UserInput.y = file locations

      #Command-line MSConvert, converts from proprietary vendor data to open mzXML
      w<-lapply(fullZ,function(x)
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
        ))



      rot<-function(x){
        system(command =
                 "cmd.exe",
               input = as.character(x))
      }


      #sapply(w,rot)


      popup()


      numCores <- detectCores()
      cl <- makeCluster(numCores-1)
      parSapply(cl,w,rot)
      stopCluster(cl)


      popup2()









    }
  })



popup<-reactive({

    showModal(modalDialog(
      title = "Important message",
      "When file-conversions are complete this pop-up will be replaced by a summary of the conversion.",br(),
      "IDBac uses parallel processing to make these computations faster, unfortunately this means we can't show a progress bar.",br(),
      "This also means your computer might be slow during these file conversions.",br(),
      "To check what has been converted you can navigate to:",

      paste0(selectedDirectory(), "\\IDBac\\Converted_To_mzML"),
      easyClose = FALSE, size="l",footer=""
    ))


})








popup2<-reactive({

  showModal(modalDialog(
    title = "Conversion Complete",

    paste0(nrow(ldply(spectraConversion()))," files were converted into ",length(list.files(paste0(selectedDirectory(), "\\IDBac\\Converted_To_mzML"))),
           " open data format files."),br(),
    "To check what has been converted you can navigate to:",

    paste0(selectedDirectory(), "\\IDBac\\Converted_To_mzML"),
    easyClose = TRUE,
    footer = tagList(actionButton("beginPeakProcessing","Click to continue with Peak Processing"), modalButton("Close"))


  ))


})





observeEvent(input$beginPeakProcessing, {
  removeModal()
})











    functionA <- function(z) {

      strReverse <- function(x) {
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
      }




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
      Required_Packages = c("snow","parallel","shiny", "MALDIquant", "MALDIquantForeign", "mzR", "readxl","networkD3","factoextra","ggplot2","ape","FactoMineR","dendextend","networkD3","reshape2","dplyr","igraph","rgl")


      # Install and Load Packages
      Install_And_Load(Required_Packages)


            if ( length(mzR::header(mzR::openMSfile(file = z))$seqNum) > 1) {
        spectraImport <-  sapply(z, function(x)mzR::peaks(mzR::openMSfile(file = x)))
        spectraList <- lapply(z, function(x)(mzR::openMSfile(file = x)))
        names <- strReverse(unlist(lapply(strReverse(sapply(spectraList, fileName)), function(x)strsplit(x, "/")[[1]][1])))[[1]]
        spectraImport <-lapply(1:length(spectraImport), function(x)createMassSpectrum(mass = spectraImport[[x]][, 1],intensity = spectraImport[[x]][, 2],metaData = list(File = names)))
      } else{
        spectraImport <-  lapply(z, function(x)mzR::peaks(mzR::openMSfile(file = x)))
        spectraList <- lapply(z, function(x)(mzR::openMSfile(file = x)))
        names <- strReverse(unlist(lapply(strReverse(sapply(spectraList, fileName)), function(x)strsplit(x, "/")[[1]][1])))[[1]]
        spectraImport <-createMassSpectrum(mass = spectraImport[[1]][, 1],intensity = spectraImport[[1]][, 2],metaData = list(File = names))
        spectraImport<-list(spectraImport)
      }


      sampleNames <- strsplit(names, ".mzXML")[[1]][1]


      for (i in 1:length(spectraImport)) {
        spectraImport[[i]]@metaData$Strain <- sampleNames
      }

      labs <-
        sapply(spectraImport, function(x)
          metaData(x)$Strain)[[1]]


      proteinSpectra <-
        spectraImport[which(sapply(spectraImport, function(x)
          max(mass(x))) > 10000)]
      smallSpectra <-
        spectraImport[which(!sapply(spectraImport, function(x)
          max(mass(x))) > 10000)]

      if(length(proteinSpectra) > 0){
        averaged <- averageMassSpectra(proteinSpectra, method = "mean")
        saveRDS(
          averaged,
          paste0(

            getwd(),
            "/Peak_Lists/",
            averaged@metaData$Strain[[1]],
            "_",
            "SummedProteinSpectra.rds"
          )
        )
        remove(averaged)




        proteinSpectra <-
          transformIntensity(proteinSpectra, method = "sqrt")
        proteinSpectra <-
          smoothIntensity(proteinSpectra,
                          method = "SavitzkyGolay",
                          halfWindowSize = 20)
        proteinSpectra <-
          removeBaseline(proteinSpectra, method = "TopHat")
        proteinSpectra <-
          detectPeaks(
            proteinSpectra,
            method = "MAD",
            halfWindowSize = 20,
            SNR = 4
          )
        saveRDS(
          proteinSpectra,
          paste0(

            getwd(),
            "/Peak_Lists/",
            labs,
            "_",
            "ProteinPeaks.rds"
          )
        )





      }

      if(length(smallSpectra) > 0){
        ############
        #Spectra Preprocessing, Peak Picking
        smallSpectra <-
          smoothIntensity(smallSpectra,
                          method = "SavitzkyGolay",
                          halfWindowSize = 20)
        smallSpectra <- removeBaseline(smallSpectra, method = "TopHat")
        smallSpectra <-
          detectPeaks(
            smallSpectra,
            method = "SuperSmoother",
            halfWindowSize = 20,
            SNR = 1
          )
        saveRDS(
          smallSpectra,
          paste0(

            getwd(),
            "/Peak_Lists/",
            labs,
            "_",

            "SmallMoleculePeaks.rds"
          )
        )
        remove(smallSpectra)

        gc()

      }








    }
    # Spectra processing





    observe({


    if (is.null(input$beginPeakProcessing)){}else if(input$beginPeakProcessing > 0) {

      setwd(paste0(selectedDirectory(),"/IDBac"))

      fileList <-
        list.files(list.dirs(("Converted_To_mzML")),
                   pattern = ".mzXML",
                   full.names = TRUE)

popup3()


    #  sapply(fileList,functionA)

       numCores <- detectCores()
       cl <- makeCluster(numCores)
       parSapply(cl,fileList,functionA)



          stopCluster(cl)

popup4()


    }

})





    popup3<-reactive({

      showModal(modalDialog(
        title = "Important message",
        "When spectra processing is complete you will be able to begin with the data-analysis",br(),
        "IDBac uses parallel processing to make these computations faster, unfortunately this means we can't show a progress bar.",br(),
        "This also means your computer might be slow during the computations.",br(),
        "The step allows for fast interaction during the various data analysis",

        easyClose = FALSE, size="l",footer=""
      ))


    })


    popup4<-reactive({

      showModal(modalDialog(
        title = "Spectra Processing is Now Complete",
        br(),
        easyClose = TRUE,
        footer = modalButton("Continue to Data Analysis by consecutively visiting tabs at the top of the page.")))





    })










  spectra <- reactive({ unlist(sapply(list.files(paste0(idbacDirectory(), "\\Peak_Lists"),full.names=TRUE)[grep(".SummedProteinSpectra.", list.files(paste0(idbacDirectory(), "\\Peak_Lists")))], readRDS))
  })


  trimmedP <- reactive({
    all <-unlist(sapply(list.files(paste0(idbacDirectory(), "\\Peak_Lists"),full.names = TRUE)[grep(".ProteinPeaks.", list.files(paste0(idbacDirectory(), "\\Peak_Lists")))], readRDS))
    all<-binPeaks(all, tolerance =.02)
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
    new2


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

      } else {
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
        numericInput("percentPresenceP", label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%)"),value = 70,step=10,min=70,max=70),
        numericInput("pSNR", label = h5("Signal To Noise Cutoff"),value = 4,step=.5,min=1.5,max=100),


        numericInput("upperMass", label = h5("Upper Mass Cutoff"),value = 15000,step=50),
        numericInput("lowerMass", label = h5("Lower Mass Cutoff"),value = 3000,step=50),

        selectInput("Spectra1", label=h5("Spectrum 1 (up; matches to bottom spectrum are blue, non-matches are red)"),
                    choices = sapply(seq(1,length(spectra()),by=1),function(x)metaData(spectra()[[x]])$Strain)),
        selectInput("Spectra2", label=h5("Spectrum 2 (down, grey)"),
                    choices = sapply(seq(1,length(spectra()),by=1),function(x)metaData(spectra()[[x]])$Strain)),

        p("Note: Mass Cutoff and Percent Replicate values selected here will be used in all later analyses."),
        p("Displayed spectra are the mean spectrum for a sample.")

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
  # This creates the PCA plot and the calculation required for such.
  output$pcaplot <- renderPlot({
    c<-PCA(proteinMatrix(),graph=FALSE)
    a<-c$ind$coord
    a<-as.data.frame(a)
    nam<-rownames(a)
    a<-cbind(a,nam)
    if(input$kORheight=="2"){
      d<-cutree(dendro(),h=input$height)
    }
    else{
      d<-cutree(dendro(),k=input$kClusters)

    }
    e<-as.data.frame(cbind(a,d))
    if(input$PCA3d==1){
      plot3d(x=e$Dim.1,y=e$Dim.2,z=e$Dim.3,xlab="", ylab="", zlab="")
      text3d(x=e$Dim.1,y=e$Dim.2,z=e$Dim.3,text=e$nam,col=factor(d))}
    ggplot(e,aes(Dim.1,Dim.2,label=nam))+geom_label(aes(col=factor(d)),size=6)+xlab("Dimension 1")+ylab("Dimension 2")+ggtitle("PCA of Protein MALDI Spectra (colors based on clusters from hiearchical clustering)")+theme(plot.title=element_text(size=15)) + scale_color_discrete(name="Clusters from hierarchical clustering")+theme(legend.position="none")
  },height=750)


  # Create PCA ui
  output$PCAui <-  renderUI({


    sidebarLayout	(
      sidebarPanel(
        radioButtons("PCA3d", label = h4("PCA 3d Plot"),
                     choices = list("Yes" = 1, "No" = 2),selected = 2)
      ),
      mainPanel(plotOutput("pcaplot"))
    )

  })





  ################################################
  #Create the hierarchical clustering based upon the user input for distance method and clustering technique
  dendro <- reactive({
    ret<-{
      if(input$distance=="cosineD"){
        a<-as.function(get(input$distance))
        dend <- proteinMatrix() %>% a %>% hclust(method=input$clustering) %>% as.dendrogram
      }
      else{
        dend <- proteinMatrix() %>% dist(method=input$distance) %>% hclust(method=input$clustering) %>% as.dendrogram
      }

    }
    ret
  })








  #User input changes the height of the main heirarchical clustering plot
  plotHeight <- reactive({
    return(as.numeric(input$hclustHeight))
  })





  ################################################
  #Create the hierarchical clustering plot as well as the calculations needed for such.
  output$hclustPlot <- renderPlot({

    # sampleMappings<-as.data.frame(read_excel(input$sampleMap$datapath,1))
    # toHighlite<-which(input$sampleMap
    #  dendro %>% color_labels(labels=labels(dendro)[c(4,5,6,16)],col=brewer.pal(3,"Set1")[[3]]) %>% plot

    #If no sample map is selected, run this:
    if(input$kORheight=="2"){
      par(mar=c(5,5,5,10))
      dendro() %>% color_branches(h=input$height) %>% plot(horiz=TRUE,lwd=8)
      abline(v=input$height,lty=2)
    }
    else{
      par(mar=c(5,5,5,10))
      dendro() %>% color_branches(k=input$kClusters)   %>% plot(horiz=TRUE,lwd=8)
    }

  },height=plotHeight)






  output$hclustui <-  renderUI({
    if(input$kORheight!="2"){return(NULL)}else{
      numericInput("height", label = h5("Cut Tree at Height"),value = .5,step=.1,min=0)}
  })


  output$groupui <-  renderUI({
    if(input$kORheight=="1"){
      numericInput("kClusters", label = h5("Number of Groups"),value = 1,step=1,min=1)}
  })







  # sampleMapAttributes <- reactive({
  # sampleMappings<-as.data.frame(read_excel(input$sampleMap$datapath,1))
  # colnames(sampleMappings)[!grepl("id",colnames(sampleMappings),ignore.case = TRUE)]



  # #    dendro %>% color_labels(labels=labels(dendro)[c(4,5,6,16)],col=brewer.pal(3,"Set1")[[3]]) %>% plot
  # })


















  output$sampleMapColumns<-renderUI({
    sampleMappings<-as.data.frame(read_excel(input$sampleMap$datapath,1))
    sampleMappings<-colnames(sampleMappings)[!grepl("id",colnames(sampleMappings),ignore.case = TRUE)]
    selectInput("sampleMapChosenAttribute", label = h5("Select a Group Mapping"),
                choices = as.list(sampleMappings))

  })




  # Create Heir ui
  output$Heirarchicalui <-  renderUI({

    sidebarLayout	(
      sidebarPanel(
        #checkboxGroupInput("Library", label=h5("Inject Library Phylum"),
        #                    choices = levels(phyla)),
        selectInput("distance", label = h5("Distance Method"),
                    choices = list("cosine"="cosineD","euclidean"="euclidean","maximum"="maximum","manhattan"="manhattan","canberra"="canberra", "binary"="binary","minkowski"="minkowski"),
                    selected = "cosineD"),
        selectInput("clustering", label = h5("Agglomeration Method for Tree"),
                    choices = list("ward.D"="ward.D","ward.D2"="ward.D2", "single"="single", "complete"="complete", "average (UPGMA)"="average", "mcquitty (WPGMA)"="mcquitty", "median (WPGMC)"="median","centroid (UPGMC)"="centroid"),
                    selected = "complete"),
        radioButtons("booled", label = h5("Include peak intensities in calculations or use presence/absence?"),
                     choices = list("Presence/Absence" = 1, "Intensities" = 2),
                     selected = 2),
        radioButtons("kORheight", label = h5("Create clusters based on specified number of groups or height?"),
                     choices = list("# Groups" = 1, "Height" = 2),
                     selected = 1),
        uiOutput("hclustui"),
        uiOutput("groupui"),
        numericInput("hclustHeight", label = h5("Tree Plot Height"),value = 750,step=50,min=100),
        fileInput('sampleMap', label = "Sample Mapping" , accept =c('.xlsx','.xls')),
        uiOutput("sampleMapColumns")




      ),
      mainPanel("Hierarchical Clustering",textOutput("Clusters"),plotOutput("hclustPlot"))
    )
  })







  smallPeaks <- reactive({ unlist(sapply(list.files(paste0(idbacDirectory(), "\\Peak_Lists"),full.names=TRUE)[grep(".SmallMoleculePeaks.", list.files(paste0(idbacDirectory(), "\\Peak_Lists")))], readRDS))
  })






  ################################################
  #This creates the network plot and calculations needed for such.
  output$metaboliteAssociationNetwork <- renderSimpleNetwork({


    labs <- sapply(smallPeaks(), function(x)metaData(x)$Strain)



    if(is.null(input$plot_brush$ymin)){
      #This takes the cluster # selection from the left selection pane and passes that cluster of samples for MAN analysis

      if(input$kORheight=="2"){
        combinedSmallMolPeaks<-smallPeaks()[grep(paste0(c(labels(which(cutree(dendro(),h=input$height)==input$Group)),"Matrix"),"$",collapse="|"), labs,ignore.case=TRUE)]
      }
      else{
        combinedSmallMolPeaks<-smallPeaks()[grep(paste0(c(labels(which(cutree(dendro(),k=input$kClusters)==input$Group)),"Matrix"),"$",collapse="|"), labs,ignore.case=TRUE)]

      }
    }else{
      #This takes a brush selection over the heirarchical clustering plot within the MAN tab and uses this selection of samples for MAN analysis


      location_of_Heirarchical_Leaves<-get_nodes_xy(dendro())
      minLoc<-input$plot_brush$ymin
      maxLoc<-input$plot_brush$ymax
      threeColTable<<-data.frame(location_of_Heirarchical_Leaves[location_of_Heirarchical_Leaves[,2]==0,],labels(dendro()))
      #column 1= y-values of dendrogram leaves
      #column 2= node x-values we selected for only leaves by only returning nodes with x-values of 0
      #column 3= leaf labels
      w<- which(threeColTable[,1] > minLoc & threeColTable[,1] < maxLoc)
      brushed<-as.vector(threeColTable[,3][w])
      labs<-as.vector(sapply(smallPeaks(),function(x)metaData(x)$Strain))
      combinedSmallMolPeaks<-smallPeaks()[grep(paste0(c(brushed,"Matrix"),"$",collapse="|"), labs,ignore.case=TRUE)]

    }




    #if(length(grep("Matrix",sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain),ignore.case=TRUE))==0){"No Matrix Blank!!!!!!!"}else{


    combinedSmallMolPeaksm<-combinedSmallMolPeaks[grep("Matrix",sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain),ignore.case=TRUE)][[1]]





    combinedSmallMolPeaks<-combinedSmallMolPeaks[which(!grepl("Matrix",sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain),ignore.case=TRUE))]

    labs <-as.vector(sapply(smallPeaks(), function(x)metaData(x)$Strain))

    for (i in 1:length(combinedSmallMolPeaks)){
      combinedSmallMolPeaks[[i]]@mass<-combinedSmallMolPeaks[[i]]@mass[which(combinedSmallMolPeaks[[i]]@snr>input$smSNR)]
      combinedSmallMolPeaks[[i]]@intensity<-combinedSmallMolPeaks[[i]]@intensity[which(combinedSmallMolPeaks[[i]]@snr>input$smSNR)]
      combinedSmallMolPeaks[[i]]@snr<-combinedSmallMolPeaks[[i]]@snr[which(combinedSmallMolPeaks[[i]]@snr>input$smSNR)]
    }
    combinedSmallMolPeaksm@mass<-combinedSmallMolPeaksm@mass[which(combinedSmallMolPeaksm@snr>6)]
    combinedSmallMolPeaksm@intensity<-combinedSmallMolPeaksm@intensity[which(combinedSmallMolPeaksm@snr>6)]
    combinedSmallMolPeaksm@snr<-combinedSmallMolPeaksm@snr[which(combinedSmallMolPeaksm@snr>6)]

    combinedSmallMolPeaks<-c(combinedSmallMolPeaksm,combinedSmallMolPeaks)



    allS<-binPeaks(combinedSmallMolPeaks[which(sapply(combinedSmallMolPeaks,function(x)length(mass(x)))!=0)],tolerance=.002)
    tt<- trim(allS,c(input$lowerMassSM,input$upperMassSM))
    labs <- sapply(tt, function(x)metaData(x)$Strain)
    labs<-factor(labs)
    new2<-NULL
    newPeaks<-NULL

    for (i in seq_along(levels(labs))){

      specSubset<-(which(labs==levels(labs)[[i]]))
      if(specSubset>1){
        perGroup<-intensityMatrix(tt[specSubset])
        bool<-perGroup
        bool[is.na(bool)] <- 0
        bool<-as.data.frame(bool)
        bool<- ifelse(bool > 0,1,0)
        freq<-colSums(bool)
        freq<-as.data.frame(freq)
        group<-length(tt[specSubset])
        perc<-ceiling(((input$percentPresenceSM/100))*group)
        for (i in seq_along(freq)){
          ind<-which(freq$freq>=perc)
        }
        commonMasses<-as.double(rownames(freq)[ind])
        newPeaks <-  mergeMassPeaks(tt[specSubset])
        newPeaks@mass<-newPeaks@mass[which(sapply(mass(newPeaks),function(x) round(x,digits=4)) %in% sapply(commonMasses,function(x) round(x,digits=4)))]
        newPeaks@intensity<-newPeaks@intensity[which(sapply(mass(newPeaks),function(x) round(x,digits=4)) %in% sapply(commonMasses,function(x) round(x,digits=4)))]
        newPeaks@snr<-newPeaks@snr[which(sapply(mass(newPeaks),function(x) round(x,digits=4)) %in% sapply(commonMasses,function(x) round(x,digits=4)))]
        new2<-c(new2,newPeaks)}
      else{


        new2<-c(new2,tt[specSubset])}

    }





    combinedSmallMolPeaks<-new2
    ############
    #Filter out peaks that occur in less than xx percent of replicates.(rounded !up! to the nearest whole number replicate)
    #We realize MALDIquant's "filter peaks" exists but needed to write the following for some work within KNIME
    #For a different percentage of peak presence: change the value of percentToRoundTo  to a percent between 0 and 1 (percent/100)  eg 0.7 = 70% presence
    labs <- sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain)
    labs<-factor(labs)
    ############
    #Find the matrix sample index
    labs <- sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain)
    matrixIndex<-grep("Matrix",labs,ignore.case=TRUE)
    ############
    #Removing Peaks which share the m/z as peaks that are in the Matrix Blank
    #peaksa = samples
    #peaksb = matrix blank
    peaksa<-combinedSmallMolPeaks[-matrixIndex]
    peaksb<-combinedSmallMolPeaks[[matrixIndex]]
    for (i in 1:length(peaksa)){
      commonIons<-which(!peaksa[[i]]@mass %in% setdiff(peaksa[[i]]@mass,peaksb@mass))
      peaksa[[i]]@mass<-peaksa[[i]]@mass[-commonIons]
      peaksa[[i]]@intensity<-peaksa[[i]]@intensity[-commonIons]
      peaksa[[i]]@snr<-peaksa[[i]]@snr[-commonIons]
      peaksa[[i]]@metaData<-peaksa[[i]]@metaData[-commonIons]
    }
    ############
    #Trim Peak Lists ends so that <200 m/z and >2000 m/z are removed

    ############
    # Turn Peak-List into a table, add names, change from wide to long, and export as Gephi-compatible edge list
    smallNetwork<-intensityMatrix(peaksa)
    temp<-NULL
    for (i in 1:length(peaksa)){
      temp<-c(temp,combinedSmallMolPeaks[[i]]@metaData$Strain)
    }
    peaksaNames<-factor(temp)
    remove(temp)
    rownames(smallNetwork) <- paste(peaksaNames)
    bool<-smallNetwork
    bool[is.na(bool)] <- 0
    bool<-as.data.frame(bool)
    bool<- ifelse(bool > 0,1,0)
    bool<-bool
    bool<-as.data.frame(bool)
    #The network is weighted by the inverse of percentage of presence of the peak, this de-emphasizes commonly occuring peaks and "pulls" rarer peaks closer to their associated sample
    bool[,colnames(bool)] <- sapply(bool[,colnames(bool)],function(x) ifelse(x==1,1/sum(x),x))
    #Create a matrix readable by Gephi as an edges file
    bool<-cbind(rownames(bool),bool)
    bool<-melt(bool)
    bool<-subset(bool, value!=0)
    colnames(bool)<-c("Source","Target","Weight")
    #Round m/z values to a single decimal
    bool$Target<-round(as.numeric(as.matrix(bool$Target)),digits=1)
    if(input$save=="FALSE"){
    }

    else{

      workdir<-idbacDirectory()
      write.csv(as.matrix(bool),paste0(workdir, "\\Saved_MANs\\Current_Network.csv"))



    }

    a<-as.undirected(graph_from_data_frame(bool))

    wc<-fastgreedy.community(a)


    b<-igraph_to_networkD3(a, group = (wc$membership + 1))




    z<-b$links
    zz<-b$nodes
    forceNetwork(Links = z, Nodes = zz, Source = "source",
                 Target = "target", NodeID = "name",
                 Group = "group", opacity = 1, zoom = TRUE)

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
    if(input$kORheight=="2"){

      paste("You have ", length(unique(cutree(dendro(),h=input$height)))," Cluster(s)")
    }
    else{
      paste("You have ", length(unique(cutree(dendro(),k=input$kClusters)))," Cluster(s)")
    }

  })


  output$plot1 <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })



  output$info <- renderText({
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
  })





  #User input changes the height of the heirarchical clustering plot within the network analysis pane
  plotHeightHeirNetwork <- reactive({
    return(as.numeric(input$hclustHeightNetwork))
  })



  output$netheir<-renderPlot({

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
        numericInput("percentPresenceSM", label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%)"),value = 70,step=10,min=0,max=100),
        numericInput("smSNR", label = h5("Signal To Noise Cutoff"),value = 4,step=.5,min=1.5,max=100),
        numericInput("Group", label = h5("View Small-Molecule Network for Cluster #:"),value = 1,step=1,min=1),
        checkboxInput("save", label = "Save Current Network?", value = FALSE),
        numericInput("upperMassSM", label = h5("Upper Mass Cutoff"),value = 2000,step=50,max=max(sapply(smallPeaks(),function(x)max(mass(x))))),
        numericInput("lowerMassSM", label = h5("Lower Mass Cutoff"),value = 200,step=50,min=min(sapply(smallPeaks(),function(x)min(mass(x))))),
        numericInput("hclustHeightNetwork", label = h5("Tree Plot Height"),value = 750,step=50,min=100)


      ),
      mainPanel(textOutput("Clusters2"),simpleNetworkOutput("metaboliteAssociationNetwork"),
                plotOutput("netheir",
                           click = "plot_click",
                           dblclick = "plot_dblclick",
                           hover = "plot_hover",
                           brush = "plot_brush")
      ))
  })





  # session$onSessionEnded(function() {
  # stopApp()
  # q("no")
  # })


  }

