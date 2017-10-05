
# The server portion of the Shiny app serves as the backend, performing data processing and creating the visualizations to be displayed as specified in the U function(input, output,session) {

# Reactive variable returning the user-chosen working directory as string
function(input,output,session){
  
  
  # "appLocation" will be assigned to the directory of where IDBac was installed to.  This will be used later in order to access MSConvert
  # which is used for converting raw MALDI files to mzML via command line.
  appLocation <- getwd()
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
  
  
  observe({
    
    if (is.null(input$rawORreanalyze)){}else if (input$rawORreanalyze == 1){
      output$ui1<-renderUI({
        
        column(12,
               br(),
               br(),
               column(5,
                      h3("Instructions:"),
                      br(),
                      p("Left-click the button to the right to select 
                        where you would like to create an IDBac working directory."),
                      p("This will create folders within a main directory named \"IDBac\""),
                        
                    p("Your working directory is the folder which contains \"Converted_To_mzML\",\"Peak_Lists\", and \"Saved_MANs\"       "),
                      img(src="WorkingDirectory.png", style="width:200px;height:125px")
               ),
               
               
               
               column(5,style = "background-color:#F5F5F5",
                   h3("Start With Raw Data"),
                   br(),
                   p(strong("1:"), " Your Working Directory is where files will be created"),
                   actionButton("selectedWorkingDirectory", label = "Click to select your Working Directory"),
               fluidRow(column(12, verbatimTextOutput("selectedWorkingDirectory", placeholder = TRUE))),
                   br(),
                   p(strong("2:"), "Your RAW data should be one folder that contains: two folders containing protein and small-molecule data"),
                   actionButton("rawFileDirectory", label = "Click to select the location of your RAW data"),
               fluidRow(column(12, verbatimTextOutput("rawFileDirectory", placeholder = TRUE))),
                   br(),
                   p(strong("3:"), "Choose  your Sample Map file, the excel sheet which IDBac will use to rename your files"),
                   fileInput('excelFile', label = NULL , accept =c('.xlsx','.xls')),
                   actionButton("run", label = "run"),
                   actionButton("run2", label = "run2"),
                   actionButton("run3", label = "run3")
               )
        
        
        )
        
        
      })
    }
  })
  
  
  
  observe({
    
    if (is.null(input$rawORreanalyze)){}else if (input$rawORreanalyze == 2){
      output$ui1<-renderUI({
        
        column(12,
               
               br(),
               column(5,
                      h2("Instructions:"),
                      br(),
                      p("Left-click the button to the right to select the previous working directory"),
                      br(),
                      p("Your working directory is the folder (originally named \"IDBac\"), which contains the folders:"), 
                      tags$ul(
                          tags$li("Converted_To_mzML"),
                          tags$li("Peak_Lists"),
                        tags$li("Saved_MANs")
                        ),
                      br(),
                      tags$b("Example:"),br(),
                        img(src="WorkingDirectory.png", style="width:200px;height:125px"),
                      br(),br(),
                      p("Note: Sometimes the browser window won't pop up, but will still appear in the appliacation bar.")
                      
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
      "No Folder Selected"
    }else if (input$selectedWorkingDirectory > 0) {
      choose.dir()
    }
  })
  
  
  output$selectedWorkingDirectory <-
    renderPrint(selectedDirectory())
  
  
  
  idbacDirectory<-reactive({
    
    if(is.null(input$idbacDirectoryButton)){
      "No Folder Selected"
    } else if(input$idbacDirectoryButton==0){
      "No Folder Selected"
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
    return(NULL)
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
  
  #This handles reading in the excel file and displaying it on the    page.
  
  output$sampleMap <- renderTable({
    inFile <- input$excelFile
    if (is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep = ""))
    
    
    read_excel(paste(inFile$datapath, ".xlsx", sep = ""), 1)
    
  })
  
  
  
  
  
  
  
  # Spectra conversion
  #This observe event waits for the user to select the "run" action button and then creates the folders for storing data and converts the raw data to mzML
  
  observe({
    if (is.null(input$run)){}else if(input$run > 0) {
      dir.create(paste0(selectedDirectory(), "/IDBac"))
      dir.create(paste0(selectedDirectory(), "/IDBac/Converted_To_mzML"))
      dir.create(paste0(selectedDirectory(), "/IDBac/Sample_Spreadsheet_Map"))
      dir.create(paste0(selectedDirectory(), "/IDBac/Peak_Lists"))
      dir.create(paste0(selectedDirectory(), "/IDBac/Saved_MANs"))
      
      
      
      excelMap <-
        as.data.frame(read_excel(paste0(input$excelFile$datapath), 2))
      lookupExcel <-
        as.data.frame(cbind(
          sapply(excelMap$Key, function(x)
            paste0("0_", x)),
          excelMap$Value,
          make.unique(excelMap$Value, sep = "_replicate-")
        ))
      lookupExcel <- split(lookupExcel$V1, lookupExcel$V2)
      fullZ <-
        list.dirs(list.dirs(rawFilesLocation(), recursive = FALSE),
                  recursive = FALSE)
      
      
      
      
      
      excelTable<-ldply(lookupExcel, data.frame)
      fullZ<-cbind.data.frame(fullZ,unlist(lapply(fullZ,function(x)strsplit(x,"/")[[1]][[3]])))
      colnames(fullZ)<-c("UserInput","ExcelCell")
      colnames(excelTable)<-c("UserInput","ExcelCell")
      
      
      
      
      fullZ<-merge(excelTable,fullZ,by=c("ExcelCell"))
      
      
      fullZ[,3]<-normalizePath(as.character(fullZ[,3]))
      
      fullZ<-dlply(fullZ,.(UserInput.x))
      
      saveRDS(fullZ,"fullZ.rds")
      
      
      workdir <- selectedDirectory()
      
      outp <- paste0(workdir, "\\IDBac\\Converted_To_mzML")
      
      w<-lapply(fullZ,function(x)
        paste0("pwiz\\msconvert.exe",
               
               " ",
               paste0(x[[3]],collapse = "",sep=" "),
               " --mzXML --merge -z",
               " -o ",
               outp,
               " --outfile ",
               
               
               
               paste0(x[[2]][1],".mzXML")
        ))
      
      
      
      
      
      rot<-function(x){
        system(command =
                 "cmd.exe",
               input = as.character(x))
      }
      
      
      
      
      
      numCores <- detectCores()
      cl <- makeCluster(numCores)
      parSapply(cl,w,rot)
      
      
      stopCluster(cl)
      
      
      
      
      
      
      
    }
  })
  
  
  
  
      functionA <- function(z) {
        setwd(paste0(selectedDirectory(),"//IDBac"))
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
    
    
    
    if (is.null(input$run2)){}else if(input$run2 > 0) {
      
      setwd(paste0(selectedDirectory(),"/IDBac"))
      
      fileList <-
        list.files(list.dirs(("Converted_To_mzML")),
                   pattern = ".mzXML",
                   full.names = TRUE)
      
      
      
      
      sapply(fileList,functionA)
      
   #   numCores <- detectCores()
   #   cl <- makeCluster(numCores)
   #   parSapply(cl,fileList,functionA)
      
      
      
  #    stopCluster(cl)
      
      
      
      
    }
    
    
    
    
    
    
    
    
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
    
    
    sdsdsdsdd<<-new2
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
  
  
  
  output$inversePeakComparisonPlot <- renderPlot({
    
    p1<-collapsedPeaksP()[[grep(paste0(input$Spectra1,"$"),sapply(seq(1,length(collapsedPeaksP()),by=1),function(x)metaData(collapsedPeaksP()[[x]])$Strain))]]
    p2<-collapsedPeaksP()[[grep(paste0(input$Spectra2,"$"),sapply(seq(1,length(collapsedPeaksP()),by=1),function(x)metaData(collapsedPeaksP()[[x]])$Strain))]]
    
    
      p1@mass<-p1@mass[which(p1@snr>input$pSNR)]
      p1@intensity<-p1@intensity[which(p1@snr>input$pSNR)]
      p1@snr<-p1@snr[which(p1@snr>input$pSNR)]
    
    
    
      p2@mass<-p2@mass[which(p2@snr>input$pSNR)]
      p2@intensity<-p2@intensity[which(p2@snr>input$pSNR)]
      p2@snr<-p2@snr[which(p2@snr>input$pSNR)]
    
    
    p1s<<-spectra()[[grep(paste0(input$Spectra1,"$"),sapply(seq(1,length(spectra()),by=1),function(x)metaData(spectra()[[x]])$Strain))]]
    p2s<<-spectra()[[grep(paste0(input$Spectra2,"$"),sapply(seq(1,length(spectra()),by=1),function(x)metaData(spectra()[[x]])$Strain))]]
    
    p1b<-as.data.frame(cbind(p1@mass,p1@intensity))
    p1b<-as.data.frame(cbind(p1@mass,p1@intensity))
    p2b<-as.data.frame(cbind(p2@mass,p2@intensity))
    p2b<-as.data.frame(cbind(p2@mass,p2@intensity))
    #Create peak plots and color each peak according to whether it occurs in the other spectrum
    
    p3b<<-data.frame(p1b,rep("red",length=length(p1b$V1)),stringsAsFactors = F)
    colnames(p3b)<<-c("Mass","Intensity","Color")
    
    p4b<<-data.frame(p2b,rep("grey",length=length(p2b$V1)),stringsAsFactors = F)
    colnames(p4b)<<-c("Mass","Intensity","Color")
    
    
    p3b$Color[which(p3b$Mass %in% intersect(p3b$Mass,p4b$Mass))]<-"blue"
    
    observe({
      brush <- input$plot2_brush
      if (!is.null(brush)) {
        ranges2$x <- c(brush$xmin, brush$xmax)
        ranges2$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges2$x <- NULL
        ranges2$y <- c(-max(p2s@intensity),max(p1s@intensity))
      }
    })
    
    plot(p1s@mass,p1s@intensity,ylim=c(-max(p2s@intensity),max(p1s@intensity)),type="l",col=adjustcolor("Black", alpha=0.3),xlab="m/z",ylab="Intensity")
    lines(p2s@mass,-p2s@intensity)
    rect(xleft=p3b$Mass-.5, ybottom=0, xright=p3b$Mass+.5, ytop=((p3b$Intensity)*max(p1s@intensity)/max(p3b$Intensity)),border=p3b$Color)
    rect(xleft=p4b$Mass-.5, ybottom=0, xright=p4b$Mass+.5, ytop=-((p4b$Intensity)*max(p2s@intensity)/max(p4b$Intensity)),border=p4b$Color)
    
  })
  
  
  #Used in the the inverse-peak plot for zooming
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$inversePeakComparisonPlotZoom <- renderPlot({
    
    plot(p1s@mass,p1s@intensity,type="l",col=adjustcolor("Black", alpha=0.3), xlim = ranges2$x, ylim = ranges2$y,xlab="m/z",ylab="Intensity")
    lines(p2s@mass,-p2s@intensity)
    rect(xleft=p3b$Mass-.5, ybottom=0, xright=p3b$Mass+.5, ytop=((p3b$Intensity)*max(p1s@intensity)/max(p3b$Intensity)),border=p3b$Color)
    rect(xleft=p4b$Mass-.5, ybottom=0, xright=p4b$Mass+.5, ytop=-((p4b$Intensity)*max(p2s@intensity)/max(p4b$Intensity)),border=p4b$Color)
    
    
    
    
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
        
        h4("Note: Mass Cutoff and Percent Replicate values selected here will be used in all later analyses")
        
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
  
  
  #User input changes the height of the heirarchical clustering plot
  plotHeight <- reactive({
    return(as.numeric(input$hclustHeight))
  })
  
  
  ################################################
  # This creates the PCA plot and the calculation required for such.
  output$pcaplot <- renderPlot({
    c<-PCA(proteinMatrix(),graph=FALSE)
    a<-c$ind$coord
    a<-as.data.frame(a)
    nam<-rownames(a)
    a<-cbind(a,nam)
    if(input$kmeansORheight=="2"){
      d<-cutree(dendro(),h=input$height)
    }
    else{
      d<-cutree(dendro(),k=input$kmeansClusters)
      
    }
    e<-as.data.frame(cbind(a,d))
    if(input$PCA3d==1){
      plot3d(x=e$Dim.1,y=e$Dim.2,z=e$Dim.3,xlab="", ylab="", zlab="")
      text3d(x=e$Dim.1,y=e$Dim.2,z=e$Dim.3,text=e$nam,col=factor(d))}
    ggplot(e,aes(Dim.1,Dim.2,label=nam))+geom_label(aes(col=factor(d)),size=6)+xlab("Dimension 1")+ylab("Dimension 2")+ggtitle("PCA of Protein MALDI Spectra (colors based on clusters from hiearchical clustering)")+theme(plot.title=element_text(size=15)) + scale_color_discrete(name="Clusters from hierarchical clustering")+theme(legend.position="none")
  },height=750)
  ################################################
  #Create the hierarchical clustering plot as well as the calculations needed for such.
  output$hclust <- renderPlot({
    if(input$kmeansORheight=="2"){
      par(mar=c(5,5,5,10))
      dendro() %>% color_branches(h=input$height) %>% plot(horiz=TRUE,lwd=8)
      abline(v=input$height,lty=2)
    }
    else{
      par(mar=c(5,5,5,10))
      dendro() %>% color_branches(k=input$kmeansClusters)   %>% plot(horiz=TRUE,lwd=8)
    }
  },height=plotHeight)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  #Create the hierarchical clustering plot as well as the calculations needed for such.
  output$hclust <- renderPlot({
    if(input$kmeansORheight=="2"){
      par(mar=c(5,5,5,10))
      dendro() %>% color_branches(h=input$height) %>% plot(horiz=TRUE,lwd=8)
      abline(v=input$height,lty=2)
    }
    else{
      par(mar=c(5,5,5,10))
      dendro() %>% color_branches(k=input$kmeansClusters)   %>% plot(horiz=TRUE,lwd=8)
    }
  },height=plotHeight)
  
  
  
  
  
  
  
  output$netheir<-renderPlot({
    
    if(input$kmeansORheight=="2"){
      par(mar=c(5,5,5,10))
      dendro() %>% color_branches(h=input$height) %>% plot(horiz=TRUE,lwd=8)
      abline(v=input$height,lty=2)
    }
    else{
      par(mar=c(5,5,5,10))
      dendro() %>% color_branches(k=input$kmeansClusters)   %>% plot(horiz=TRUE,lwd=8)
    }
    
    
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
        radioButtons("booled", label = h4("Include peak intensities in calculations or use presence/absence?"),
                     choices = list("Presence/Absence" = 1, "Intensities" = 2),
                     selected = 2),
        radioButtons("kmeansORheight", label = h4("Create clusters based on k-means or height?"),
                     choices = list("k-means" = 1, "Height" = 2),
                     selected = 2),
        numericInput("height", label = h5("Cut Tree at Height"),value = .5,step=.1,min=0),
        numericInput("kmeansClusters", label = h5("Number of clusters if using k-means"),value = 2,step=1,min=1),
        numericInput("hclustHeight", label = h5("Tree Plot Height"),value = 750,step=50,min=100)
        
      ),
      mainPanel("Hierarchical Clustering",textOutput("Clusters"),plotOutput("hclust"))
    )
  })
  
  
  
  
  
  
  smallPeaks <- reactive({ unlist(sapply(list.files(paste0(idbacDirectory(), "\\Peak_Lists"),full.names=TRUE)[grep(".SmallMoleculePeaks.", list.files(paste0(idbacDirectory(), "\\Peak_Lists")))], readRDS))
  })
  
  
  ################################################
  #This creates the network plot and calculations needed for such.
  output$ddd <- renderSimpleNetwork({
    

    labs <- sapply(smallPeaks(), function(x)metaData(x)$Strain)
    
    if(input$kmeansORheight=="2"){
      combinedSmallMolPeaks<-smallPeaks()[grep(paste0(c(labels(which(cutree(dendro(),h=input$height)==input$Group)),"Matrix"),"$",collapse="|"), labs,ignore.case=TRUE)]
    }
    else{
      combinedSmallMolPeaks<-smallPeaks()[grep(paste0(c(labels(which(cutree(dendro(),k=input$kmeansClusters)==input$Group)),"Matrix"),"$",collapse="|"), labs,ignore.case=TRUE)]
      
    }
    
    
    combinedSmallMolPeaksm<-combinedSmallMolPeaks[which(grepl("Matrix",sapply(combinedSmallMolPeaks, function(x)metaData(x)$Strain),ignore.case=TRUE))][[1]]
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
    remove(smallAveragedSpectra)
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
    else{ write.csv(as.matrix(bool),"Current Network.csv")
    }
    
    simpleNetwork(bool,zoom=TRUE)
    
  })
  ################################################
  #This displays the number of clusters created on the hierarchical clustering tab- displays text at top of the clustering page.
  output$Clusters <- renderText({
    
    # Display text of how many clusters were created.
    if(input$kmeansORheight=="2"){
      paste("You have Created ", length(unique(cutree(dendro(),h=input$height)))," Cluster(s)")
    }
    else{
      paste("You have Created ", length(unique(cutree(dendro(),k=input$kmeansClusters)))," Cluster(s)")
    }
  })
  ################################################
  ##This displays the number of clusters created on the hierarchical clustering tab- displays text at top of the networking page.
  output$Clusters2 <- renderText({
    
    # Display text of how many clusters were created.
    if(input$kmeansORheight=="2"){
      
      paste("You have ", length(unique(cutree(dendro(),h=input$height)))," Cluster(s)")
    }
    else{
      paste("You have ", length(unique(cutree(dendro(),k=input$kmeansClusters)))," Cluster(s)")
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
  
  
  
  
  # Create Heir ui
  output$MANui <-  renderUI({
    
    
    sidebarLayout(
      sidebarPanel(
        numericInput("percentPresenceSM", label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%)"),value = 70,step=10,min=0,max=100),
        numericInput("smSNR", label = h5("Signal To Noise Cutoff"),value = 4,step=.5,min=1.5,max=100),
        numericInput("Group", label = h5("View Small-Molecule Network for Cluster #:"),value = 1,step=1,min=1),
        checkboxInput("save", label = "Save Current Network?", value = FALSE),
        numericInput("upperMassSM", label = h5("Upper Mass Cutoff"),value = 2000,step=50,max=max(sapply(smallPeaks(),function(x)max(mass(x))))),
        numericInput("lowerMassSM", label = h5("Lower Mass Cutoff"),value = 200,step=50,min=min(sapply(smallPeaks(),function(x)min(mass(x)))))
      ),
      mainPanel(textOutput("Clusters2"),simpleNetworkOutput("ddd"),
                plotOutput("netheir", 
                           click = "plot_click",
                           dblclick = "plot_dblclick",
                           hover = "plot_hover",
                           brush = "plot_brush"),
                verbatimTextOutput("info"))
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # session$onSessionEnded(function() {
  # stopApp()
  # q("no")
  # })
  
  
  
  
  
  
  
  
  
  
  
  
  
}

