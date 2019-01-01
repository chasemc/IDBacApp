oneMaldiPlate <- function(id){
  fluidRow(
    column(12,
           br(),
           br(),
           fluidRow(
             column(12, offset = 3,
                    h3("Starting with a Single MALDI Plate of Raw Data"))),
           br(),
           br(),
           column(5,
                  fluidRow(
                    column(5,offset = 3,
                           h3("Instructions"))),
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
           ),
           column(1),
           column(5, style = "background-color:#7777770d",
                  fluidRow(
                    h3("Workflow Pane",
                       align="center")),
                  br(),
                  column(12, align="center",
                         p(strong("1: Enter a Name for this New Experiment")),
                         p("Only numbers, \"_\", and A-Z. Shouldn't start with a number."),
                         textInput("newExperimentName",
                                   label = ""),
                         tags$hr(size=20)),
                  br(),
                  p(strong("2: Click to select the location of your RAW data"), align= "center"),
                  column(12, align="center",
                         actionButton("rawFileDirectory",
                                      label = "Raw Data Folder"),
                         verbatimTextOutput("rawFileDirectory",
                                            placeholder = TRUE),
                         tags$hr(size = 20)),
                  br(),
                  column(12, align = "center",
                         p(strong("3:", "Choose  your Sample Map file, the excel sheet that IDBac will use to rename your files.")),
                         fileInput('excelFile',
                                   label = NULL ,
                                   accept = c('.xlsx','.xls')),
                         tags$hr(size = 20)),
                  column(12, align = "center",
                         p(strong("4:","Click \"Process Data\" to begin spectra conversion.")),
                         actionButton("run",
                                      label = "Process Data"),
                         tags$hr(size = 20)),
                  br(),
                  br(),
                  br(),
                  br(),
                  p(strong("Note:","If you canceled out of the popup after spectra conversion completed, you can process your converted spectra using the button below: (but only after all files have been converted) This step is not necessary otherwise."))
           )
    )
  )
}


multipleMaldiPlates <- function(id){
  
  
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
                         textInput("newExperimentName",
                                   label = ""),
                         tags$hr(size = 20)),
                  fluidRow(
                    column(12,
                           verbatimTextOutput("newExperimentNameText",
                                              placeholder = TRUE))),
                  br(),
                  p(strong("2:"), "Your RAW data will be one folder that contains folders for each MALDI plate."),
                  br(),
                  p(strong("2: Click to select the location of your RAW data"), align= "center"),
                  actionButton("multipleMaldiRawFileDirectory",
                               label = "Click to select the location of your RAW data"),
                  fluidRow(column(12,
                                  verbatimTextOutput("multipleMaldiRawFileDirectory",
                                                     placeholder = TRUE))),
                  br(),
                  column(12, align = "center",
                         p(strong("4:","Click \"Process Data\" to begin spectra conversion.")),
                         actionButton("run",
                                      label = "Process Data"),
                         tags$hr(size = 20))
           )
    )
  )
}


beginWithMZ <- function(id){
  fluidRow(
    column(12, align = "center",
           h3("Starting with mzML or mzXML Data:"),
           
           column(2),
           column(8, style = "background-color:#7777770d", align = "center",
                  
                  br(),
                  column(12,align = "center",
                         p(strong("1: Enter a filename for this new experiment")),
                         p("Only numbers, \"_\", and A-Z. Shouldn't start with a number."),
                         textInput("newExperimentName",
                                   label = ""),
                         tags$hr(size=20)),
                  
                  br(),
                  p(strong("2: Click to select the location of your mzML files"), align= "center"),
                  column(12, align="center",
                         actionButton("mzmlRawFileDirectory",
                                      label = "Raw Data Folder"),
                         verbatimTextOutput("mzmlRawFileDirectory",
                                            placeholder = TRUE),
                         tags$hr(size = 20)),
                  br(),
                  p("Samples will be named according to the file name of the provided files"),
                  br(),
                  column(12, align = "center",
                         p(strong("4:","Click \"Process Data\" to begin spectra conversion.")),
                         actionButton("run",
                                      label = "Process Data"),
                         tags$hr(size = 20))
                  
           )
    )
  )
}

beginWithTXT <- function(id){
  fluidRow(
    p(".txt and .csv support coming soon!"),
    actionButton("delimitedDirectoryP",
                 label = "Raw Data P Folder"),
    actionButton("delimitedDirectorySM",
                 label = "Raw Data SM Folder"),
    actionButton("runDelim",
                 label = "Process Data"),
    verbatimTextOutput("delimitedLocationPo",
                       placeholder = TRUE),
    verbatimTextOutput("delimitedLocationSMo",
                       placeholder = TRUE)
  )
}



conversionsUI <- function(id){
  
  fluidPage(
    navlistPanel(widths = c(3, 7), id = "ConversionsNav",
                 "Create an IDBac experiment",
                 tabPanel(tags$ul(tags$li("Click here to convert Bruker files")),
                          value = "convert_bruker_nav",

                          
                            radioButtons("rawORreanalyze",
                                         label = h3("Begin by selecting an option below:"),
                                         choices = list("Select here to convert and analyze raw-data from a single MALDI-plate" = 1,
                                                        "Select here to convert and analyze raw-data from multiple MALDI-plates at once" = 2),
                                         selected = 0,
                                         inline = FALSE,
                                         width = "100%"),
                   mainPanel(
                            uiOutput("conversionMainUI1")
                   )
                          
                      
                            
                          
                          
                 ),
                 tabPanel(tags$ul(tags$li("Click here to convert mzML/mzXML files")),
                          value = "convert_mzml_nav",
                          fluidRow(
                            uiOutput("conversionMainUI2")
                          )
                          
                 ),
                 tabPanel(tags$ul(tags$li("Click here to convert txt files")),
                          value = "convert_txt_nav",
                          # radioButtons("rawORreanalyze",label = h3("Begin by selecting an option below:"),
                          #              choices = list("Select here if you want to use .txt peak list files" = 3,
                          #                             "Select here if you want to use .csv peak list files" = 4),
                          #              selected = 3,
                          #              inline = FALSE,
                          #              width = "100%"),
                          mainPanel(
                            uiOutput("conversionMainUI3")
                          )
                          
                 )
    ))}
                 
                          
                           