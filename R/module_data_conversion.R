#' convertDataTab_UI
#'
#' @param id namespace
#'
#' @return ui
#' @export
#'
convertDataTab_UI <- function(id) {
  ns <- shiny::NS(id)
  
  navlistPanel(
    widths = c(3, 8),
    id = ns("ConversionsNav"),
    "Create an IDBac experiment",
    tabPanel(
      tags$ul(
        tags$li("Click here to convert Bruker files",
                align = "left")),
      value = ns("convert_bruker_nav"),
      uiOutput(ns("brukerConversionUi"))
      
    ),
    tabPanel(
      tags$ul(
        tags$li("Click here to convert mzML/mzXML files", align="left")),
      value = ns("convert_mzml_nav"),
      wellPanel(class = "intro_WellPanel",
                align = "center",
                IDBacApp::convertMZ_UI(ns("beginWithMZ"))
      )
    ),
    tabPanel(
      tags$ul(
        tags$li("Click here to convert txt files", align = "left")),
      value = ns("convert_txt_nav"),
      wellPanel(class = "intro_WellPanel",
                align = "center",
                IDBacApp::convertDelim_UI(ns("convertDelim"))
      )
    ),
    tabPanel(
      tags$ul(
        tags$li("Click here to convert Microtyper files", align = "left")),
      value = ns("convert_microtyper_nav"),
      wellPanel(class = "intro_WellPanel",
                align = "center",
                IDBacApp::convertMicrotyper_UI(ns("convertMicrotyper"))
      )
    )
  )
}




#' convertDataTab_Server
#'
#' @param input shiny
#' @param output  shiny
#' @param session  shiny
#' @param tempMZDir  directory to create temp mzML if needed
#' @param sqlDirectory  where to write new SQLite file
#' @param availableExperiments update availableExperiments
#' @param pwizAvailable whether msconvert was found, logical
#'
#' @return none, updates availableExperiments reactive value though
#' @export
#'

convertDataTab_Server <- function(input,
                                  output,
                                  session,
                                  tempMZDir,
                                  sqlDirectory, 
                                  availableExperiments,
                                  pwizAvailable){
  
  
  output$brukerConversionUi <- renderUI({
    IDBacApp::controlBrukerDisplay(session,
                                   pwizAvailable)
  })
  
  shiny::callModule(convertMZ_Server,
                    "beginWithMZ",
                    sqlDirectory = sqlDirectory,
                    availableExperiments = availableExperiments)
  
  
  shiny::callModule(convertOneBruker_Server,
                    "convertOneBruker",
                    tempMZDir = tempMZDir,
                    sqlDirectory = sqlDirectory,
                    availableExperiments = availableExperiments)
  
  
  
  shiny::callModule(convertDelim_Server,
                    "convertDelim",
                    tempMZDir = tempMZDir,
                    sqlDirectory = sqlDirectory,
                    availableExperiments = availableExperiments)
  
  
  shiny::callModule(convertMicrotyper_Server,
                    "convertMicrotyper",
                    tempMZDir = tempMZDir,
                    sqlDirectory = sqlDirectory,
                    availableExperiments = availableExperiments)
  
  
  
  
}







#' multipleMaldiPlates
#'
#' @param id NA
#'
#' @return NA
#' @export
#'

multipleMaldiPlates <- function(id){
  ns <- shiny::NS(id)
  fluidRow(
    column(12,
           br(),
           br(),
           fluidRow(
             column(12,offset = 3,
                    h3("Starting with Multiple MALDI Plates of raw data"))), br(), br(),
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
                  p(strong("2:"),"The raw data file will be one folder that contains individual folders for each
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
                  p(strong("2:"), "Your raw data will be one folder that contains folders for each MALDI plate."),
                  br(),
                  p(strong("2: Click to select the location of your raw data"), align = "center"),
                  actionButton(ns("multipleMaldiRawFileDirectory"),
                               label = "Click to select the location of your raw data"),
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



#' Controls display of Bruker conversion page
#'
#' @param pwizAvailable whether msconvert was found
#' @param session shiny session
#' @param ostest for testing function
#'
#' @return html
#' @export
#'
controlBrukerDisplay <- function(session, 
                                 pwizAvailable, 
                                 ostest = NULL){
  if (IDBacApp::getOS(test = ostest) != "windows") {
    wellPanel(class = "intro_WellPanel",
              align = "center",
              wellPanel(class = "intro_WellPanel",
                        align = "center",
                        p("Converting Bruker raw data requires msconvert.exe, which is only available for windows-computers. However, you can begin with any of the other input options.")
              )
    )
  } else if (pwizAvailable != "error") {
    wellPanel(class = "intro_WellPanel",
              align = "center",
              IDBacApp::convertOneBruker_UI(session$ns("convertOneBruker"))
    )
  } else {
    wellPanel(class = "intro_WellPanel",
              align = "center",
              wellPanel(class = "intro_WellPanel",
                        align = "center",
                        p("Msconvert.exe not found, please install proteowizard (http://proteowizard.sourceforge.net/download.html#Installation) or select another input option")
              )
    )
  }
}
