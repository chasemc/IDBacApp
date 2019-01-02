ui_sqlUI <- function(id, availableExperiments) {

  fluidPage(
    navlistPanel(widths = c(3, 7), id = "ExperimentNav",
                 "Introduction to Experiments:",
                 tabPanel(tags$ul(tags$li("Introduction to Experiments")),
                          value = "experiment_info_tab",
                          column(width = 12, 
                                 offset = 1,
                                 style = "background-color:#7777770d",
                                 h3("What is an \"experiment\" in IDBac?", align = "center"),
                                 tags$ul(
                                   tags$li("A user-defined group of samples that were analyzed by MALDI MS..."),
                                   tags$li("Physically, each experiment is a separate \"SQLite\" database that can be shared between colleagues
                                         or submitted alongside a manuscript."),
                                   tags$li("Experiments contain the converted mzML file, instrument and data collection info (if found), 
                                         processed spectra, as well as any user-input sample information.")),
                                 h3("What does this mean for me?", align = "center"),
                                 
                                 tags$ul(
                                   tags$li("Experiments are how you organize your data."),
                                   tags$li("Experiments should only contain the samples you want to analyze."),
                                   tags$li("It is possible to \"mix and match\" samples from different experiments to create new experiments.")
                                   
                                 ),
                                 h3("Begin analysis by selecting an available experiment:", align = "center"),
                                 column(12,align="center",actionButton("moveToSelectExperiment",
                                                                       "Click to select an experiment to analyze"))
                          )),
                 "Select/Create Experiments",
                 tabPanel(tags$ul(tags$li("Select an experiment to analyze")), 
                          value = "experiment_select_tab",
                          
                          
                          column(width = 12, 
                                 offset = 1,
                                 style = "background-color:#7777770d",
                                 align = "center",
                                 selectInput("selectExperiment",
                                             label = h3("Select an experiment to analyze:"),
                                             choices = availableExperiments,
                                             selected = availableExperiments[[1]],
                                             width= "100%"
                                 ),
                                 actionButton("moveToAnalysis",
                                              "Click to start analysis"),
                                 
                                 p("Location of experiment file:", align = "center"),
                                 verbatimTextOutput("selectedSQLText",
                                                    placeholder = TRUE))
                 ),
                 tabPanel(tags$ul(tags$li("Create an experiment, pulling samples from previous experiments")), 
                          value = "experiment_mixMatch_tab",
                          column(12, align = "center",
                                 style = "background-color: #7777770d",
                                 offset = 1,
                                 h3("Transfer samples from previous experiments to new/other experiments.", align="center"),
                                 p("Note: For data integrity, samples cannot be removed from experiments.", align= "center"),
                                 selectInput("selectMixNmatchExperiment",
                                             label = "Available Experiments:",
                                             choices = availableExperiments,
                                             selected = availableExperiments[[1]]),
                                 
                                 p("Move strains between boxes by clicking the strain's name
                                 and then an arrow. Strains in the right box will be used for analysis."),
                                 uiOutput("chooseNewDBSamples"),
                                 verbatimTextOutput("selection"),
                                 br(),
                                 textInput("nameformixNmatch",
                                           label = "Enter name for new experiment"),
                                 
                                 actionButton("addtoNewDB", "Add to new Experiment")
                          )
                 ),
                 "Modify Sample Info",
                 tabPanel(tags$ul(tags$li("Add/modify information about samples")), 
                          id = "experiment_metaData_tab",
                          
                          selectInput("selectExperimentforMeta",
                                      label = "Available Experiments:",
                                      choices = availableExperiments,
                                      selected = availableExperiments[[1]]),
                          # actionButton("searchNCBI",
                          #              "Search NCBI"),
                          actionButton("saven",
                                       "save"),
                          
                          rHandsontableOutput("metaTable", height=800)
                 )
    ))
  
}