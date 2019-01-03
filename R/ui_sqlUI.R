ui_sqlUI <- function(id, availableExperiments) {
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Instructions:", align="center"),
        tags$b("What is an \"experiment\" in IDBac?"),
        tags$ul(
          tags$li("A user-defined group of samples that were analyzed by MALDI MS."),
          tags$li("Physically, each experiment is a separate \"SQLite\" database that can be shared between colleagues
                                         or submitted alongside a manuscript."),
          tags$li("Experiments contain the converted mzML file, instrument and data collection info (if found), 
                                         processed spectra, as well as any user-input sample information.")),
        tags$b("What does this mean for me?"),
        
        tags$ul(
          tags$li("Experiments are how you organize your data."),
          tags$li("Experiments should only contain the samples you want to analyze."),
          tags$li("It is possible to \"mix and match\" samples from different experiments to create new experiments.")
          
        ),
        tags$b("Begin analysis by selecting an available experiment to the right.")
      ),        
      
      mainPanel(
        fluidRow(
          #    h3("Select an experiment to analyze"),
          column(width = 5,
                 offset = 2,
                 #style = "background-color:#7777770d",
                 align = "center",
                 selectInput("selectExperiment",
                             label = h3("First, select an experiment:"),
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
        br(),
        p("You also have the option, below, to:"),
        tags$ul(
          tags$li("Create new experiments using data frfom previous experiments"),
          tags$li("Add information about strains (for coloring plots later, or just as a record)")
          
        ),
        tabsetPanel(id = "ExperimentNav",
                    tabPanel("Create an experiment, pulling samples from previous experiments",
                             value = "experiment_mixMatch_tab",
                             column(12, align = "center",
                                    style = "background-color: #7777770d",
                                    offset = 1,
                                    h3("Transfer samples from previous experiments to new/other experiments.", align="center"),
                                    p("Note: For data integrity, samples cannot be removed from experiments.", align= "center"),
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
                    
                    tabPanel("Add/modify information about samples",
                             value = "experiment_metaData_tab",
                             
                             
                             # actionButton("searchNCBI",
                             #              "Search NCBI"),
                             actionButton("saven",
                                          "save"),
                             
                             rHandsontableOutput("metaTable", height=800)
                    )
                    
    
                    
        )
        

      )
      
      
      
    ))
}

