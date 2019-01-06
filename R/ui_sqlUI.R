ui_sqlUI <- function(id, availableExperiments) {
  
  fluidPage(
    fluidRow(
      column(width=4,
             fluidRow(
               # bsCollapse(id = "collapseSQLSelector", open = "Panel 1",
               #            bsCollapsePanel(h4("Click here to select an experiment", align = "center"),  
                                           fluidRow(
                                            column(width = 12,
                                                   align = "center",
                                                   selectInput("selectExperiment",
                                                               label = h3("First, select an experiment:"),
                                                               choices = availableExperiments,
                                                               selected = availableExperiments[[1]],
                                                               width= "50%"
                                                   ),
                                                   actionButton("moveToAnalysis",
                                                                "Click here to begin analysis"),
                                                   
                                                   p("Location of experiment file:", align = "center"),
                                                   verbatimTextOutput("selectedSQLText",
                                                                      placeholder = TRUE)
                                            )
                          #                 )
                          # )
               )
             ),
             
             fluidRow(
               bsCollapse(id = "collapseSQLInstructions", open = "Panel 1",
                          bsCollapsePanel(h4("Open/Close Instructions", align = "center"),
                                          tags$b("What is an \"experiment\" in IDBac?"),
                                          tags$ul(
                                            tags$li("A user-defined group of samples that were analyzed by MALDI MS."),
                                            tags$li("Physically, each experiment is a separate \"SQLite\" database that can be shared between colleagues
                                                  or submitted alongside a manuscript."),
                                            tags$li("Experiments contain the converted mzML file, instrument and data collection info (if found), 
                                                  processed spectra, as well as any sample information input into IDBac by you.")),
                                          tags$b("What does this mean for me?"),
                                          
                                          tags$ul(
                                            tags$li("Experiments are how you organize your data."),
                                            tags$li("Experiments should only contain the samples you want to analyze."),
                                            tags$li("It is possible to \"mix and match\" samples from different experiments to create new experiments.")
                                          ),
                                          tags$b("Begin analysis by selecting an available experiment to the right."), 
                                          br(),
                                          br(),
                                          tags$b("You also have the option, below, to:"),
                                          tags$ul(
                                            tags$li("Create new experiments using data frfom previous experiments"),
                                            tags$li("Add information about strains (for coloring plots later, or just as a record)")
                                            
                                          )
                          )
                          
               )
             )
      ),
      
      
      column(width = 8,
             
             
             bsCollapse(id = "modifySqlCollapse",
                        bsCollapsePanel(h4("Click here to modify the selected experiment", align = "center"),  
                                        
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
                        
             )
      )
      
      
    )
  )
}

