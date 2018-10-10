# Add "packageLibrary" to libraries


#----------
# Changes Zenodo DOI
zenodoId <- "1203781"


# The UI section of the Shiny app provides the "User Interface" and is the means of user interaction.
# "h3", "p", and "br" are HTML,the other lines setup the different panels, tabs, plots, etc of the user interface.
#tags$body(background = "rotatingPCA.gif",
navbarPage(collapsible = T, theme = "css.css",
           "IDBac",
           tabPanel(
             "Introduction",
             fluidPage(
               shinyjs::useShinyjs(),
               fluidRow(
                 column(3,
                        br(),
                        div(img(src = "IDBac_Computer_SVG_300DPI.png",
                                style = "width:75%;height:75%")
                            )
                        ),
                 column(7,
                        h1("Welcome to IDBac",
                           align = "center"),
                        br(),
                        h4("General Information:",
                           align = "center"),
                        tags$ul(
                          tags$li(
                            p("For more information about the method, as well as links to the full code, please visit ", 
                              a(href = "https://chasemc.github.io/IDBac/",
                                target = "_blank", "chasemc.github.io/IDBac")
                              )
                            ),
                          tags$li(
                            p("Bugs and suggestions may be reported on the ",
                              a(href = "https://github.com/chasemc/IDBacApp/issues",
                                target = "_blank","IDBac Issues Page on GitHub", 
                                img(border = "0",
                                    title = "https://github.com/chasemc/IDBacApp/issues",
                                    src = "GitHub.png",
                                    width = "25",
                                    height = "25")
                                )
                              )
                            ),
                          tags$li(actionButton("updateIDBac",
                                               label = "Check for Updates")
                                  )
                          ),
                        br(),
                        h4("How to Cite IDBac:",
                           align = "center"),
                        tags$ul(
                          tags$li(
                            p("Publication:")),
                          tags$ul(
                            tags$li(
                              p(("Coupling MALDI-TOF mass spectrometry protein and specialized metabolite analyses to rapidly discriminate bacterial function"),br(),
                                "Chase M. Clark, Maria S. Costa, Laura M. Sanchez, Brian T. Murphy;
                                PNAS May 8, 2018 115 (19) 4981-4986; doi:",
                                a(href = "https://doi.org/10.1073/pnas.1801247115", 
                                  target = "_blank", "10.1073/pnas.1801247115")
                                )
                              )
                            )
                          ),
                        tags$ul(
                          tags$li(
                            p("Software:")
                          ),
                          tags$ul(
                            tags$li(
                              p("For reproducibility, cite this specific version of IDBac as version:",
                                a(href = paste0("http://doi.org/10.5281/zenodo.",
                                                zenodoId),
                                  target = "_blank",
                                  img(border = "0",
                                      title = "Click to go to Zenodo",
                                      src = "https://zenodo.org/badge/DOI/10.5281/zenodo.1185404.svg")
                                  )
                                )
                              )
                            )
                          ),
                        br(),
                        h4("Select \"PreProcessing\" above to begin", 
                           align="center")
                        )
                 )
               )
             ),
           tabPanel(
             "Starting With Raw Data",
             fluidPage(
               fluidRow(style = "background-color:#7777770d",
                        column(width = 3,
                               radioButtons("startingWith",
                                            label = "",
                                            choices = list("Starting with raw data files" = 1,
                                                           "Starting with peak list files (.txt or .csv)" = 2),
                                            selected = 0,
                                            inline = FALSE,
                                            width = "100%")
                               ),
                        column(width = 1,
                               fluidRow(
                                 
                                 ),
                               fluidRow(
                                 uiOutput("arrowPNG")
                                 ),
                               fluidRow(
                                 
                                 )
                               ),
                        column(width = 4,
                               uiOutput("startingWithUI")
                               )
                        ), 
               fluidRow(
                 tags$hr(size = 20)
                 ),
               fluidRow(
                 uiOutput("ui1")
                 )
               )
             ),
           tabPanel("SQL",
                    uiOutput("sqlUI")
                    ),
           tabPanel("Compare Two Samples (Protein)",
                    uiOutput("inversepeakui")
                    ),
           tabPanel("Hierarchical Clustering (Protein)",
                    uiOutput("Heirarchicalui")
                    ),
           tabPanel("PCA (Protein)",
                    uiOutput("PCAui")
                    ),
           tabPanel("Metabolite Association Network (Small-Molecule)",
                    uiOutput("MANui")
                    ),
           tabPanel("In-House Spectra Libraries",
                    uiOutput("libraryTab")
                    )
           )
