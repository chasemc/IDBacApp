
#' Main UI of IDBac
#'
#' @return Main UI of IDBac
#' @export
#'
app_ui <- function(){
  
  #----------
  # Changes Zenodo DOI
  #zenodoId <- "1203781"
  
  # The UI section of the Shiny app provides the "User Interface" and is the means of user interaction.
  # "h3", "p", and "br" are HTML,the other lines setup the different panels, tabs, plots, etc of the user interface.
  #tags$body(background = "rotatingPCA.gif",
  navbarPage(collapsible = T,
             # theme = "css.css",
             title = "IDBac",
             id = "mainIDBacNav",
             tabPanel(
               title = "Introduction",
               div(class = "introPageImage",
                   
                   div(class = "introPage",
                       #includeCSS(file.path(system.file('www', package = 'IDBacApp'), 'style.css')),
                       tags$head(tags$link(href = "www/styles.css", rel = "stylesheet")),
                       tags$head(tags$link(href = "www/collapse/shinyBS.js")),
                       tags$head(tags$link(href = "www/collapse/shinyBS.css")),
                       shinyjs::useShinyjs(),
                                wellPanel(class = "intro_WellPanel",
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
                                                  target = "_blank","IDBac issues page on GitHub", 
                                                  img(border = "0",
                                                      title = "https://github.com/chasemc/IDBacApp/issues",
                                                      src = "www/GitHub.png",
                                                      width = "25",
                                                      height = "25")
                                                )
                                              )
                                            ),
                                            tags$li(
                                              p("For discussions and general questions please use the ",
                                                a(href = "https://groups.google.com/forum/#!forum/idbac",
                                                  target = "_blank","IDBac Google group forum"
                                                )
                                              )
                                            ),
                                            column(width = 12,
                                                   align = "center",
                                                   actionButton("updateIDBac",
                                                                label = "Check for Updates",
                                                                align = "center")
                                            )
                                          ),
                                          br(),
                                          br(),
                                          tags$ul(
                                            tags$li(
                                              p("Publication:")),
                                            tags$ul(
                                              tags$li(
                                                
                                                div(id = 'pnasTitle',
                                                    class = 'publicationTitle',
                                                    "Coupling MALDI-TOF mass spectrometry protein and specialized metabolite analyses to rapidly discriminate bacterial function"),
                                                div(id = 'pnasauthors',
                                                    class = 'publicationAuthors',
                                                    "Chase M. Clark, Maria S. Costa, Laura M. Sanchez, Brian T. Murphy"),
                                                div(id = 'pnasDetails',
                                                    class = 'publicationDetails',
                                                    "PNAS May 8, 2018 115 (19) 4981-4986; doi:",
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
                                                                  "zenodoId"),
                                                    target = "_blank",
                                                    shiny::HTML('<a href="https://doi.org/10.5281/zenodo.1410518" target="_blank"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.1410518.svg" alt="DOI"></a>')
                                                  )
                                                )
                                              )
                                            )
                                          ),
                                          br(),
                                          h4("Use the navigation bar at the top of the page to begin", 
                                             align = "center"),
                                          div(align = "center",
                                              IDBacApp::selectDirectory_UI("userWorkingDirectory",
                                                                           label = "Select IDBac Data Storage Location"),
                                              verbatimTextOutput("userWorkingDirectoryText")
                                          )
                                )
                   )
                       
                   
               ) 
             ),
             tabPanel(
               "Starting with Raw Data",
               value = "rawDataUiTab",
               IDBacApp::convertDataTab_UI("convertDataTab"))
             
  )
}
