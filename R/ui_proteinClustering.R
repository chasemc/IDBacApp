#' ui_proteinClustering
#'
#' @return NA
#' @export
#'

ui_proteinClustering <- function() {
  
  fluidPage(
    sidebarLayout(
      
      sidebarPanel(style = "background-color:#7777770d", 
                   width = 4,
                   IDBacApp::bsCollapse(id = "collapseManSettings",
                                        open = "Panel 1",
                                        IDBacApp::bsCollapsePanel(p("Select Samples", 
                                                                    align = "center"),
                                                                  p("Move samples between boxes by clicking the samples's name
                       and then an arrow. Samples in the right box will be used for analysis."),
                                                                  IDBacApp::sampleChooser_UI("proteinSampleChooser")
                                        )
                   ),
                   IDBacApp::bsCollapse(id = "collapseManSettings",
                                        open = "Panel 1",
                                        IDBacApp::bsCollapsePanel(p("Choose Clustering Settings", 
                                                                    align = "center"),
                                                                  IDBacApp::dendrogramCreatorUI("proteinHierOptions")
                                        )
                   ),
                   IDBacApp::bsCollapse(id = "collapseManSettings",
                                        open = "Panel 1",
                                        IDBacApp::bsCollapsePanel(p("Adjust the Dendrogram", 
                                                                    align = "center"),
                                                                  numericInput("hclustHeight",
                                                                               label = h5(strong("Expand Tree")),
                                                                               value = 750,
                                                                               step = 50,
                                                                               min = 100),
                                                                  numericInput("dendparmar",
                                                                               label = h5(strong("Adjust right margin of dendrogram")),
                                                                               value = 20),
                                                                  fluidRow(IDBacApp::colordendLabelsUI("proth")),
                                                                  fluidRow(IDBacApp::colordendLinesUI("proth")),
                                                                  fluidRow(IDBacApp::addDotsActionUI("proth"))
                                        )
                   ),
                   
                   IDBacApp::bsCollapse(id = "collapseManSettings",
                                        open = "Panel 1",
                                        IDBacApp::bsCollapsePanel(p("Insert Samples from Another Experiment", 
                                                                    align = "center"),          
                                                                  IDBacApp::selectInjections_UI("proteinInject")
                                        )
                   ),
                   br(),
                   h4("Suggestions for Reporting Protein Analysis:"),
                   uiOutput("proteinReport")
                   
      ),
      
      
      mainPanel(
        
        IDBacApp::plotHier("proth"),
        IDBacApp::dendDotsUI("proth")
        
        
      )
    )
  )
}



