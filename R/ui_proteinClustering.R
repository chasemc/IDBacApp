#' ui_proteinClustering
#'
#' @return NA
#' @export
#'

ui_proteinClustering <- function() {
  
  fluidPage(
    sidebarLayout(
      
      sidebarPanel(style = "background-color:#7777770d", 
                   width = 3,
                   selectInput("proteinUIselector",
                               label = "Available options:",
                               choices = list("Hierarchical Clustering Settings" = 1,
                                              "Show PCA, PCoA, t-SNE" = 2,
                                              "Insert samples from another experiment" = 3)),
                   
                   
                   # Hierarchical Clustering Settings
                   #----
                   conditionalPanel(
                     condition = "input.proteinUIselector == '1'",
                     h5(strong("Select Samples")),
                     p("Move samples between boxes by clicking the samples's name
                       and then an arrow. Samples in the right box will be used for analysis."),
                     IDBacApp::sampleChooser_UI("proteinSampleChooser")
                   ),
                   IDBacApp::dendrogramCreatorUI("proteinHierOptions"),
                   numericInput("hclustHeight",
                                label = h5(strong("Expand Tree")),
                                value = 750,
                                step = 50,
                                min = 100),
                   numericInput("dendparmar",
                                label = h5(strong("Adjust right margin of dendrogram")),
                                value = 20),
                   
                   
                   IDBacApp::addDotsActionUI("proth"),
                   IDBacApp::colordendLabelsUI("proth"),
                   IDBacApp::colordendLinesUI("proth"),
                   br(),
                   IDBacApp::selectInjections_UI("proteinInject"),
                   br(),
                   h4("Suggestions for Reporting Protein Analysis:"),
                   uiOutput("proteinReport")
                   # shiny::absolutePanel(
                   #   bottom = "20%",
                   #   right =  "0%",
                   #   width = "40%",
                   #   ##   width = "20%",
                   #   fixed = T,
                   #   draggable = F,
                   #   style = "z-index:1002;",
                   #   style = "opacity: 1",
                   #   shiny::wellPanel( IDBacApp::bsCollapse(id = "collapsInstructions",
                   #                                          open = "Panel 1",
                   #                                          IDBacApp::bsCollapsePanel(h4("Open\\Close Instructions", 
                   #                                                                       align = "center"),
                   #                                                                    tabsetPanel(id = "ProteinScatterPlots", 
                   #                                                                                tabPanel("PCA",
                   #                                                                                         value = "proteinPCA",                    
                   #                                            IDBacApp::pca_UI("proteinPCA")))
                   #                                          )
                   #                     
                   #   )
                   #   )
                   #   )
                     ),
      
      
      mainPanel(

        IDBacApp::plotHier("proth"),
       IDBacApp::dendDotsUI("proth")
       
        
      )
    )
  )
}



