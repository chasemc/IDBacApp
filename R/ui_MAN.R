# Create MAN UI
#----

#' ui_smallMolMan
#'
#' @return UI
#' @export
#'
ui_smallMolMan <-  function(){
  
  fluidPage(sidebarLayout(
    
    
    sidebarPanel(style = 'padding:30px',
                 IDBacApp::bsCollapse(id = "collapseManSettings",
                                      open = "Panel 1",
                                      IDBacApp::bsCollapsePanel(p("Show/Hide MAN Settings", 
                                                                  align = "center"),
                                                                numericInput("percentPresenceSM",
                                                                             label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%) (Experiment/Hypothesis dependent)"),
                                                                             value = 70,
                                                                             step = 10,
                                                                             min = 0,
                                                                             max = 100),
                                                                numericInput("smSNR",
                                                                             label = h5(strong("Signal To Noise Cutoff")),
                                                                             value = 4,
                                                                             step = 0.5,
                                                                             min = 1.5,
                                                                             max = 100),
                                                                numericInput("upperMassSM",
                                                                             label = h5(strong("Upper Mass Cutoff")),
                                                                             value = 2000,
                                                                             step = 20,
                                                                             max = 3000),
                                                                numericInput("lowerMassSM",
                                                                             label = h5(strong("Lower Mass Cutoff")),
                                                                             value = 200,
                                                                             step = 20,
                                                                             min = 3000)
                                      )),
                 uiOutput("matrixSelector"),
                 IDBacApp::bsCollapse(id = "collapseManSettings",
                                      open = "Panel 1",
                                      IDBacApp::bsCollapsePanel(p("Adjust Protein Dendrogram", 
                                                                  align = "center"),
                                                                numericInput("hclustHeightNetwork",
                                                                             label = h5(strong("Expand Tree")),
                                                                             value = 750,
                                                                             step = 50,
                                                                             min = 100),
                                                                numericInput("dendparmar2",
                                                                             label = h5(strong("Adjust right margin of dendrogram")),
                                                                             value = 5)
                                      )),
                 div(align = "center",
                     IDBacApp::downloadSmNet_UI("smMAN")
                 ),
                 br(),
                 p(strong("Hint 1:"),
                   "Use mouse to select parts of the tree and display the MAN of corresponding samples."),
                 p(strong("Hint 2:"),
                   "Use mouse to click & drag parts (nodes) of the MAN if it appears congested."),
                 br(),
                 
                 p(strong("Note 1:"), "For publication-quality networks click \"Download Current Network Data\".
                   This will download a .csv file of the currently-displayed
                   network that can be easily imported into Gephi or Cytoscape."),
                 br(),
                 h4("Suggestions for Reporting MAN Analysis:"),
                 uiOutput("manReport")
                 
                 
    ),
    mainPanel(
      column(width = 5,

             
                           tabsetPanel(type = "tabs",           
                                       tabPanel(value = "protd","Protein Dendrogram",
                                               
                                                manPageProtDend_UI("manProtDend")
             
             
      ))),
      column(width = 6,
        
                           tabsetPanel(type = "tabs",           
                                       tabPanel(value = "smallMolMANUI","Small Molecule MAN",
                                                IDBacApp::smMANPlot_UI("smMAN")),
                                       tabPanel(value = "smallMolPCAUi","SmallMolecule PCA",
                                                
                                                IDBacApp::pca_UI("smallMolPcaPlot")
                                       )               
                           
                           
                           
             ))
    )
  ))
  
  
}












