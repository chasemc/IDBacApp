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
                 radioButtons("matrixSamplePresent",
                              label = h5(strong("Do you have a matrix blank?")),
                              choices = list("Yes" = 1, 
                                             "No (Also Turns Off Matrix Subtraction)" = 2),
                              selected = 1),
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
                              min = 3000),
                 numericInput("hclustHeightNetwork",
                              label = h5(strong("Expand Tree")),
                              value = 750,
                              step = 50,
                              min = 100),
                 numericInput("dendparmar2",
                              label = h5(strong("Adjust right margin of dendrogram")),
                              value = 5),
                 
                 IDBacApp::downloadSmNet_UI("smMAN"),
                 br(),
                 p(strong("Hint 1:"),
                   "Use mouse to select parts of the tree and display the MAN of corresponding samples."),
                 p(strong("Hint 2:"),
                   "Use mouse to click & drag parts (nodes) of the MAN if it appears congested."),
                 br(),
                 
                 p(strong("Note 1:"), "For publication-quality networks click \"Download Current Network.\"
                   while selected- this saves a .csv file of the currently-displayed
                   network to the \"Saved_MANs\" folder in your working directory This can be easily imported into Gephi or Cytoscape.
                   While checked, any update of the network will overwrite this file. Also, an error saying: \"cannot open the connection\"
                   means this box is checked and the file is open in another program, either uncheck or close the file."),
                 br(),
                 h4("Suggestions for Reporting MAN Analysis:"),
                 uiOutput("manReport"),
                 br(),
                 h4("Suggestions for Reporting Protein Analysis"),
                 uiOutput("proteinReport2")
                 ),
    mainPanel(
      column(width = 5,
             manPageProtDend_UI("manProtDend")
      ),
      column(width = 6,
             style = "padding: 14px 0px; margin:0%",
             absolutePanel(fixed = TRUE,
                           width = "50%",
                           tabsetPanel(type = "tabs",           
                                       tabPanel(value = "smallMolMANUI","MAN",
                                                IDBacApp::smMANPlot_UI("smMAN")),
                                       tabPanel(value = "smallMolPCAUi","PCA",
                                               
                                                IDBacApp::pca_UI("smallMolPcaPlot")
                                       ))
                           
                           
                           
             ))
    )
  ))


}












