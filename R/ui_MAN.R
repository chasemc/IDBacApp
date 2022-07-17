# Create MAN UI
#----

#' ui_smallMolMan
#'
#' @return UI
#' 
#'
ui_smallMolMan <-  function(){
  
  fluidPage(
    sidebarLayout(
      
      
      sidebarPanel(width = 3,
                   style = 'padding:30px',
                   bsCollapse(id = "collapseManSettings",
                                        open = "Panel 1",
                                        bsCollapsePanel(p("Show/Hide MAN Settings", 
                                                                    align = "center"),
                                                                  div(align = "center",
                                                                      peakRetentionSettings_UI("smallMirror",
                                                                                                         min_mass = 200, 
                                                                                                         max_mass = 2000)
                                                                  )
                                        )),
                   uiOutput("matrixSelector"),
                   bsCollapse(id = "collapseManSettings",
                                        open = "Panel 1",
                                        bsCollapsePanel(p("Adjust MAN", 
                                                                    align = "center"),
                                                                  colorMANBy_UI("smMAN"),
                                                                  saveNetSVG("smMAN")
                                        )),
                   bsCollapse(id = "collapseManSettings",
                                        open = "Panel 1",
                                        bsCollapsePanel(p("Adjust Protein Dendrogram", 
                                                                    align = "center"),
                                                                  numericInput("hclustHeightNetwork",
                                                                               label = h5(strong("Expand Tree")),
                                                                               value = 750,
                                                                               step = 50,
                                                                               min = 100),
                                                                  numericInput("dendparmar2",
                                                                               label = h5(strong("Adjust right margin of dendrogram")),
                                                                               value = 5)
                                        )
                   ),
                   div(align = "center",
                       downloadSmNet_UI("smMAN")
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
      mainPanel(width = 9,
                
                absolutePanel(width = "25%",
                              fixed = FALSE,
                              tabsetPanel(type = "tabs",           
                                          tabPanel(value = "protd","Protein Dendrogram",
                                                   
                                                   manPageProtDend_UI("manProtDend")
                                          )      
                              )
                ),
                absolutePanel(
                  top = 70,
                  bottom = 0,
                  fixed = TRUE,
                  left = "50%",
                  right = 0,
                  tabsetPanel(type = "tabs",   
                              tabPanel(value = "smallMolMANUI","Small Molecule MAN",
                                       p("Nodes in the MAN are colored by modularity score, not the same colors as the protein dendrogram.",
                                         style = "font-size: 0.75em"),
                                       verbatimTextOutput("noSmallPeaksText"),
                                       smMANPlot_UI("smMAN")),
                              tabPanel(value = "smallMolPCAUi","Small Molecule PCA",
                                       
                                       plotly::plotlyOutput("smallMolPcaPlot")
                              ),
                              tabPanel(value = "smallMirror", "Small Molecule Mirror Plot",
                                       smallmirrorPlotsSampleSelect_UI("smallMirror"),
                                       smallmirrorPlots_UI("smallMirror"))
                  )
                )
      )
    )
  )
}







