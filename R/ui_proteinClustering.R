#' ui_proteinClustering
#'
#' @return NA
#' @export
#'

ui_proteinClustering <- function() {
  
  
  # in hier -> delineatwe settings menu vs optional
  fluidPage(
    # sidebarLayout(
    
    sidebarPanel(style = "background-color:#7777770d", 
                 width = 4,
                 IDBacApp::bsCollapse(id = "proteinPeakSettingsDropDown",
                                      open = "Panel 1",
                                      IDBacApp::bsCollapsePanel(title = div(p("Choose How Peaks are Retained for Analayses", align = "center"),
                                                                            p("(Effects all protein analysis)",
                                                                              style = "font-size: 0.75em",
                                                                              align = "center")), 
                                                                value = "proteinPeakSettingsDropDown2",
                                                                div(align = "center",
                                                                    IDBacApp::peakRetentionSettings_UI("protMirror")
                                                                )
                                                                
                                                                
                                      ),
                                      IDBacApp::bsCollapsePanel(p("Select Samples", 
                                                                  align = "center"),
                                                                value = "selectProteinSamplesDropDown",
                                                                p("Move samples between boxes by clicking the samples's name
                                                                    and then an arrow. Samples in the right box will be used for analysis."),
                                                                IDBacApp::sampleChooser_UI("proteinSampleChooser")
                                                                
                                      ),
                                      IDBacApp::bsCollapsePanel(p("Choose Clustering Settings", 
                                                                  align = "center"),
                                                                value = "proteinClustSettingsDropDown",
                                                                IDBacApp::dendrogramCreatorUI("proteinHierOptions")
                                                                
                                      ),
                                      IDBacApp::bsCollapsePanel(p("Adjust the Dendrogram", 
                                                                  align = "center"),
                                                                value = "adjustProteinDendDropDown",
                                                                numericInput("hclustHeight",
                                                                             label = h5(strong("Expand dendrogram")),
                                                                             value = 750,
                                                                             step = 50,
                                                                             min = 100),
                                                                numericInput("dendparmar",
                                                                             label = h5(strong("Adjust right margin of dendrogram")),
                                                                             value = 20),
                                                                radioButtons("dendOrPhylo", 
                                                                             label = p("Label Positions:"),
                                                                             choices = list("Plot all labels at x = 0" = "Dendrogram", 
                                                                                            "Hang labels" = "Phylogram"),
                                                                             selected = "Dendrogram"),
                                                                fluidRow(IDBacApp::colordendLabelsUI("proth")),
                                                                fluidRow(IDBacApp::colordendLinesUI("proth")),
                                                                fluidRow(IDBacApp::addDotsActionUI("proth"))
                                                                
                                      ),
                                      IDBacApp::bsCollapsePanel(p("Insert Samples from Another Experiment", 
                                                                  align = "center"),     
                                                                value = "proteinInjectDropDown",
                                                                IDBacApp::selectInjections_UI("proteinInject")
                                                                
                                      ),
                                      IDBacApp::bsCollapsePanel(p("PCA, PCoA, t-SNE", 
                                                                  align = "center"),   
                                                                value = "proteinMuliDimDropDown",
                                                                p("Principal Component Analysis (PCA)"),
                                                                IDBacApp::popupPlot_UI("proteinPCA", "PCA"),
                                                                
                                                                p("Principal Coordinates Analysis (PCoA)"),
                                                                IDBacApp::popupPlot_UI("proteinPCOA", "PCoA"),
                                                                
                                                                p("t-Distributed Stochastic Neighbor Embedding (t-SNE)"),
                                                                IDBacApp::popupPlotTsne_UI("tsnePanel")
                                                                
                                                                
                                                                
                                      ),
                                      IDBacApp::bsCollapsePanel(p("Save Dendrogram", 
                                                                  align = "center"),   
                                                                value = "proteinDendSaveDropDown",
                                                                
                                                                IDBacApp::downloadHier("proth"),
                                                                IDBacApp::downloadSvg("proth")
                                      )
                 ),
                 br(),
                 uiOutput("proteinReport")
                 
    ),
    
    mainPanel(
    
    tabsetPanel(type = "tabs",           
                tabPanel(value = "proteinMirror","Mirror Plots",
                         h3("Mirror Plots", align = "center"),
                         br(),
                         p("Choose two samples to compare in the mirror plots below.
                           Matching peaks will be colored blue and non-matching peaks
                           will be colored red."),
                         br(),
                         fluidRow(
                         IDBacApp::mirrorPlotsSettings_UI("protMirror")
                         ),
                         fluidRow(
                           IDBacApp::mirrorPlots_UI("protMirror")
                         ),
                         fluidRow(
                           IDBacApp::mirrorPlotDownload_UI("protMirror")
                         )
                ),
                tabPanel(value = "proteinDendrogram","Dendrogram",
                         
                         IDBacApp::plotHier("proth"),
                         IDBacApp::dendDotsUI("proth")
                )
    )
    )
    
  )
  
  
}



