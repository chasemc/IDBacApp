#' ui_proteinClustering
#'
#' @return NA
#' 
#'

ui_proteinClustering <- function() {
  
  
  # in hier -> delineatwe settings menu vs optional
  fluidPage(
    # sidebarLayout(
    
    sidebarPanel(style = "background-color:#7777770d", 
                 width = 4,
                 wellPanel(
                   div(p("Analysis Settings",
                         style = "text-decoration: underline; font-weight: bold; font-size: 1.2em;"),
                       align = "center"),
                   bsCollapse(id = "proteinPeakSettingsDropDown",
                                        open = "Panel 1",
                                        
                                        bsCollapsePanel(title = div(p("Choose How Peaks Are Retained For Analyses", align = "center"),
                                                                              p("(Effects all protein analysis)",
                                                                                style = "font-size: 0.75em",
                                                                                align = "center")), 
                                                                  value = "proteinPeakSettingsDropDown2",
                                                                  div(align = "center",
                                                                      peakRetentionSettings_UI("protMirror")
                                                                  )
                                                                  
                                                                  
                                        ),
                                        bsCollapsePanel(p("Select Samples", 
                                                                    align = "center"),
                                                                  value = "selectProteinSamplesDropDown",
                                                                  p("Move samples between boxes by clicking the samples's name
                                                                    and then an arrow. Samples in the right box will be used for analysis."),
                                                                  sampleChooser_UI("proteinSampleChooser")
                                                                  
                                        ),
                                        bsCollapsePanel(p("Choose Clustering Settings", 
                                                                    align = "center"),
                                                                  value = "proteinClustSettingsDropDown",
                                                                  div(align = "center",
                                                                  dendrogramCreatorUI("proteinHierOptions")
                                                                  )
                                        )           )
                 ),    
                 wellPanel(
                   div(p("Optional Settings", style = "text-decoration: underline; font-weight: bold; font-size: 1.2em;"), align = "center" ),
                   bsCollapse(id = "optionalProteinPeakSettingsDropDown",
                                        
                                        bsCollapsePanel(p("Adjust The Dendrogram", 
                                                                    align = "center"),
                                                                  value = "adjustProteinDendDropDown",
                                                                  shiny::numericInput("hclustHeight",
                                                                               label = h5(strong("Expand dendrogram")),
                                                                               value = 750,
                                                                               step = 50,
                                                                               min = 100),
                                                                  numericInput("dendparmar",
                                                                               label = h5(strong("Adjust right margin of dendrogram")),
                                                                               value = 20,
                                                                               min = 0),
                                                                  div(align = "left",
                                                                  radioButtons("dendOrPhylo", 
                                                                               label = p("Label Positions:"),
                                                                               choices = list("Plot all labels at x = 0" = "Dendrogram", 
                                                                                              "Hang labels" = "Phylogram"),
                                                                               selected = "Dendrogram")
                                                                  ),
                                                                  fluidRow(colordendLabelsUI("proth")),
                                                                  fluidRow(colordendLinesUI("proth")),
                                                                  fluidRow(dendrogramActionsUI("proth"))
                                                                  
                                        ),
                                        bsCollapsePanel(p("Insert Samples From Another Experiment", 
                                                                    align = "center"),     
                                                                  value = "proteinInjectDropDown",
                                                                  selectInjections_UI("proteinInject")
                                                                  
                                        ),
                                        bsCollapsePanel(p("PCA, PCoA, t-SNE", 
                                                                    align = "center"),   
                                                                  value = "proteinMuliDimDropDown",
                                                                  p("Principal Component Analysis (PCA)"),
                                                                  popupPlot_UI("proteinPCA", "PCA"),
                                                                  
                                                                  p("Principal Coordinates Analysis (PCoA)"),
                                                                  popupPlot_UI("proteinPCOA", "PCoA"),
                                                                  
                                                                  p("t-Distributed Stochastic Neighbor Embedding (t-SNE)"),
                                                                  popupPlotTsne_UI("tsnePanel")
                                                                  
                                                                  
                                        ),
                                        bsCollapsePanel(p("Save Dendrogram", 
                                                                    align = "center"),   
                                                                  value = "proteinDendSaveDropDown",
                                                                  
                                                                  downloadHier("proth"),
                                                                  downloadSvg("proth")
                                        )             )
                 ),
                 br(),
                 uiOutput("proteinReport")
                 
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",           
                  tabPanel(value = "proteinMirror","Mirror Plots",
                           h3("Mirror Plot", align = "center"),
                           br(),
                           p("Choose two samples to compare in the mirror plot below.
                           Matching peaks will be colored blue and non-matching peaks
                           will be colored red."),
                           p("Note: Binning algorithm for mirror plot and dendrogram is different.", style = "font-size: 0.75em"),
                           br(),
                           fluidRow(
                             mirrorPlotsSettings_UI("protMirror")
                           ),
                           fluidRow(
                             mirrorPlots_UI("protMirror")
                           ),
                           fluidRow(
                             mirrorPlotDownload_UI("protMirror")
                           )
                  ),
                  tabPanel(value = "proteinDendrogram","Dendrogram",
                           displayMissingProteinUI("proth"),
                           plotHier("proth"),
                           dendDotsUI("proth"),
                           appendDendLabsUI("proth")
                           
                  )
      )
    )
    
  )
  
  
}



