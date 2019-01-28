#' ui_proteinClustering
#'
#' @param x NA
#'
#' @return NA
#' @export
#'
#' @examples NA
ui_proteinClustering <- function() {
  
  fluidPage(
    sidebarLayout(
      
      sidebarPanel(style = "background-color:#7777770d", width=3,
                   
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
                     p("Move strains between boxes by clicking the strain's name
                       and then an arrow. Strains in the right box will be used for analysis.")
                #     IDBacApp::sampleChooserUI("proteinSampleChooser")),
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
            #      IDBacApp::dendDotsUI("proth"),
                   IDBacApp::colordendLabelsUI("proth"),
                   IDBacApp::colordendLinesUI("proth"),
                   br(),
                   h4("Suggestions for Reporting Protein Analysis:"),
                   uiOutput("proteinReport")
                   
                   
                   # 
                   # uiOutput("protLineMod"),
                   # uiOutput("protLabelMod"),
                   # IDBacApp::dendDotsUI("sdvdwv"),
                   
                   
                   
                   
                   
                   
                   
      ),
      
      
      mainPanel(
        IDBacApp::plotHier("proth"),
       IDBacApp::dendDotsUI("proth")
        
      )
    )
  )
}







# 
# 
# 
# 
#                      checkboxGroupInput("selectproteinMenu", label = h3("Checkbox group"),
#                                         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
#                                         selected = 2),
# 
#                      numericInput("hclustHeight",
#                                   label = h5(strong("Expand Tree")),
#                                   value = 750,
#                                   step = 50,
#                                   min = 100),
# 
#                      numericInput("dendparmar",
#                                   label = h5(strong("Adjust right margin of dendrogram")),
#                                   value = 20),
# 
# #                      
# #                      uiOutput("groupui"),
# #                      uiOutput("sampleGroupColoringui"),
# #                      br(),
# #                      h4("Suggestions for Reporting Protein Analysis:"),
# #                      uiOutput("proteinReport"),
# #                      br(),
# #                      downloadButton("downloadHeirSVG",
# #                                     label = "Save Dendrogram as SVG"),
# #                      actionButton("tester", 
#                                   label = "tester"),
#                      br(),
#                      br(),
#                      downloadButton("downloadHierarchical",
#                                     "Save as Newick File"),
#                      radioButtons('format',
#                                   'Document format', 
#                                   c('HTML'),
#                                   inline = TRUE),
#                      downloadButton('downloadReport')
#                    ),    
#                    
#                    # Show PCA, PCoA, t-SNE
#                    #----    
#                    conditionalPanel(
#                      condition = "input.proteinUIselector == '2'",
#                      wellPanel(
#                        p("Principle Components Analysis (PCA)"),
#                        plotlyOutput("pcaPlot",
#                                     width = "100%",
#                                     height = "100%"),
#                        tags$hr(),
#                        p("Principle Coordinates Analysis (PCoA)"),
#                        plotlyOutput("pcoaPlot",
#                                     width = "100%",
#                                     height = "100%"),
#                        tags$hr(),
#                        p("t-SNE"),
#                        numericInput("tsnePerplexity",
#                                     label = h5(strong("t-SNE Perplexity")), 
#                                     value = 15, 
#                                     step = 10,
#                                     min = 0,
#                                     max = 300),
#                        numericInput("tsneTheta",
#                                     label = h5(strong("t-SNE Theta")),
#                                     value = 0.5, 
#                                     step = 0.1,
#                                     max = 1,
#                                     min = 0),
#                        numericInput("tsneIterations",
#                                     label = h5(strong("t-SNE Iterations")),
#                                     value = 1000,
#                                     step = 50),
#                        fluidRow(plotlyOutput("tsnePlot",
#                                              width = "100%",
#                                              height = "100%"))
#                        
#                        
#                      )
#                    ),
#                    # Insert samples from another experiment     
#                    #----  
#                    conditionalPanel(
#                      condition = "input.proteinUIselector == '3'",
#                      p("sds")
#                      
#                    )
#                    
#                    
#                    
#                    
#                    
#       ),
#       mainPanel(align = "center",
#                 plotOutput("hclustPlot")
#                 
#                 
#                 
#       )
#     ),
#     # conditionalPanel(
#     #   condition = "input.selectproteinMenu == '1'",
#     #   dendrogramCreationUI("proteinDend")
#     # )
#     
#     
#   ) 
# }
# 
# 
# 
# 
# 
# 
# 
# 
