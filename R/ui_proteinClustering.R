ui_proteinClustering <- function(id) {
  
  ns <- shiny:::NS(id)
  
  sidebarLayout(
    sidebarPanel(style = "background-color:#7777770d",
                 tabsetPanel(id = "HierarchicalSidebarTabs", 
                             type = "tabs",
                             ui_hierSettings("hierSettings"),
                             ui_pcaSettings("pcaSettings"),
                             ui_seedSettings("seedSettings")
                             
                             
                 )
    ),
    mainPanel(align="center",
              plotOutput("hclustPlot")
              
              
    )
  )
  
}



ui_hierSettings <- function(id){
  
  tabPanel("Hierarchical Clustering Settings",
           value = "hierSettings",
           h5(strong("Select Samples")),
           p("Move strains between boxes by clicking the strain's name
           and then an arrow. Strains in the right box will be used for analysis."),
           uiOutput("chooseProteinSamples"),
           selectInput("distance", 
                       label = h5(strong("Distance Algorithm")),
                       choices = list("cosine" = "cosineD",
                                      "euclidean" = "euclidean",
                                      "maximum" = "maximum",
                                      "manhattan" = "manhattan",
                                      "canberra" = "canberra",
                                      "binary" = "binary",
                                      "minkowski"= "minkowski"),
                       selected = "cosine"),
           selectInput("clustering", 
                       label = h5(strong("Clustering Algorithm")),
                       choices = list("ward.D" = "ward.D",
                                      "ward.D2" = "ward.D2",
                                      "single" = "single", 
                                      "complete" = "complete",
                                      "average (UPGMA)" = "average",
                                      "mcquitty (WPGMA)" = "mcquitty",
                                      "median (WPGMC)" = "median",
                                      "centroid (UPGMC)" = "centroid"),
                       selected = "ward.D2"),
           radioButtons("booled", 
                        label = h5(strong("Include peak intensities, or use presence/absence?")),
                        choices = list("Presence/Absence" = 1, 
                                       "Intensities" = 2),
                        selected = 2),
           numericInput("hclustHeight", 
                        label = h5(strong("Expand Tree")),
                        value = 750,
                        step = 50,
                        min = 100),
           numericInput("dendparmar",
                        label = h5(strong("Adjust right margin of dendrogram")),
                        value = 20),
           radioButtons("kORheight", 
                        label = h5(strong("Color clusters based on:")),
                        choices = list("Specified Number of Groups" = 1, 
                                       "Height (x-axis value)" = 2,
                                       "User-Defined Categories in Excel Sheet" = 3),
                        selected = 1),
           uiOutput("groupui"),
           uiOutput("hclustui"),
           uiOutput("sampleGroupColoringui"),
           br(),
           h4("Suggestions for Reporting Protein Analysis:"),
           uiOutput("proteinReport"),
           br(),
           downloadButton("downloadHeirSVG",
                          label = "Save Dendrogram as SVG"),
           actionButton("tester", 
                        label = "tester"),
           br(),
           br(),
           downloadButton("downloadHierarchical",
                          "Save as Newick File"),
           radioButtons('format',
                        'Document format', 
                        c('HTML'),
                        inline = TRUE),
           downloadButton('downloadReport')
  )
  
}


ui_pcaSettings <- function(id){
  
  tabPanel("PCA, PCoA, t-SNE ",
           value = "proteinScatters",
           wellPanel(
             p("Principle Components Analysis (PCA)"),
             plotlyOutput("pcaPlot",
                          width = "100%",
                          height = "100%"),
             tags$hr(),
             p("Principle Coordinates Analysis (PCoA)"),
             plotlyOutput("pcoaPlot",
                          width = "100%",
                          height = "100%"),
             tags$hr(),
             p("t-SNE"),
             numericInput("tsnePerplexity",
                          label = h5(strong("t-SNE Perplexity")), 
                          value = 15, 
                          step = 10,
                          min = 0,
                          max = 300),
             numericInput("tsneTheta",
                          label = h5(strong("t-SNE Theta")),
                          value = 0.5, 
                          step = 0.1,
                          max = 1,
                          min = 0),
             numericInput("tsneIterations",
                          label = h5(strong("t-SNE Iterations")),
                          value = 1000,
                          step = 50),
             fluidRow(plotlyOutput("tsnePlot",
                                   width = "100%",
                                   height = "100%"))
             
             
           )
           
           
  )}


ui_seedSettings <- function(id){
  tabPanel("Insert samples from another experiment ",
           value = "seedPanel",
           
           p("sd")
  ) 
}
