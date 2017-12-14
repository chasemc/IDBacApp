# Function to Install and Load R Packages
Install_And_Load <- function(Required_Packages)
{
  Remaining_Packages <-
    Required_Packages[!(Required_Packages %in% installed.packages()[, "Package"])]


  if (length(Remaining_Packages))
  {
    install.packages(Remaining_Packages)

  }
  for (package_name in Required_Packages)
  {
    library(package_name,
            character.only = TRUE,
            quietly = TRUE)

  }
}

# Required packages to install and load
Required_Packages = c("plotly","colourpicker","snow","parallel","shiny", "MALDIquant", "MALDIquantForeign", "mzR", "readxl","networkD3","factoextra","ggplot2","ape","FactoMineR","dendextend","networkD3","reshape2","plyr","dplyr","igraph","rgl")


# Install and Load Packages
Install_And_Load(Required_Packages)




# The UI section of the Shiny app provides the "User Interface" and is the means of user interaction.
# "h3", "p", and "br" are HTML,the other lines setup the different panels, tabs, plots, etc of the user interface.
navbarPage(
  "IDBac",
  tabPanel(
    "Introduction",
    fluidPage(
      fluidRow(
        column(3,
               p("To begin, select \"PreProcessing\" above:"),
               
               div(img(src="IDBac_Computer_SVG_300DPI.png",style="width:75%;height:75%"))),
        column(8,
               h3("Welcome to the IDBac application."),
               br(),
               h4("General Information:"),
              tags$ul( 
               tags$li(p("For more information about the method, as well as links to the full code, please visit ", a(href="https://chasemc.github.io/IDBac/", target="_blank", "chasemc.github.io/IDBac"))),
               tags$li(p("Bugs and suggestions may be reported on the ", a(href="https://github.com/chasemc/IDBac_app/issues",target="_blank","IDBac Issues Page on GitHub.", img(border="0", title="https://github.com/chasemc/IDBac_app/issues", src="GitHub.png", width="25" ,height="25"))))),
            
              br(),
              h4("How to Cite IDBac:"),
              
               p("If you use IDBac in your work please cite:") ,
               p(strong("Reference"),
                 p("Also cite this version of IDBac using the DOI:",tags$b("10.5281/zenodo.1115619")),
                 p("Coupling MALDI-TOF mass spectrometry protein and specialized metabolite analyses to rapidly discriminate bacterial function
                   Chase M. Clark, Maria S. Costa, Laura M. Sanchez, Brian T. Murphy
                   bioRxiv 215350; doi: https://doi.org/10.1101/215350"),
                 p("For past versions of IDBac can be found here:",a(href="https://doi.org/10.5281/zenodo.1115620",img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.1115620.svg"),target="_blank") ),  
                 a(href="https://doi.org/10.5281/zenodo.1115620",img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.1115620.svg"),target="_blank")
                 
                 
                 
                 
  ))))),
  
  
  
  tabPanel(
    "PreProcessing",
    fluidPage(
       

        column(1),
        column(4,

                 

        radioButtons("rawORreanalyze", label = h3("Begin by selecting an option below:"),
                     choices = list("Select here to convert and analyze raw-data from a single MALDI-plate" = 1,
                                    "Select here to convert and analyze raw-data from multiple MALDI-plates at once" = 3,
                                    "Select here if you have already converted data with IDBac and want to re-analyze all of it" = 2,
                                    "Select here if you have already converted data with IDBac and want to re-analyze select files" = 4),selected=0,inline=FALSE)),

            uiOutput("ui1")

    )),



  tags$hr(),


  tabPanel("Compare Two Samples (Protein)",
           uiOutput("inversepeakui")),
  tabPanel("Hierarchical Clustering (Protein)",
           uiOutput("Heirarchicalui")),
  tabPanel("PCA (Protein)",
           uiOutput("PCAui")),
  tabPanel("Metabolite Association Network (Small-Molecule)",
           uiOutput("MANui"))

)
