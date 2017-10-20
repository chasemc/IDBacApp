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
    "PreProcessing",
    fluidPage(
        column(1,div(img(src="IDBac_Computer_SVG_300DPI.png",style="width:250px;height:250px"))),
        column(1),column(4,
               h3("Welcome to the IDBac application."),
              p("For more information, as well as links to the full code, please visit our ", a(href="https://chasemc.github.io/IDBac/", target="_blank", "website.")),
               p("Bugs and suggestions may be reported on the ", a(href="https://github.com/chasemc/IDBac_app/issues",target="_blank","IDBac Issues Page on GitHub.", img(border="0", title="https://github.com/chasemc/IDBac_app/issues", src="GitHub.png", width="25" ,height="25"))),
              p("If you use IDBac in your work please cite our paper:") ,
              br(),
              p(strong("Reference"))),

        column(1),
        column(4,




        radioButtons("rawORreanalyze", label = h2("Begin by selecting an option below:"),
                     choices = list("Select here to convert and analyze raw-data from a single MALDI-plate" = 1,
                                    "Select here to convert and analyze raw-data from multiple MALDI-plates at once" = 3,
                                    "Select here if you have already converted data and just want to re-analyze it" = 2),selected=0,inline=TRUE)),

            uiOutput("ui1")

    )),



  tags$hr(),


  tabPanel("Compare Two Samples (Protein)",
           uiOutput("inversepeakui")),
  tabPanel("Heirarchical Clustering (Protein)",
           uiOutput("Heirarchicalui")),
  tabPanel("PCA (Protein)",
           uiOutput("PCAui")),
  tabPanel("Metabolite Association Network (Small-Molecule)",
           uiOutput("MANui"))

)
