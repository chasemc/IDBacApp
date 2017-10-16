
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
Required_Packages = c("snow","parallel","shiny", "MALDIquant", "MALDIquantForeign", "mzR", "readxl","networkD3","factoextra","ggplot2","ape","FactoMineR","dendextend","networkD3","reshape2","plyr","dplyr","igraph","rgl")


# Install and Load Packages
Install_And_Load(Required_Packages)










# The UI section of the Shiny app provides the "User Interface" and is the means of user interaction.
# "h3", "p", and "br" are HTML,the other lines setup the different panels, tabs, plots, etc of the user interface.
 navbarPage(
  "IDBac",
  tabPanel(
    "PreProcessing",
    fluidPage(
      fluidRow(
        column(9,h2("IDBac"), br(),
        p("Welcome to the IDBac application. For documentation please visit our ", a(href="https://chasemc.github.io/IDBac/", target="_blank", "website.")),
        p("Bugs and suggestions may be reported on the ",a(href="https://github.com/chasemc/IDBac_app/issues", target="_blank", "IDBac Github.")))),
      #  column(3,div(img(src="placeholder.gif",style="width:370px;height:210px")))),
      #br(),
      fluidRow(
        radioButtons("rawORreanalyze", label = h4("Begin with selecting an option below:"),
      choices = list("Select here to convert raw-data from a single MALDI-plate" = 1,
                     "Select here to convert raw-data from Multiple MALDI-plates" = 3,
                     "Select here if you have already converted data and just want to re-analyze the data" = 2),selected=0),
      uiOutput("ui1"))

  )),





  tabPanel("Inverse Peak Comparison",
           uiOutput("inversepeakui")),
  tabPanel("Heirarchical Clustering",
           uiOutput("Heirarchicalui")),
  tabPanel("PCA",
           uiOutput("PCAui")),
  tabPanel("Metabolite Association Network",
           uiOutput("MANui"))

)



