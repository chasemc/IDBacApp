library("RInno")

setwd("~/GitHub/IDBac_App")
a<-list.files("exe/pwiz")
create_app(
  app_name    = "IDBac",
  app_dir     = "exe",
  dir_out     = "wizard",
  pkgs        = c("snow","parallel","shiny", "MALDIquant", "MALDIquantForeign", "mzR", "readxl","networkD3","factoextra","ggplot2","ape","FactoMineR","dendextend","networkD3","reshape2","plyr","dplyr","igraph","rgl"),  # CRAN-like repo packages
  files       = a,

  #remotes     = c("talgalili/installr", "daattali/shinyjs"), # GitHub packages
  include_R   = TRUE,   # Download R and install it with your app, if necessary
  #R_version   = 3.4,  # Old versions of R
  privilege   = "none", # Admin only installation
  default_dir = "userdocs")   # Install app in to DUCUMENTS
