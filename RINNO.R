library("RInno")

setwd("~/GitHub/IDBac_App")
filesToInclude<-c(list.files("exe/www",full.names = T),list.files("exe/pwiz",full.names = T))
create_app(
  app_name    = "IDBac",
  app_dir     = "exe",
  dir_out     = "wizard",
  pkgs        = c("plotly","snow","parallel","shiny", "MALDIquant", "MALDIquantForeign", "mzR", "readxl","networkD3","factoextra","ggplot2","ape","FactoMineR","dendextend","networkD3","reshape2","plyr","dplyr","igraph","rgl"),  # CRAN-like repo packages
  files       = filesToInclude,
  include_R   = TRUE,   # Download R and install it with your app, if necessary
  R_version   = "3.4.2",  # Specified version to include of R
  privilege   = "none", # Admin only installation
  default_dir = "userdocs",
  app_icon   = "SmallAppIcon3.ico",
  setup_icon = "SmallSetupIcon3.ico",
  compression = "bzip",
  info_after = "infoafter.txt",
  info_before = "infobefore.txt",
  license_file = "gpl.txt",
  app_version = "0.0.3")

compile_iss()
