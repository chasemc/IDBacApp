library("RInno")


filesToInclude<-c(list.files("C:/Users/chase/Documents/GitHub/IDBacInstaller/IDBac/inst/app/www",full.names = T),list.files("C:/Users/chase/Documents/GitHub/IDBacInstaller/IDBac/inst/app/pwiz",full.names = T))
setwd("~/GitHub/IDBac_App/inst/app")

create_app(
  app_name    = "IDBac",
  dir_out     = "wizard",
  pkgs        = c("devtools","svglite","shinyjs", "mzR","plotly","colourpicker","shiny", "MALDIquant", "MALDIquantForeign","readxl","networkD3","ape","FactoMineR","dendextend","networkD3","reshape2","plyr","igraph"),
  files       = filesToInclude,
  include_R   = TRUE,   # Download R and install it with your app, if necessary
  R_version   = "3.4.3",  # Specified version to include of R
  privilege   = "none", # Admin only installation
  default_dir = "userdocs",
  app_icon   = "SmallAppIcon3.ico",
  setup_icon = "SmallSetupIcon3.ico",
  compression = "bzip",
  info_after = "infoafter.txt",
  info_before = "infobefore.txt",
  license_file = "gpl.txt",
  app_version = "0.0.15")

compile_iss()
