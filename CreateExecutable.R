library("RInno")

setwd("~/GitHub/IDBac_App/inst/app")
filesToInclude<-c(list.files("www",full.names = T),list.files("pwiz",full.names = T))
create_app(
  app_name    = "IDBac",
 # app_dir     = "app",
  dir_out     = "wizard",
  #pkgs        =
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
  app_version = "0.0.9")

compile_iss()
