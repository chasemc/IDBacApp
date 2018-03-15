library("RInno")

setwd("~/GitHub/IDBac_App/inst/app")
filesToInclude<-c(list.files("www",full.names = T),file.path("proteowizardinstallation.zip"))

setwd("~/GitHub/IDBac_App/inst/app")

create_app(
  app_name    = "IDBac",
  dir_out     = "wizard",
#  pkgs        = c("checkpoint"),
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
  app_version = "0.0.13")

compile_iss()
