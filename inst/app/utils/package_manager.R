
# capture the current working directory
# set the package search path to the app specific library
appwd <- getwd()
applibpath <- file.path(appwd, "library")
config <- jsonlite::fromJSON(file.path(appwd, "utils/config.cfg"))

.libPaths(c(file.path(getwd(),"packageLibrary"),.libPaths()))


message("library paths in package_manager:\n", paste0("... ", .libPaths(), collapse = "\n"))
message("working path in package_manager:\n", paste("...", appwd))


# Load functions to ensure software dependencies and check the internet
source("utils/ensure.R")


# Read the application config
library("jsonlite", character.only = TRUE)
library("devtools", character.only = TRUE)
library("httr", character.only = TRUE)

# Package dependency list
pkgs <- config$pkgs$pkgs; remotes <- config$remotes; locals <- config$locals$pkgs

# Provide some initialization status updates
pb <- winProgressBar(
  title = sprintf("Starting %s ...", config$appname),
  label = "Initializing ...")

# If an app repository has been provided, install the app from there
if (config$app_repo[[1]] != "none") {
  source("utils/get_app_from_app_url.R")
}

# Use tryCatch to display error messages in config$logging$filename
appexit_msg <- tryCatch({

  # ensure all package dependencies are installed
  message("ensuring packages: ", paste(pkgs, collapse = ", "))
  setWinProgressBar(pb, 0, label = "Ensuring package dependencies ...")
  if (class(try(httr::http_error("www.google.com"))) != "try-error") {
    ._ <- mapply(ensure, pkgs, names(pkgs))
    if (remotes[1] != "none") {
      message("ensuring remotes: ", paste(remotes, collapse = ", "))
      setWinProgressBar(pb, 0, label = "Ensuring remote package dependencies ...")
      ._ <- lapply(remotes, ensure_remotes)
    }
  }
  if (locals[1] != "none") {
    message("ensuring locals: ", paste(locals, collapse = ", "))
    setWinProgressBar(pb, 0, label = "Ensuring local package dependencies ...")
    ._ <- mapply(ensure_local, locals, names(locals),
                 lib.path = file.path(appwd, config$locals$local))
  }

  for (i in seq_along(pkgs)) {
    setWinProgressBar(pb,
      value = i / (length(pkgs) + 1),
      label = sprintf("Loading package - %s", pkgs[i]))

    library(names(pkgs)[i], character.only = TRUE)
  }

  setWinProgressBar(pb, 1.00, label = "Starting application")
  close(pb)

  source(file.path(appwd, "utils/app.R"))

  "application terminated normally"
},
error = function(e) {
  msg <- sprintf("Startup failed with error(s):\n\n%s", e$message)
  tcltk::tk_messageBox(
    type = "ok",
    message = msg,
    icon = "error")

  msg
},
finally = {
  close(pb)
})

message(appexit_msg)
