#----
#' popup3
#'
#' @return NA
#' @export
#'
popup3 <- function(){
  showModal(modalDialog(
    size = "m",
    title = "Important message",
    "When spectra processing is complete you will be able to begin with the data analysis",
    br(),
    "To check the progress, observe the progress bar at bottom right.",
    easyClose = FALSE, 
    footer = ""))
}


# Popup notifying user when spectra processing is complete
#----
#' popup4
#'
#' @return NA
#' @export
#'
popup4 <- function(){
  showModal(modalDialog(
    size = "m",
    title = "Spectra Processing is Now Complete",
    br(),
    easyClose = FALSE,
    tagList(actionButton("processToAnalysis", 
                         "Click to continue"))
  ))
  
}


# Modal to display while converting to mzML
#----
#' brukerToMzml_popup
#'
#' @return NA
#' @export
#'
brukerToMzml_popup <- function(){
  showModal(modalDialog(
    size = "m",
    title = "Important message",
    p("IDBac is converting your Bruker files to open-source mzML, 
    during this step there is no progress bar."),
    p("After this step IDBac will begin to convert your files into an
      IDBac experiment."),
    easyClose = FALSE, 
    footer = ""))
}

