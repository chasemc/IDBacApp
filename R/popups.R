#----
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
