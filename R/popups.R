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
                         "Click to continue")),
    footer = NULL
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


#' While Database is copying, block user-interaction
#'
#' @return modal
#' @export
#'
copyingDbPopup <- function(){
  showModal(
    modalDialog(
      title = "Important message",
      "When file-conversions are complete this pop-up will be replaced by a summary of the conversion.",
      br(),
      "To check what has been converted, you can navigate to:",
      easyClose = FALSE, 
      size = "l",
      footer = "")
  )
}




#' Modular modal Warn user that database alredy exists
#'
#' @param dbName name of database to be created
#'
#' @return modular modal
#' @export
#'
dbExists_UI <- function(dbName){
  
  showModal(
    modalDialog(
      title = "Warning",
      glue::glue("Experiment: {dbName} already exists, overwrite?"),
      easyClose = FALSE, 
      size = "m",
        actionButton("continueTransfer", "Continue", icon("check")),
      actionButton("stopTransfer", "Stop", icon = icon("remove")),
      footer = NULL
        
   
        )
  )
}



#' Modular modal Warn user that database alredy exists
#'
#' @param newDBName newDBName
#' @param continue reactivevalue
#'
#' @return modular modal
#' @export
#'
dbExists_server <- function(input,
                            output,
                            session,
                            newDBName,
                            continue){
  
  IDBacApp::dbExists_UI(newDBName)

  observeEvent(input$continueTransfer, 
               ignoreInit  = F, {
                 print("dfsda")
                 removeModal()
                 continue$val <- TRUE   
                 
               })
  
  observeEvent(input$stop, 
               ignoreInit  = TRUE, {
                 continue$val <- FALSE    
                 removeModal()
               })
  
  
  
  
}

