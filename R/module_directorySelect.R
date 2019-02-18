selectDirectory_UI <- function(id, label){
  
  ns <- NS(id)
  
  actionButton(ns("rawFileDirectory"),
               label = label)
  
  
}


selectDirectory_Server <- function(input,
                                   output,
                                   session,
                                   sqlDirectory){
  
  
  observeEvent(input$rawFileDirectory, {
    loc <- IDBacApp::choose_dir()
    
    if (!is.na(loc)) {
      sqlDirectory$sqlDirectory <- loc
    } else {
      sqlDirectory$sqlDirectory <- normalizePath(getwd())
    }
    
    
  })
  
  
}

#----

