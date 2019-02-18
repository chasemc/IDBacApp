selectDirectory_UI <- function(id, label){
  
  ns <- NS(id)
  
  actionButton(ns("rawFileDirectory"),
               label = label)
  

}


selectDirectory_Server <- function(input,
                                   output,
                                   session){
  
  
  rawFilesLocation <- shiny::reactive({
    
    if (input$rawFileDirectory > 0) {
      loc <- IDBacApp::choose_dir()
      
      if (!is.na(loc)) {
        return(loc)
      } else {
        return(getwd())
      }
    } else {
      return(getwd())
    }
  })
  
  return(rawFilesLocation)
}

#----


showSelectedDirectory_UI <- function(id){
  
  ns <- NS(id)
  textOutput(ns("rawFileDirectorytext"))
                     
}


showSelectedDirectory_Server <- function(input,
                                         output,
                                         session,
                                         location){
  
  # Creates text showing the user which directory they chose for raw files
  #----
  output$rawFileDirectorytext <- renderText({
(location)
    if (!is.null(location())) {
      return(location()) 
    } else {
      return(getwd())
    }
  })
}
  