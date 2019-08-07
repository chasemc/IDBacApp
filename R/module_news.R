


#' News/Update module
#'
#' @return NA
#' @export
#'
newUpdate <- function(){
  
  showModal(
    modalDialog(
      size = "m",
      title = paste0("IDBac Version ", 
                     utils::packageVersion("IDBacApp")),
      
      shiny::h3("NEWS:"),
      tags$ul(
        strong("BugFix:"),
        tags$li(
          p(" When updating IDBac software, experiments saved into the default directory would sometimes be deleted by the installer. 
          This has been changed and the default save directory should be '~Documents/IDBac_experiments'.")
        ),
        strong("Improvement:"),
        
        tags$li(
          p("Improved support for importing data from Microtyper instruments.")
        ),
        
        strong("Improvement:"),
        tags$li(
          p("Changed how IDBac first connects to an experiment database for faster connect especially with large experiments.")
        ),
        strong("Improvement:"),
        tags$li(
          p("Mirror plots code was improved and placed into a standalone function for coding with IDBac outside of the Shiny app.")
        ),
        strong("BugFix:"),
        tags$li(
          p("'Binning' algorithm checks for empty peak lists and added check for list output from mapply() ")
        ),
        strong("Improvement:"),
        tags$li(
          p("Continuous integration now passes for mac and linux builds.")
        )
      ),
      easyClose = TRUE, 
      footer = "")
  )
  
}