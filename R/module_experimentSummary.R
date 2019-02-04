
#' experimentSummary_UI
#'
#' @param id namespace
#'
#' @return .
#' @export
#'

experimentSummary_UI <- function(id) {
  ns <- shiny::NS(id)
  rhandsontable::rHandsontableOutput(ns("summaryTable"), height = 800)
}




#' experimentSummary_Server
#'
#' @param input modules
#' @param output modules 
#' @param session modules
#' @param pool modules
#'
#' @return modules
#' @export
#'

experimentSummary_Server <- function(input,
                                     output,
                                     session,
                                     pool){

 
  
  summarized <- reactive({
    checkedPool <- pool::poolCheckout(pool())
    
    smallReplicates <- DBI::dbGetQuery(checkedPool, "SELECT (Strain_ID) 
                                       FROM IndividualSpectra
                                      WHERE smallMoleculePeaks IS NOT NULL")
    
    proteinReplicates <- DBI::dbGetQuery(checkedPool, "SELECT (Strain_ID) 
                                       FROM IndividualSpectra
                                      WHERE proteinPeaks IS NOT NULL")
    pool::poolReturn(checkedPool)
    
    
    if (nrow(proteinReplicates) > 0) {
      a <- as.data.frame(table(proteinReplicates))
      colnames(a) <- c("ID", "Replicates")
    } else {
      a <- data.frame("ID" = NA, "proteinReplicates" = NA)[-1, ]
    }
    
    
    if (nrow(smallReplicates) > 0) {
      b <- as.data.frame(table(smallReplicates))
      colnames(b) <- c("ID", "Small Molecule Replicates")
    } else {
      b <- data.frame("ID" = NA, "Small Molecule Replicates" = NA)[-1, ]
      
    }
  
    
    a <- merge(a, b, "ID", all = TRUE)
    
    colnames(a) <- c("ID", "Protein Replicates", "Small Molecule Replicates")
    return(a)
    
  })
  
  
  
  output$summaryTable <- rhandsontable::renderRHandsontable({
    
    rhandsontable::rhandsontable(summarized(),
                                 useTypes = FALSE,
                                 contextMenu = FALSE,
                                 readOnly = TRUE)
    })
  
  
  
}  
  