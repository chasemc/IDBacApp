#' experimentSummary_UI
#'
#' @param id namespace
#'
#' @return .
#'
#'
experimentSummary_UI <- function(id) {
  ns <- shiny::NS(id)
  rhandsontable::rHandsontableOutput(ns("summaryTable"))
}


#' experimentSummary_Server
#'
#' @param input modules
#' @param output modules
#' @param session modules
#' @param pool modules
#'
#' @return modules
#'
#'
experimentSummary_Server <- function(input,
                                     output,
                                     session,
                                     pool) {
  summarized <- reactive({
    checkedPool <- pool::poolCheckout(pool())
    smallReplicates <- DBI::dbGetQuery(checkedPool, "SELECT (strain_id)
                                       FROM spectra
                                       WHERE max_mass < 6000")
    proteinReplicates <- DBI::dbGetQuery(checkedPool, "SELECT (strain_id)
                                       FROM spectra
                                       WHERE max_mass > 6000")
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
      readOnly = TRUE
    )
  })
}
