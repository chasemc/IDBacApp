#' Export mzML/mzXML from SQLite DB
#'
#' @param userDBCon pool connection, not-checked-out 
#' @param sampleIDs sample IDs
#' @param saveToDir directory files should be saved to
#'
#' @return NA, files written
#' @export
#'
exportmzML <- function(userDBCon,
                       sampleIDs,
                       saveToDir){
  userDBCon<<-userDBCon
  sampleIDs<<-sampleIDs
  saveToDir <<-saveToDir
  conn <- pool::poolCheckout(userDBCon)
  
  query <-  DBI::dbSendStatement("SELECT XML.XML,IndividualSpectra.Strain_ID
FROM `XML`
LEFT JOIN `IndividualSpectra`
ON XML.mzMLSHA = IndividualSpectra.mzMLSHA
WHERE Strain_ID == ?
GROUP BY Strain_ID;
", con = conn)
  
  
  DBI::dbBind(query,
              list(as.character(as.vector(sampleIDs))))
  
  
  
  shiny::withProgress(message = 'Processing in progress',
                      value = 0,
                      max = 100, {
                        inc <- 100 / length(sampleIDs) / 5
                        counter = 0
                        
                        showModal(modalDialog(
                          size = "m",
                          title = "Exporting Spectra",
                          easyClose = FALSE, 
                          footer = ""))   
                        
                        while (!DBI::dbHasCompleted(query)) {
                          
                          chunk <- DBI::dbFetch(query, 5)
                          
                          counter <- counter - inc
                          ids <- chunk$Strain_ID
                          fileLoc <- base::file.path(saveToDir, ids)
                          
                          chunk <- lapply(chunk$XML, function(x) unserialize(unlist(x)))
                          
                          for (i in seq_along(ids)) {
                            
                            shiny::setProgress(value = counter,
                                               message = 'Exporting...',
                                               detail = glue::glue(" \n Sample: {ids[[i]]}"))
                            
                            
                            
                            writeLines(unlist(chunk[[i]]), 
                                       fileLoc[[i]])
                            
                            
                            whichMZ <- xml2::read_xml(fileLoc[[i]])
                            
                            whichMZ <- xml2::xml_name(whichMZ)
                            
                            if (grepl("mzml", whichMZ, ignore.case = TRUE)) {
                              whichMZ <- "mzML"
                            } else if (grepl("mzXml", whichMZ, ignore.case = TRUE)) {
                              whichMZ <- "mzXML"
                            }
                            
                            
                            file.rename(fileLoc, paste0(fileLoc, ".", whichMZ))
                            
                            
                            warning(glue::glue("Exported: {ids[[i]]}"))
                            
                          }
                          
                        }
                        
                      })
  removeModal()
  
  
  DBI::dbClearResult(query)
  DBI::dbDisconnect(conn)
  
  pool::poolReturn(conn)
  # pool::poolClose(userDBCon)
  
  
  
}
