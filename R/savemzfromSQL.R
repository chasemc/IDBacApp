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
                        inc <- 100 / length(sampleIDs)
                        counter = 0
                        while (!DBI::dbHasCompleted(query)) {
                          
                          chunk <- DBI::dbFetch(query, 1)
                       
                          shiny::setProgress(value = counter ,
                                             message = 'Exporting...',
                                             detail = glue::glue(" \n Sample: {chunk$Strain_ID}"))
                          counter <- counter - inc
                          fileLoc <- base::file.path(saveToDir, chunk$Strain_ID)
                          
                          chunk <- lapply(chunk$XML, function(x) unserialize(unlist(x)))
                          
                          
                          writeLines(unlist(chunk), 
                                     fileLoc)
                          
                          chunk <- xml2::read_xml(fileLoc)
                          
                          chunk <- xml2::xml_name(chunk)
                          file.rename(fileLoc, paste0(fileLoc, ".", chunk))
                          
                          
                          warning(glue::glue("Exported: {chunk}"))
                          
                  
                          
                        }
                      })
  
  
  
  DBI::dbClearResult(query)
  DBI::dbDisconnect(conn)
  
  pool::poolReturn(conn)
 # pool::poolClose(userDBCon)
  
  
  
}
