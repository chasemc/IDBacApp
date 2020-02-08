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
 
  conn <- pool::poolCheckout(userDBCon)
  
  query <-  DBI::dbSendStatement("SELECT xml.xml,spectra.strain_id
FROM `xml`
LEFT JOIN `spectra`
ON xml.xml_hash = spectra.xml_hash
WHERE strain_id == ?
GROUP BY strain_id;
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
                          ids <- chunk$strain_id
                          ids <- base::make.unique(sanitize(ids))
                          ids <- gsub("\\.",
                                      "_",
                                      ids)
                          
                          
                          fileLoc <- base::file.path(saveToDir, ids)
                          
                          
                          
                          chunk <- lapply(chunk$xml, 
                                          function(x){
                                            x <- decompress(unlist(x))
                                            rawToChar(x)
                                            })

                          
                          
                          for (i in seq_along(ids)) {
                            
                            shiny::setProgress(value = counter,
                                               message = 'Exporting...',
                                               detail = glue::glue(" \n Sample: {ids[[i]]}"))
                            
                         
                            
                            writeLines(unlist(chunk[[i]]), 
                                       fileLoc[[i]])
                            
                            fl <- readLines(fileLoc[[i]],
                                           n = 20)
                            
                            if (any(grepl("<mzML", fl, ignore.case = TRUE))) {
                              whichMZ <- "mzML"
                            } else if (any(grepl("<mzXML", fl, ignore.case = TRUE))) {
                              whichMZ <- "mzXML"
                            } else {
                              stop("exportmzML() wasn't mzXML or mzML")
                            }
                            
                            
                            file.rename(fileLoc, paste0(fileLoc, ".", whichMZ))
                            
                            
                            warning(glue::glue("Exported: {ids[[i]]}"))
                            
                          }
                          
                        }
                        
                      })
  removeModal()
  
  
  DBI::dbClearResult(query)
  
  pool::poolReturn(conn)
  # pool::poolClose(userDBCon)
  
  
  
}
