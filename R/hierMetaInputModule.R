

runDendDots <- function(dendrogram, pool, columnID) {
  
  conn <- pool::poolCheckout(pool)
  dendLabs <- labels(dendrogram)
  
  
  if(!is.null(columnID)) {
    
    columnID <- columnID
    
    
    query <- DBI::dbSendStatement("SELECT *
                                  FROM metaData
                                  WHERE `Strain_ID` = ?",
                                  con=conn)
    DBI::dbBind(query, list(dendLabs))
    selectedMeta <- DBI::dbFetch(query)
    dbClearResult(query)
    
    
    selectedMeta <- base::cbind.data.frame(ids = selectedMeta$Strain_ID,
                                           meta = selectedMeta[,colnames(selectedMeta) %in% columnID])
    
    
    cols <- IDBacApp::colorBlindPalette()
    
    
    colsd <- cols[factor(selectedMeta[,2])]
    
    
    
    IDBacApp::colored_dots(colsd,
                           dendrogram,
                           horiz = T,
                           sort_by_labels_order = FALSE)
    
  } 
}





