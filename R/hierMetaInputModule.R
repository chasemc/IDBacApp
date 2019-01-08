

runDendDots <- function(rawDendrogram, trimdLabsDend, pool, columnID) {
  
  conn <- pool::poolCheckout(pool)
  dendLabs <- labels(rawDendrogram)
  
  
    query <- DBI::dbSendStatement("SELECT *
                                  FROM metaData
                                  WHERE `Strain_ID` = ?",
                                  con=conn)
    
    DBI::dbBind(query, list(dendLabs))
    selectedMeta <- DBI::dbFetch(query)
    
    dbClearResult(query)
    
    
    selectedMeta <- selectedMeta[ ,colnames(selectedMeta) %in% columnID]
    uniq <- unique(selectedMeta)
    selectedMeta <- sapply(uniq, function(x) selectedMeta %in% x)
    
    selectedMeta[selectedMeta == TRUE] <- "#000000" 
    selectedMeta[selectedMeta == FALSE] <- "#00000000" 
    colnames(selectedMeta) <- uniq
    
    IDBacApp::colored_dots(selectedMeta,
                           trimdLabsDend,
                           horiz = T,
                           #rowLabels = uniq,
                           sort_by_labels_order = FALSE)
    
  
}




  
