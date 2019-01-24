
selectedSmallMolPeakList




labelsFromBrushedDendrogram
dendBrushToID <- function(dendrogram,
                          ymin,
                          ymax){
  
  
  
  
  
}




function(checkedPool,
         sampleIDs,
         dendrogram
         
         ){



combinedSmallMolPeaks <- NULL
combinedSmallMolPeaksAll <- NULL
matrixID <- NULL

# Check if there is a protein dendrogram, if TRUE: use to subset strains, if FALSE, show all MAN
if(!is.null(dendrogram)){  
  
  if(is.null(input$plot_brush$ymin)){ # If there is a protein dendrogram and user hasn't brushed:
    # Don't ovrwhelm the browser by displaying everthing when page loads
    
    if(length(labels(dendro())) >= 25){
      # If more than 25 strains present, only display 10 to start, otherwise display all
      # Get random 10 strain IDs from dendrogram
      combinedSmallMolPeaks <- labels(dendro())[1:sample.int(10, 1)]
    } else {
      combinedSmallMolPeaks <- labels(dendro())
    }
  } else {
    combinedSmallMolPeaks <- IDBacApp::networkViaBrushedDendrogram(dendrogram = dendro(),
                                                                   brushYmin = input$plot_brush$ymin,
                                                                   brushYmax = input$plot_brush$ymax)
  }
} else {
  
  # retrieve all Strain_IDs in db, check for matrix.
  combinedSmallMolPeaksAll <- glue::glue_sql("
                                             SELECT DISTINCT `Strain_ID`
                                             FROM `IndividualSpectra`
                                             WHERE (`smallMoleculePeaks` IS NOT NULL)",
                                             .con = userDBCon()
  )
  
  conn <- pool::poolCheckout(userDBCon())
  combinedSmallMolPeaksAll <- DBI::dbGetQuery(conn, combinedSmallMolPeaksAll)[ , 1]
  pool::poolReturn(con)
}

# input$matrixSamplePresent (User selection of whether to subtract matrix sample)  1 = Yes, 2 = No
if(input$matrixSamplePresent == 1){
  
  if(!exists("combinedSmallMolPeaksAll")){
    # retrieve all Strain_IDs in db, check for matrix.
    combinedSmallMolPeaksAll <- glue::glue_sql("
                                               SELECT DISTINCT `Strain_ID`
                                               FROM `IndividualSpectra`
                                               WHERE (`smallMoleculePeaks` IS NOT NULL)",
                                               .con = userDBCon()
    )
    
    conn <- pool::poolCheckout(userDBCon())
    combinedSmallMolPeaksAll <- DBI::dbGetQuery(conn, combinedSmallMolPeaksAll)[ , 1] # return as vector of strain IDs
    pool::poolReturn(conn)
  }
  
  # Get sample IDs that begin with "matrix" (need this to search sql db)
  # Also give opportunity to later add ability for letting user interactively select which sample is the blank
  matrixID <- grep("^matrix",
                   combinedSmallMolPeaksAll,
                   ignore.case = TRUE,
                   value = TRUE)
  
  
  # Check if there is a matrix sample
  validate(
    need(length(matrixID) == 0, "Matrix blank not found.  Try selecting \"No\" under \"Do you have a matrix blank\" to left." )
  )
  
} else {
  # Don't add matrix blank to sample ID vector (leave as NULL)
}

# retrieve small mol peaks, spectrumSHA, and strain_id , given Strain_ID.
sqlQ <- glue::glue_sql("
                       SELECT `spectrumSHA`, `Strain_ID`
                       FROM (SELECT *
                       FROM `IndividualSpectra`
                       WHERE (`Strain_ID` IN ({strainIds*})))
                       WHERE (`smallMoleculePeaks` != 'NA')",
                       strainIds = c(combinedSmallMolPeaks,combinedSmallMolPeaksAll, matrixID),
                       .con = userDBCon()
)

conn <- pool::poolCheckout(userDBCon())

sqlQ <- DBI::dbGetQuery(conn, sqlQ)
pool::poolReturn(conn)
sqlQ <- split(sqlQ$spectrumSHA, sqlQ$Strain_ID)
sqlQ <- lapply(sqlQ,
               function(x){
                 IDBacApp::collapseSmallMolReplicates(fileshas = x,
                                                      db = userDBCon(),
                                                      smallMolPercentPresence = input$percentPresenceSM,
                                                      lowerMassCutoff = input$lowerMassSM,
                                                      upperMassCutoff = input$upperMassSM) %>% unname
               }) 

for(i in 1:length(sqlQ)){
  snr1 <-  which(MALDIquant::snr(sqlQ[[i]]) >= input$smSNR)
  sqlQ[[i]]@mass <- sqlQ[[i]]@mass[snr1]
  sqlQ[[i]]@snr <- sqlQ[[i]]@snr[snr1]
  sqlQ[[i]]@intensity <- sqlQ[[i]]@intensity[snr1]
}


return(sqlQ)


}