

#library(dplyr)


#a <- "C:/Users/CMC/Desktop/hi2.sqlite"


# where rds was before




#---------------------------
#---------------------------

getSmallMolPeakData <-   function(db, fileshas){

  sqlQ <- glue::glue_sql("
                          SELECT `smallMoleculePeaks`
                          FROM (SELECT *
                                  FROM `IndividualSpectra`
                                WHERE (`spectrumSHA` IN ({shas*})))
                          WHERE (`smallMoleculePeaks` IS NOT NULL)",
                         shas = fileshas,
                         .con = db
  )

  conn <- pool::poolCheckout(db)
  temp <- DBI::dbSendQuery(conn, sqlQ)
  temp <- DBI::dbFetch(temp)
  pool::poolReturn(conn)
   temp <- unname(unlist(temp,
                        recursive = FALSE))
  unlist(lapply(temp,
                function(x){
                  unserialize(memDecompress(x,
                                            type= "gzip"))
                })
         )


}


collapseSmallMolReplicates <- function(db,
                                       fileshas,
                                       smallMolPercentPresence,
                                       lowerMassCutoff,
                                       upperMassCutoff){
  fileshas %>%
    IDBacApp::getSmallMolPeakData(db = db,
                                        fileshas = .) %>%
    MALDIquant::binPeaks(.,
                         tolerance = .02,
                         method = "relaxed") %>%
    MALDIquant::filterPeaks(.,
                            minFrequency = smallMolPercentPresence / 100) %>%
    MALDIquant::mergeMassPeaks(.,
                               method = "mean") %>%
    MALDIquant::trim(object = .,
                     range = c(lowerMassCutoff,
                               upperMassCutoff))
}






getAllStrain_IDsfromSQL <- function(databaseConnection, table){
# Gets unique Strain_IDs given a RSQLite database connecction and table name

 dbQuery <- glue::glue_sql("SELECT DISTINCT `Strain_ID`
                            FROM ({tab*})",
                           tab = table,
                           .con = databaseConnection)

conn <- pool::poolCheckout(databaseConnection)
dbQuery <- DBI::dbSendQuery(conn, dbQuery)
dbQuery <- DBI::dbFetch(dbQuery)[ , 1]


}




