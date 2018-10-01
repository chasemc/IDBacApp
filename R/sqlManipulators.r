

#library(dplyr)


#a <- "C:/Users/CMC/Desktop/hi2.sqlite"


# where rds was before



getProteinPeakData <-  function(db, fileshas){

  sqlQ <- glue::glue_sql("
                          SELECT `proteinPeaks`
                          FROM (SELECT *
                                  FROM `IndividualSpectra`
                                WHERE (`filesha1` IN ({airports*})))
                          WHERE (`proteinPeaks` IS NOT NULL)",
                                                   airports = fileshas,
                                                   .con = db
  )

  conn <- pool::poolCheckout(db)
  airport <- DBI::dbSendQuery(conn, sqlQ)

  p <- DBI::dbFetch(airport)
  pool::poolReturn(conn)

  p <- unname(unlist(p, recursive = FALSE))
  unlist(lapply(p, function(x) unserialize(memDecompress(x, type= "gzip")))  )
}


collapseProteinReplicates <- function(db,
                                      fileshas,
                                      proteinPercentPresence,
                                      lowerMassCutoff,
                                      upperMassCutoff){

                                IDBacApp::getProteinPeakData(db,
                                                             fileshas) %>%
                                MALDIquant::binPeaks(.,
                                                     tolerance = .02) %>%
                                MALDIquant::filterPeaks(.,
                                                          minFrequency = proteinPercentPresence / 100) %>%
                                MALDIquant::mergeMassPeaks() %>%
                                MALDIquant::trim(.,
                                                 c(lowerMassCutoff,
                                                   upperMassCutoff))
}





















#---------------------------
#---------------------------








getSmallMolPeakData <-   function(db, fileshas){

  sqlQ <- glue::glue_sql("
                          SELECT `smallMoleculePeaks`
                          FROM (SELECT *
                                  FROM `IndividualSpectra`
                                WHERE (`filesha1` IN ({shas*})))
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



