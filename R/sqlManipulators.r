

#library(dplyr)


#a <- "C:/Users/CMC/Desktop/hi2.sqlite"


# where rds was before


#need to get and collapse small and protein in diff functions b/c binpeak tolerance

getProteinPeakData <-  function(db, filesha1){

  # "db" = dplyr connection to SQLite DB

              var <- enquo(filesha1)
              db %>%
                filter(filesha1 %in% rlang::eval_tidy(var)) %>%
                filter(proteinPeaks != "NA") %>%
                select(proteinPeaks) %>%
                collect() %>%
              return(.) -> p

              p <- unname(unlist(p, recursive = FALSE))

            unlist(lapply(p, function(x) unserialize(memDecompress(x, type= "gzip")))  )
}


collapseProteinReplicates <- function(db, filesha1, proteinPercentPresence){
                                IDBacApp::getProteinPeakData(db, filesha1) %>%
                                MALDIquant::binPeaks(., tolerance = .002) %>%
                                MALDIquant::filterPeaks(.,
                                                          minFrequency = proteinPercentPresence / 100) %>%

                                  MALDIquant::mergeMassPeaks()
}





proteiny <- function(fileshas,
                     db,
                     proteinPercentPresence,
                     lowerMassCutoff,
                     upperMassCutoff){



          # get filesha1 and strain ids
          db %>%
            filter(filesha1 %in% fileshas) %>%
            filter(proteinPeaks != "NA") %>%
            pull(filesha1) %>%
                        IDBacApp::collapseProteinReplicates(db = db,
                                                filesha1 = .,
                                                proteinPercentPresence = proteinPercentPresence) %>%
            MALDIquant::trim(., c(lowerMassCutoff,
                                  upperMassCutoff))
}





















#---------------------------
#---------------------------








getSmallMolPeakData <-  function(db, filesha1){

  # "db" = dplyr connection to SQLite DB

  var <- enquo(filesha1)
  db %>%
    filter(filesha1 %in% rlang::eval_tidy(var)) %>%
    filter(smallMoleculePeaks != "NA") %>%
    select(smallMoleculePeaks) %>%
    collect() %>%
    return(.) -> p

  p <- unname(unlist(p, recursive = FALSE))

  unlist(lapply(p, function(x) unserialize(memDecompress(x, type= "gzip")))  )
}


collapseSmallMolReplicates <- function(db, filesha1, smallMolPercentPresence){
  IDBacApp::getSmallMolPeakData(db, filesha1) %>%
    MALDIquant::binPeaks(., tolerance = .002) %>%
    MALDIquant::filterPeaks(.,
                            minFrequency = smallMolPercentPresence / 100) %>%

    MALDIquant::mergeMassPeaks()
}




smallMoly <- function(fileshas,
                     db,
                     smallMolPercentPresence,
                     lowerMassCutoff,
                     upperMassCutoff){



  # get filesha1 and strain ids
  db %>%
    filter(filesha1 %in% fileshas) %>%
    filter(smallMoleculePeaks != "NA") %>%
    pull(filesha1) %>%
    IDBacApp::collapseSmallMolReplicates(db = db,
                                        filesha1 = .,
                                        smallMolPercentPresence = smallMolPercentPresence) %>%
    MALDIquant::trim(., c(lowerMassCutoff,
                          upperMassCutoff))
}
























































































































