

#library(dplyr)


#a <- "C:/Users/CMC/Desktop/hi2.sqlite"


# where rds was before


#need to get and collapse small and protein in diff functions b/c binpeak tolerance

getProteinPeakData <-  function(db, filesha1){

  # "db" = dplyr connection to SQLite DB

              var <- enquo(filesha1)
              db %>%
                filter(filesha1 %in% rlang::eval_tidy(var)) %>%
                filter(proteinPeaksRDS != "NA") %>%
                select(proteinPeaksRDS) %>%
                collect() %>%
              return(.) -> p

              p <- unname(unlist(p, recursive = FALSE))

            unlist(lapply(p, function(x) unserialize(memDecompress(x, type= "gzip")))  )
}


collapseProteinReplicates <- function(db, filesha1, proteinPercentPresence){
                                IDBacApp::getProteinPeakData(db, filesha1) %>%
                                  MALDIquant::filterPeaks(.,
                                                          minFrequency = proteinPercentPresence / 100) %>%
                                  MALDIquant::binPeaks(., tolerance = .002) %>%
                                  MALDIquant::mergeMassPeaks()
}








