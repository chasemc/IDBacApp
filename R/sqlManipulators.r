

#library(dplyr)


#a <- "C:/Users/CMC/Desktop/hi2.sqlite"


# where rds was before




getProteinPeakData <-  function(db, filesha1){

  # "db" = dplyr connection to SQLite DB
  # "
              var <- enquo(filesha1)
              db %>%
                filter(filesha1 %in% !!var) %>%
                filter(proteinPeaksRDS != "NA") %>%
                select(proteinPeaksRDS) %>%
                collect() %>%
              return(.) -> p

              p <- unname(unlist(p, recursive = FALSE))

            unlist(lapply(p, function(x) unserialize(memDecompress(x, type= "gzip")))  )
}

#
collapseProteinReplicates2 <- function(db, filesha1){
                                IDBacApp::getProteinPeakData(db, filesha1) %>%
                                  MALDIquant::binPeaks(., tolerance = .002) %>%
                                  MALDIquant::mergeMassPeaks()
                             }


#lapply(c("172-1","172-10"), collapse)
