# a <- "C:/Users/chase/Desktop/hi2.sqlite"
# db_con <- DBI::dbConnect(RSQLite::SQLite(), a)
#
#
# library(dplyr)
#
#
#
#
# # where rds was before
# db_con <- DBI::dbConnect(RSQLite::SQLite(), a)
#
# oneDB <- dplyr::tbl(db_con, "IndividualSpectra")
#
#
#
# getProteinPeakData <-  function(db, oneSampleID){
#
#   # "db" = dplyr connection to SQLite DB
#   # "
#
#               var <- enquo(oneSampleID)
#
#               db %>%
#                 filter(Strain_ID == !!var) %>%
#                 filter(proteinPeaksRDS != "NA") %>%
#                 select(proteinPeaksRDS) %>%
#                 collect() %>%
#               return(.) -> p
#
#               p <- unname(unlist(p, recursive = FALSE))
#
#             unlist(lapply(p, function(x) unserialize(memDecompress(x, type= "gzip")))  )
#
#               }
