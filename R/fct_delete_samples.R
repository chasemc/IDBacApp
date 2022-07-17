# 
# 
# delete_samples <- function(sample_ids,
#                            pool){
#   
#   
#   
#   
#   
#   
#   
# }
# 
# 
# sample_id  <- '172-10.mzXML'
# pool::poolWithTransaction(pool, function(conn){
#   statement <- DBI::dbSendStatement(conn, 'DELETE FROM spectra WHERE strain_id=$sample_id')
#   statement <- DBI::dbBind(statement, list(sample_id = sample_id))
#   DBI::dbGetRowsAffected(statement)
#   DBI::dbClearResult(statement)
# })
# 
# tbl(pool, "spectra")
# 
# 
# 
# pool::poolWithTransaction(pool, function(conn){
#   statement <- DBI::dbExecute(conn, 
#                                     'DELETE FROM xml WHERE xml_hash IN
# (SELECT xml.xml_hash FROM xml LEFT JOIN spectra ON xml.xml_hash=spectra.xml_hash WHERE xml.xml_hash IS NULL);')
# 
# })
# 
# tbl(pool, "spectra")
# 
# 
# 
# 
# 
# 
# 
# conn <- pool::poolCheckout(pool)
# 
# 
# DBI::dbGetQuery(conn, 
#                 'DELETE b FROM xml b 
#                 LEFT JOIN spectra f ON f.xm_hash = b.xml_hash 
#                 WHERE f.xml_hash IS NULL'
#                 )
#   
# 
# 
# 
# DBI::dbExecute(conn, 
#                'PRAGMA foreign_keys = ON;'
# )
# 
# 
# 
# 
