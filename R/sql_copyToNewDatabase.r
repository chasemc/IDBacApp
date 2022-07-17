#' Copy from one database to another, selecting by Sample ID
#'
#' @param existingDBPool is a single (not list) pool object referencing the database to transfer from 
#' @param newDBPool  is a single (not list) pool object referencing the database to transfer to
#' @param sampleIDs  sample IDs to transfer 
#'
#' @return Nothing, side effect of creating a new sqlite databse
#' 
#'

copyToNewDatabase <- function(existingDBPool,
                              newDBPool,
                              sampleIDs){


                        Sys.sleep(1) 
                        
                        # Connect to both databases (create pool and checkout)
                        newDBconn <- pool::poolCheckout(newDBPool)
                        existingDBconn <- pool::poolCheckout(existingDBPool)
                        newdbPath <- gsub("\\\\", "/", newDBconn@dbname)
                        
                        warning(paste0("Begin migration of \n",
                                       existingDBconn@dbname,
                                       " to \n", newDBconn@dbname))
                        
                        if (!is.null(shiny::getDefaultReactiveDomain())) {
                        setProgress(value = 0.2, 
                                    message = 'Copying data to new database',
                                    detail = 'Setting up new experiment...',
                                    session = getDefaultReactiveDomain())
                        }
                        # Create sqlite tables in new database
                        #-----
                    
                        
                    
                    
                        # Setup New metadata ------------------------------------------------------
                        
                        sql_create_metadata_table(sqlConnection = newDBconn)
                        
                        # Setup New xml -----------------------------------------------------------
                        
                        sql_create_xml_table(sqlConnection = newDBconn)
                        
                        
                        # Setup New mass_index ---------------------------------------------
                        
                        
                        sql_create_massindex_table(sqlConnection = newDBconn)
                        
                        
                        # Setup New spectra ---------------------------------------------

                        sql_create_spectra_table(sqlConnection = newDBconn)
                        

                        # Setup version table -----------------------------------------------------

                        sql_create_version_table(sqlConnection = newDBconn)
                        sql_fill_version_table(pool = newDBPool)
                        sql_create_locale_table(sqlConnection = newDBconn)
                        sql_fill_locale_table(pool = newDBPool)
                        
# Copy  -------------------------------------------------------------------
                        # Attach new database to existing database
                        #----
                        
                          copyDB_dbAttach(newdbPath = newdbPath, 
                                                  existingDBconn = existingDBconn)
                        
                        Sys.sleep(1) 
                        
                        if (!is.null(shiny::getDefaultReactiveDomain())) {
                        
                        setProgress(value = 0.5, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying metadata...',
                                    session = getDefaultReactiveDomain())
                      }
                        
                        a <- DBI::dbReadTable(existingDBconn, "metadata")
                        b <- DBI::dbReadTable(newDBconn, "metadata")
                        
                        DBI::dbWriteTable(conn = newDBconn,
                                          name = "metadata", 
                                          value = merge(b,
                                                        a[a$strain_id %in% sampleIDs, ], 
                                                        sort = F, all = T),
                                          overwrite = T)
                        remove(a, b)
                        
                        if (!is.null(shiny::getDefaultReactiveDomain())) {
                        
                        setProgress(value = 0.7, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying individual spectra...',
                                    session = getDefaultReactiveDomain())
                        
                        }
                        
                        state <- DBI::dbSendStatement(existingDBconn, 
                                                      "INSERT INTO newDB.spectra
                                                      SELECT *
                                                      FROM main.spectra
                                                      WHERE (strain_id = ?)")
                        DBI::dbBind(state, list(sampleIDs))
                        warning(state@sql)
                        DBI::dbClearResult(state) 
                        
                        # Copy xml table ----------------------------------------------------------
                        if (!is.null(shiny::getDefaultReactiveDomain())) {
                           setProgress(value = 0.8, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying mzML files...',
                                    session = getDefaultReactiveDomain())
                        }
                        state <- DBI::dbSendQuery(existingDBconn, 
                                                  "SELECT DISTINCT xml_hash
                                                      FROM main.spectra
                                                      WHERE (strain_id = ?)")
                        DBI::dbBind(state, list(sampleIDs))
                        res <- DBI::fetch(state)
                        DBI::dbClearResult(state)
                        
                        state <- DBI::dbSendStatement(existingDBconn, 
                                                      "INSERT INTO newDB.xml
                                                      SELECT *
                                                      FROM main.xml
                                                      WHERE (xml_hash = ?)")
                        DBI::dbBind(state, list(res[ , 1]))
                        warning(state@sql)
                        DBI::dbClearResult(state) 
                        
                        
                        
                        
                        
                        
                        # Copy mass_index ----------------------------------------------------------
                        
                        
                        state <- DBI::dbSendQuery(existingDBconn, 
                                                  "SELECT DISTINCT spectrum_mass_hash
                                                      FROM main.spectra
                                                      WHERE (strain_id = ?)")
                        DBI::dbBind(state, list(sampleIDs))
                        res <- DBI::fetch(state)
                        DBI::dbClearResult(state)
                        
                        state <- DBI::dbSendStatement(existingDBconn, 
                                                      "INSERT INTO newDB.mass_index
                                                      SELECT *
                                                      FROM main.mass_index
                                                      WHERE (spectrum_mass_hash = ?)")
                        
                        DBI::dbBind(state, list(res[ , 1]))
                        warning(state@sql)
                        DBI::dbClearResult(state) 
                        
                      
                        if (!is.null(shiny::getDefaultReactiveDomain())) {
                        shiny::setProgress(value = 1, 
                                           message = 'Copying data to new database',
                                           detail = 'Finishing...',
                                           session = getDefaultReactiveDomain())
                        }
                        
                        copyDB_dbDetach(newdbPath = newdbPath, 
                                                  existingDBconn = existingDBconn)
                  
                        
                        pool::poolReturn(existingDBconn)
                        pool::poolReturn(newDBconn)
                        
                        
                        Sys.sleep(1)
                        warning(paste0("End migration of \n",
                                       existingDBconn@dbname,
                                       " to \n", newDBconn@dbname))
                        
                       
  
}


#' Attach new database to existing database
#'
#' @param newdbPath newdbPath
#' @param existingDBconn existingDBconn
#'
#' @return NA
#' 
#'
copyDB_dbAttach <- function(newdbPath, 
                            existingDBconn){
  
  sqlQ <- glue::glue_sql("ATTACH DATABASE {newdbPath} as newDB;",
                         .con = existingDBconn) 
  temp <- DBI::dbSendStatement(existingDBconn, sqlQ)
  warning(temp@sql)
  DBI::dbClearResult(temp)
  
}


#' Detach new database to existing database
#'
#' @param newdbPath newdbPath
#' @param existingDBconn existingDBconn
#'
#' @return NA
#' 
#'
copyDB_dbDetach <- function(newdbPath, 
                            existingDBconn){
  
  sqlQ <- glue::glue_sql("DETACH DATABASE newDB;",
                         .con = existingDBconn) 
  temp <- DBI::dbSendStatement(existingDBconn, sqlQ)
  warning(temp@sql)
  DBI::dbClearResult(temp)
  
}

#' Setup metadata DB table
#'
#' @param newDBconn newDBconn 
#' @param existingDBconn  existingDBconn
#'
#' @return NA
#' 
#'
copyDB_setupMeta <- function(newDBconn,
                             existingDBconn){
  # Below, when setting up the new tables, we need to add the existing columns which may not be present in 
  # the current architecture, and also have the current architecture (ie we can't just copy/paste from the old DB)
  
  sql_create_metadata_table(sqlConnection = newDBconn)
  
  a <- DBI::dbListFields(existingDBconn, "metadata") 
  b <- DBI::dbListFields(newDBconn, "metadata") 
  colToAppend <- a[which(!a %in% b)]            
  
  if (length(colToAppend) > 0) {
    for (i in colToAppend) {
      state <- glue::glue_sql("ALTER TABLE metadata
                                     ADD {i} TEXT",
                              .con = newDBconn)
      
      DBI::dbSendStatement(newDBconn, state)
    }
  }
}


