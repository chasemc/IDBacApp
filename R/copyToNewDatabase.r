#' Copy from one database to another, selecting by Sample ID
#'
#' @param existingDBPool un-checked pool that samples are being pulled from 
#' @param newDBPool  new unchecked pool
#' @param sampleIDs  smaples to transfer
#' @param newdbName new db name
#'
#' @return NA
#' @export
#'

copyToNewDatabase <- function(existingDBPool,
                              newDBPool,
                              newdbName,
                              sampleIDs){
  
  # Run everything with a Shiny progress bar  
  shiny::withProgress(message = 'Copying data to new database',
                      detail = 'Connecting experiments...',
                      value = 0, {
                        
                        Sys.sleep(1) 
                        
                        # Connect to both databases (create pool and checkout)
                        newDBconn <- pool::poolCheckout(newDBPool)
                        existingDBconn <- pool::poolCheckout(existingDBPool)
                        newdbPath <- gsub("\\\\", "/", newDBconn@dbname)
                        
                        warning(paste0("Begin migration of \n",
                                       existingDBconn@dbname,
                                       " to \n", newDBconn@dbname))
                        
                      
                        setProgress(value = 0.2, 
                                    message = 'Copying data to new database',
                                    detail = 'Setting up new experiment...',
                                    session = getDefaultReactiveDomain())
                        
                        # Create sqlite tables in new database
                        #-----
                    
                        
                        
                        # Account for modifications to DB structure -------------------------------
                        
                        
                        # Below, when setting up the new tables, we need to add the existing columns which may not be present in 
                        # the current architecture, and also have the current architecture (ie we can't just copy/paste from the old DB)
                        
                        
                        # Setup New metaData ------------------------------------------------------
                        
                        IDBacApp::copyDB_setupMeta(newDBconn = newDBconn,
                                                   existingDBconn = existingDBconn,
                                                   arch = arch)
                        
                        # Setup New XML -----------------------------------------------------------
                        
                        IDBacApp::sql_CreatexmlTable(sqlConnection = newDBconn)
                        
                        
                        # Setup New massTable ---------------------------------------------
                        
                        
                        IDBacApp::sql_CreatemassTable(sqlConnection = newDBconn)
                        
                        
                        # Setup New IndividualSpectra ---------------------------------------------

                        IDBacApp::sql_CreateIndividualSpectra(sqlConnection = newDBconn)
                        

                        # Setup version table -----------------------------------------------------

                        IDBacApp::sql_CreateVersionTable(sqlConnection = newDBconn)
                        
# Copy  -------------------------------------------------------------------
                        # Attach new database to existing database
                        #----
                        
                          IDBacApp::copyDB_dbAttach(newdbPath = newdbPath, 
                                                  existingDBconn = existingDBconn)
                        
                        Sys.sleep(1) 
                        
                        
                        
                        setProgress(value = 0.5, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying metadata...',
                                    session = getDefaultReactiveDomain())
                        
                        
                        
                        state <- DBI::dbSendStatement(existingDBconn, 
                                                      "INSERT INTO newDB.metaData
                                                      SELECT *
                                                      FROM main.metaData
                                                      WHERE (Strain_ID = ?)")
                        DBI::dbBind(state, list(sampleIDs))
                        warning(state@sql)
                        DBI::dbClearResult(state) 
                        
                        
                        
                        
                        setProgress(value = 0.7, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying individual spectra...',
                                    session = getDefaultReactiveDomain())
                        
                        
                        
                        state <- DBI::dbSendStatement(existingDBconn, 
                                                      "INSERT INTO newDB.IndividualSpectra
                                                      SELECT *
                                                      FROM main.IndividualSpectra
                                                      WHERE (Strain_ID = ?)")
                        DBI::dbBind(state, list(sampleIDs))
                        warning(state@sql)
                        DBI::dbClearResult(state) 
                        
                        # Copy XML table ----------------------------------------------------------
                        setProgress(value = 0.8, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying mzML files...',
                                    session = getDefaultReactiveDomain())
                        
                        state <- DBI::dbSendQuery(existingDBconn, 
                                                  "SELECT DISTINCT XMLHash
                                                      FROM main.IndividualSpectra
                                                      WHERE (Strain_ID = ?)")
                        DBI::dbBind(state, list(sampleIDs))
                        res <- DBI::fetch(state)
                        DBI::dbClearResult(state)
                        
                        state <- DBI::dbSendStatement(existingDBconn, 
                                                      "INSERT INTO newDB.XML
                                                      SELECT *
                                                      FROM main.XML
                                                      WHERE (XMLHash = ?)")
                        DBI::dbBind(state, list(res[ , 1]))
                        warning(state@sql)
                        DBI::dbClearResult(state) 
                        
                        
                        
                        
                        
                        
                        # Copy massTable ----------------------------------------------------------
                        
                        
                        state <- DBI::dbSendQuery(existingDBconn, 
                                                  "SELECT DISTINCT spectrumMassHash
                                                      FROM main.IndividualSpectra
                                                      WHERE (Strain_ID = ?)")
                        DBI::dbBind(state, list(sampleIDs))
                        res <- DBI::fetch(state)
                        DBI::dbClearResult(state)
                        
                        state <- DBI::dbSendStatement(existingDBconn, 
                                                      "INSERT INTO newDB.massTable
                                                      SELECT *
                                                      FROM main.massTable
                                                      WHERE (spectrumMassHash = ?)")
                        
                        DBI::dbBind(state, list(res[ , 1]))
                        warning(state@sql)
                        DBI::dbClearResult(state) 
                        
                      
                        
                        shiny::setProgress(value = 1, 
                                           message = 'Copying data to new database',
                                           detail = 'Finishing...',
                                           session = getDefaultReactiveDomain())
                        
                        pool::poolReturn(existingDBconn)
                        pool::poolReturn(newDBconn)
                        pool::poolClose(newDBPool)
                        
                        Sys.sleep(1)
                        warning(paste0("End migration of \n",
                                       existingDBconn@dbname,
                                       " to \n", newDBconn@dbname))
                        
                      })
  
}


#' Attach new database to existing database
#'
#' @param newdbPath newdbPath
#' @param existingDBconn existingDBconn
#'
#' @return NA
#' @export
#'
copyDB_dbAttach <- function(newdbPath, 
                            existingDBconn){
  
  sqlQ <- glue::glue_sql("ATTACH DATABASE {newdbPath} as newDB;",
                         .con = existingDBconn) 
  temp <- DBI::dbSendStatement(existingDBconn, sqlQ)
  warning(temp@sql)
  DBI::dbClearResult(temp)
  
}


#' Setup metadata DB table
#'
#' @param newDBconn newDBconn 
#' @param existingDBconn  existingDBconn
#' @param arch DB architecture 
#'
#' @return NA
#' @export
#'
copyDB_setupMeta <- function(newDBconn,
                             existingDBconn,
                             arch){
  
  IDBacApp::sql_CreatemetaData(sqlConnection = newDBconn)
  
  a <- DBI::dbListFields(existingDBconn, "metaData") 
  b <- DBI::dbListFields(newDBconn, "metaData") 
  colToAppend <- a[which(!a %in% b)]            
  
  if (length(colToAppend) > 0) {
    for (i in colToAppend) {
      state <- glue::glue_sql("ALTER TABLE metaData
                                     ADD {i} TEXT",
                              .con = newDBconn)
      
      DBI::dbSendStatement(newDBconn, state)
    }
  }
}


