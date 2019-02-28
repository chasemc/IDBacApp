#' Copy from one database to another, selecting by Sample ID
#'
#' @param existingDBPool un-checked pool that samples are being pulled from 
#' @param newdbPath  path to where new db should be created
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
                        
                        
                        
                        # Attach new database to existing database
                        #----
                        
                        
                        IDBacApp::copyDB_dbAttach(newdbPath = newdbPath, 
                                                  existingDBconn = existingDBconn)
                        
                        setProgress(value = 0.2, 
                                    message = 'Copying data to new database',
                                    detail = 'Setting up new experiment...',
                                    session = getDefaultReactiveDomain())
                        
                        # Create sqlite tables in new database
                        #-----
                        # Get IDBac database table structures
                        arch <- IDBacApp::sqlTableArchitecture(1)
                        
                        # Account for modifications to DB structure -------------------------------
                        
                        
                        # Below, when setting up the new tables, we need to add the existing columns which may not be present in 
                        # the current architecture, and also have the current architecture (ie we can't just copy/paste from the old DB)
                        
                        
                        # Setup New metaData ------------------------------------------------------
                        
                        
                        IDBacApp::copyDB_setupMeta(newDBconn = newDBconn,
                                                   existingDBconn = existingDBconn,
                                                   arch = arch)
                        
                        
                        # Setup New XML -----------------------------------------------------------
                        
                        IDBacApp::copyDB_setupXML(newDBconn = newDBconn,
                                                  existingDBconn = existingDBconn,
                                                  arch = arch)
                        
                        # Setup New IndividualSpectra ---------------------------------------------
                        
                        IDBacApp::copyDB_setupIndividualSpectra(newDBconn = newDBconn,
                                                                existingDBconn = existingDBconn,
                                                                arch = arch)
                        
                        # Copy over the data corresponding to the samples selected ----------------
                        
                        Sys.sleep(1) 
                        setProgress(value = 0.5, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying sample information...',
                                    session = getDefaultReactiveDomain())
                        
                        
                        
                        
                        #
                        
                        setProgress(value = 0.7, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying individual spectra...',
                                    session = getDefaultReactiveDomain())
                        
                        
                        # Get fileshas from old db so we don't  add duplicates to new database
                        sqlQ <- glue::glue_sql("SELECT DISTINCT `spectrumSHA`
                                               FROM `IndividualSpectra`
                                               WHERE (`Strain_ID` IN ({strainIds*}))",
                                               strainIds = sampleIDs,
                                               .con = existingDBconn
                        )
                        olddbshas1 <- DBI::dbSendStatement(existingDBconn, sqlQ)
                        warning(olddbshas1@sql)
                        olddbshas <- DBI::dbFetch(olddbshas1)[ , 1]
                        DBI::dbClearResult(olddbshas1) 
                        
                        sqlQ <- glue::glue_sql("SELECT DISTINCT `spectrumSHA`
                                               FROM `IndividualSpectra`",
                                               .con = newDBPool
                        )
                        
                        newdbshas1 <- DBI::dbSendStatement(newDBconn, sqlQ)
                        warning(newdbshas1@sql)
                        newdbshas <- DBI::dbFetch(newdbshas1)[ , 1]
                        DBI::dbClearResult(newdbshas1) 
                        
                        
                        
                        newdbshas <- olddbshas[!olddbshas %in% newdbshas]
                        a <- DBI::dbListFields(existingDBconn, "IndividualSpectra")
                        if(length(newdbshas) > 0){
                          sqlQ <- glue::glue_sql("INSERT INTO newDB.IndividualSpectra ({`a`*})
                                                 SELECT {`a`*}
                                                 FROM IndividualSpectra
                                                 WHERE (`spectrumSHA` = ?)",
                                                 .con = existingDBconn)
                          temp <- DBI::dbSendStatement(existingDBconn, sqlQ)
                          rw <- DBI::dbBind(temp, list(newdbshas))
                          
                          warning(rw@sql)
                          DBI::dbClearResult(temp)
                        }
                        
                        setProgress(value = 0.8, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying mzML files...',
                                    session = getDefaultReactiveDomain())
                        
                        
                        
                        # Get fileshas from old db so we don't  add duplicates to new database
                        sqlQ <- glue::glue_sql("SELECT DISTINCT `mzMLHash`
                                               FROM `IndividualSpectra`
                                               WHERE (`Strain_ID` IN ({strainIds*}))",
                                               strainIds = sampleIDs,
                                               .con = existingDBconn
                        )
                        olddbshas1 <- DBI::dbSendStatement(existingDBconn, sqlQ)
                        warning(olddbshas1@sql)
                        olddbshas <- DBI::dbFetch(olddbshas1)[ , 1]
                        DBI::dbClearResult(olddbshas1)
                        
                        sqlQ <- glue::glue_sql("SELECT DISTINCT `mzMLHash`
                                               FROM `IndividualSpectra`",
                                               .con = newDBPool
                        )
                        
                        newdbshas1 <- DBI::dbSendStatement(newDBconn, sqlQ)
                        warning(newdbshas1@sql)
                        newdbshas <- DBI::dbFetch(newdbshas1)[ , 1]
                        DBI::dbClearResult(newdbshas1)
                        
                        newdbshas <- olddbshas[!olddbshas %in% newdbshas]
                        
                        if (length(newdbshas) > 0) {
                          
                          
                          
                          sqlQ <- glue::glue_sql("INSERT INTO newDB.XML
                                                 SELECT * 
                                                 FROM `XML`
                                                 WHERE (`mzMLHash` IN ({mzMLHash*}))",
                                                 mzMLHash = newdbshas,
                                                 .con = existingDBconnection

                          )
                          
                          temp <- DBI::dbSendStatement(existingDBconn, sqlQ)
                          warning(temp@sql)
                          DBI::dbClearResult(temp)
                          
                        }
                        
                        # Clean up
                        #-----
                        
                        # There's probably a better way of setting up the new database tables for the transfer
                        # but the ways it is now, a row of NA's are input so need to be removed 
                        
                        sqlQ <- glue::glue_sql("DELETE FROM `XML`
                                               WHERE `mzMLHash` IS NULL",
                                               .con = newDBPool
                        )
                        
                        temp <- DBI::dbSendStatement(newDBconn, sqlQ)
                        warning(temp@sql)
                        DBI::dbClearResult(temp)
                        sqlQ <- glue::glue_sql("DELETE FROM `IndividualSpectra`
                                               WHERE `Strain_ID` IS NULL",
                                               .con = newDBPool
                        )
                        
                        temp <- DBI::dbSendStatement(newDBconn, sqlQ)
                        warning(temp@sql)
                        DBI::dbClearResult(temp)
                        sqlQ <- glue::glue_sql("DELETE FROM `metaData`
                                               WHERE `Strain_ID` IS NULL",
                                               .con = newDBPool
                        )
                        
                        temp <- DBI::dbSendStatement(newDBconn, sqlQ)
                        warning(temp@sql)
                        DBI::dbClearResult(temp)
                        
                        
                        
                        
                        shiny::setProgress(value = 0.9, 
                                           message = 'Copying data to new database',
                                           detail = 'Indexing new database...',
                                           session = getDefaultReactiveDomain())
                        warning("Creating index")
                        
                        
                        a <- DBI::dbSendStatement('CREATE INDEX IF NOT EXISTS ids ON IndividualSpectra (Strain_ID);',
                                                  conn = newDBconn)
                        DBI::dbClearResult(a)
                        
                        warning("Created index")
                        
                        shiny::setProgress(value = 0.99, 
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
#' @param newdbPath 
#' @param existingDBconn 
#'
#' @return
#' @export
#'
#' @examples
copyDB_dbAttach <- function(newdbPath, 
                            existingDBconn){
  
  sqlQ <- glue::glue_sql("attach database ({dbPath*}) as newDB;",
                         dbPath = newdbPath,
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
  
  a <- DBI::dbListFields(existingDBconn, "metaData") 
  colToAppend <- a[which(!a %in% colnames(arch$metaData))]                        

  #Write table structures to database
  DBI::dbWriteTable(conn = newDBconn,
                    name = "metaData", # SQLite table to insert into
                    arch$metaData, # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite
}


#' Setup XML DB table
#'
#' @param newDBconn newDBconn 
#' @param existingDBconn  existingDBconn
#' @param arch DB architecture 
#'
#' @return NA
#' @export
#'
copyDB_setupXML <- function(newDBconn,
                            existingDBconn,
                            arch){
  a <- DBI::dbListFields(existingDBconn, "XML") 
  colToAppend <- a[which(!a %in% colnames(arch$XML))]                        
  if (length(colToAppend) > 0) {
    colToAppend <- stats::setNames(rep(NA, length(colToAppend)), colToAppend)
    arch$XML <- cbind(arch$XML, colToAppend)
  }
  DBI::dbWriteTable(conn = newDBconn,
                    name = "XML", # SQLite table to insert into
                    arch$XML, # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite
}




#' Setup IndividualSpectra DB table
#'
#' @param newDBconn newDBconn 
#' @param existingDBconn  existingDBconn
#' @param arch DB architecture 
#'
#' @return NA
#' @export
#'
copyDB_setupIndividualSpectra <- function(newDBconn,
                                          existingDBconn,
                                          arch){
  a <- DBI::dbListFields(existingDBconn, "IndividualSpectra") 
  colToAppend <- a[which(!a %in% colnames(arch$IndividualSpectra))]                        
  if (length(colToAppend) > 0) {
    colToAppend <- stats::setNames(rep(NA, length(colToAppend)), colToAppend)
    arch$IndividualSpectra <- cbind(arch$IndividualSpectra, colToAppend)
  }
  DBI::dbWriteTable(conn = newDBconn,
                    name = "IndividualSpectra", # SQLite table to insert into
                    arch$IndividualSpectra, # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite
}






function(newDBconn,
         existingDBconn,
         arch,
         sampleIDs){
 
  temp <- DBI::dbSendQuery(newDBconn,
                                     "SELECT DISTINCT `Strain_ID` FROM `metaData`")
  message(temp@conn@dbname)
  message(temp@sql)
  checkStrainIds <- DBI::dbFetch(checkStrainIds)[ , 1]
  DBI::dbClearResult(temp) 
  sampleIDsneeded <- sampleIDs[!sampleIDs %in% checkStrainIds]
  
  if (length(sampleIDsneeded) > 0) {
    
    temp <- DBI::dbSendStatement(existingDBconn, "INSERT INTO newDB.metaData
                                                  SELECT * 
                                                  FROM `metaData`
                                                  WHERE (`Strain_ID` IN ?)")
    
    DBI::dbBind(temp, sampleIDsneeded)
    
    sqlQ <- glue::glue_sql("INSERT INTO newDB.metaData
                         SELECT * 
                                                 FROM `metaData`
                                                 WHERE (`Strain_ID` IN ({strainIds*}))",
                           strainIds = sampleIDsneeded,
                           .con = existingDBconn
    )
    temp <- DBI::dbSendStatement(existingDBconn, sqlQ)
    warning(temp@sql)
    DBI::dbClearResult(temp) 
  }
}