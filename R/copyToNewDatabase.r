copyToNewDatabase <- function(existingDBPool,
                              newdbPath, 
                              sampleIDs){
  
  
  # Run everything with a Shiny progress bar  
  shiny::withProgress(message = 'Copying data to new database',
                      detail = 'Connecting experiments...',
                      value = 0, {
                        
                        Sys.sleep(1) 
                        
                        # Connect to both databases (create pool and checkout)
                        
                        
                        newDBPool <- pool::dbPool(drv = RSQLite::SQLite(),
                                                  dbname = newdbPath)
                        
                        existingDBconnection <- pool::poolCheckout(existingDBPool)
                        newDBconnection <- pool::poolCheckout(newDBPool)
                        
                        # Attach new database to existing database
                        #----
                        
                        sqlQ <- glue::glue_sql("attach database ({dbPath*}) as newDB;",
                                               dbPath = newdbPath,
                                               .con = existingDBconnection) 
                        
                        DBI::dbSendStatement(existingDBconnection, sqlQ)
                        
                        
                        Sys.sleep(1) 
                        setProgress(value = 0.2, 
                                    message = 'Copying data to new database',
                                    detail = 'Setting up new experiment...',
                                    session = getDefaultReactiveDomain())
                        
                        # Create sqlite tables in new database
                        #-----
                        # Get IDBac database table structures
                        a <- IDBacApp::sqlTableArchitecture(1)
                        
                        #Write table structures to database
                        DBI::dbWriteTable(conn = newDBconnection,
                                          name = "metaData", # SQLite table to insert into
                                          a$metaData, # Insert single row into DB
                                          append = TRUE, # Append to existing table
                                          overwrite = FALSE) # Do not overwrite
                        
                        DBI::dbWriteTable(conn = newDBconnection,
                                          name = "XML", # SQLite table to insert into
                                          a$XML, # Insert single row into DB
                                          append = TRUE, # Append to existing table
                                          overwrite = FALSE) # Do not overwrite
                        
                        DBI::dbWriteTable(conn = newDBconnection,
                                          name = "IndividualSpectra", # SQLite table to insert into
                                          a$IndividualSpectra, # Insert single row into DB
                                          append = TRUE, # Append to existing table
                                          overwrite = FALSE) # Do not overwrite
                        
                        # Copy over the data corresponding to the samples selected
                        #-----
                        
                        Sys.sleep(1) 
                        setProgress(value = 0.5, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying sample information...',
                                    session = getDefaultReactiveDomain())
                        
                        
                        
                        checkStrainIds <- glue::glue_sql("SELECT DISTINCT `Strain_ID`
                                                         FROM `metaData`",
                                                         .con = newDBPool
                        )
                        
                        checkStrainIds <- DBI::dbSendStatement(newDBconnection, checkStrainIds)
                        checkStrainIds <- DBI::dbFetch(checkStrainIds)[ , 1]
                        sampleIDsneeded <- sampleIDs[!sampleIDs %in% checkStrainIds]
                        
                        if(length(sampleIDsneeded) > 0){
                          sqlQ <- glue::glue_sql("INSERT INTO newDB.metaData
                                                 SELECT * 
                                                 FROM `metaData`
                                                 WHERE (`Strain_ID` IN ({strainIds*}))",
                                                 strainIds = sampleIDsneeded,
                                                 .con = existingDBconnection
                          )
                          DBI::dbSendStatement(existingDBconnection, sqlQ)
                          
                        }
                        
                        setProgress(value = 0.7, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying individual spectra...',
                                    session = getDefaultReactiveDomain())
                        
                        
                        # Get fileshas from old db so we don't  add duplicates to new database
                        sqlQ <- glue::glue_sql("SELECT DISTINCT `spectrumSHA`
                                               FROM `IndividualSpectra`
                                               WHERE (`Strain_ID` IN ({strainIds*}))",
                                               strainIds = sampleIDs,
                                               .con = existingDBconnection
                        )
                        olddbshas <- DBI::dbSendStatement(existingDBconnection, sqlQ)
                        olddbshas <- DBI::dbFetch(olddbshas)[ , 1]
                        
                        
                        sqlQ <- glue::glue_sql("SELECT DISTINCT `spectrumSHA`
                                               FROM `IndividualSpectra`",
                                               .con = newDBPool
                        )
                        
                        newdbshas <- DBI::dbSendStatement(newDBconnection, sqlQ)
                        newdbshas <- DBI::dbFetch(newdbshas)[ , 1]
                        
                        newdbshas <- olddbshas[!olddbshas %in% newdbshas]
                        
                        if(length(newdbshas) > 0){
                          
                          sqlQ <- glue::glue_sql("INSERT INTO newDB.IndividualSpectra
                                                 SELECT * 
                                                 FROM `IndividualSpectra`
                                                 WHERE (`spectrumSHA` IN ({spectrumSHAs*}))",
                                                 spectrumSHAs = newdbshas,
                                                 .con = existingDBconnection
                          )
                          
                          DBI::dbSendStatement(existingDBconnection, sqlQ)
                          
                        }
                        
                        
                        setProgress(value = 0.8, 
                                    message = 'Copying data to new database',
                                    detail = 'Copying mzML files...',
                                    session = getDefaultReactiveDomain())
                        
                        
                        
                        # Get fileshas from old db so we don't  add duplicates to new database
                        sqlQ <- glue::glue_sql("SELECT DISTINCT `mzMLSHA`
                                               FROM `IndividualSpectra`
                                               WHERE (`Strain_ID` IN ({strainIds*}))",
                                               strainIds = sampleIDs,
                                               .con = existingDBconnection
                        )
                        olddbshas <- DBI::dbSendStatement(existingDBconnection, sqlQ)
                        olddbshas <- DBI::dbFetch(olddbshas)[ , 1]
                        
                        
                        sqlQ <- glue::glue_sql("SELECT DISTINCT `mzMLSHA`
                                               FROM `IndividualSpectra`",
                                               .con = newDBPool
                        )
                        
                        newdbshas <- DBI::dbSendStatement(newDBconnection, sqlQ)
                        newdbshas <- DBI::dbFetch(newdbshas)[ , 1]
                        
                        
                        newdbshas <- olddbshas[!olddbshas %in% newdbshas]
                        
                        if(length(newdbshas) > 0){
                          
                          
                          
                          sqlQ <- glue::glue_sql("INSERT INTO newDB.XML
                                                 SELECT * 
                                                 FROM `XML`
                                                 WHERE (`mzMLSHA` IN ({mzMLSHA*}))",
                                                 mzMLSHA = newdbshas,
                                                 .con = existingDBconnection
                          )
                          
                          DBI::dbSendStatement(existingDBconnection, sqlQ)
                          
                          
                        }
                        
                        # Clean up
                        #-----
                        
                        # There's probably a better way of setting up the new database tables for the transfer
                        # but the ways it is now, a row of NA's are input so need to be removed 
                        
                        sqlQ <- glue::glue_sql("DELETE FROM `XML`
                                               WHERE `mzMLSHA` IS NULL",
                                               .con = newDBPool
                        )
                        
                        DBI::dbSendStatement(newDBconnection, sqlQ)
                        
                        sqlQ <- glue::glue_sql("DELETE FROM `IndividualSpectra`
                                               WHERE `Strain_ID` IS NULL",
                                               .con = newDBPool
                        )
                        
                        DBI::dbSendStatement(newDBconnection, sqlQ)
                        
                        sqlQ <- glue::glue_sql("DELETE FROM `metaData`
                                               WHERE `Strain_ID` IS NULL",
                                               .con = newDBPool
                        )
                        
                        DBI::dbSendStatement(newDBconnection, sqlQ)
                        
                        
                        
                        poolReturn(existingDBconnection)
                        poolReturn(newDBconnection)
                        poolClose(newDBPool)
                        
                        
                        setProgress(value = 0.9, 
                                    message = 'Copying data to new database',
                                    detail = 'Finishing...',
                                    session = getDefaultReactiveDomain())
                        
                        Sys.sleep(1)
                        
                        
                      })
  
}