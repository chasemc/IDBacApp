copyToNewDatabase <- function(existingDBpath,
                              newdbPath, 
                              sampleIDs){

  # Connect to both databases (create pool and checkout)
  
existingDBPool <- pool::dbPool(drv = RSQLite::SQLite(),
                          dbname = existingDBpath)

newDBPool <- pool::dbPool(drv = RSQLite::SQLite(),
                          dbname = newdbPath)

existingDBconnection <- pool::poolCheckout(existingDBPool)
newDBconnection <- pool::poolCheckout(newDBPool)

# Attach new database to existing database
#----

sqlQ <- glue::glue_sql("attach database ({dbPath*}) as newDB;",
                       dbPath = newdbPath,
                       .con = existingDBPool) 
  
DBI::dbSendStatement(existingDBconnection, sqlQ)


# Create sqlite tables in new database
#-----
# Get IDBac database table structures
a <- IDBacApp::sqlTableArchitecture(1)



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

sqlQ <- glue::glue_sql("INSERT INTO newDB.metaData
                        SELECT * 
                        FROM `metaData`
                        WHERE (`Strain_ID` IN ({strainIds*}))",
                       strainIds = sampleIDs,
                       .con = existingDBPool
)
DBI::dbSendStatement(existingDBconnection, sqlQ)



sqlQ <- glue::glue_sql("INSERT INTO newDB.IndividualSpectra
                        SELECT * 
                       FROM `IndividualSpectra`
                       WHERE (`Strain_ID` IN ({strainIds*}))",
                       strainIds = sampleIDs,
                       .con = existingDBPool
)

DBI::dbSendStatement(existingDBconnection, sqlQ)




samples <- glue::glue_sql("SELECT DISTINCT `mzMLSHA`
                          FROM `IndividualSpectra`",
                          .con = newDBPool
)

samples <- DBI::dbSendQuery(newDBconnection, samples)

sampleIDs <- (DBI::dbFetch(samples)[ , 1])


sqlQ <- glue::glue_sql("INSERT INTO newDB.XML
                        SELECT * 
                       FROM `XML`
                       WHERE (`mzMLSHA` IN ({mzMLSHA*}))",
                       mzMLSHA = sampleIDs[!is.na(sampleIDs)],
                       .con = existingDBPool
)

DBI::dbSendStatement(existingDBconnection, sqlQ)

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


}