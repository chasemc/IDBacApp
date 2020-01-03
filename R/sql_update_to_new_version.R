

old_pool <- IDBacApp::createPool("old_idbac",
                                 "C:/Users/CMC/Documents/IDBac_experiments")[[1]]
new_pool <- IDBacApp::createPool("new_idbac",
                                 "C:/Users/CMC/Documents/IDBac_experiments")[[1]]


# Need to update:
# massTable -> mass_index
# metaData -> metadata    # This is actually ok b/c SQL= not case dependent
# XML -> xml              # This is actually ok b/c SQL= not case dependent
# IndividualSpectra -> spectra
# version bump


old_t <- DBI::dbListTables(old_pool)
new_t <- DBI::dbListTables(new_pool)

# Make sure tables are in correct order
old_t <- c("IndividualSpectra",
           "XML",
           "locale",
           "massTable",
           "metaData",
           "version")

new_t <- c( "spectra",
            "xml",
            "locale",
            "mass_index",
            "metadata",
            "version")     


old_f <- lapply(old_t, function(x) DBI::dbListFields(old_pool, x))
new_f <- lapply(new_t, function(x) DBI::dbListFields(new_pool, x))


mapply(function(old, new){
  paste0(old, " is now ", new)
},
old_f,
new_f)
