# Add function to check for existing sample IDs

addNewLibrary <- function(samplesToAdd, newDatabase, selectedIDBacDataFolder){

# samplesToAdd:
   # Sample table displayed in IDBac that User adds metadata to
# newDatabase
   # Name of new database
# selectedIDBacDataFolder
  # File path of where the selected IDBac folder w/data resides



# Detect which samples had metadata entered
# This function looks in "samplesToAdd" for any row that contains a column with a string vector with length > 0
toAdd <- which(sapply(as.data.frame(nchar(t(samplesToAdd)[-1, ])), sum) > 0)

# Character vector of sample IDs to be added to the database (Samples with MetaInfo)
toAdd <- as.character(samplesToAdd[ , 1])[toAdd]
# List rds files currently available
rdsFiles <- list.files(paste0(selectedIDBacDataFolder, "/Peak_Lists"),
                       pattern = "ProteinPeaks.rds|_SummedProteinSpectra.rds|_SmallMoleculePeaks.rds",
                       full.names = TRUE)

# Only paths of samples to be added
rdsFiles <- rdsFiles[grep(paste0("/", toAdd , "_", collapse = "|"), rdsFiles)]
# Example rds file name ->   "Sample-XYZ_SummedSmallMoleculeSpectra.rds"  ...  "Sample-XYZ" + "_SummedSmallMoleculeSpectra.rds"
# Get the sample IDs from the rds filename

filesNoPath <- basename(rdsFiles)
lastUnderscore <- unlist(lapply(filesNoPath, function(x) tail(which(strsplit(x[[1]], "")[[1]] %in% "_"),1)))
lengthString <- unlist(lapply(filesNoPath, nchar))

rdsSampleIDs <- stringr::str_sub(filesNoPath, 1, lastUnderscore - 1) # Get only sample IDs






# At minimum we will require a ProteinPeaks.rds file
proteinPeaksRDS <- [grep("ProteinPeaks.rds", filesNoPath)]


# Get the rds type (eg "ProteinPeaks" or "SmallMoleculePeaks") from the rds filename
rdsType <- stringr::str_sub(filesNoPath, lastUnderscore + 1, lengthString) # Get only sample types
# Combine into a dataframe
rdsFiles <- cbind.data.frame(rdsFiles, rdsSampleIDs, rdsType)
# Split based on sample ID
rdsFiles <- split(rdsFiles, rdsSampleIDs)
# Each element of list is named by Sample ID  and contains a dataframe with three columns
# Column 1 -> File paths of rds files
# Column 2 -> Sample IDs (taken from the rds file name)
# Column 3 -> rds type (eg "ProteinPeaks" or "SmallMoleculePeaks") (taken from the rds filename)

# convert into list of lists
rdsFiles <- lapply(rdsFiles, function(x)split(x, x$rdsType))


#--------------- mzXML files
# Get Instrument info
mzXmlSpectraLocation <- list.files(paste0(selectedIDBacDataFolder, "/Converted_To_mzXML"), full.names = TRUE)

# Only paths of samples to be added
mzXmlSpectraLocation <- mzXmlSpectraLocation[grep(paste0("/", toAdd , ".mzXML", collapse = "|"), mzXmlSpectraLocation)]


for (i in 1:length(toAdd)){

  rdsFiles[[i]]$mzXML <- mzXmlSpectraLocation[[i]]

}


kdslmf <<- rdsFiles
wwe<<-samplesToAdd
wwr <<- toAdd
samplesWithMetadata <- samplesToAdd[which((samplesToAdd$Strain_ID) %in% toAdd), ]

orderToAdd <- sapply(samplesWithMetadata$Strain_ID, function(x) which(x == toAdd))


for (i in orderToAdd){

  rdsFiles[[i]]$Meta <- samplesWithMetadata[i,]

}


for(yeppy in rdsFiles){





# This doesn't load spectra, only a pointer
  onemzXmlSpectra <- lapply(yeppy$mzXML, function(x) mzR::openMSfile(x))


instrumentInfo <- lapply(onemzXmlSpectra, function(x) data.frame(mzR::instrumentInfo(x)))


# read in mzxml file
onemzXmlSpectra <- xml2::read_xml(yeppy$mzXML)
# Create hash of mzXMLs
mzXMLhash <- digest::digest(onemzXmlSpectra)

# serialize so we can insert into DB
onemzXmlSpectra <- xml2::xml_serialize(onemzXmlSpectra, NULL)
# compress
onemzXmlSpectra <- memCompress(onemzXmlSpectra, type = "gzip")




# Commented-out columns are already present / what user was presented and filled-in

sqlDataFrame <- data.frame(# "Strain_ID" = "",
  #   "Genbank_Accession" = "",
  #   "NCBI_TaxID = ""
  #  "Kingdom" = "",
  #   "Phylum"  = "",
  #    "Class"   = "",
  #    "Order"   = "",
  #    "Family"  = "",
  #   "Genus"   = "",
  #  "Species" = "",
  #   "Strain"  = "",
  "manufacturer" = instrumentInfo[[1]]$manufacturer,
  "model"        = instrumentInfo[[1]]$model,
  "ionisation"   = instrumentInfo[[1]]$ionisation,
  "analyzer"     = instrumentInfo[[1]]$analyzer,
  "detector"     = instrumentInfo[[1]]$detector,
  "Protein_Replicates"   = NA,
  "Small_Molecule_Replicates" = NA,
  "mzXML"   = NA,
  "proteinPeaksRDS"     = NA,
  "proteinSummedSpectrumRDS"     = NA,
  "mzXMLhash"   = mzXMLhash,
  "proteinPeaksRDShash"     = NA,
  "proteinSummedSpectrumRDShash"     = NA,
  "smallMoleculePeaksRDShash"     = NA

)

# Create SQL Database structure
sqlDataFrame <- cbind.data.frame(yeppy$Meta, sqlDataFrame)

#--
#--

# Insert "rds" files into SQL with hash


# This function takes data that will be turned into a blob object in the SQL table
# It creates a unique hash for the object which is will be inserted as a column with
# the same header as the object, except with "hash" appended.
# It also binzrizes and compresses


addtoDB <- function(inputData, hashID, colID){

                # Path of raw (or processed) data
                readIn <- file.path(data)
                # Read raw (or processed) data
                readIn <- readRDS(readIn)
                # Create hash
                sqlDataFrame[[hashID]] <- digest::digest(readIn)
                # Binarize and  compress
                readIn <- memCompress(serialize(object = readIn,
                                                connection = NULL,
                                                ascii = FALSE,
                                                xdr = FALSE,
                                                version = 3),
                                      type="gzip")
                # Insert into SQL as type blob
                sqlDataFrame[[colID]] <- list(readIn)
            }

# Add protein peaks to database
addtoDB(inputData = yeppy$ProteinPeaks.rds$rdsFiles,
        hashID = "proteinPeaksRDShash",
        colID = "proteinPeaksRDS")
# Add summed protein spectra to database
addtoDB(inputData = SummedProteinSpectra.rds$rdsFiles,
        hashID = "proteinSummedSpectrumRDShash",
        colID = "proteinSummedSpectrumRDS")

# Add small molecule peaks to database
addtoDB(inputData = yeppy$SmallMoleculePeaks.rds$rdsFiles,
        hashID = "smallMoleculePeaksRDShash",
        colID = "smallMoleculePeaksRDS")




#--

# Insert "mzXML" files into SQL
sqlDataFrame$mzXML <- list(onemzXmlSpectra)


#--
#--


# Creates database if one isn't present, otherwise appends to it
DBI::dbWriteTable(conn = newDatabase,
                  name = "IDBacDatabase",
                  sqlDataFrame[1, ],
                  append = TRUE,
                  overwrite = FALSE)
}

}
