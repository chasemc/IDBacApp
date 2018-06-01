addNewLibrary <- function(samplesToAdd, newDatabase, IDBacAppLocation){

# samplesToAdd == Sample table displayed in IDBac that User adds metadata to

###  Add function to check for existing sample IDs

# Which samples had strain info inputs in the table
# This function just looks in "samplesToAdd" for any row that contains a column with a string vector of length > 0
toAdd <-  which(sapply(as.data.frame(nchar(t(samplesToAdd)[2:9, ])), sum) > 0)

# Character vector of only samples to be added to SQL DB (Samples with MetaInfo)
toAdd <- as.character(samplesToAdd[,1])[toAdd]

# List rds files currently available
rdsFiles <- list.files(paste0(IDBacAppLocation, "\\Peak_Lists"),
                       pattern = "ProteinPeaks.rds|_SummedProteinSpectra.rds|_SmallMoleculePeaks.rds",
                       full.names = TRUE)

# Example rds file name ->   "Sample-XYZ_SummedSmallMoleculeSpectra.rds"  ...  "Sample-XYZ" + "_SummedSmallMoleculeSpectra.rds"
# Get the sample IDs from the rds filename

filesNoPath <- basename(rdsFiles)
lastUnderscore <- unlist(lapply(filesNoPath, function(x) tail(which(strsplit(x[[1]], "")[[1]] %in% "_"),1)))
lengthString <- unlist(lapply(filesNoPath, nchar))

rdsSampleIDs <- stringr::str_sub(filesNoPath, 1, lastUnderscore - 1) # Get just sample IDs

# Get the rds type (eg "ProteinPeaks" or "SmallMoleculePeaks") from the rds filename
rdsType <- stringr::str_sub(filesNoPath, lastUnderscore + 1, lengthString) # Get just sample types
# Combine into a dataframe
rdsFiles <- cbind.data.frame(rdsFiles, rdsSampleIDs, rdsType)
# Split based on sample ID
rdsFiles <- split(rdsFiles, rdsSampleIDs)
# Each element of list is named by Sample ID  and contains a dataframe with three columns
# Column 1 -> File paths of rds files
# Column 2 -> Sample IDs (taken from the rds file name)
# Column 3 -> rds type (eg "ProteinPeaks" or "SmallMoleculePeaks") (taken from the rds filename)


# Get RDS files that had metadata inserted by the user

rdsFiles <- rdsFiles[which(names(rdsFiles) %in% toAdd)]

#--------------- mzXML files
# Get Instrument info
mzXmlSpectraLocation <- list.files(paste0(IDBacAppLocation, "/Converted_To_mzXML"), full.names = TRUE)
mzXMLSampleIDs <- unlist(lapply(mzXmlSpectraLocation, function(x) strsplit(basename(x), ".mz")[[1]][[1]]))
mzXmlSpectra <- lapply(mzXmlSpectraLocation, function(x) mzR::openMSfile(x))
names(mzXmlSpectra) <- mzXMLSampleIDs

mzXmlSpectra <- mzXmlSpectra[which(names(mzXmlSpectra) %in% toAdd)]

samplesWithMetadata <- samplesToAdd[which((samplesToAdd$Strain_ID) %in% toAdd), ]

for(i in 1:length(rdsFiles)){

  rdsContents <- sapply(as.character(rdsFiles[i][[1]][,1]), function(x) readRDS(x))
  # Make names of list elements correspond to rds file it came from
  # "ProteinPeaks.rds"         "SmallMoleculePeaks.rds"   "SummedProteinSpectra.rds"
  names(rdsContents) <- as.character(rdsFiles[i][[1]][,3])
  # nest list into a one-element list that will be named the sample ID
  rdsContents <- setNames(list(rdsContents), names(rdsFiles[i]))
  # eg ->  rdsContents$`172-1`$ProteinPeaks.rds


# What remains is rdsContents, which is a list the length of the number sample IDs,
#  > names(rdsContents)
# [1] "172-1"  "172-10" "172-11" "172-7"

# Each sample ID list element contains 3 lists each names based on the type of rds
# > names(rdsContents$`172-1`)
# [1] "ProteinPeaks.rds"         "SmallMoleculePeaks.rds"   "SummedProteinSpectra.rds"


# Each rds-type list element contains a list of MALDIquant objects for that sample and that rds type
# > rdsContents$`172-1`$ProteinPeaks.rds
#  [[1]]
#  S4 class type            : MassPeaks
#  Number of m/z values     : 144
#  Range of m/z values      : 1919.939 - 10444.253
#  Range of intensity values: 2.682e+00 - 1.082e+02
#  Range of snr values      : 4.032 - 162.6
#  Memory usage             : 4.914 KiB
#
#  [[2]]
#  S4 class type            : MassPeaks
#  Number of m/z values     : 133
#  Range of m/z values      : 1919.939 - 9506.526
#  Range of intensity values: 2.541e+00 - 7.357e+01
#  Range of snr values      : 4.039 - 116.964
#  Memory usage             : 4.656 KiB






# serialize rds contents
rdsContents <- sapply(rdsContents, function(x) memCompress(serialize(x, NULL), type="gzip"))

onemzXmlSpectra <- mzXmlSpectra[i]
instrumentInfo <- lapply(onemzXmlSpectra, function(x) data.frame(mzR::instrumentInfo(x)))

# mzR::peaks will get mzml spectra and then we'll create a serialized blob
#    This winds up being a list (each element = a different spectrum) of two-column matrices
#       matrix[ , 1] == m/z
#       matrix[ , 2] == Intensity
onemzXmlSpectra <- lapply(onemzXmlSpectra, function(x) mzR::peaks(x))

# serialize rds contents
onemzXmlSpectra <- lapply(onemzXmlSpectra, function(x) memCompress(serialize(onemzXmlSpectra, NULL), type= "gzip"))

# Now we have 3 pieces of info
#
# instrumentInfo    = character columns
# rdsContents       = blob
# mzXmlSpectra      = blob


# Make connection to database
# Probably need to make multiple "x.sqlite" DBs for each library created so people can easily share


# Commented-out columns are already present

sqlDataFrame <- data.frame(# "Strain_ID" = "",
                       #   "Genbank_Accession" = "",
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
                          "rds"     = NA
)

sqlDataFrame <- cbind.data.frame(samplesWithMetadata[i, ], sqlDataFrame)

sqlDataFrame$rds <- list(rdsContents[1][[1]])

sqlDataFrame$mzXML <- list(onemzXmlSpectra[1][[1]])


DBI::dbWriteTable(newDatabase, "IDBacDatabase", sqlDataFrame[1, ], append = TRUE , overwrite = FALSE)


}

}
