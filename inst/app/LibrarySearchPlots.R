


# Libraries that must be installed:
#library(dplyr)
#library(RSQLite)
#library(DBI)
#library(MALDIquant)

# output:

# A three-column tibble with columns:

#"unknownStrainIDs" "librarySpectrum" "score"


# unknown librarySpectrum        score
# 1  114A-1          114C-6 1.560207e-01
# 2  114A-2          114A-2 2.220446e-16



databaseSearch <- function(idbacPath, databasePath, wantReport){

  # Inputs:
  # idbacPath     == idbacDirectory$filePath
  # databasePath  == input$selectedSearchLibrary        #.sqlite database path
  # wantReport == Logical     if TRUE == make ggplots here... will be slower  if == FALSE don't make ggplots... faster
  # Returns:
  # Returns list of top hit matches. Each list element is one unknown sample, with one best cosine similarity score + lib sample ID



  # Get "Unknown" strain protein peak files
  unknownProteinPeakFiles <- list.files(paste0(idbacPath, "\\Peak_Lists"),full.names = TRUE, pattern = "ProteinPeaks.rds")

  unknownStrainIDs <- unlist(strsplit(basename(unknownProteinPeakFiles), "_ProteinPeaks.rds"))





  # This function processes unknown spectra, one at a time (to save RAM)
  # read rds protein peaks file (so we can bin against library spectrum later), bin and filter percent presence based on user input
  # return single MALDIquant object of protein peaks
  unknownProcessing <-  function(unknownProteinPeakFiles){

    unknownSpectrum <- unlist(lapply(unknownProteinPeakFiles, function(x) readRDS(x)))

    labs <- sapply(unknownSpectrum, function(x)metaData(x)$Strain)
    labs <- factor(labs)
    new2 <- NULL
    newPeaks <- NULL
    for (i in seq_along(levels(labs))) {
      specSubset <- (which(labs == levels(labs)[[i]]))
      if (length(specSubset) > 1) {
        unknownSpectrum <- MALDIquant::trim(unknownSpectrum, c(3000,15000))
        unknownSpectrum <- MALDIquant::binPeaks(unknownSpectrum, tolerance = .002, method = "relaxed")
        new <- filterPeaks(unknownSpectrum[specSubset],minFrequency= 0/100)
        new<-mergeMassPeaks(new,method="mean")
        new2 <- c(new2, new)
      } else{
        new2 <- c(new2, unknownSpectrum[specSubset])
      }

    }
    unknownSpectrum <- new2

  }


  unknowns <- unknownProcessing(unknownProteinPeakFiles=unknownProteinPeakFiles)














  # This is the workhorse of the searching.
  # Takes a list of unknown *Sample IDs* as input

  searchLibrary <- function(unk, databasePathin){
    .libPaths("C:/Users/chase/Documents/IDBacPackages")

    library(dplyr)
    library(RSQLite)
    library(DBI)
    library(MALDIquant)

    # This function processes library spectra, one at a time (to save RAM)
    # read SQLite, bin and filter percent presence based on user input
    # return single MALDIquant object of protein peaks
    libraryProcessing <-  function(singleLibSpec){

      # Return the "rds" SQL blob for the individual strain
      libProteinPeaks <-  libSpec %>%
        filter(Strain_ID == singleLibSpec) %>%
        select(rds) %>%
        collect()
      #Decompress blob
      libProteinPeaks <- memDecompress(libProteinPeaks[[1]][[1]], type="gzip")
      # Unserialize blob
      libProteinPeaks <- unserialize(libProteinPeaks, NULL)
      # Unlist rds file list
      libProteinPeaks <- unlist(libProteinPeaks, recursive = TRUE)
      # Return only protein peak MALDIquant objects
      libProteinPeaks <- libProteinPeaks[grep("ProteinPeaks", names(libProteinPeaks))]

          libProteinPeaks <- MALDIquant::trim(libProteinPeaks, c(3000,15000))
          libProteinPeaks <- MALDIquant::binPeaks(libProteinPeaks, tolerance = .002, method = "relaxed")
          libProteinPeaks <- mergeMassPeaks(new, method="sum")

    }



    # connect to user-specified database
    dbcon <- DBI::dbConnect(RSQLite::SQLite(), databasePathin)

    # Connect dplyr to database
    db <- dplyr::tbl(dbcon, "IDBacDatabase")

    # Filter database by whatever metadata
    # Return only rds column and strain ID
    libSpec <- db %>%
      #filter(Strain_ID == "114A-2") %>%
      dplyr::select(c(Strain_ID,rds))



    # Note: cannot "slice()" or select by index within SQL table, so will just
    # filter by strain ID
    libStrainIDs <- libSpec %>%
      select(Strain_ID) %>%
      collect() %>%
      unlist() %>%
      as.vector()



    # Use Cosine Similarity to determine spectra similarity (Note: 0 is close, 1 is not close)
    cosineD <- function(x) {
      as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
    }
    # Process single unknown sample spectrum
    unk1 <- unk
    # Perform cosine similarity search across all library spectra
    qq <-lapply(libStrainIDs, function(lib){
      # Process single library strain
      lib1 <- libraryProcessing(lib)
      # Bin one unknown and one library spectra (strict = one peak per bin)
      a <- MALDIquant::binPeaks(c(lib1[[1]], unk1), tolerance = 0.02, method = "strict")
      # Turn into a matrix, rows = samples, columns = binned peaks, cells = peak intensity
      b <- MALDIquant::intensityMatrix(a)
      # Get sample names of the two spectra
      rownames(b) <- sapply(a, function(x) x@metaData$Strain)
      # Replace NA values with 0
      b[is.na(b)] <- 0
      # Perform cosine similarity function
      qq <- cosineD(b)
      qq <- as.matrix(qq)
      qq <- as.data.frame(qq)
      qq[upper.tri(qq, diag = TRUE)] <- 1
      min(qq) # return min cosine score
    })



    # This collects all lib search results for one unknown spectrum and turns into a tbl
    # Colums: "librarySpectrum" = library sample ID, "score" = cosine similarity score
bind_cols(librarySpectrum = libStrainIDs, cosine = unlist(qq))
    # Return only the closest library match


  }







  # Run the library search. Inputs: "unknownStrainIDs" ; "searchLibrary" = function
  # Returns list of top hit matches. Each list element = 1 unknown sample, with 1 best cosine similarity score = lib sample ID
  allScores <- searchLibrary(unknowns, databasePathin = databasePath)

#
#
#         try(numCores <- parallel::detectCores())
#
#         if(exists("numCores") & numCores > 2){
#         numCores <- parallel::makeCluster(numCores-1)
#
#         allScores <- parallel::parLapply(numCores, unknownStrainIDs, searchLibrary, databasePathin = databasePath)
#
#         parallel::stopCluster(numCores)
#         }







  #----------------------------------------------------------------------------
  #----------------------------------------------------------------------------
  # create plots

  #eg of (t(allScores))
  # [,1]           [,2]
  # unknown         "114A-1"       "114A-2"
  # librarySpectrum "114A-2"       "114A-2"
  # score           "7.240327e-01" "2.220446e-16"




 if(wantReport == "TRUE"){
  unknownStrain <- unknownProcessing(sampLibids[[1]])            # sampLibids[[1]] = id of unknonw strain
  # Perform cosine similarity search across all library spectra
  # Process single library strain
  libraryStrain <- libraryProcessing(sampLibids[[2]])            # sampLibids[[2]] = id of unknown strain
  # Bin one unknown and one library spectra (strict = one peak per bin)
  binned <- MALDIquant::binPeaks(c(unknownStrain[[1]], libraryStrain[[1]]), tolerance = 0.02, method = "strict")

  # Create dataframes for peak plots and color each peak according to whether it occurs in the other spectrum
  # binned[[1]] = unknown
  # binned[[2]] = library
  unknownStrain <- as.data.frame(cbind(binned[[1]]@mass, binned[[1]]@intensity))
  libraryStrain <- as.data.frame(cbind(binned[[2]]@mass, binned[[2]]@intensity))

  # Color all positive peaks red
  unknownStrain <- data.frame(unknownStrain,rep("red",length = length(unknownStrain$V1)), stringsAsFactors = F)
  colnames(unknownStrain) <- c("Mass", "Intensity", "Color")


  # Color all negative peaks grey
  libraryStrain <- data.frame(libraryStrain, rep("grey", length = length(libraryStrain$V1)), stringsAsFactors = F)
  colnames(libraryStrain) <- c("Mass", "Intensity", "Color")

  # Color all peak matches blue for positive peaks
  unknownStrain$Color[which(unknownStrain$Mass %in% intersect(unknownStrain$Mass, libraryStrain$Mass))] <- "blue"

  # get full spectra
  # Return the "rds" SQL blob for the individual strain
  librarySpectrum <-  libSpec %>%
    filter(Strain_ID == sampLibids[[2]]) %>%
    select(rds) %>%
    collect()
  #Decompress blob
  librarySpectrum <- memDecompress(unlist(librarySpectrum), type="gzip")
  # Unserialize blob
  librarySpectrum <- unserialize(librarySpectrum, NULL)
  # Unlist rds file list
  librarySpectrum <- unlist(librarySpectrum, recursive = TRUE)
  # Return only protein peak MALDIquant objects
  librarySpectrum <- librarySpectrum[grep("SummedProteinSpectra", names(librarySpectrum))][[1]]


  # read unknown dull spectrum
  unknownSpectrum <- list.files(paste0(idbacPath, "\\Peak_Lists"),full.names = TRUE, pattern = "_SummedProteinSpectra.rds")
  unknownSpectrumBase <- unlist(strsplit(basename(unknownSpectrum), "_SummedProteinSpectra.rds"))


  unknownSpectrum <- readRDS(unknownSpectrum[which(unknownSpectrumBase == sampLibids[[1]])])


p <- ggplot() +
  geom_line(aes(x = unknownSpectrum@mass,
                y = unknownSpectrum@intensity)) +
  geom_line(aes(x = librarySpectrum@mass,
                y = -librarySpectrum@intensity)) + # invert spectrum intensity
  geom_bar(aes(x = unknownStrain$Mass,
               y = ((unknownStrain$Intensity) * max(unknownSpectrum@intensity)/max(unknownStrain$Intensity))), # invert spectrum intensity
               fill = unknownStrain$Color,
               stat = "identity",
               width = 10) +
  geom_bar(aes(x = libraryStrain$Mass,
               y = -((libraryStrain$Intensity) * max(librarySpectrum@intensity) / max(libraryStrain$Intensity))),
               fill = libraryStrain$Color,
               stat ="identity",
               width = 10) +
  coord_cartesian(ylim = c(-max(librarySpectrum@intensity), max(unknownSpectrum@intensity))) +
  xlab(expression(italic("m/z"))) +
  ylab("Intensity")

annotation <- paste0("Top- Searched Specrum: ", sampLibids[[1]], "\n",
                     "Bottom- Closest Library Spectrum: ", sampLibids[[2]])

  p2 <- ggplot() +
    annotate("text",
             x = 1,
             y = 1,
             label = annotation) +
             theme_void()


  filtered <-db %>%
    filter(Strain_ID == sampLibids[[2]]) %>%
    select(-c("manufacturer",
              "model",
              "ionisation",
              "analyzer",
              "detector",
              "Protein_Replicates",
              "Small_Molecule_Replicates",
              "mzXML",
              "rds")) %>%
    collect()



    }else{ # wantReport == FALSE


      return(allScores)


}





}






