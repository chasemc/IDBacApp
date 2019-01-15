





#' 
#'  Using an excel spreadsheet, get the filepath for msconvert and the user-supplied name
#'
#' @param brukerDataPath path to directory containg bruker files
#' @param excel path to excel file
#'
#' @return named list, names are sample IDs, values are paths
#' @export
#'
#' @examples
brukerDataNamesPaths <- function(brukerDataPath,
                                 excel){
  
  files <- list.files(brukerDataPath, pattern="acqus", recursive = TRUE, full.names = TRUE)

  instrument_MetaFile  <- lapply(files, function(x)  read.delim(x, sep="\n"))
  
  
  # Find Acqu file
  spots <- try(lapply(instrument_MetaFile , function(x) as.character(x[grep("SPOTNO=", x[,1]),])),
               silent = TRUE)
  
  validate(need(length(spots) > 0, "Something happened when trying to get the spot position from the acqus file."))
  names(spots) <- dirname(files)
  
  #Parse the Acqu file for the mass error row
  spots <- sapply(spots, function(x) strsplit(x, "##$SPOTNO= ", fixed = TRUE)[[1]][[2]])
  spots <- base::gsub(">", "" ,spots)
  spots <- base::gsub("<", "" ,spots)
  spots <- base::trimws(spots)
  
  
  userExcel <- readxl::read_excel(excel, col_names = FALSE, range ="B2:Y17")
  userExcel <- as.matrix(userExcel)
  lets <- LETTERS[1:16]
  nums <- 1:24
  
  aa <- sapply(nums, function(x) paste0(lets, x))
  aa <- matrix(aa, nrow = 16, ncol = 24)
  
  aa <- sapply(spots, function(x) userExcel[which(aa %in% x)])
  
  
split(names(aa), aa)
  
}



















 
 # 
 # 
 # applibpath <- base::file.path(workingDirectory,
 #                         "library")
 # msconvertLocation <- installed.packages(c(.libPaths(),
 #                                           applibpath))
 # msconvertLocation <- as.list(msconvertLocation[grep("proteowizardinstallation",
 #                                                     msconvertLocation), ])
 # msconvertLocation <- base::file.path(msconvertLocation$LibPath, 
 #                                "proteowizardinstallation", 
 #                                "pwiz")
 # 
 # msconvertLocation <- IDBacApp::findMSconvert(msconvertLocation)
 # 
 # if(msconvertLocation != "error"){
 #   
 #   shiny::showModal(
 #     shiny::modalDialog(
 #       title = "Unable to find msconvert",
 #     shiny::p("Please navigate to:"),
 #       shiny::tags$a("proteowizard.sourceforge.net", href="http://proteowizard.sourceforge.net/download.html")
 #     ))
 #   
 #   
 #   
 #   
 #   
 #   
 #   
 # rawFileConverter <- function()  
 #   
 #   
 #   
 #   
 #   
 #   
 # mzXXconverter <- function() 
 #   
 #   
 #   mzFileInput <- list.files(mzmlRawFilesLocation(),
 #                             recursive = TRUE,
 #                             full.names = TRUE,
 #                             pattern = ".mz") 
 #   
 #   popup1()
 #   
 #   mzmlRawFileDirectory1 <-input$mzmlRawFileDirectory
 #   mzmlRawFilesLocation <- mzFileInput
 #   msconvertLocation <- msconvertLocation
 #   outDir <- tempMZ
 #   
 #   conversions <- IDBacApp::convertToMzml(mzmlRawFileDirectory = input$mzmlRawFileDirectory,
 #                                          mzmlRawFilesLocation = mzFileInput,
 #                                          # spectraConversion = spectraConversion(),
 #                                          msconvertLocation = file.path(msconvertLocation,"msconvert.exe"),
 #                                          outDir = tempMZ)
 #   aw<<-conversions
 #   return(conversions)
 # }