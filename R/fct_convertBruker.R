#' Convert Bruker raw fids to IDBac database
#'
#' @param dataDirectory path to directory containing fids (warning: does a recursive path search for fids)
#' @param fileName name of IDBac SQLite file
#' @param filePath path where IDBac SQLite file will be written
#' @param sampleMap matrix or df representing the MALDI target plate (you can replace values in matrix created with IDBacApp:::map384Well())
#' @param anyMissing  advanced, mainly here to be used by the GUI code
#' @param acquisitionInfo advanced, mainly here to be used by the GUI code
#' @param tempDir temporary folder with read/write permission to write intermediate mzml files to
#'
#' @return none, side effect of writing SQLite  file
#' @export
#'
db_from_bruker <- function(dataDirectory = NULL,
                           fileName = NULL,
                           filePath = NULL,
                           anyMissing = NULL,
                           acquisitionInfo = NULL,
                           sampleMap = NULL,
                           tempDir = NULL){
  
  if (length(dataDirectory) != 1L) {
    stop("dataDirectory must be character of length 1")
  }
  if (!dir.exists(dataDirectory)){
    stop("dataDirectory doesn't exist")
  }
  if (length(filePath) != 1L) {
    stop("filePath must be character of length 1")
  }
  if (!dir.exists(filePath)){
    stop("dataDirectory doesn't exist")
  }
  if (!inherits(sampleMap, c("matrix", "data.frame"))) {
    stop("sampleMap must be a matrix or dataframe")
  }
  if (length(fileName) != 1L) {
    stop("fileName must be character of length 1")
  }
  if (!is.character(fileName)) {
    stop("fileName must be character")
  }
  
  
  # Get acqu info if not provided -------------------------------------------
  if (is.null(acquisitionInfo) | !inherits(acquisitionInfo, "list")) {
    acquisitionInfo <- readBrukerAcqus(dataDirectory)
  }
  
  # Associate ids and spot-location in acqu ---------------------------------
  
  if (is.null(anyMissing) | !inherits(anyMissing, "list")) {
    
    spots <- unlist(lapply(acquisitionInfo, function(x) x$spot))
    anyMissing <- findMissingSampleMapIds(spots = spots, 
                                          sampleMap = sampleMap,
                                          ignoreMissing = TRUE)
  }
  
  acquisitionInfo <- split(acquisitionInfo, anyMissing$matching) 
  
  files <- lapply(acquisitionInfo, function(x){
    lapply(x, function(y) y$file)
  })
  
  brukerToMzml_popup()
  
  forProcessing <- proteoWizConvert(msconvertPath = "",
                                    samplePathList = files,
                                    convertWhere = tempMZDir)
  popup3()
  
  idbac_create(fileName = fileName,
               filePath = filePath)
  
  idbacPool <- idbac_connect(fileName = fileName,
                             filePath = filePath)[[1]]
  
  db_from_mzml(mzFilePaths = forProcessing$mzFile,
               sampleIds = forProcessing$sampleID,
               idbacPool = idbacPool,
               acquisitionInfo = acquisitionInfo)
  pool::poolClose(idbacPool)
  
}