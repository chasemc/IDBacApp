
#' peakBinner
#'
#' @param peakList should be MALDIquant 
#' @param ppm ppm
#' @param massStart massStart 
#' @param massEnd massEnd 
#'
#' @return list of bin vectors
#' @export
#'
peakBinner <- function(peakList,
                       ppm = 300,
                       massStart = 3000,
                       massEnd = 15000){

  peakList <- IDBacApp::mQuantToMassVec(peakList)
    scalePpm <- function(mass,
                         ppm = ppm
    ){
      1 / ((ppm / 10e5) * mass)
    }
    
    scaler <- scalePpm(ppm = ppm, 
                       mass = massStart)
    
    toSub <- round(massStart * scaler)
    
    
    mm <- lapply(peakList, function(x){
      # scaling allows representing decimal places as integer
      x <- round(x * scaler) - toSub
      # get scaled and integerized ppm 
      x2 <- round((x * ppm) / 10e5)
      # create IRange.adjust scale so starts at 1L
      IRanges::IRanges(start = x - x2, end = x + x2)
      
    })
    
    
    poiz <- lapply(mm,
                   function(x){
                     x <- x@width
                     yy <- lapply(x, function(x) 1:x)
                     x <- unlist(mapply(dpois,yy, x/2, SIMPLIFY =  ))
                     x <- x * 1000
                     as.integer(x)
                   })
    
    
    #want subject hits
    
    vecLength <- (massEnd * scaler) - toSub
    zz <- IRanges::IRanges(start = 1:vecLength, width = rep(1, vecLength))
    z <- lapply(mm, function(x) IRanges::findOverlaps(zz, x)@from)
    
    
    zz <- sort(unique(unlist(z)))
    
    
    zx <- lapply(z, function(x) as.integer(zz %in% x))
    
    
    w <- mapply(function(x,y){
      x[x == 1] <- y
      x  } , zx, poiz)
    
    #Matrix::Matrix(w)
  w
  
  
  
}




#' MALDIquant to Mass Vectors
#'
#' @param peakList MALDIquant peak list
#'
#' @return list of mass vectors
#' @export
#'
mQuantToMassVec <- function(peakList){
  
  if (MALDIquant::isMassPeaksList(peakList)) {
    
    return(lapply(peakList, function(x) x@mass))
    
  } else {
    
    peakList <- list(unlist(peakList))
    if (MALDIquant::isMassPeaksList(peakList)) {
      
      return(lapply(peakList, function(x) x@mass))
      
    } else {
      warning("mQuantToMassVec: not a MALDIquant mass peaks list, unable to convert to MALDIquant mass peaks list")
    return(NULL)
      }
    
  }
}






