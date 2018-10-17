
proteinDistanceMatrix <- function(peakList, method){
  if(method == "cosineD"){

    peakList %>%
      MALDIquant::binPeaks(., method = "strict", tolerance = 2) %>%
      MALDIquant::intensityMatrix() %>%
      replace(., is.na(.), 0) %>%
      coop::tcosine(.) -> p



      rownames(p) <- labels(peakList)

      1- as.dist(p)


  }else{

    peakList %>%
      MALDIquant::binPeaks(., method = "relaxed", tolerance = .02) %>%
      MALDIquant::intensityMatrix() %>%
      replace(., is.na(.), 0) %>%
     dist(., method = method) %>%
      as.matrix -> p


    rownames(p) <- labels(peakList)

    as.dist(p)



  }
  }





  proteinDistanceMatrix2 <- function(peakList, method){

  if(method == "cosineD"){


awq2<<-peakList

for(i in 1:length(peakList)){

  if(length(peakList[[i]]@intensity) < 100){}else{
  snr1 <-  order(peakList[[i]]@intensity, decreasing = TRUE)[1:100]
  peakList[[i]]@mass <- peakList[[i]]@mass[snr1]
  peakList[[i]]@snr <- peakList[[i]]@snr[snr1]
  peakList[[i]]@intensity <- peakList[[i]]@intensity[snr1]
  }
}

  # Takes as input a flat-list of MALDIquant S4 MassPeaks objects



tc <- rep(NA, choose(length(peakList),2))
for(i in seq_along(tc)){

  ind <-     combinadic(length(peakList), # number samples
                                  2, # pairwise
                                  i) # index

  zz <- MALDIquant::binPeaks(peakList[ind], method = "relaxed", tolerance = .02)
  mass <- unlist(lapply(zz, function(x) x@mass))
  uniqueMass <- sort.int(unique(mass))

  n <- lengths(zz)
  r <- rep.int(seq_along(zz), n)
  i2 <- findInterval(mass, uniqueMass)
  m <- matrix(0, nrow = length(zz), ncol = length(uniqueMass),
              dimnames = list(NULL, uniqueMass))
  m[cbind(r, i2)] <- 1
  tc[i] <- coop::tcosine(m)[[2]]


}







    # Create native R distance matrix from cosine scores
    mat <- matrix(nrow = length(peakList), ncol = length(peakList))


mat[lower.tri(mat)] <- tc
diztance <- as.dist(mat)


  }else{
    # Takes as input a flat-list of MALDIquant S4 MassPeaks objects, named by sample

    # Create native R distance matrix from cosine scores
    mat <- matrix(nrow = length(peakList), ncol = length(peakList))


    # Create a pairwise combinations of samples (returns list of list, where each sublist is a pair combination)
    combn(x = peakList,
          m = 2,
          simplify = F) %>%
      lapply(., function(x){# Get cosine similarity across all pairs

        x %>%
          MALDIquant::binPeaks(., tolerance = .002) %>%
          MALDIquant::intensityMatrix() %>%
          replace(., is.na(.), 0) %>%
          stats::dist(., method = method)%>%
          .[[1]]

      }) %>%
      unlist -> asdf


    mat[lower.tri(mat)] <- asdf

   diztance <-  as.dist(mat)

      }



  labels(diztance) <- names(peakList)
  diztance

  }




  combinadic <- function(n, r, i) {

    # http://msdn.microsoft.com/en-us/library/aa289166(VS.71).aspx
    # http://en.wikipedia.org/wiki/Combinadic

    if(i < 1 | i > choose(n,r)) stop("'i' must be 0 < i <= n!/(n-r)!")

    largestV <- function(n, r, i) {
      #v <- n-1
      v <- n                                  # Adjusted for one-based indexing
      #while(choose(v,r) > i) v <- v-1
      while(choose(v,r) >= i) v <- v-1        # Adjusted for one-based indexing
      return(v)
    }

    res <- rep(NA,r)
    for(j in 1:r) {
      res[j] <- largestV(n,r,i)
      i <- i-choose(res[j],r)
      n <- res[j]
      r <- r-1
    }
    res <- res + 1
    return(res)
  }

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  proteinDistanceMatrix3 <- function(peakList, method){
    if(method == "cosineD"){
      
      
      
      
      
      z <- lapply(peakList, function(x){
        
        x@mass
        
        
      })
      
      
      fullz <- unique(unlist(z))
      
      
      zq <- lapply(z, function(x){
        temp <- Hmisc::find.matches(fullz, x, maxmatch=1, tol=10)$matches
        temp[temp > 0] <- 1
        temp
      })
      
      
      zqq <- do.call(rbind, zq)
      zz<-coop::tcosine(zqq)
      rownames(zz) <- c("11", "1", "7", "10")
      colnames(zz) <- c("11", "1", "7", "10")
      
    
      
      
      
      rownames(p) <- labels(peakList)
      
      1- as.dist(p)
      
      
    }else{
      
      peakList %>%
        MALDIquant::binPeaks(., method = "relaxed", tolerance = .02) %>%
        MALDIquant::intensityMatrix() %>%
        replace(., is.na(.), 0) %>%
        dist(., method = method) %>%
        as.matrix -> p
      
      
      rownames(p) <- labels(peakList)
      
      as.dist(p)
      
      
      
    }
  }
  