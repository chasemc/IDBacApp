
proteinDistanceMatrix <- function(binnedData, method){
  if(method == "cosineD"){

    nam <- names(binnedData)
    zz1 <- do.call(rbind, binnedData)
    
    zz1[!is.na(zz1)] <- 1
    zz1[is.na(zz1)] <- 0
    as.dist(1-coop::tcosine(zz1))
    



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

