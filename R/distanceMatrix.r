
proteinDistanceMatrix <- function(binnedData, method){
  if(method == "cosineD"){

    nam <- names(binnedData)
    binnedData <- do.call(rbind,binnedData)
    
    binnedData %>%
      coop::tcosine(.) -> p

      rownames(p) <- nam
weer<<-p
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

