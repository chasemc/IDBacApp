
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

  if(length(peakList[[i]]@intensity) < 10){}else{
  snr1 <-  order(peakList[[i]]@intensity, decreasing = TRUE)[1:100]
  peakList[[i]]@mass <- peakList[[i]]@mass[snr1]
  peakList[[i]]@snr <- peakList[[i]]@snr[snr1]
  peakList[[i]]@intensity <- peakList[[i]]@intensity[snr1]
  }
}

  # Takes as input a flat-list of MALDIquant S4 MassPeaks objects

# Create native R distance matrix from cosine scores
mat <- matrix(nrow = length(peakList), ncol = length(peakList))


# Create a pairwise combinations of samples (returns list of list, where each sublist is a pair combination)
z <- combn(x = peakList,
            m = 2,
            simplify = F)



binMat <- function(x){

  zz <- MALDIquant::binPeaks(x, method = "relaxed", tolerance = .02)
  mass <- unlist(lapply(zz, function(x) x@mass))
  uniqueMass <- sort.int(unique(mass))

  n <- lengths(zz)
  r <- rep.int(seq_along(zz), n)
  i <- findInterval(mass, uniqueMass)
  m <- matrix(0, nrow = length(zz), ncol = length(uniqueMass),
              dimnames = list(NULL, uniqueMass))
  m[cbind(r, i)] <- 1

  coop::tcosine(m)[[2]]
}

numCores <- parallel::detectCores()
cl <- parallel::makeCluster(numCores-1)
 z <- parallel::parLapply(cl,
                    z,
                    binMat)
parallel::stopCluster(cl)




mat[lower.tri(mat)] <- unlist(z)
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
