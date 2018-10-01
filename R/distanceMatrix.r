
proteinDistanceMatrix <- function(peakList, method){
  aws2<<-peakList
  if(method == "cosineD"){

    peakList %>%
      MALDIquant::binPeaks(., method = "strict", tolerance = 2) %>%
      MALDIquant::intensityMatrix() %>%
      replace(., is.na(.), 0) %>%
      coop::tcosine(.) -> p



      rownames(p) <- labels(peakList)

      as.dist(p)


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





  proteinDistanceMatrix2 <- function(){

  if(method == "cosineD"){


aws2<<-peakList
  # Takes as input a flat-list of MALDIquant S4 MassPeaks objects

# Create native R distance matrix from cosine scores
mat <- matrix(nrow = length(peakList), ncol = length(peakList))


# Create a pairwise combinations of samples (returns list of list, where each sublist is a pair combination)
z <- combn(x = peakList,
            m = 2,
            simplify = F)


z <- lapply(z, function(x){ binPeaks(x, method = "relaxed", tolerance = .02)})


z <-lapply(z, function(zz){

  mass <- unlist(lapply(zz, function(x) x@mass))
  uniqueMass <- sort.int(unique(mass))

  n <- lengths(zz)
  r <- rep.int(seq_along(zz), n)
  i <- findInterval(mass, uniqueMass)
  m <- matrix(0, nrow = length(zz), ncol = length(uniqueMass),
              dimnames = list(NULL, uniqueMass))
  m[cbind(r, i)] <- 1
  m
})


z <- unlist(lapply(qq, function(x) coop::tcosine(x)[[2]]))




mat[lower.tri(mat)] <- z
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
