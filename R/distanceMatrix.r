
proteinDistanceMatrix <- function(peakList, method){


  if(method == "cosineD"){



  # Takes as input a flat-list of MALDIquant S4 MassPeaks objects

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
            coop::tcosine() %>%
            .[[2]]

  }) %>%
  unlist -> asdf


mat[lower.tri(mat)] <- asdf
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
