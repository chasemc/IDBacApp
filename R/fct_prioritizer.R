#' Prioritize Samples
#'
#' @param pool pool connection to IDBac database
#' @param percent_metabolite_peaks  percent of peaks that must be present in sample replicates (0 to 1)
#' @param small_mol_lowerMassCutoff lower mass cutoff for small molecule spectra
#' @param small_mol_upperMassCutoff upper mass cutoff for small molecule spectra
#' @param small_mol_SNR minimum SNR for small molecule spectra
#'
#' @inheritParams MALDIquant::binPeaks
#' @inheritParams dendextend::cutree
#'
#' @return prioritization list
#' @export
#'
prioritizer <- function(pool,
                        dendrogram,
                        h = NULL,
                        k = NULL,
                        percent_metabolite_peaks,
                        small_mol_peakPercentPresence = 0.6,
                        small_mol_lowerMassCutoff = 200,
                        small_mol_upperMassCutoff = 2000,
                        small_mol_SNR = 10,
                        tolerance = .002){
  
  
  
  if (!inherits(dendrogram, "dendrogram")) {
    stop("prioritizer(dendrogram =) requires a dendrogram object")
  }
  if (!is.null(h)) {
    if(!is.numeric(h)) {
      stop("prioritizer(h =) must be numeric")
    }
  }
  if (!is.null(k)) {
    if(!is.numeric(k)) {
      stop("prioritizer(k =) must be numeric")
    }
  }
  if (is.null(h) + is.null(k) > 1L) {
    stop("One of prioritizer(k =, k =) must be numeric")
    
  }
  
  small_peaks <- IDBacApp::collapseReplicates(pool = pool,
                                              #   sampleIDs = unlist(for_small_prioritization),
                                              sampleIDs = labels(dendrogram),
                                              peakPercentPresence = small_mol_peakPercentPresence,
                                              lowerMassCutoff = small_mol_lowerMassCutoff,
                                              upperMassCutoff = small_mol_upperMassCutoff,
                                              minSNR = small_mol_SNR,
                                              tolerance = small_mol_tolerance,
                                              protein = FALSE)
  
  # Split/cut dendrogram
  dend_split <- get_subtrees(dend = dendrogram,
                             h = h,
                             k = k)
  
  dend_split_labels <- sapply(dend_split, function(x) labels(x), simplify = FALSE)
  
  # Get the number of leaves in each subtree
  dend_split_lengths <- lengths(dend_split_labels)
  
  only_protein <- which(dend_split_lengths < 2)
  # Get IDs of samples in groups containing more than 1 sample
  for_small_prioritization <- unlist(lapply(dend_split[which(dend_split_lengths > 1)], labels))
  
  
  prioritized <- lapply(dend_split_labels, 
                        function(x){
                          if (length(x) > 1) {
                            a <- MALDIquant::intensityMatrix(MALDIquant::binPeaks(small_peaks[x], 
                                                                                  method = "relaxed",
                                                                                  tolerance = small_mol_tolerance))
                            a[is.na(a)] <- 0L
                            a[a > 0] <- 1L
                            a <- t(a)
                            
                            perc <- 0
                            original_total_peaks <- nrow(a)
                            index <- c()
                            
                            while (perc < percent_metabolite_peaks) {
                              samp_peaks <- colSums(a)
                              chosen <- order(samp_peaks, decreasing = TRUE)[[1]]
                              perc <- perc + (samp_peaks[chosen] / original_total_peaks * 100)
                              index <- c(index, chosen)
                              a <- a[-which(a[,chosen] == 1L), ]
                              
                            }
                            
                            return(labels(small_peaks[x])[index])
                          } else {
                            x
                          }
                        })
  
  list(dend_split = dend_split,
       protein_singletons = unlist(dend_split_labels[only_protein]),
       prioritized = prioritized,
       small_peaks = lengths(small_peaks))
  
}
