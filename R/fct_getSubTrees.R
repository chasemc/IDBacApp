#' Recursively look for a tree within a tree
#'
#' @param subdend dendrogram
#' @param clusters leaf indices of cluster
#'
#' @return dendrogram
.get_one_subtree <- function(subdend,
                             clusters) {
  # Iterate over bifurcation until only desired samples are left
  # If only cluster samples are left, quit
  if (all(unlist(subdend) %in% clusters)) {
    return(subdend)
  }
  # Are samples in first split or second split?
  # recurse until only cluster samples are left
  if (any(unlist(subdend[[1]]) %in% clusters)) {
    return(.get_one_subtree(
      subdend[[1]],
      clusters
    ))
  } else {
    return(.get_one_subtree(
      subdend[[2]],
      clusters
    ))
  }
}


#' Get subtree
#'
#' @param dend dendrogram
#'
#' @inheritParams dendextend::cutree
#'
#' @return dendrogram
#'
get_subtrees <- function(dend,
                         k = NULL,
                         h = NULL,
                         use_labels_not_values = FALSE,
                         order_clusters_as_data = FALSE) {
  clusters <- dendextend::cutree(
    tree = dend,
    k = k,
    h = h,
    order_clusters_as_data,
    use_labels_not_values
  )
  lapply(
    unique(clusters),
    function(x) {
      .get_one_subtree(
        subdend = dend,
        clusters = which(clusters == x)
      )
    }
  )
}
