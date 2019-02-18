#From: https://github.com/sgibb/bootstrap

## Copyright 2013 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## It is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## See <http://www.gnu.org/licenses/>

#' bootstrap
#'
#' This function provides bootstrapping for hierarchical clustering
#' (\code{\link{hclust}} objects).
#'
#' @param x \code{matrix}, rows: individuals, columns: observations
#' @param fun function which creates the individual hclust object
#' @param n \code{integer}, number of bootstrap replicates
#' @param mc.cores \code{integer}, number of processes to run in parallel
#'
#' @seealso \code{\link{dist}}, \code{link{hclust}}
#'
#' @return \code{numeric} vector with frequencies of each node
#'
#' @references
#' Felsenstein, Joseph.
#' \emph{Confidence limits on phylogenies: an approach using the bootstrap.}
#' Evolution (1985): 783-791.
#'
#' Efron, Bradley, Elizabeth Halloran, and Susan Holmes.
#' \emph{Bootstrap confidence levels for phylogenetic trees.}
#' Proceedings of the National Academy of Sciences 93.23 (1996): 13429-13429.
#'
#' @examples
#'
#' ## hclust example
#' createHclustObject <- function(x)hclust(dist(x), "ave")
#'
#' ## bootstrap
#' b <- bootstrap(USArrests, fun=createHclustObject, n=100L)
#'
#' ## plot
#' hc <- createHclustObject(USArrests)
#' plot(hc)
#'
#' ## draw bootstrap values to corresponding node
#' bootlabels.hclust(hc, b, col="blue")
#'
#' @rdname bootstrap
#' @importFrom parallel mclapply
#' @export
bootstrap <- function(x, fun, n=1000L) {
  fun <- match.fun(fun)
  
  origin <- .clust(x, fun=fun)
  
  v <- lapply(seq_len(n), function(y, origin, size, fun, nco) {
    current <- .clust(.resample(x, size=size), fun=fun)
    return(.calculateMatches(origin, current, nco))
  }, origin=origin, size=ncol(x), fun=fun, nco=ncol(origin))
  
  return(colSums(do.call(rbind, v))/n)
}





## based on pvclust:::hc2split (pvclust 1.2-2) by
## Ryota Suzuki <suzuki@ef-prime.com>
as.binary.matrix.hclust <- function(x) {
  nr <- as.integer(nrow(x$merge))
  
  m <- matrix(0L, nrow=nr, ncol=nr+1L)
  
  for (i in seq_len(nr)) {
    left <- x$merge[i, 1L]
    
    if (left < 0L) {
      ## negative values correspond to observations
      m[i, -left] <- 1L
    } else {
      ## positive values correspond to childcluster
      m[i, ] <- m[left, ]
    }
    
    right <- x$merge[i, 2L]
    
    if (right < 0L) {
      ## negative values correspond to observations
      m[i, -right] <- 1L
    } else {
      ## positive values correspond to childcluster
      m[i, ] <- m[i,] | m[right, ]
    }
  }
  
  return(m)
}

## based on pvclust:::hc2axes (pvclust 1.2-2) by
## Ryota Suzuki <suzuki@ef-prime.com>
.text.coord.hclust <- function(x) {
  nr <- as.integer(nrow(x$merge))
  
  p <- matrix(c(rep(0L, nr), x$height), nrow=nr, ncol=2, byrow=FALSE,
              dimnames=list(c(), c("x", "y")))
  o <- order(x$order)
  tmp <- double(2)
  
  for (i in seq_len(nr)) {
    left <- x$merge[i, 1L]
    
    if (left < 0L) {
      ## negative values correspond to observations
      tmp[1L] <- o[-left]
    } else {
      ## positive values correspond to childcluster
      tmp[1L] <- p[left, 1L]
    }
    
    right <- x$merge[i, 2L]
    
    if (right < 0L) {
      ## negative values correspond to observations
      tmp[2L] <- o[-right]
    } else {
      ## positive values correspond to childcluster
      tmp[2L] <- p[right, 1L]
    }
    
    p[i, 1L] <- mean(tmp)
  }
  
  return(p)
}

#' Print bootstrap values.
#'
#' This function prints bootstrap values to the corresponding node.
#'
#' @param x \code{hclust} object
#' @param bootstrapValues \code{numeric}, bootstrap values
#' @param horiz print values for a horizontal tree?
#'
#' @seealso \code{\link{bootstrap}}
#' @rdname bootlabels
#' @export
bootlabels.hclust <- function(x, bootstrapValues, horiz=FALSE, ...) {
  p <- .text.coord.hclust(x)
  if (horiz) {
    p[, c(2,1)] <- p
  }
  
  labs <- sprintf("%.2f", bootstrapValues)
  text(p, labels=labs, ...)
  invisible(NULL)
}

.clust <- function(x, fun) {
  hc <- fun(x)
  return(as.binary.matrix.hclust(hc))
}

.calculateMatches <- function(origin, current, nc=ncol(origin)) {
  ## both 1
  one <- tcrossprod(origin, current)
  ## both 0
  zero <- tcrossprod(1-origin, 1-current)
  
  ## calc matches
  return(rowSums(one + zero == nc))
}

.resample <- function(x, size=ncol(x)) {
  sel <- sample.int(ncol(x), size=size, replace=TRUE)
  return(x[, sel])
}

#' Simple Bootstrapping for Hierarchical Clustering
#'
#' This package provides bootstrapping for hierarchical clustering.
#'
#' \tabular{ll}{
#' Package: \tab bootstrap \cr
#' Version: \tab 0.1\cr
#' Date: \tab 2013-06-27\cr
#' License: \tab GPL (>= 3)\cr
#' URL: \tab http://www.github.com/sgibb/bootstrap/ \cr
#' }
#'
#' @docType package
#' @name bootstrap-package
#' @author Sebastian Gibb <\email{mail@@sebastiangibb.de}>
#' @references \url{http://www.github.com/sgibb/bootstrap/}
#' @keywords package
#' @rdname bootstrap-package
#'
NULL
