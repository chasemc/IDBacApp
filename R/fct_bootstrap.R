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


#' Bootstrap
#'
#' @param x see: https://github.com/sgibb/bootstrap
#' @param fun see: https://github.com/sgibb/bootstrap
#' @param n see: https://github.com/sgibb/bootstrap
#'
#' @return see: https://github.com/sgibb/bootstrap
#' 
#'
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
#' as.binary.matrix.hclust 
#'
#' @param x see: https://github.com/sgibb/bootstrap
#'
#' @return see: https://github.com/sgibb/bootstrap
#' 
#'
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
#' .text.coord.hclust
#'
#' @param x  see: https://github.com/sgibb/bootstrap
#'
#' @return see: https://github.com/sgibb/bootstrap
#'  
#'
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
#' @param ...   see: https://github.com/sgibb/bootstrap
#'
#' @seealso \code{\link{bootstrap}}
#' @rdname bootlabels
#' 
bootlabels.hclust <- function(x, bootstrapValues, horiz=FALSE, ...) {
  p <- .text.coord.hclust(x)
  if (horiz) {
    p[, c(2,1)] <- p
  }
  
  labs <- sprintf("%.2f", bootstrapValues)
  text(p, labels=labs, ...)
  invisible(NULL)
}

#' .clust
#'
#' @param x   see: https://github.com/sgibb/bootstrap
#' @param fun   see: https://github.com/sgibb/bootstrap
#'
#' @return  see: https://github.com/sgibb/bootstrap
#' 
#'
.clust <- function(x, fun) {
  hc <- fun(x)
  return(as.binary.matrix.hclust(hc))
}

#' .calculateMatches
#'
#' @param origin   see: https://github.com/sgibb/bootstrap
#' @param current   see: https://github.com/sgibb/bootstrap
#' @param nc   see: https://github.com/sgibb/bootstrap
#'
#' @return  see: https://github.com/sgibb/bootstrap
#' 
#'
.calculateMatches <- function(origin, current, nc=ncol(origin)) {
  ## both 1
  one <- tcrossprod(origin, current)
  ## both 0
  zero <- tcrossprod(1-origin, 1-current)
  
  ## calc matches
  return(rowSums(one + zero == nc))
}

#' resample
#'
#' @param x  see: https://github.com/sgibb/bootstrap
#' @param size  see: https://github.com/sgibb/bootstrap
#'
#' @return see: https://github.com/sgibb/bootstrap
#' 
#'
.resample <- function(x, size=ncol(x)) {
  sel <- sample.int(ncol(x), size=size, replace=TRUE)
  return(x[, sel])
}



