

#' readOnly
#'
#' @param input NA
#' @param spots NA
#'
#' @return NA
#' @export
#'

readOnly <- function(input, 
                     spots){
  
  
  lets <- LETTERS[1:16]
  nums <- 1:24
  aa <- sapply(nums, function(x) paste0(lets, x))
  aa <- matrix(aa, nrow = 16, ncol = 24)
  
  w <- match(spots, aa)
  
  aa[w] <-TRUE
  w <- which(aa == "TRUE", arr.ind = T)
  # 
  # for(i in 1:nrow(w)){
  #   
  #   input <- hot_cell(input, w[i, 1], w[i, 2], readOnly = TRUE)
  # }
  # 
  return(w)
  
  
}
