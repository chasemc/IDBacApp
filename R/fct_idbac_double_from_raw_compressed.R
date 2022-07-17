
#' Convert raw compresed data from sqlite into R vector
#'
#' @param x raw compressed 
#'
#' @return dataframe
#' 
#' @importFrom fst decompress_fst

double_from_raw_compressed <- function(x){

deserial(rawToChar(decompress_fst(x)))

}
                             
                             
                             
                             
                             