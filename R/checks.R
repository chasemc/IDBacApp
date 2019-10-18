#' Check for pool object
#'
#' @param pool variable to check
#'
#' @export
#'

checkSinglePool <- function(pool){
  
  shiny::validate({
    need(class(pool)[[1]] == "Pool", 
         glue::glue("Expected a pool object, received: {class(pool)}"))
  })
  
  
}
