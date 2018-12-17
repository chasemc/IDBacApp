choose_dir <- function(caption = 'Select data directory') {
  if (exists("choose.dir", where="package:utils", mode='function')) {
   a <- choose.dir(caption = caption) 
  }else {
  a <-  tcltk::tk_choose.dir(caption = caption)
  }
  return(a)
}