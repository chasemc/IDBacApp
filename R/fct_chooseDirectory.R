#' Choose Directory, choose.dir vs tcltk
#'
#' @param caption NA
#'
#' @return NA
#'
#'
choose_dir <- function(caption = "Select data directory") {
  if (getOS() == "osx") {
    toreturn <- tryCatch(
      system('osascript -l JavaScript -e \'a=Application.currentApplication();a.includeStandardAdditions=true;a.chooseFolder({withPrompt:"Please select a file to process:"}).toString()\'', intern = TRUE, ignore.stderr = T),
      error = function(x) paste("Error trying to select a directory with osascript"),
      finally = ""
    )
    if (length(toreturn) > 0) {
      print(toreturn)
      return(toreturn)
    } else {
      return(NA)
    }
  }
  if (exists("choose.dir", where = "package:utils", mode = "function")) {
    toreturn <- utils::choose.dir(caption = caption)
  } else {
    toreturn <- tcltk::tk_choose.dir(caption = caption)
  }
  print(toreturn)
  return(toreturn)
}
