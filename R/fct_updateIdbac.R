#' GET api.github.com page for latest release
#'
#' @param base_url url to GET
#'
#' @return httr content
.getLatestStableVersionPage <- function(base_url = "https://api.github.com/repos/chasemc/IDBacApp/releases/latest") {
  version <- tryCatch(httr::GET(base_url),
    error = function(x) paste("Error connecting with GitHub")
  )
  shiny::validate(shiny::need(version$status_code == 200, "Error getting info from GitHub"))
  parsed_response <- httr::content(version,
    "parsed",
    encoding = "utf-8"
  )
  parsed_response
}

#' Download IDBac exe
#'
#' @param latest_version_httr httr content
#'
#' @return none
.download_idbac_exe <- function(latest_version_httr) {
  # Not currently used b/c haven't test if
  # works properly while electron is open
  # to use:
  # exe_path <- .download_idbac_exe(latest_version_httr = latest_version_httr)
  # and then system2() it
  latest_version_httr <- latest_version_httr$assets[[1]]$browser_download_url
  temp_file_path <- tempfile()
  utils::download.file(
    url = latest_version_httr,
    destfile = temp_file_path,
    mode = "wb"
  )
  return(temp_file_path)
}

#' Check if there's a new stable version of IDBac
#'
#' @return character string to print to user
#' @export
#'
new_version_check <- function() {
  # Internet? ---------------------------------------------------------------
  if (curl::has_internet()) {
    # Get the latest version url ----------------------------------------------
    latest_version_httr <- .getLatestStableVersionPage()
    # Compare versions --------------------------------------------------------
    local_version <- utils::packageVersion("IDBacApp")
    # comp_ver will equal -1 if the latest release version # is higher
    comp_ver <- utils::compareVersion(
      as.character(local_version),
      as.character(latest_version_httr$tag_name)
    )
    if (comp_ver == -1) {
      to_return <- latest_version_httr$html_url
    } else {
      to_return <- shiny::tagList("Installed version is latest version")
    }
  } else {
    to_return <- shiny::tagList(
      h3("New version Available!"),
      p('Install latest "exe" from: ')
    )
  }
  return(to_return)
}

#' Display update info
#'
#' @return html modal
#' @importFrom shiny showModal modalDialog modalButton
update_idbac_modal <- function() {
  # TODO: Make this look better
  showModal(
    modalDialog(
      title = "Checking for updates",
      new_version_check(),
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close"),
      fade = FALSE
    )
  )
}
