downfunc <- function() {

  devtools::install_github("chasemc/IDBacPackage",force=TRUE,quiet = F,quick=T)

}

#--------------------------------------------------

withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "message")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

#--------------------------------------------------

showModal(modalDialog(
  title = "IDBac Update",
  tags$li(paste0("Checking for Internet Connection: ")),
  tags$li(paste0("Installed Version: ")),
  tags$li(paste0("Latest Stable Release: ")),

  easyClose = FALSE, size="l",footer="",fade=FALSE
))

#--------------------------------------------------

runPing<- function(){

  internetPing <-  !suppressWarnings(system(paste("ping -n 1", "www.google.com")))

  if (internetPing == TRUE){


    internetPingResponse <- "Successful"




showModal(modalDialog(
  title = "IDBac Update",
  tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
  tags$li(paste0("Installed Version: ")),
  tags$li(paste0("Latest Stable Release: ")),

  easyClose = TRUE, size="l",footer="",fade=FALSE
))


Sys.sleep(.75)


#--------------------------------------------------


# Currently installed version
local_version <<- try(packageVersion("IDBac"))



showModal(modalDialog(
  title = "IDBac Update",
  tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
  tags$li(paste0("Installed Version: ", local_version)),
  tags$li(paste0("Latest Stable Release: ")),

  easyClose = TRUE, size="l",footer="",fade=FALSE
))


Sys.sleep(.75)

#--------------------------------------------------


showModal(modalDialog(
  title = "IDBac Update",
  tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
  tags$li(paste0("Installed Version: ", local_version)),
  tags$li(paste0("Latest Stable Release: ")),

  easyClose = TRUE, size="l",footer="",fade=FALSE
))


Sys.sleep(.75)

#--------------------------------------------------




# Latest GitHub Release
getLatestStableVersion <- function(){
  base_url <- "https://api.github.com/repos/chasemc/IDBacPackage/releases"
  response <- httr::GET(base_url)
  parsed_response <- httr::content(response, "parsed", encoding = "utf-8")
  parsed_response[[1]]$tag_name
}

latestStableVersion <<- try(getLatestStableVersion())


showModal(modalDialog(
  title = "IDBac Update",
  tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
  tags$li(paste0("Installed Version: ", local_version)),
  tags$li(paste0("Latest Stable Release: ", latestStableVersion)),

  easyClose = TRUE, size="l",footer="",fade=FALSE
))




if (class(latestStableVersion) == "try-error"){


  showModal(modalDialog(
    title = "IDBac Update",
    tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
    tags$li(paste0("Installed Version: ", local_version)),
    tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
    tags$li("Unable to connect to IDBac GitHub repository"),
    easyClose = TRUE, size="l",footer="",fade=FALSE
  ))




}else{


     if (local_version != latestStableVersion) {

      showModal(modalDialog(
        title = "IDBac Update",
        tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
        tags$li(paste0("Installed Version: ", local_version)),
        tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
        tags$li("Updating to latest version..."),
        pre(id = "console"),
        tags$li(""),
        easyClose = TRUE, size="l",footer="",fade=FALSE
      ))



         withCallingHandlers(
           downfunc(),
           message = function(m) {
             shinyjs::html("console", m$message, TRUE)
           }
         )


         devtools::install_github("chasemc/IDBacPackage",force=TRUE,quiet = T,quick=T)






      showModal(modalDialog(
          title = "IDBac Update",
          tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
          tags$li(paste0("Installed Version: ", local_version)),
          tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
          tags$li("Updating to latest version..."),
          tags$li(label="Finished Updating, Close Dialog"),
          easyClose = TRUE, size="l",footer="",fade=FALSE))


          # From RInno
          launch_browser <- function(appUrl) {
            message('Browser path: ', chrome)
            system(sprintf('"%s" --app=%s', chrome, appUrl))
          }
          shiny::runApp(app_path, launch.browser = launch_browser, port = 1984)


           session$onSessionEnded(function() {
             stopApp()
             q("no")
            })










    }else{

      showModal(modalDialog(
        title = "IDBac Update",
        tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
        tags$li(paste0("Installed Version: ", local_version)),
        tags$li(paste0("Latest Stable Release: ", latestStableVersion)),
        tags$li("Latest Version is Already Installed"),
        easyClose = TRUE, size="l",footer="",fade=FALSE
      ))

  }


}





}else{
# if internet ping is false:

  internetPingResponse <- "Unable to Connect"

  showModal(modalDialog(
    title = "IDBac Update",
    tags$li(paste0("Checking for Internet Connection: ", internetPingResponse)),
    tags$li(paste0("Installed Version: ")),
    tags$li(paste0("Latest Stable Release: ")),
    easyClose = FALSE, size="l",footer="",fade=FALSE
  ))


}


}





runPing()

