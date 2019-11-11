options(idbac_testing = TRUE)
idbacHome <- findIdbacHome()



ex_url <- "https://github.com/chasemc/IDBacApp/releases/download/0.0.15.2/test.sqlite"

download.file(url = ex_url,
              destfile = file.path(idbacHome, 
                                   "test.sqlite"),
              mode = "wb")




app <- shinytest::ShinyDriver$new(system.file(file.path("app"), package = "IDBacApp"))

app$setWindowSize(width, height)




app$getTitle() == "IDBac"

app$setInputs(action = "click")



