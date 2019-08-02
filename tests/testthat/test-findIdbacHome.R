zz <- as.list(Sys.getenv())$HOME
zz2 <- as.list(Sys.getenv())$LOCALAPPDATA


res <- IDBacApp::findIdbacHome()

exp <- file.path(as.list(Sys.getenv())$HOME, "IDBac_experiments", fsep = "/")

Sys.setenv(HOME = NA)

res2 <- IDBacApp::findIdbacHome()
exp2 <- file.path(as.list(Sys.getenv())$LOCALAPPDATA, "IDBac_experiments", fsep = "/")


Sys.setenv(LOCALAPPDATA = "as")


res3 <- IDBacApp::findIdbacHome()


test_that("findIdbacHome works", {
  expect_equal(res, normalizePath(exp, winslash = "/"))
  expect_equal(res2, normalizePath(exp2, winslash = "/"))
  expect_equal(res3, normalizePath(file.path(getwd(),"IDBac_experiments"), winslash = "/"))
})


Sys.setenv(HOME = zz)
Sys.setenv(LOCALAPPDATA = zz2)