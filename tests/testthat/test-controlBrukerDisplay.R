
session<-NULL

ns_prefix <- ""

session$ns <- function(id) 
{
  id
}


a <- IDBacApp::controlBrukerDisplay(session, "error")
b <- IDBacApp::controlBrukerDisplay(session, "literallyAnythingButerror")
d <- IDBacApp::controlBrukerDisplay(session, "error", ostest = "sjd")


test_that("controlBrukerDisplay works", {
  expect_known_hash(a, "fc64e7ede3")
  expect_known_hash(b, "5a13d034da")
  expect_known_hash(d, "6f0929edb5")
})
