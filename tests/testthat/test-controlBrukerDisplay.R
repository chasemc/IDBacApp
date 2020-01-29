
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
  skip_on_os(c("mac", "linux", "solaris"))
  expect_known_hash(d, "4715410005")
  expect_known_hash(a, "a877f31186")
  expect_known_hash(b, "f74c411de8")
})
