context("test-findmissingsamplemapids")




a <- IDBacApp::map384Well()

rn <- c("A",
        "B",
        "C",
        "D",
        "E",
        "F",
        "G",
        "H",
        "I",
        "J",
        "K",
        "L",
        "M",
        "N",
        "O",
        "P")


cn <- c(
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "10",
  "11",
  "12",
  "13",
  "14",
  "15",
  "16",
  "17",
  "18",
  "19",
  "20",
  "21",
  "22",
  "23",
  "24"
)



colnames(a) <- cn
rownames(a) <- rn
b <- as.data.frame(a)
b[] <- NA

b["A","1"] <- "hello"
b["D","7"] <- "hello2"




test_that("findMissingSampleMapIds() works", {
  
  expect_identical(findMissingSampleMapIds(spots = c("D7"), 
                                           sampleMap = b),
                   character(0)
  )
  expect_identical(findMissingSampleMapIds(spots = c("D10"), 
                                           sampleMap = b),
                   "D10"
  )
  expect_identical(findMissingSampleMapIds(spots = c("D10", "D11"), 
                                           sampleMap = b),
                   c("D10", "D11")
  )
  
  expect_identical(findMissingSampleMapIds(spots = c("D7", "A1"), 
                                           sampleMap = b),
                   character(0)
  )
  

})






a <- list()
a$rt <- IDBacApp::nulledMap384Well()

b <- IDBacApp::sampleMapViewer(b)


 IDBacApp::sampleMapSaver(sampleData = b,
                         sampleMapReactive = a)


