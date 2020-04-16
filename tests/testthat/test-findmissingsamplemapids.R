# context("test-findmissingsamplemapids")
# 
# 
# 
# 
# a <- IDBacApp:::map384Well()
# 
# b <- as.data.frame(a)
# 
# 
# 
# 
# test_that("findMissingSampleMapIds() works", {
#   
#   expect_identical(findMissingSampleMapIds(spots = c("D7"), 
#                                            sampleMap = b,
#                                            ignoreMissing = F)$missing,
#                    character(0)
#   )
# 
#   
#   expect_identical(names(findMissingSampleMapIds(spots = c("D7"), 
#                                                  sampleMap = b,
#                                                  ignoreMissing = F)$matching),
#                    "D7"
#   )
#   
#   
#   
#   expect_identical(findMissingSampleMapIds(spots = c("D10"), 
#                                            sampleMap = b,
#                                            ignoreMissing = F)$missing,
#                    "D10"
#   )
#   expect_identical(findMissingSampleMapIds(spots = c("D10", "D11"), 
#                                            sampleMap = b,
#                                            ignoreMissing = F)$missing,
#                    c("D10", "D11")
#   )
#   
#   expect_identical(findMissingSampleMapIds(spots = c("D7", "A1"), 
#                                            sampleMap = b,
#                                            ignoreMissing = F)$missing,
#                    character(0)
#   )
#   
# 
# })
# 
# 
# 
