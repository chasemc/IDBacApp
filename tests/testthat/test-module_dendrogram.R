# context("test-module_dendrogram")
# set.seed(42)
# 
# 
# 
# # colordendLabelsUI -------------------------------------------------------
# 
# a <- colordendLabelsUI("hi")
# test_that("colordendLabelsUI", {
#   expect_identical(class(a), "shiny.tag")
#   expect_identical(a$name, "div")
#   expect_identical(a$attribs$id, "hi-absPanelDendLabels")
#   expect_identical(a$attribs$class, "shiny-html-output")
#   expect_known_hash(a, "e8d66dac3e")
#   
# })
# 
# 
# # addDotsActionUI ---------------------------------------------------------
# 
# a <- addDotsActionUI("hi")
# test_that("addDotsActionUI", {
#   expect_identical(class(a),c("shiny.tag.list", "list"))
#   expect_known_hash(a, "d659acae6c")
# })
# 
# 
# # plotHier ----------------------------------------------------------------
# 
# a <- plotHier("hi")
# test_that("plotHier", {
#   expect_identical(class(a),c("shiny.tag.list", "list"))
#   expect_known_hash(a, "bffad66f2f")
# })
# 
# 
# # downloadHier ------------------------------------------------------------
# 
# a <- downloadHier("hi")
# test_that("downloadHier", {
#   expect_identical(class(a),"shiny.tag")
#   expect_known_hash(a, "a181c75d70")
# })
# 
# # downloadSvg -------------------------------------------------------------
# 
# a <- downloadSvg("hi")
# test_that("downloadSvg", {
#   expect_identical(class(a[[1]]),"shiny.tag")
#   expect_identical(class(a[[2]]),"shiny.tag")
#   expect_identical(class(a[[2]]),"shiny.tag")
#   expect_known_hash(a, "addf5fc4a8")
# })
# 
# 
# 
# 
# 
# 
# 
# # modDendLines_WellPanel --------------------------------------------------
# 
# a <- modDendLines_WellPanel(function(x)x)
# test_that("modDendLines_WellPanel", {
#   expect_identical(class(a),c("shiny.tag.list", "list" ))
#   expect_known_hash(a, "26f4ebdf4a")
# })
# 
# 
# # modDendDotsMod_WellPanel ------------------------------------------------
# 
# a <- modDendDotsMod_WellPanel(function(x)x)
# test_that("modDendDotsMod_WellPanel", {
#   expect_identical(class(a),c("shiny.tag.list", "list" ))
#   expect_known_hash(a, "da265f2a5d")
# })
# 
# 
# # colorPicker -------------------------------------------------------------
# 
# 
# a <- colorPicker(function() 1:3,
#                            function(x)x)
# 
# test_that("colorPicker", {
#   expect_known_hash(a, "5928c7abe0")
# })
# 
# 
# 
# 
# 
