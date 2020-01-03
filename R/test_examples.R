# a <- list.files("C:/Users/CMC/Documents/binnr/inst/extdata/New folder", full.names = T)
# 
# 
# a <- lapply(a, MALDIquantForeign::importMzXml)
# 
# 
# 
# 
# b <- lapply(a, 
#             function(x){
#               lapply(x, 
#                      function(y){
#                        
#                        if(max(y@mass) > 5000){
#                          MALDIquant::trim(y, c(5450,6500))
#                        }else{
#                          MALDIquant::trim(y, c(540,600))
#                        }
#                          }
#                      )
#               }
#             )
#   
# d <- lapply(b, function(x) x[1:2])
# d[[4]] <- d[[4]][[1]]
# 
# 
# 
# folder <- "C:/Users/CMC/Documents/binnr/inst/extdata/New folder/a"
# 
# sapply(d, function(x) x[[1]]@metadata$file)
# 
# 
# 
# lapply(1:4, function(x) MALDIquantForeign::exportMzMl(d[[x]], paste0(folder,"/",c("1","ten","11","matrix")[[x]],".mzML")))
