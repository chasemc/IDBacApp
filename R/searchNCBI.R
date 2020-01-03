#' searchNCBI
#'
#' @return NA
#' @export
#'

# searchNCBI <- function(){
#   
#   aqw <-  rhandsontable::hot_to_r(input$metaTable)
#   for(i in 1:ncol(aqw)){
#     aqw[ ,i] <- as.character(aqw[ ,i])
#   }
#   
#   ind <- is.na(aqw[-1,]$genbank_accession)
#   
#   providedAccessions <- as.character(aqw[-1,]$genbank_accession[!ind])
#   
#   ncbiResults <- lapply(as.list(providedAccessions), function(x){
#     try(traits::ncbi_byid(x),
#         silent = TRUE)
#   })
#   
#   
#   zerror <- unlist(lapply(ncbiResults, function(x) inherits(x, 'try-error')))
#   
#   ind[!ind] <- zerror
#   
#   
#   ncbiResults <- ncbiResults[!zerror]
#   
#   
#   genus <- sapply(ncbiResults, function(x) strsplit(x$taxon, " ")[[1]][[1]])
#   species <- sapply(ncbiResults, function(x) strsplit(x$taxon, " ")[[1]][[2]])
#   dna_16s <- lapply(ncbiResults, function(x){
#     if(as.numeric(x$length) < 2000){
#       x$sequence
#     } else {NA}
#   })
#   
#   taxo <- lapply(ncbiResults, function(x){
#     q <- taxize::classification(x$taxon,
#                                 db="ncbi",
#                                 return_id = FALSE)[[1]]
#     
#     if(!is.na(q)){
#       q2 <- as.list(q$name)
#       names(q2) <- q$rank
#       q2
#     } else {
#       NA
#     }
#   })
#   
#   
#   
#   keys <- unique(unlist(lapply(taxo, names)))
#   taxo <-  stats::setNames(do.call(mapply, c(FUN=c, lapply(taxo, `[`, keys))), keys)
#   
#   # get rhandsontable minus the example row
#   awe <-  aqw[-1, ]
#   # ind is a logical vector of rows with input accessions
#   awe$kingdom[!ind] <- taxo$superkingdom
#   awe$phylum[!ind] <- taxo$phylum
#   awe$class[!ind] <- taxo$class
#   awe$order[!ind] <- taxo$order
#   awe$family[!ind] <- taxo$family
#   awe$genus[!ind] <- taxo$genus
#   awe$species[!ind] <- taxo$species
#   awe$dna_16s[!ind] <- unlist(dna_16s)
#   
#   # Update reactive value
#   qwerty$rtab <- rbind(rhandsontable::hot_to_r(input$metaTable)[1, ], awe)
#   
# }