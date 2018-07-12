
collpaseProteinReplicates <- function(trimmedProteinPeakList, proteinPercentPresence){



labs <- sapply(trimmedProteinPeakList, function(x)metaData(x)$Strain)
labs <- factor(labs)
new2 <- NULL
newPeaks <- NULL
for (i in seq_along(levels(labs))) {
  specSubset <- (which(labs == levels(labs)[[i]]))
  if (length(specSubset) > 1) {
    new <- filterPeaks(trimmedProteinPeakList[specSubset], minFrequency = proteinPercentPresence / 100)
    new <- mergeMassPeaks(new, method="mean")
    new2 <- c(new2, new)
  } else{
    new2 <- c(new2, trimmedProteinPeakList[specSubset])
  }

}

new2

}
