
collapseProteinReplicates <- function(trimmedProteinPeakList, proteinPercentPresence){

labs <- sapply(trimmedProteinPeakList, function(x)  MALDIquant::metaData(x)$Strain)
labs <- factor(labs)
new2 <- NULL
newPeaks <- NULL
for (i in seq_along(levels(labs))) {
  specSubset <- (which(labs == levels(labs)[[i]]))
  if (length(specSubset) > 1) {
    new <- MALDIquant::filterPeaks(trimmedProteinPeakList[specSubset], minFrequency = proteinPercentPresence / 100)
    new <- MALDIquant::mergeMassPeaks(new, method="sum")
  # If we use "mean", peaks that show up often can be weighted less than a peak that shows up once with high intensity, so now using "sum"

    new2 <- c(new2, new)
  } else{
    new2 <- c(new2, trimmedProteinPeakList[specSubset])
  }

}

new2

}
