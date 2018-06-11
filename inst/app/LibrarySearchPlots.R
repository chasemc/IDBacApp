# This function is used to create the inverse-comparison plots for displaying the results of library searches

libraryInverseComparisonPlots <- function(peaksSampleOne, peaksSampleTwo, meanSpectrumSampleOne, meanSpectrumSampleTwo ){

# Takes as input:
  # peaksSampleOne  - Sample
  # peaksSampleTwo  - Library Strain
  # meanSpectrumSampleOne
  # meanSpectrumSampleTwo




# Create dataframes for peak plots and color each peak according to whether it occurs in the other spectrum
p1b <- as.data.frame(cbind(peaksSampleOne@mass, peaksSampleOne@intensity))
p1b <- as.data.frame(cbind(peaksSampleOne@mass, peaksSampleOne@intensity))
p2b <- as.data.frame(cbind(peaksSampleTwo@mass, peaksSampleTwo@intensity))
p2b <- as.data.frame(cbind(peaksSampleTwo@mass, peaksSampleTwo@intensity))


# Color all positive peaks red
p3b<-data.frame(p1b,rep("red",length = length(p1b$V1)), stringsAsFactors = F)
colnames(p3b) <- c("Mass", "Intensity", "Color")


# Color all negative peaks grey
p4b <- data.frame(p2b, rep("grey", length = length(p2b$V1)), stringsAsFactors = F)
colnames(p4b) <- c("Mass", "Intensity", "Color")

# Color all peak matches blue for positive peaks
p3b$Color[which(p3b$Mass %in% intersect(p3b$Mass, p4b$Mass))] <- "blue"


a <- (list(meanSpectrumSampleOne, meanSpectrumSampleTwo, p1b, p2b, p3b, p4b))
names(a) <- c("meanSpectrumSampleOne", "meanSpectrumSampleTwo", "p1b", "p2b", "p3b", "p4b")
return(a)



temp<- listOfDataframesForInversePeakComparisonPlot()
meanSpectrumSampleOne <-temp$meanSpectrumSampleOne
meanSpectrumSampleTwo <-temp$meanSpectrumSampleTwo
p1b <-temp$p1b
p2b <-temp$p2b
p3b <-temp$p3b
p4b <-temp$p4b



#Create peak plots and color each peak according to whether it occurs in the other spectrum
plot(meanSpectrumSampleOne@mass,meanSpectrumSampleOne@intensity,ylim=c(-max(meanSpectrumSampleTwo@intensity),max(meanSpectrumSampleOne@intensity)),type="l",col=adjustcolor("Black", alpha=0.3),xlab="m/z",ylab="Intensity")
lines(meanSpectrumSampleTwo@mass,-meanSpectrumSampleTwo@intensity)
rect(xleft=p3b$Mass-.5, ybottom=0, xright=p3b$Mass+.5, ytop=((p3b$Intensity)*max(meanSpectrumSampleOne@intensity)/max(p3b$Intensity)),border=p3b$Color)
rect(xleft=p4b$Mass-.5, ybottom=0, xright=p4b$Mass+.5, ytop=-((p4b$Intensity)*max(meanSpectrumSampleTwo@intensity)/max(p4b$Intensity)),border=p4b$Color)
}
