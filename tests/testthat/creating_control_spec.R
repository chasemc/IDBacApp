#  # https://github.com/bryanhanson/SpecHelpers
# 
# library(SpecHelpers)
# 
# 
# a <- seq(1200, 15000, 1000)
# chrom <- data.frame(mu = a, sd = rep(40, length(a)),
#                     area = rep(10, length(a)), tail =  rep(NA, length(a)))
# ex1 <- makeSpec(chrom, x.range = c(2000, 20000), plot = TRUE, curves = F,
#                 dd = .1, main = "Chromatogram with Underlying Pure Curves")
# a <- MALDIquant::createMassSpectrum(ex1[1,], ex1[2, ])
