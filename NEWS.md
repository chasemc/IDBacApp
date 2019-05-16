---
title: "NEWS"
author: "Chase Clark"
---



# IDBac VERSION 1.0

This release is more or less an entire re-write of IDBac, so... many, many changes. Only some are listed below.

### Installer Changes

RInno had a couple pretty massive API changes which led to my needing to write https://github.com/chasemc/electricShine.  This means that IDBac is now an Electron app that is self-contained (includes R, packages,Electron, browser, etc). All packages and R were installed from a specified MRAN snapshot. The installer is built using Continuous Deployment on appVeyor (https://github.com/chasemc/electricShine/blob/master/appveyor.yml) and attached to each GitHub release. 

### Structural Changes
This is likely the largest change and the most significant for anyone wanting to add-on or contribute to the codebase. 

While the first version of IDBac followed a valid paradigm for small Shiny apps (two files: server.r and ui.r), it became quickly apparent that it was an unsustainable and unscalable model for future development when shiny modules was released.

 New paradigms:
 - IDBac is an R package
   - Now IDBac is structured as a regular R-package. This means that significant computation and code chunks have been pulled out of server.r and are now located in the package's `R` directory. 
 - IDBac is testable
   - While methods for testing the interactivity of Shiny apps is still a little shaky, many functions have been extracted from the shiny logic and now have `testthat` tests, writing tests for old functions is an ongoing effort.
     - To contribute to writing tests, see https://codecov.io/gh/chasemc/IDBacApp for a breakdown of functions that need tests.
 
### Structural Changes (continued)

Prior to this release IDBac relied on the filesystem for storing and retrieving mzXML files, processed data, etc. This has been completely revised and moved to a SQLite based filesystem. This has the advantage of producing a single file (database) per "experiment" (colelction of samples with MALDI data). This makes it easy to transfer and keep track of data, scales with large data sets, and allows more organized spectra data and metadata. Lastly, it opens the potential for data handlingg through other languages. 
 
 
### Algorithm changes (AKA Breaking results changes)...not comprehensive

##### Peak Binning
The largest change here was that of how protein spectra are compared. The algorithm contained in MALDIquant and used by the old version of IDBac [binPeaks()](https://github.com/sgibb/MALDIquant/blob/master/R/binPeaks-functions.R) has some drwabacks:
 binPeaks() outline is [here](https://github.com/sgibb/MALDIquant/blob/master/man/binPeaks-functions.Rd)
  1. The "bins" will change depending on which samples are provided.
    - Given peak lists A, B, C.  You bin peaks from A and B and then, separately, bin peaks from A and C. The two resulting peak lists are not comparable.

##### PCA

Now performed via the irlba package which should make the computation faster but will provide some differences to prcomp

##### Cosine Similarity

Now performed with the coop package which is a fast and memory-efficient implementation.
    
