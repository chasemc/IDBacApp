IDBac releases are preserved at Zenodo:   
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1115619.svg)](https://doi.org/10.5281/zenodo.1115619)



![](/ReadMe_Images/MainImage.png)





# IDBac Instruction Manual for MALDI-TOF MS Acquistion and Data Analysis 


## Contents
- [Sample Preparation](#sample-preparation)
- [Renaming Bruker Raw Data Files](#renaming-bruker-raw-data-files)
- [Data Acquisition](#data-acquisition)
- [IDBac Software Installation](#idbac-software-installation)
- [Data Analysis: Quick Start](#data-analysis-quick-start)






### Sample Preparation
This section details our easy way to label sample data as well as preparing the MALDI target plate for data acquisition.


### Renaming Bruker Raw Data Files
If you haven't already, download the Excel template for renaming raw data files [here](/MALDI-Plate_Template/384_Spot_MALDI_Template.xltx)

  
#### Naming Samples
- We created an excel template for 384-spot MALDI target plates (template will also work for 96-spot and 48-spot plates). Using this template removes the need to name your files within Bruker’s flexControl software and is the preferred method for IDBac. 
- Simply enter your sample names into the spreadsheet as seen below.
- Be sure to reserve at least one spot on the MALDI target plate for the two calibration standards, at least one spot for PepMix and at least one other spot for Bruker’s BTS Standard!
- Be sure to include a matrix blank to be subtracted from small-molecule acquisitions. This spot should contain the word “matrix” (any capitalization)

![](/ReadMe_Images/Naming_Files.png)

If you don’t have access to Microsoft Excel, we have successfully tested this with the free Excel alternative:  Apache Open Office 4 “Calc”. Which can be found at www.openoffice.org. Just ensure when saving the template you save it as type “Microsoft Excel 97/2000/XP (.xls)”



#### Cleaning MALDI Target Plates

The MALDI plate should be properly cleaned before use.  In order to clean the MALDI plate use the steps below:
method adapted from [Freiwald & Sauer](http://www.nature.com/nprot/journal/v4/n5/full/nprot.2009.37.html?foxtrotcallback=true)

1. Remove target plate from holder and rinse with acetone.
2. To remove trace protein/lipids, use non-abrasive liquid soap.
3. Rinse with HPLC grade water ~2 min.
4. Sonicate in HPLC grade water (Ultrasonic bath).
5. Rinse with HPLC grade water.
6. Rinse with HPLC grade methanol.


#### MALDI Matrix Preparation
1. Prepare the matrix solution in an Eppendorf centrifuge tube: 10 mg/mL CHCA [α-cyano-4-hydroxycinnamic acid](http://www.sigmaaldrich.com/catalog/search?term=28166-41-8&interface=CAS%20No.&N=0&mode=partialmax&lang=en&region=US&focus=product) in 50% Acetonitrile (ACN), 2.5% Trifluoroacetic Acid (TFA).
2. 100 µL of solution = 50 µL ACN + 47.5 µL H2O + 2.5µL TFA + 1 mg CHCA

**Note**: Use fresh matrix solution and be sure to store unused solid CHCA between 2-8°C
**Note 2**: There are many MALDI matrix alternatives, we have simply had luck using CHCA which is also more common for protein profiling bacteria.



#### Applying Samples to the MALDI Plate

Apply bacteria directly without any prior chemical treatment. Smear a single bacterial colony as a thin film directly onto the plate using a sterile toothpick.

**Note** For alternative sample preparation methods please refere to [Freiwald & Sauer](http://www.nature.com/nprot/journal/v4/n5/full/nprot.2009.37.html?foxtrotcallback=true)



![](/ReadMe_Images/ApplyingToMALDIPlate1.png)
![](/ReadMe_Images/ApplyingToMALDIPlate2.png)
![](/ReadMe_Images/ApplyingToMALDIPlate3.png)





### Data Acquisition
Once you have prepared your MALDI target plate protein and small molecule data you will need tosetup autoXecute and begin the data acquisition process


![](/ReadMe_Images/Acquire1.PNG)
![](/ReadMe_Images/Acquire2.PNG)
![](/ReadMe_Images/Acquire3.PNG)
![](/ReadMe_Images/Acquire4.PNG)
![](/ReadMe_Images/Acquire5.png)
![](/ReadMe_Images/Acquire6.PNG)
![](/ReadMe_Images/Acquire7.PNG)
![](/ReadMe_Images/Acquire8.PNG)





### IDBac Software Installation
The IDBac software, MALDI plate sample template, and example data may be downloaded [here](https://drive.google.com/open?id=0B0n1AhTXfxHqSmJFSHEyWjdaNmM)



### Data Analysis: Quick Start]
The software was designed with explanations and guidance throughout.  If you are stuck or 





### Contributions
Below are the some of the relevant R packages that we utilized within IDBac’s code. Without these authors’ hard work, this project would not be possible.
- R Core
  - R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
- KNIME
  - Berthold, M. R., Cebron, N., Dill, F., Gabriel, T. R., Kötter, T., Meinl, T., … Wiswedel, B. (2007). KNIME: The {K}onstanz {I}nformation {M}iner. In Data Analysis, Machine Learning and Applications:Proceedings of the 31st Annual Conference of the Gesellschaft für Klassifikation e.V., Albert-Ludwigs-Universität Freiburgand (pp. 319–326). Springer.
- MALDIquant
  - S. Gibb and K. Strimmer. 2012. MALDIquant: a versatile R package for the analysis of mass spectrometry data. Bioinformatics 28, 2270-2271.
- MALDIquantForeign
  - Sebastian Gibb (2015). MALDIquantForeign: Import/Export Routines for MALDIquant. R package version 0.10. https://CRAN.R-project.org/package=MALDIquantForeign
- Shiny
  - Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework for R. R package version 1.0.3. https://CRAN.R-project.org/package=shiny
- ggplot2
  - Hadley Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2009. http://ggplot2.org
- dplyr
  - Hadley Wickham, Romain Francois, Lionel Henry and Kirill Müller (2017). dplyr: A Grammar of Data Manipulation. R package version 0.7.0. https://CRAN.R-project.org/package=dplyr
- FactoMineR
  - Sebastien Le, Julie Josse, Francois Husson (2008). FactoMineR: An R Package for Multivariate  Analysis. Journal of Statistical Software, 25(1), 1-18. 10.18637/jss.v025.i01
- dendextend
  - Tal Galili (2015). dendextend: an R package for visualizing, adjusting, and comparing trees of  hierarchical clustering. Bioinformatics. DOI:10.1093/bioinformatics/btv428
- networkD3
  - J.J. Allaire, Christopher Gandrud, Kenton Russell and CJ Yetman (2017). networkD3: D3 JavaScript Network Graphs from R. R package version 0.4. https://CRAN.R-project.org/package=networkD3
- reshape2
  - Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software,  21(12), 1-20. URL http://www.jstatsoft.org/v21/i12/
- rgl
  - Daniel Adler, Duncan Murdoch and others (2017). rgl: 3D Visualization Using OpenGL. R package version 0.98.1. https://CRAN.R-project.org/package=rgl
- mzR
  - Chambers, C. M, et al. (2012). “A cross-platform toolkit for mass spectrometry and proteomics.” Nat Biotech, 30(10), pp. 918–920. doi: 10.1038/nbt.2377, http://dx.doi.org/10.1038/nbt.2377.
Martens L, Chambers M, et al. (2010). “mzML - a Community Standard for Mass Spectrometry Data.” Mol Cell Proteomics. doi: 10.1074/mcp.R110.000133.
Pedrioli PGA,et al.  (2004). “A common open representation of mass spectrometry data and its application to proteomics research.” Nat Biotechnol, 22(11), pp. 1459–1466. doi: 10.1038/nbt1031.
Keller A, Eng J, Zhang N, Li X and Aebersold R (2005). “A uniform proteomics MS/MS analysis platform utilizing open XML file formats.” Mol Syst Biol.
Kessner D, Chambers M, Burke R, Agus D and Mallick P (2008). “ProteoWizard: open source software for rapid proteomics tools development.” Bioinformatics, 24(21), pp. 2534–2536. doi: 10.1093/bioinformatics/btn323.

