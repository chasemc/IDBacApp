
Every IDBac release (Including this Manual) is preserved at Zenodo for citability/reproducibility:   
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1115619.svg)](https://doi.org/10.5281/zenodo.1115619)

<H1 align="center"> IDBac Instruction Manual for MALDI-TOF MS Acquistion and Data Analysis </H1>  

- If you haven't already, please see the IDBac publication:
    - https://www.biorxiv.org/content/early/2017/11/08/215350 
- And the main website:  
    - https://chasemc.github.io/IDBac  
    

## Table of Contents
- [Naming Samples](#naming-samples)
  - Instructions for Bruker MALDI-TOF MS Instruments 
  - Instructions for Other Instruments
- [Sample Preparation](#sample-preparation)
  - Cleaning MALDI Target Plates
  - MALDI Matrix Preparation
  - Applying Samples to the MALDI Plate
- [Data Acquisition](#data-acquisition)
- [IDBac Software Installation](#idbac-software-installation)
- [Data Analysis: Quick Start](#data-analysis-quick-start)
- [IDBac Limitations](idbac-limitations)

---
---
---
<H1 align="center"> Naming Samples</H1>
 
### Instructions for Bruker MALDI-TOF MS Instruments 
As it is quite cumbersome to label many samples from within Bruker flexControl, we have created an easy method for naming samples using an Excel®/OpenOffice™ template.
    
If you haven't already, download the Excel template for renaming raw data files [here.](/MALDI-Plate_Template/384_Spot_MALDI_Template.xltx) This template was designed to work with MALDI plates of up to 384 spots.

---

- Useing the Excel®/OpenOffice™ template mentioned above, simply enter your sample names into the spreadsheet as seen below.
- You should always reserve at least three spots on the MALDI target plate.
   1. One spot for small-molecule calibration.
      - We use Bruker's Peptide Calibration Standard [Version 1](https://www.bruker.com/fileadmin/user_upload/8-PDF-Docs/Separations_MassSpectrometry/InstructionForUse/IFU_206195_Peptide_Cal_Stand_Rev1.pdf) or [Version 2](https://www.bruker.com/fileadmin/user_upload/8-PDF-Docs/Separations_MassSpectrometry/InstructionForUse/IFU_8222570_Peptide_Calibration_Standard_II_Revision_B.pdf)
   2. One spot for protein calibration
      - We use [Bruker BTS Standard.](https://www.bruker.com/fileadmin/user_upload/8-PDF-Docs/Separations_MassSpectrometry/InstructionForUse/IFU_Bruker_Bacterial_Test_Standard_Revision_C.pdf) Note this is not the same as "BTS IVD" which is the more expensive clinic-approved matrix.
   3. One spot for a matrix blank to be subtracted from small-molecule acquisitions. 
      - This spot should contain the word “matrix”.

<p align="center">
  <img src= "/ReadMe_Images/Naming_Files.png" />
</p>


If you don’t have access to Microsoft Excel, we have successfully tested this with the free Excel alternative:  Apache OpenOffice™ “Calc”, which can be found at [www.openoffice.org](www.openoffice.org). When saving the file, ensure you save it as type “Microsoft Excel 97/2000/XP (.xls)”.

---

* ### Instructions for Other Instruments
    * If you have another MALDI instrument please open a new [issue](https://github.com/chasemc/IDBacApp/issues) so that we can incorporate your file types into the system
  
---
---
---
<H1 align="center"> Sample Preparation </H1>

### Cleaning MALDI Target Plates

The MALDI plate should be properly cleaned before use.  In order to clean the MALDI plate, use the steps below:
method adapted from [Freiwald & Sauer](http://www.nature.com/nprot/journal/v4/n5/full/nprot.2009.37.html?foxtrotcallback=true)

1. Remove target plate from holder and rinse with acetone.
2. To remove trace protein/lipids, use non-abrasive liquid soap.
3. Rinse with distilled water ~2 min to completely remove soap.
4. Sonicate in HPLC grade water (Ultrasonic bath) for ~5 min.
5. Rinse with HPLC grade water.
6. Rinse with HPLC grade methanol.

---

### MALDI Matrix Preparation
1. Prepare 10 mg/mL [α-cyano-4-hydroxycinnamic acid](http://www.sigmaaldrich.com/catalog/search?term=28166-41-8&interface=CAS%20No.&N=0&mode=partialmax&lang=en&region=US&focus=product) (CHCA) in MS-grade solvents:
   - 50% Acetonitrile (ACN)
   - 47.5% Water (H<sub>2</sub>O)
   - 2.5% Trifluoroacetic Acid (TFA)
     - e.g. 100 µL of solution = 50 µL ACN + 47.5 µL H<sub>2</sub>O + 2.5 µL TFA + 1 mg CHCA

**Notes:**
- Use fresh matrix solution and store unused solid CHCA between 2-8 °C.
- There are many MALDI matrix alternatives. We have had success using CHCA, which is also more common for protein profiling of bacteria. It may be worth trying CHCA + DHB. Matrix selection depends on individual user needs.
  - For alternative small-molecule matrices please see: https://doi.org/10.1055/s-0042-104800

---

### Applying Samples to the MALDI Plate


|               |               |
| ------------- | ------------- |
| 1. Apply bacteria directly without any prior chemical treatment. Smear a single bacterial colony in a thin layer directly onto the MALDI target plate using a sterile toothpick. | <img src= "/ReadMe_Images/ApplyingToMALDIPlate1.png" height=186 width=800 /> |
| 2. Add 1 µl of PepMix and 1ul BTS to their respective external calibration spots.   | <img src= "/ReadMe_Images/ApplyingToMALDIPlate2.png" width=900 />              |
| 3. Add 1 µL of 70% formic acid to the spot, let air dry. | <img src= "/ReadMe_Images/ApplyingToMALDIPlate2.png" height=186 width=900 />              |
| 4. Add 1 µL of MALDI matrix to every spot, let air dry.    | <img src= "/ReadMe_Images/ApplyingToMALDIPlate3.png" height=186 width=900 />           |


**Note:**
- For alternative sample preparation methods please refer to [Freiwald & Sauer](http://www.nature.com/nprot/journal/v4/n5/full/nprot.2009.37.html?foxtrotcallback=true)

---
---
---
<H1 align="center"> Data Acquisition </H1>
Once you have prepared your MALDI target plate you, will need to setup autoXecute and begin the data acquisition process.
The following section is written for a Bruker-autoflex™ model instrument, instructions may vary slightly depending on manufacturer/model.


|               |               |
| ------------- | ------------- |
| 1. Insert MALDI plate into the mass spectrometer    															| <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire1.PNG" align="center"> <img src= "/ReadMe_Images/Acquire1.PNG" width=900 />  </a> |
| 2. Select the appropriate IDBac Method <li> “IDBac_Protein.par” </li>or<li> “IDBac_SmallMolecule.par” </li>   	|  <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire13.png" align="center"> <img align="center" src= "/ReadMe_Images/Acquire13.png" width=900 /> </a>    |
| 3. Under the “AutoXecute” control panel select “New”, which is to the right of “Run” 									|  <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire2.PNG" align="center"> <img src= "/ReadMe_Images/Acquire3.PNG" width=900 /> </a>    |
| 4. If it wasn’t automatically detected, select the appropriate MALDI target plate geometry. 					|  <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire3.PNG" align="center">  <img src= "/ReadMe_Images/Acquire3.PNG" width=900 /> </a>    |
| 5. (Optional) Follow the directions to choose representative spots for laser power tuning and select “Next”   |  <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire4.PNG" align="center">  <img src= "/ReadMe_Images/Acquire4.PNG" width=900 /> </a>    |
|  Note: If you skip step 5, you should manually determine the minimum laser fluency/power needed and then press “Set initial laser power” before beginning the run         |  <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire5.PNG" align="center">  <img src= "/ReadMe_Images/Acquire5.PNG" width=900 /> </a>    |
| 6. Select “Calibrate with own template” and then select “New"             									|  <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire6.PNG" align="center">  <img src= "/ReadMe_Images/Acquire6.PNG" width=900 /> </a>    |
| 7. Follow the directions in the left panel and then select “OK”         										|  <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire7.PNG" align="center">  <img src= "/ReadMe_Images/Acquire7.PNG" width=900 /> </a>    |
| 8. Select “Next”    																							|  <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire8.PNG" align="center">  <img src= "/ReadMe_Images/Acquire8.PNG" width=900 /> </a>    |
| 9. Within the "run parameters" page it is important to ensure the correct methods are selected in the correct places. <br> For small molecule runs change both autoXecute methods to:  <br> “IDBac_Small-Molecule_autoX” <br> For protein runs change both autoXecute methods to: <br> “IDBac_Protein_AutoX.axe”. <br><br> There are four flexAnalysis methods: <br> <ul>The protein or small molecule “Calibrant Calibration” should be selected within the calibration box’s “flexAnalysis Method” pull-down menu. <ul> IDBac Protein Calibrant Calibration </ul> <ul> IDBac Small Molecule Calibrant Calibration </ul></ul> <br> The protein or small molecule “Unknown Sample Calibration” should be selected within the second “flexAnalysis Method” pull-down menu. <br> IDBac Protein Unknown Sample Calibration <br> IDBac Small Molecule Unknown Sample Calibration <br><br> When you have finished, select “Next” <br> 		| <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire10.png" align="center">  <img src= "/ReadMe_Images/Acquire10.png" width=900 /> </a>  <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire9.png" align="center">  <img src= "/ReadMe_Images/Acquire9.png" width=900 /> </a>    |
| 10. Select “Save as” and save the sequence run to your data directory.     Confirm and select “OK”       		|  <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire11.png" align="center">  <img src= "/ReadMe_Images/Acquire11.png" width=900 /> </a>   |
| 11. Under the “AutoXecute” control panel select “Start automatic Run”              							|  <a href="https://raw.githubusercontent.com/chasemc/IDBacApp/master/ReadMe_Images/Acquire12.png" align="center">  <img src= "/ReadMe_Images/Acquire12.png" width=900 /> </a>   |




---
---
---
<H1 align="center"> Data Analysis: Quick Start </H1>


---
---
---
<H1 align="center"> IDBac Software Installation </H1>
The IDBac software, MALDI plate sample template, and example data may be downloaded [here](https://drive.google.com/open?id=0B0n1AhTXfxHqSmJFSHEyWjdaNmM)




### Principle Components Analysis (PCA) / Hierarchical Clustering
A good overview of clustering methods may be found [here](http://research.med.helsinki.fi/corefacilities/proteinchem/hierarchical_clustering_basics.pdf)

---
---
---
<H1 align="center"> IDBac Limitations  </H1>

IDBac users are expected to use their own judgement and formal training in statistics to construct and validate their experiments. 
We also suggest users be familiar with the concepts covered in  Section ["6.4.3 Remarks on Statistical Problems with MS Data"](https://medschool.vanderbilt.edu/msrc/files/msrc/public_files/Forms/clinprotoolsmanual.pdf) from Bruker Daltonic's "ClinProTools User Manual".

---

### Current Known Limitations of IDBac
* Spectra Alignment
  * A common practice when preprocessing MALDI-TOF spectra (and two-dimensional spectra from similar instruments) is to do an alignment/warping/phase-correction of spectra before calling peak detection. However, this requires the prior establishment of an average/representative spectrum to which all other spectra are aligned.  This method is impractical with the number and diversity of spectra that IDBac was built to handle. For this reason, IDBac currently relies on spectra being well-calibrated and on peak-binning, which occurs after peak-picking.
* Peak Binning
  * MALDIquant's peak binning algorithm begins to falter slightly with a large number of spectra/samples (100's-1000's).
      * An overview of MALDIquant's algorithm for peak binning may be found [here](https://www.rdocumentation.org/packages/MALDIquant/versions/1.17/topics/binPeaks)
      * The actual R code may be found [here](https://github.com/sgibb/MALDIquant/blob/master/R/binPeaks-functions.R)








---
---
---
<H1 align="center"> Contributions </H1>

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
  - Chambers, C. M, et al. (2012). “A cross-platform toolkit for mass spectrometry and proteomics.” Nat Biotech, 30(10), pp. 918–920. doi: 10.1038/nbt.2377, http://dx.doi.org/10.1038/nbt.2377.
Martens L, Chambers M, et al. (2010). “mzML - a Community Standard for Mass Spectrometry Data.” Mol Cell Proteomics. doi: 10.1074/mcp.R110.000133.
Pedrioli PGA,et al.  (2004). “A common open representation of mass spectrometry data and its application to proteomics research.” Nat Biotechnol, 22(11), pp. 1459–1466. doi: 10.1038/nbt1031.
Keller A, Eng J, Zhang N, Li X and Aebersold R (2005). “A uniform proteomics MS/MS analysis platform utilizing open XML file formats.” Mol Syst Biol.
Kessner D, Chambers M, Burke R, Agus D and Mallick P (2008). “ProteoWizard: open source software for rapid proteomics tools development.” Bioinformatics, 24(21), pp. 2534–2536. doi: 10.1093/bioinformatics/btn323.
