<p align="center">
 <h3> IDBac </h3>
</p>

![alt text](/ReadMe_Images/MainImage.png)
 




### Download the Excel template for renaming raw MALDI files:
https://github.com/chasemc/IDBac_App/blob/master/MALDI-Plate_Template/384_Spot_MALDI_Template.xltx

  
  

### Download/Install the IDBac Application


#### Currently IDBAC comes in two forms:

1. The stable, easy-to-install version:
    * Version "0.0.1" may be downloaded from here:
2. Development versions of IDBac may be downloaded as an R package by following the directions below



#### Code to use IDBac from R:
The R code below will, if needed, install "devtools" for downloading IDBac and "biocLite" for downlaoding Bioconductor packages
```
Install_And_Load <- function(Required_Packages)
{  Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[, "Package"])]
  if (length(Remaining_Packages)) {
    install.packages(Remaining_Packages)  }
  for (package_name in Required_Packages)  {
    library(package_name,
            character.only = TRUE,
            quietly = TRUE)  } }
# Required packages to install and load
Required_Packages = c("devtools","BiocInstaller")
# Install and Load Packages
Install_And_Load(Required_Packages)

source("https://bioconductor.org/biocLite.R")
# Change FALSE to TRUE if you would like to be prompted before updating bioconductor packages during the execution of the below function
biocLite("mzR",ask=FALSE)


```



Once you have installed the necessary packages above, install IDBac using the code below:

```
devtools::install_github("chasemc/IDBac")
```


You can then start an instance of IDBac using the following command:

```
IDBac::runIDBac()
```



#### Tips and Tricks
Since IDBac is a Shiny app it can run multiple instances.  To do this, start IDBac and simply copy the address from the address bar in your browser into a new window or tab in your internet browser.
