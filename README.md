There are currently no easy-installs for versions >2.0 and limited support. IDBac is currently under development in another repository.
If you encounter issues with version 2.0, please fall back to version 1.1.10:
https://github.com/chasemc/IDBacApp/releases/tag/1.1.10

### IDBac Easy Install

Find installers for Windows at: https://chasemc.github.io/IDBac/

### Finding Help, Reporting Issues


We have setup a discussion forum at https://groups.google.com/forum/#!forum/idbac

Issues/Errors may be reported here (https://github.com/chasemc/IDBacApp/issues) 


### Run IDBac from R

If you would rather run IDBac directly from R:

```{r}
install.packages("remotes") # Run this if you don't have the devtools package
remotes::install_github("chasemc/IDBacApp@*release")

# And, to run the app:
IDBacApp::run_app()

```


### IDBac Method
A detailed video and text version of the method and software is available in the following publication:

Clark, C. M., Costa, M. S., Conley, E., Li, E., Sanchez, L. M., Murphy, B. T. Using the Open-Source MALDI TOF-MS IDBac Pipeline for Analysis of Microbial Protein and Specialized Metabolite Data. J. Vis. Exp. (147), e59219, doi:10.3791/59219 (2019).
[https://dx.doi.org/10.3791/59219](https://dx.doi.org/10.3791/59219)
### Citing IDBac

Clark, C. M., Costa, M. S., Sanchez, L. M., & Murphy, B. T. (2018). Coupling MALDI-TOF mass spectrometry protein and specialized metabolite analyses to rapidly discriminate bacterial function. Proceedings of the National Academy of Sciences of the United States of America, 115(19), 4981-4986. [https://www.doi.org/10.1073/pnas.1801247115](https://www.doi.org/10.1073/pnas.1801247115)

Every IDBac release is preserved at Zenodo for citability/reproducibility:   
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1115619.svg)](https://doi.org/10.5281/zenodo.1115619)


### For IDBac Developers

