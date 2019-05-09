### IDBac Easy Install

Find installers for Windows at: https://chasemc.github.io/IDBac/

### Finding Help, Reporting Issues


We have setup a discussion forum at https://groups.google.com/forum/#!forum/idbac

Issues/Errors may be reported here (https://github.com/chasemc/IDBacApp/issues) 


### Run IDBac from R

If you would rather run IDBac directly from R:

```{r}
# install.packages("devtools") # Run this if you don't have the devtools package
devtools::install_github("chasemc/IDBacApp")

# And, to run the app:
IDBacApp::run_app()

```


### IDBac Method

[Click Here](https://github.com/chasemc/IDBacApp/blob/master/method.md#-idbac-instruction-manual-for-maldi-tof-ms-acquistion-and-data-analysis-)


### Citing IDBac

Clark, C. M., Costa, M. S., Sanchez, L. M., & Murphy, B. T. (2018). Coupling MALDI-TOF mass spectrometry protein and specialized metabolite analyses to rapidly discriminate bacterial function. Proceedings of the National Academy of Sciences of the United States of America, 115(19), 4981-4986. [https://www.doi.org/10.1073/pnas.1801247115](https://www.doi.org/10.1073/pnas.1801247115)

Every IDBac release is preserved at Zenodo for citability/reproducibility:   
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1115619.svg)](https://doi.org/10.5281/zenodo.1115619)


### For IDBac Developers

Linux and Mac CI:
  - [![Build Status](https://travis-ci.org/chasemc/IDBacApp.svg?branch=master)](https://travis-ci.org/chasemc/IDBacApp)
  
Windows CI:
  - [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/78sr4bqpncnwt52y/branch/master?svg=true)](https://ci.appveyor.com/project/chasemc/IDBacApp)


Coverage:
  - [![codecov](https://codecov.io/gh/chasemc/IDBacApp/branch/master/graph/badge.svg)](https://codecov.io/gh/chasemc/IDBacApp)
