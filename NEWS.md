---
title: "NEWS"
author: "Chase Clark"
---

Updates are ordered newest to oldest.


### Updated SQLite database table and field names

- This was done for consistency, especially to make it easier for others to program with IDBac databases

- Table Names:
  - "IndividualSpectra is now spectra"
    - "spectrumMassHash is now spectrum_mass_hash"
    - "spectrumIntensityHash is now spectrum_intensity_hash"
    - "XMLHash is now xml_hash"
    - "Strain_ID is now strain_id"
    - "peakMatrix is now peak_matrix"
    - "spectrumIntensity is now spectrum_intensity"
    - "maxMass is now max_mass"
    - "minMass is now min_mass"
    - "ignore is now ignore"
    - "number is now number"
    - "timeDelay is now time_delay"
    - "timeDelta is now time_delta"
    - "calibrationConstants is now calibration_constants"
    - "v1tofCalibration is now v1_tof_calibration"
    - "dataType is now data_type"
    - "dataSystem is now data_system"
    - "spectrometerType is now spectrometer_type"
    - "inlet is now inlet"
    - "ionizationMode is now ionization_mode"
    - "acquisitionMethod is now acquisition_method"
    - "acquisitionDate is now acquisition_date"
    - "acquisitionMode is now acquisition_mode"
    - "tofMode is now tof_mode"
    - "acquisitionOperatorMode is now acquisition_operator_mode"
    - "laserAttenuation is now laser_attenuation"
    - "digitizerType is now digitizer_type"
    - "flexControlVersion is now flex_control_version"
    - "id is now id"
    - "instrument is now instrument"
    - "instrumentId is now instrument_id"
    - "instrumentType is now instrument_type"
    - "massError is now mass_error"
    - "laserShots is now laser_shots"
    - "patch is now patch"
    - "path is now path"
    - "laserRepetition is now laser_repetition"
    - "spot is now spot"
    - "spectrumType is now spectrum_type"
    - "targetCount is now target_count"
    - "targetIdString is now target_id_string"
    - "targetSerialNumber is now target_serial_number"
    - "targetTypeNumber is now target_type_number"    
  - "XML is now xml"
    - "XMLHash is now xml_hash"
    - "XML is now xml"
    - "manufacturer is now manufacturer"
    - "model is now model"
    - "ionization is now ionization"
    - "analyzer is now analyzer"
    - "detector is now detector"
    - "Instrument_MetaFile is now instrument_metafile"
  - "locale is now locale"
    - "locale is now locale"
  - "massTable is now mass_index"
    - "spectrumMassHash is now spectrum_mass_hash"
    - "massVector is now mass_vector"
  - "metaData is now metadata"
    - "Strain_ID is now strain_id"
    - "Genbank_Accession is now genbank_accession"
    - "NCBI_TaxID is now ncbi_taxid"
    - "Kingdom is now kingdom"
    - "Phylum is now phylum"
    - "Class is now class"
    - "Order is now order"
    - "Family is now family"
    - "Genus is now genus"
    - "Species is now species"
    - "MALDI_Matrix is now maldi_matrix"
    - "DSM_Agar_Media is now dsm_cultivation_media"
    - "Cultivation_Temp_Celsius is now cultivation_temp_celsius"
    - "Cultivation_Time_Days is now cultivation_time_days"
    - "Cultivation_Other is now cultivation_other"
    - "User is now user_firstname_lastname"
    - "User_ORCID is now user_orcid"
    - "PI_FirstName_LastName is now pi_firstname_lastname"
    - "PI_ORCID is now pi_orcid"
    - "dna_16S is now dna_16s"  
  - "version is now version"
    - "IDBacVersion is now idbac_version"
    - "rVersion is now r_version"





## 1.1.10

- BugFix:
  -  When updating IDBac software, experiments saved into the default directory would sometimes be deleted by the installer. 
          This has been changed so that IDBac will try to make the default save directory '~Documents/IDBac_experiments'.
- Improvement:
  - Improved support for importing data from Microtyper instruments. Including 
- Improvement:
  - Changed how IDBac first connects to an experiment database, for faster connect time, especially with large experiments (1000's of samples).
- Improvement:
  - Mirror plot code was improved and placed into a standalone function for coding with IDBac outside of the Shiny app.
- BugFix:
  - 'Binning' algorithm checks for empty peak lists and added check for list output from `mapply()`.
- Improvement:  
  - Continuous integration fixed for Mac and Linux builds.



## 1.0.0

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
The largest change here was that of how protein spectra are compared. The algorithm contained in MALDIquant and used by the old version of IDBac [binPeaks()](https://github.com/sgibb/MALDIquant/blob/master/R/binPeaks-functions.R) has some drawbacks:
The `binPeaks()` outline is [here](https://github.com/sgibb/MALDIquant/blob/master/man/binPeaks-functions.Rd).
From version 1.0.0 and forward, IDBac uses a high dimensional boolean representation of peaks in a mass spectrum. It distributes each peak's intensity onto this vector by creating a normal distribution (see: https://doi.org/10.1021/acs.analchem.6b02446) according to a mass-scaled deviation (ppm error). Simialar approaches can be found: https://doi.org/10.1186/1477-5956-5-3 and https://doi.org/10.1093/bioinformatics/btl645



##### PCA

Now performed via the irlba package which should make the computation faster but will provide some differences to prcomp

##### Cosine Similarity

Now performed with the coop package which is a fast and memory-efficient implementation.
    
