#   from: https://qa.nmrwiki.org/question/143/what-is-the-purpose-of-acqu-and-acqus-files-in-bruker-setup
#    The s at the end of a parameter file name specifies this file as a status parameterfile. Status parameters are written at the end of an acquisition or also when a FID in a multidimensional experiment is written.
#    The files without the s are the current parameters. If you change a parameter it will be changed in the files without the s.
#    Let's assume a dataset where an acquisition has already been done, acqus and acqu contain the same information. You now decide to restart the acquisition but with more scans. You enter a new number for NS. acqu will show this new value and acqus will still show the number that was used to collect the FID that is on disk. Once the new acquisition is finished acqus now also contains the new value. In multidimensional acquisition the value of TD will be updated when a new FID is written to disk. The contents of acqus are printed in parameter listings.
#    The acqu files with numbers contain the parameters for the indirect dimensions. "acqu2" and acqu2s are for the F1 dimension in a 2D. A 3D will have acqu, acqu2 and acqu3, in a 4D you will also find acqu4 etc.
# 

# Functions taken from MALDIquant source code : https://github.com/sgibb/readBrukerFlexData
# function that should be called is readAcquFile
# input is fid file path


#' Read Bruker Acqus File
#'
#' @param fidFile filepath of Bruker Acqus file
#' @param verbose should the function be verbose?
#'
#' @return a
#' @export
#'
readAcqusFile <- function(fidFile, verbose = FALSE) {
  acquFile <- sub(pattern="fid$", x=fidFile, replacement="acqu")

  if (verbose) {
    message("Reading metadata from ", sQuote(acquFile), " ...")
  }

  if (!file.exists(acquFile)) {
    stop("File ", sQuote(acquFile), " doesn't exists!")
  }

  con <- file(acquFile, "rt")
  acquLines <- readLines(con, n=-1)
  close(con)

  ## collect data
  metadata <- list()

  ## endianness
  isBigEndian <- as.integer(.grepAcquValue("##\\$BYTORDA=", acquLines)) == 1L
  metadata$byteOrder <- ifelse(isBigEndian, "big", "little")

  ## obligate
  metadata$number <- as.double(.grepAcquValue("##\\$TD=", acquLines))
  metadata$time_delay <- .grepAcquDoubleValue("##\\$DELAY=", acquLines)
  metadata$time_delta <- .grepAcquDoubleValue("##\\$DW=", acquLines)
  metadata$calibration_constants <-
    c(c1=.grepAcquDoubleValue("##\\$ML1=", acquLines),
      c2=.grepAcquDoubleValue("##\\$ML2=", acquLines),
      c3=.grepAcquDoubleValue("##\\$ML3=", acquLines))

  ## obligate HPC
  metadata$hpcLimits <-
    c(min_mass=.grepAcquDoubleValue("##\\$HPClBLo=", acquLines),
      max_mass=.grepAcquDoubleValue("##\\$HPClBHi=", acquLines))
  metadata$hpcOrder <- as.double(.grepAcquValue("##\\$HPClOrd=", acquLines))
  metadata$hpcUse <-
    as.logical(.grepAcquValue("##\\$HPClUse=", acquLines) == "yes")

  ## was HPC involved?  metadata$hpcUse seems to be always true
  isHPCused <- isTRUE(metadata$hpcUse &&
                        metadata$hpcLimits["max_mass"] > 0L &&
                        metadata$hpcLimits["min_mass"] > 0L &&
                        metadata$hpcOrder > 0L)

  if (isHPCused) {
    hpcStr <- .grepAcquValue("##\\$HPCStr=", acquLines)
    hpcConstants <- IDBacApp::extractHPCConstants(hpcStr)
    metadata$hpcCoefficients <- hpcConstants$coefficients
    metadata$hpcCalibrationConstant0 <- hpcConstants$calibrationConstant0
    metadata$hpcCalibrationConstant2 <- hpcConstants$calibrationConstant2
  }

  # https://github.com/sgibb/MALDIquantForeign/issues/19
  metadata$v1_tof_calibration <-
    grepl("V1.0CTOF2CalibrationConstants",
          .grepAcquValue("##\\$NTBCal=", acquLines))

  ## obligate LIFT
  metadata$lift <- c(.grepAcquDoubleValue("##\\$Lift1=", acquLines),
                     .grepAcquDoubleValue("##\\$Lift2=", acquLines))
  metadata$tlift <- .grepAcquDoubleValue("##\\$TLift=", acquLines)

  ## optional
  metadata$data_type <- .grepAcquValue("##DATATYPE=", acquLines)
  metadata$data_system <- .grepAcquValue("##SPECTROMETER/DATASYSTEM=", acquLines)
  metadata$spectrometer_type <-
    .grepAcquValue("##.SPECTROMETER TYPE=", acquLines)
  metadata$inlet <- .grepAcquValue("##.INLET=", acquLines)
  metadata$ionization_mode <- .grepAcquValue("##.IONIZATION MODE=", acquLines)
  metadata$date <- .grepAcquValue("##\\$DATE=", acquLines)


  metadata$acquisition_method <- .grepAcquValue("##\\$ACQMETH=", acquLines)
  metadata$acquisition_date <- .grepAcquValue("##\\$AQ_DATE=", acquLines)
  aq_mod <- .grepAcquValue("##\\$AQ_mod=", acquLines)
  if (length(aq_mod)) {
    metadata$acquisition_mode <- switch(aq_mod,
                                       "0" = { "qf" },
                                       "1" = { "qsim" },
                                       "2" = { "qseq" },
                                       { aq_mod }
    )
  }

  aqop <- .grepAcquValue("##\\$AQOP_m=", acquLines)
  if (length(aqop)) {
    metadata$tof_mode  <- switch(aqop,
                                "0" = { "LINEAR" },
                                "1" = { "REFLECTOR" },
                                { aqop }
    )
  }

  metadata$acquisition_operator_mode <- metadata$tof_mode

  metadata$laser_attenuation <- .grepAcquDoubleValue("##\\$ATTEN=", acquLines)

  metadata$comments <- .grepAcquValue("##\\$CMT.*=", acquLines)

  metadata$deflection <-
    as.logical(.grepAcquValue("##\\$DEFLON=", acquLines) == "yes")

  digtyp  <- .grepAcquValue("##\\$DIGTYP=", acquLines)
  if (length(digtyp)) {
    metadata$digitizer_type <- switch(digtyp,
                                     "0" = { "unknown" },
                                     "1" = { "Lecroy LSA1000" },
                                     "2" = { "Acqiris DP105" },
                                     "3" = { "Acqiris DP110" },
                                     "4" = { "Acqiris DP211" },
                                     "5" = { "Acqiris DP240" },
                                     "6" = { "Acqiris AP200" },
                                     "7" = { "Acqiris AP240" },
                                     "8" = { "Acqiris DC440" },
                                     "9" = { "Acqiris DC282" },
                                     "10" = { "Acqiris Unknown subtype" },
                                     "11" = { "Gage" },
                                     "12" = { "Simulator" },
                                     "13" = { "Lecroy WaveRunner" },
                                     "14" = { "Acqiris U1084A" },
                                     "15" = { "NI 5154" },
                                     "16" = { "LeCroy LSA2000" },
                                     "17" = { "Acqiris DP1400" },
                                     "18" = { "NI 5155" },
                                     "19" = { "Bruker BD0G5" },
                                     { digtyp }
    )
  }

  metadata$deflectionPulserCal1 <-
    .grepAcquDoubleValue("##\\$DPCAL1=", acquLines)
  metadata$deflectionPulserMass <-
    .grepAcquDoubleValue("##\\$DPMASS=", acquLines)
  metadata$flex_control_version <- .grepAcquValue("##\\$FCVer=", acquLines)
  metadata$id <- .grepAcquValue("##\\$ID_raw=", acquLines)

  metadata$instrument <- .grepAcquValue("##\\$INSTRUM=", acquLines)
  metadata$instrument_id <- .grepAcquValue("##\\$InstrID=", acquLines)

  instrument_type <- .grepAcquValue("##\\$InstTyp=", acquLines)
  if (length(instrument_type)) {
    metadata$instrument_type <- switch(instrument_type,
                                      "0" = { "autoflex" },
                                      "1" = { "ultraflex" },
                                      "2" = { "ultraflexTOF/TOF" },
                                      "3" = { "reflex" },
                                      "4" = { "biflex" },
                                      "5" = { "omniflex" },
                                      "6" = { "genoflex" },
                                      "7" = { "massarray" },
                                      "8" = { "autoflexTOF/TOF" },
                                      "9" = { "microflex" },
                                      "10" = { "MT10" },
                                      { instrument_type }
    )
  }

  metadata$mass_error <- .grepAcquDoubleValue("##\\$Masserr=", acquLines)

  metadata$laser_shots <- as.double(.grepAcquValue("##\\$NoSHOTS=", acquLines))

  if (length(metadata$laser_shots) == 0L) {
    warning("File ", sQuote(fidFile), " seems to be empty because ",
            "no laser shots applied to this sample.")
  }

  metadata$patch <- .grepAcquValue("##\\$PATCHNO=", acquLines)

  ## imaging data
  if (length(metadata$patch) &&
      grepl(pattern="(R[0-9]+)?X[0-9]+Y[0-9]+", x=metadata$patch,
            ignore.case=TRUE)) {
    rx <- gregexpr(pattern="[XY][0-9]+", text=metadata$patch)[[1L]]
    pos <- substring(metadata$patch, rx+1L, rx+attr(rx, "match.length")-1L)

    if (length(pos) == 2L) {
      pos <- as.double(pos)
      metadata$imaging <- list(pos=c(x=pos[1L], y=pos[2L]))
    }
  }

  metadata$path <- .grepAcquValue("##\\$PATH=", acquLines)
  metadata$laser_repetition <- .grepAcquDoubleValue("##\\$REPHZ=", acquLines)
  metadata$spot <- .grepAcquValue("##\\$SPOTNO=", acquLines)

  sptype <- .grepAcquValue("##\\$SPType=", acquLines)
  if (length(sptype)) {
    metadata$spectrum_type <- switch(sptype,
                                    "0" = { "TOF" },
                                    "1" = { "PSD" },
                                    "2" = { "LIFT" },
                                    "3" = { "PSDSegment" },
                                    { sptype }
    )
  }

  metadata$target_count <- as.double(.grepAcquValue("##\\$TgCount", acquLines))
  metadata$target_id_string <- .grepAcquValue("##\\$TgIDS", acquLines)
  metadata$target_serial_number <- .grepAcquValue("##\\$TgSer", acquLines)
  metadata$target_type_number <- .grepAcquValue("##\\$TgTyp", acquLines)

  metadata$file <- fidFile

  metadata$sampleName <- .sampleName(fidFile)
  metadata$fullName <- paste(metadata$sampleName, metadata$patch, sep=".")
  metadata$name <- metadata$fullName

  metadata
}



#' .grepAcquValue
#'
#' @param patternStr NA
#' @param srcStr NA
#'
#' @return NA
#'

.grepAcquValue <- function(patternStr, srcStr) {
  tmpLine <- grep(pattern=patternStr, x=srcStr, value=TRUE)
  gsub(pattern="(^.*= *<?)|(>? *$)", replacement="", x=tmpLine)
}



#' .grepAcquDoubleValue
#'
#' @param patternStr NA
#' @param srcStr NA
#'
#' @return NA
#'

.grepAcquDoubleValue <- function(patternStr, srcStr) {
  strValue <- .grepAcquValue(patternStr, srcStr)

  ## replace comma by dot
  as.double(gsub(",", replacement=".", strValue))
}



#' .sampleName
#'
#' @param fidFile NA
#'
#' @return NA
#'

.sampleName <- function(fidFile) {
  # regular expression for directory separator (on unix: /+, on windows \+)
  # sadly .Platform$file.sep == "/" on both
  fidFile <- chartr(old="\\", new="/", x=fidFile)

  # create array of directories (each element == one directory)
  dirs <- strsplit(x=fidFile, split="/")[[1L]]

  numDirs <- length(dirs)

  sampleName <- NA

  ## old FlexAnalysis seems to have the following directories
  ## 0_L20_1SLin/fid
  ## vs the more recent FlexAnalysis versions use
  ## 0/L20/1SLin/fid
  isShortPath <- isTRUE(numDirs > 2L &&
                          grepl("[0-9]+_[A-z][0-9]+_[0-9][A-z]+$",
                                dirs[numDirs - 1L]))
  if (isShortPath) {
    sampleName <- dirs[numDirs-2L]
  } else if (numDirs > 4L ) {
    sampleName <- dirs[numDirs-4L]

    # -, : or something like that causes errors in names()
    # TODO: use make.names in future releases?
    sampleName <- gsub(pattern="[[:punct:]]|[[:space:]]", replacement="_",
                       x=sampleName)
  }

  sampleName
}





#' from https://github.com/sgibb/readBrukerFlexData
#'
#' @param hpcStr https://github.com/sgibb/readBrukerFlexData
#'
#' @return https://github.com/sgibb/readBrukerFlexData
#' @export
#'
extractHPCConstants <- function(hpcStr) {
  tmpLine <- strsplit(x=hpcStr, split=" ")[[1L]]
  ## remove emtpy elements
  tmpLine <- tmpLine[nzchar(tmpLine)]
  
  ## extract only coefficients
  list(coefficients=as.double(tmpLine[(which(tmpLine == "V1.0VectorDouble") + 2L):
                                        (which(tmpLine == "c2") - 1L)]),
       calibrationConstant0=as.double(tmpLine[which(tmpLine == "c0") + 1L]),
       calibrationConstant2=as.double(tmpLine[which(tmpLine == "c2") + 1L]))
}
