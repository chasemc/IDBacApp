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
  metaData <- list()

  ## endianness
  isBigEndian <- as.integer(.grepAcquValue("##\\$BYTORDA=", acquLines)) == 1L
  metaData$byteOrder <- ifelse(isBigEndian, "big", "little")

  ## obligate
  metaData$number <- as.double(.grepAcquValue("##\\$TD=", acquLines))
  metaData$timeDelay <- .grepAcquDoubleValue("##\\$DELAY=", acquLines)
  metaData$timeDelta <- .grepAcquDoubleValue("##\\$DW=", acquLines)
  metaData$calibrationConstants <-
    c(c1=.grepAcquDoubleValue("##\\$ML1=", acquLines),
      c2=.grepAcquDoubleValue("##\\$ML2=", acquLines),
      c3=.grepAcquDoubleValue("##\\$ML3=", acquLines))

  ## obligate HPC
  metaData$hpcLimits <-
    c(min_mass=.grepAcquDoubleValue("##\\$HPClBLo=", acquLines),
      max_mass=.grepAcquDoubleValue("##\\$HPClBHi=", acquLines))
  metaData$hpcOrder <- as.double(.grepAcquValue("##\\$HPClOrd=", acquLines))
  metaData$hpcUse <-
    as.logical(.grepAcquValue("##\\$HPClUse=", acquLines) == "yes")

  ## was HPC involved?  metaData$hpcUse seems to be always true
  isHPCused <- isTRUE(metaData$hpcUse &&
                        metaData$hpcLimits["max_mass"] > 0L &&
                        metaData$hpcLimits["min_mass"] > 0L &&
                        metaData$hpcOrder > 0L)

  if (isHPCused) {
    hpcStr <- .grepAcquValue("##\\$HPCStr=", acquLines)
    hpcConstants <- IDBacApp::extractHPCConstants(hpcStr)
    metaData$hpcCoefficients <- hpcConstants$coefficients
    metaData$hpcCalibrationConstant0 <- hpcConstants$calibrationConstant0
    metaData$hpcCalibrationConstant2 <- hpcConstants$calibrationConstant2
  }

  # https://github.com/sgibb/MALDIquantForeign/issues/19
  metaData$v1tofCalibration <-
    grepl("V1.0CTOF2CalibrationConstants",
          .grepAcquValue("##\\$NTBCal=", acquLines))

  ## obligate LIFT
  metaData$lift <- c(.grepAcquDoubleValue("##\\$Lift1=", acquLines),
                     .grepAcquDoubleValue("##\\$Lift2=", acquLines))
  metaData$tlift <- .grepAcquDoubleValue("##\\$TLift=", acquLines)

  ## optional
  metaData$dataType <- .grepAcquValue("##DATATYPE=", acquLines)
  metaData$dataSystem <- .grepAcquValue("##SPECTROMETER/DATASYSTEM=", acquLines)
  metaData$spectrometerType <-
    .grepAcquValue("##.SPECTROMETER TYPE=", acquLines)
  metaData$inlet <- .grepAcquValue("##.INLET=", acquLines)
  metaData$ionizationMode <- .grepAcquValue("##.IONIZATION MODE=", acquLines)
  metaData$date <- .grepAcquValue("##\\$DATE=", acquLines)


  metaData$acquisitionMethod <- .grepAcquValue("##\\$ACQMETH=", acquLines)
  metaData$acquisitionDate <- .grepAcquValue("##\\$AQ_DATE=", acquLines)
  aq_mod <- .grepAcquValue("##\\$AQ_mod=", acquLines)
  if (length(aq_mod)) {
    metaData$acquisitionMode <- switch(aq_mod,
                                       "0" = { "qf" },
                                       "1" = { "qsim" },
                                       "2" = { "qseq" },
                                       { aq_mod }
    )
  }

  aqop <- .grepAcquValue("##\\$AQOP_m=", acquLines)
  if (length(aqop)) {
    metaData$tofMode  <- switch(aqop,
                                "0" = { "LINEAR" },
                                "1" = { "REFLECTOR" },
                                { aqop }
    )
  }

  metaData$acquisitionOperatorMode <- metaData$tofMode

  metaData$laserAttenuation <- .grepAcquDoubleValue("##\\$ATTEN=", acquLines)

  metaData$comments <- .grepAcquValue("##\\$CMT.*=", acquLines)

  metaData$deflection <-
    as.logical(.grepAcquValue("##\\$DEFLON=", acquLines) == "yes")

  digtyp  <- .grepAcquValue("##\\$DIGTYP=", acquLines)
  if (length(digtyp)) {
    metaData$digitizerType <- switch(digtyp,
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

  metaData$deflectionPulserCal1 <-
    .grepAcquDoubleValue("##\\$DPCAL1=", acquLines)
  metaData$deflectionPulserMass <-
    .grepAcquDoubleValue("##\\$DPMASS=", acquLines)
  metaData$flexControlVersion <- .grepAcquValue("##\\$FCVer=", acquLines)
  metaData$id <- .grepAcquValue("##\\$ID_raw=", acquLines)

  metaData$instrument <- .grepAcquValue("##\\$INSTRUM=", acquLines)
  metaData$instrumentId <- .grepAcquValue("##\\$InstrID=", acquLines)

  instrumentType <- .grepAcquValue("##\\$InstTyp=", acquLines)
  if (length(instrumentType)) {
    metaData$instrumentType <- switch(instrumentType,
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
                                      { instrumentType }
    )
  }

  metaData$massError <- .grepAcquDoubleValue("##\\$Masserr=", acquLines)

  metaData$laserShots <- as.double(.grepAcquValue("##\\$NoSHOTS=", acquLines))

  if (length(metaData$laserShots) == 0L) {
    warning("File ", sQuote(fidFile), " seems to be empty because ",
            "no laser shots applied to this sample.")
  }

  metaData$patch <- .grepAcquValue("##\\$PATCHNO=", acquLines)

  ## imaging data
  if (length(metaData$patch) &&
      grepl(pattern="(R[0-9]+)?X[0-9]+Y[0-9]+", x=metaData$patch,
            ignore.case=TRUE)) {
    rx <- gregexpr(pattern="[XY][0-9]+", text=metaData$patch)[[1L]]
    pos <- substring(metaData$patch, rx+1L, rx+attr(rx, "match.length")-1L)

    if (length(pos) == 2L) {
      pos <- as.double(pos)
      metaData$imaging <- list(pos=c(x=pos[1L], y=pos[2L]))
    }
  }

  metaData$path <- .grepAcquValue("##\\$PATH=", acquLines)
  metaData$laserRepetition <- .grepAcquDoubleValue("##\\$REPHZ=", acquLines)
  metaData$spot <- .grepAcquValue("##\\$SPOTNO=", acquLines)

  sptype <- .grepAcquValue("##\\$SPType=", acquLines)
  if (length(sptype)) {
    metaData$spectrumType <- switch(sptype,
                                    "0" = { "TOF" },
                                    "1" = { "PSD" },
                                    "2" = { "LIFT" },
                                    "3" = { "PSDSegment" },
                                    { sptype }
    )
  }

  metaData$targetCount <- as.double(.grepAcquValue("##\\$TgCount", acquLines))
  metaData$targetIdString <- .grepAcquValue("##\\$TgIDS", acquLines)
  metaData$targetSerialNumber <- .grepAcquValue("##\\$TgSer", acquLines)
  metaData$targetTypeNumber <- .grepAcquValue("##\\$TgTyp", acquLines)

  metaData$file <- fidFile

  metaData$sampleName <- .sampleName(fidFile)
  metaData$fullName <- paste(metaData$sampleName, metaData$patch, sep=".")
  metaData$name <- metaData$fullName

  metaData
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
