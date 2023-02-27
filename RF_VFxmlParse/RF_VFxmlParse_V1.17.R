####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
## Parsing Forum XML's VERSION 1.6                                                                ##
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

## Version 1.0: Support for 24-2, 30-2 and 60-4
## Directory with the XML files / where the new file will be saved
# XML_DIRECTORY <- file.path("/Users/nicolebrye/Desktop/HGC/Data_Management/RF_VFxmlParse/",
#                            "VO0009ParserTestPt_xml")

XML_DIRECTORY <- file.path("/Users/nicolebrye/Desktop/HGC/Data_Management/RF_VFxmlParse/",
                           "VO0009ParserTestPt_xml")

## Version of R File
R_VERSION <- "v1.17"







####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################


####################################################################################################
## Functions                                                                                      ##
####################################################################################################
options(stringsAsFactors = FALSE)

is_inst <- function(pkg) {
  nzchar(system.file(package = pkg))
}

if (is_inst("XML")) {
  library(XML)
} else {
  install.packages("XML")
  library(XML)
}

extractValue <- function (x, pattern = ">\\s*(.*?)\\s*<", skippr = 1) {
  if (length(x) == 0) {
    temp <- "XXXXXXNOTFOUND"
  } else {
    temp <- regmatches(x, regexec(pattern, x))[[skippr]][2]
  }
  return(temp)
}

jFetch <- function(x, NODE = flatNode, OFFSET = 1, ...) {
  extractValue(NODE[grep(x, NODE) + OFFSET], ...)
}

jDate <- function (x) {
  paste(substr(x, 1, 4), substr(x, 5, 6), substr(x, 7, 8), sep = "-")
}

jTime <- function (x) {
  paste(substr(x, 1, 2), substr(x, 3, 4), substr(x, 5, 6), sep = ":")
}

jTime2 <- function (x) {
  paste(sprintf("%02d", floor(x / 3600)),
        sprintf("%02d", floor(x %% 3600 / 60)),
        sprintf("%02d", floor(x %% 60 )),
        sep = ":")
}

jReplacer <- function(x, old = NULL, new){
  if(is.null(old)) old = names(table(x))
  if(length(old) != length(new)) stop("Length of replacement vector is unequal.")
  kpr <- x
  for(i in 1:length(old)) kpr[x == old[i]] <- new[i]
  return(kpr)
}

dhms <- function(t) {
  paste(t %/% (60 * 60 * 24), 
        paste(formatC(t %/% (60 * 60) %% 24, width = 2, format = "d", flag = "0"),
              formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0"),
              formatC(t %% 60, width = 2, format = "d", flag = "0"), sep = ":"))
}

####################################################################################################
## Processing Files                                                                               ##
####################################################################################################
XML_TIMESTAMP <- paste(strsplit(as.character(Sys.time()), " ")[[1]], collapse = "T")
l604 <- l302 <- l242 <- list()
k604 <- k302 <- k242 <- 1
XMLS <- which(substr(list.files(XML_DIRECTORY), nchar(list.files(XML_DIRECTORY)) - 3, 
                     nchar(list.files(XML_DIRECTORY))) == ".xml")
XMLS <- list.files(XML_DIRECTORY)[XMLS]
NFILES <- length(XMLS)
cat("Processing", NFILES, "XML's...\n")
nn302 <- NULL
for (i in 1:length(XMLS)) {
  xml1     <- xmlParse(file.path(XML_DIRECTORY, XMLS[i]))
  flatNode <- capture.output(xml1)
  TYPE <- jFetch("Performed Protocol Code Sequence", OFFSET = 8)

  if (TYPE == "Visual Field 60-4 Test Pattern") {
    n604 <- c("HGCParserVersion", "XMLConversionTimeStamp", "HFA_XML_VERSION", "LAST_NAME", 
              "GIVEN_NAME", "MIDDLE_NAME", "NAME_PREFIX", "NAME_SUFFIX", "FULL_NAME", "PATIENT_ID", 
              "BIRTH_DATE", "VISIT_DATE", "SERIES_DATE_TIME", "MODALITY", "SITE", "INSTRUMENT_NAME", 
              "MANUFACTURER", "MODEL_NUMBER", "SERIAL_NUMBER", "HFA2_SOFTWARE_VERSION", "DISPLAY_NAME", 
              "CLINICAL_NOTES", "EXAM_TIME", "CD_HORIZONTAL", "CD_VERTICAL", "IOP", "TRIAL_RX_SPHERE", 
              "TRIAL_RX_CYLINDER", "TRIAL_RX_AXIS", "DISTANCE_RX_SPHERE", "DISTANCE_RX_CYLINDER", 
              "DISTANCE_RX_AXIS", "PUPIL_DIAMETER", "PUPIL_DIAMETER_AUTO", "DIAGNOSIS_CODE", 
              "PROCEDURE_CODE", "VA", "VA_STRING", "TEST_TYPE", "IMAGE_TYPE", "IMAGE_FILE_NAME",
              "TEST_PATTERN", "TEST_STRATEGY", "STIMULUS_COLOR", "STIMULUS_SIZE", "BACKGROUND_COLOR", 
              "EXAM_DURATION", "FIXATION_TARGET", "FIXATION_MONITOR", "BLIND_SPOT_X", "BLIND_SPOT_Y", 
              "BLIND_SPOT_STIMULUS_SIZE", "FALSE_NEGATIVE_METHOD", "FALSE_NEGATIVE_PERCENT", 
              "FALSE_POSITIVE_METHOD", "FALSE_POSITIVE_PERCENT", "TRIALS", "ERRORS", "FOVEAL_RESULT", 
              "FOVEAL_THRESHOLD",
              "CENTRAL_REF_LEVEL", "THROWN_OUT_POINTS", "MINIMUM_STIMULUS", "FIELD_SIZE", "LANGUAGE", 
              "QUESTIONS_ASKED", "SF_STATUS", "SF", "NUM_THRESHOLD_POINTS", 
              "N30_S42_Thr", "N18_S42_Thr", "N6_S42_Thr", "T6_S42_Thr", "T18_S42_Thr", "T30_S42_Thr", 
              "N54_S30_Thr", "N42_S30_Thr", "N30_S30_Thr", "N18_S30_Thr", "N6_S30_Thr", "T6_S30_Thr", 
              "T18_S30_Thr", "T30_S30_Thr", "T42_S30_Thr", "T54_S30_Thr", "N54_S18_Thr", "N42_S18_Thr", 
              "N30_S18_Thr", "T30_S18_Thr", "T42_S18_Thr", "T54_S18_Thr", "N54_S6_Thr", "N42_S6_Thr", 
              "N30_S6_Thr", "T30_S6_Thr", "T42_S6_Thr", "T54_S6_Thr", "N54_I6_Thr", "N42_I6_Thr", 
              "N30_I6_Thr", "T30_I6_Thr", "T42_I6_Thr", "T54_I6_Thr", "N54_I18_Thr", "N42_I18_Thr", 
              "N30_I18_Thr", "T30_I18_Thr", "T42_I18_Thr", "T54_I18_Thr", "N42_I30_Thr", "N30_I30_Thr", 
              "N18_I30_Thr", "N6_I30_Thr", "T6_I30_Thr", "T18_I30_Thr", "T30_I30_Thr", "T42_I30_Thr", 
              "N42_I42_Thr", "N30_I42_Thr", "N18_I42_Thr", "N6_I42_Thr", "T6_I42_Thr", "T18_I42_Thr", 
              "T30_I42_Thr", "T42_I42_Thr", "N18_I54_Thr", "N6_I54_Thr", "T6_I54_Thr", "T18_I54_Thr", 
              "STATPAC_STATUS",
              "N30_S42_Defect", "N18_S42_Defect", "N6_S42_Defect", "T6_S42_Defect", "T18_S42_Defect", 
              "T30_S42_Defect", "N54_S30_Defect", "N42_S30_Defect", "N30_S30_Defect", "N18_S30_Defect", 
              "N6_S30_Defect", "T6_S30_Defect", "T18_S30_Defect", "T30_S30_Defect", "T42_S30_Defect", 
              "T54_S30_Defect", "N54_S18_Defect", "N42_S18_Defect", "N30_S18_Defect", "T30_S18_Defect", 
              "T42_S18_Defect", "T54_S18_Defect", "N54_S6_Defect", "N42_S6_Defect", "N30_S6_Defect", 
              "T30_S6_Defect", "T42_S6_Defect", "T54_S6_Defect", "N54_I6_Defect", "N42_I6_Defect", 
              "N30_I6_Defect", "T30_I6_Defect", "T42_I6_Defect", "T54_I6_Defect", "N54_I18_Defect", 
              "N42_I18_Defect", "N30_I18_Defect", "T30_I18_Defect", "T42_I18_Defect", "T54_I18_Defect", 
              "N42_I30_Defect", "N30_I30_Defect", "N18_I30_Defect", "N6_I30_Defect", "T6_I30_Defect", 
              "T18_I30_Defect", "T30_I30_Defect", "T42_I30_Defect", "N42_I42_Defect", "N30_I42_Defect", 
              "N18_I42_Defect", "N6_I42_Defect", "T6_I42_Defect", "T18_I42_Defect", "T30_I42_Defect", 
              "T42_I42_Defect", "N18_I54_Defect", "N6_I54_Defect", "T6_I54_Defect", "T18_I54_Defect", 
              "NUM_DEFECT_DEPTH_POINTS")
    pID   <- jFetch("Patient ID")
    pID2  <- jFetch("Patient’s Name")
    pID2x <- gsub("^", " ", pID2, fixed = TRUE)
    sSer  <- jFetch("Device Serial Number")
    
    ## Threshold Values
    xCr  <- as.numeric(sapply(flatNode[grep("Visual Field Test Point X-Coordinate", flatNode) + 1], 
                              extractValue))
    yCr  <- as.numeric(sapply(flatNode[grep("Visual Field Test Point Y-Coordinate", flatNode) + 1], 
                              extractValue))
    sens <- sapply(flatNode[grep("-Sensitivity Value-", flatNode) + 1], extractValue)
    
    EYE <- jFetch("Laterality")
    if (EYE == "R") {
      llab <- paste(ifelse(xCr < 0, "N", "T"), abs(xCr), "_", 
                    ifelse(yCr < 0, "I", "S"), abs(yCr), "_", "Thr", sep = "")
    }
    
    if (EYE == "L") {
      llab <- paste(ifelse(xCr < 0, "T", "N"), abs(xCr), "_", 
                    ifelse(yCr < 0, "I", "S"), abs(yCr), "_", "Thr", sep = "")
    }
    
    jOrd <- match(n604[70:129], llab)
    sens <- sens[jOrd]
    llab <- llab[jOrd]
    MTYPE <- try(jFetch("Model Name-", skippr = 2), silent = TRUE)
    if (class(MTYPE) == "try-error") {
      MTYPE <- jFetch("Model Name-", skippr = 1)
    }
    
    ## Fixation Monitor Bit
    ff1   <- jFetch("Fixation Monitoring Code Sequence", OFFSET = 8)
    ff2   <- jFetch("Fixation Monitoring Code Sequence", OFFSET = 16)
    testr <- paste(sort(c(ff1, ff2)), collapse = "; ")
    
    if (testr == "Automated Optical; Blind Spot Monitoring") {
      fixfill <- "Gaze/Blind Spot"
    } else {
      fixfill <- ff1
    }
    
    fixfill <- jReplacer(fixfill, c("Automated Optical", "Blind Spot Monitoring"),
                         c("Gaze", "Blind Spot"))
    
    ## Make Line
    linr <- c(
      R_VERSION,
      XML_TIMESTAMP,
      jFetch("czm_xml_version"),
      strsplit(pID2, "^", fixed = TRUE)[[1]][1],
      strsplit(pID2, "^", fixed = TRUE)[[1]][2],
      rep("", 3),
      pID2x,
      pID,
      jFetch("Patient’s Birth Date"),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), 
             jFetch("Performed Procedure Step Start Date-"), 
             jFetch("Referenced SOP Instance UID-")),
      "",
      jFetch("Modality"),
      EYE,
      MTYPE,
      jFetch("Manufacturer-"),
      sSer,
      sSer,
      strsplit(jFetch("Software Version"), "\\\\")[[1]][3],
      rep("", 2),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), 
             jFetch("Performed Procedure Step Start Time-"), ""),
      rep("", 3),
      jFetch("Spherical Lens Power"), 
      jFetch("Cylinder Lens Power"), 
      jFetch("Cylinder Axis"),
      rep("", 3),
      jFetch("Pupil Size"),
      rep("", 5),
      jFetch("00400260", OFFSET = 21),
      rep("", 2),
      gsub(" Test Pattern", "", gsub("Visual Field ", "", TYPE, fixed = TRUE), fixed = TRUE),
      gsub(" Test Strategy", "", gsub("Visual Field ", "", jFetch("Strategy", OFFSET = 0), 
                                      fixed = TRUE), fixed = TRUE),
      jFetch("Stimulus Color Code Sequence", OFFSET = 8),
      jFetch("00240025", OFFSET = 0),
      jFetch("Background Illumination Color Code Sequence", OFFSET = 8),
      jFetch("Visual Field Test Duration"),
      jFetch("fixation_target_type"),
      fixfill,
      jFetch("Blind Spot X-Coordinate"),
      jFetch("Blind Spot Y-Coordinate"),
      rep("", 1),
      jReplacer(jFetch("00240045", OFFSET = 0), c("YES", "NO"), c("1", "0")),
      jFetch("False Negatives Estimate-"),
      jReplacer(jFetch("00240053", OFFSET = 0), c("YES", "NO"), c("1", "0")),
      jFetch("False Positives Estimate-"),
      jFetch("Fixation Checked Quantity"),
      jFetch("Patient Not Properly Fixated Quantity"),
      jFetch("00240086", OFFSET = 0),
      jFetch("00240087", OFFSET = 0),
      jFetch("Screening Baseline Value"),
      rep("", 2),
      jFetch("Visual Field Horizontal Extent"),
      rep("", 2),
      jReplacer(jFetch("00240074", OFFSET = 0), c("YES", "NO"), c("On", "Off")),
      jFetch("00240075", OFFSET = 0),
      "60",
      sens,
      "",
      rep("", length(sens)),
      "60"
    )
    
    l604[[k604]] <- linr
    k604 <- k604 + 1
  }
  
  if (TYPE == "Visual Field 30-2 Test Pattern") {
    n302 <- c("HGCParserVersion", "XMLConversionTimeStamp", "HFA2_SOFTWARE_VERSION", "HFA_XML_VERSION",
              "PATIENT_ID", "FULL_NAME", "GIVEN_NAME", "MIDDLE_NAME", "LAST_NAME", "NAME_PREFIX", 
              "NAME_SUFFIX", "BIRTH_DATE", "VISIT_DATE", "STUDY_UID", "SERIES_DATE_TIME", "MODALITY", 
              "SITE", "INSTRUMENT_NAME", "INSTRUMENT_MANUFACTURER", "INSTRUMENT_MODEL_NUMBER", 
              "INSTRUMENT_SERIAL_NUMBER", "INSTRUMENT_SOFTWARE_VERSION", "DISPLAY_NAME", 
              "CLINICAL_NOTES", "EXAM_TIME", "CD_HORIZONTAL", "CD_VERTICAL", "IOP", "TRIAL_RX_SPHERE", 
              "TRIAL_RX_CYLINDER", "TRIAL_RX_AXIS", "DISTANCE_RX_SPHERE", "DISTANCE_RX_CYLINDER", 
              "DISTANCE_RX_AXIS", "PUPIL_DIAMETER", "PUPIL_DIAMETER_AUTO", "DIAGNOSIS_CODE", 
              "PROCEDURE_CODE", "VA", "VA_STRING", "TEST_TYPE", "IMAGE_TYPE", "IMAGE_FILE_NAME", 
              "TEST_PATTERN", "TEST_STRATEGY", "STIMULUS_COLOR", "STIMULUS_SIZE", "BACKGROUND_COLOR", 
              "EXAM_DURATION", "FIXATION_TARGET", "FIXATION_MONITOR", "BLIND_SPOT_X", "BLIND_SPOT_Y", 
              "BLIND_SPOT_STIMULUS_SIZE", "FOVEAL_RESULT", "FOVEAL_THRESHOLD", "CENTRAL_REF_LEVEL", 
              "THROWN_OUT_POINTS", "MINIMUM_STIMULUS", "FIELD_SIZE", "LANGUAGE", 
              "FALSE_NEGATIVE_METHOD", "FALSE_NEGATIVE_TRIALS", "FALSE_NEGATIVE_ERRORS", 
              "FALSE_NEGATIVE_PERCENT", "FALSE_POSITIVE_METHOD", "FALSE_POSITIVE_TRIALS", 
              "FALSE_POSITIVE_ERRORS", "FALSE_POSITIVE_PERCENT", "FIXATION_CHECK_TRIALS", 
              "FIXATION_CHECK_ERRORS", "QUESTIONS_ASKED", "REFERENCE_TEST_DATE", 
              "REFERENCE_TEST_CODE", "SF_STATUS", "SF", "NUM_THRESHOLD_POINTS", 
              "N9_S27_Thr", "N3_S27_Thr", "T3_S27_Thr", "T9_S27_Thr", "N15_S21_Thr", "N9_S21_Thr", 
              "N3_S21_Thr", "T3_S21_Thr", "T9_S21_Thr", "T15_S21_Thr", "N21_S15_Thr", "N15_S15_Thr", 
              "N9_S15_Thr", "N3_S15_Thr", "T3_S15_Thr", "T9_S15_Thr", "T15_S15_Thr", "T21_S15_Thr", 
              "N27_S9_Thr", "N21_S9_Thr", "N15_S9_Thr", "N9_S9_Thr", "N3_S9_Thr", "T3_S9_Thr", 
              "T9_S9_Thr", "T15_S9_Thr", "T21_S9_Thr", "T27_S9_Thr", "N27_S3_Thr", "N21_S3_Thr", 
              "N15_S3_Thr", "N9_S3_Thr", "N3_S3_Thr", "T3_S3_Thr", "T9_S3_Thr", "T15_S3_Thr", 
              "T21_S3_Thr", "T27_S3_Thr", "N27_I3_Thr", "N21_I3_Thr", "N15_I3_Thr", "N9_I3_Thr", 
              "N3_I3_Thr", "T3_I3_Thr", "T9_I3_Thr", "T15_I3_Thr", "T21_I3_Thr", "T27_I3_Thr", 
              "N27_I9_Thr", "N21_I9_Thr", "N15_I9_Thr", "N9_I9_Thr", "N3_I9_Thr", "T3_I9_Thr", 
              "T9_I9_Thr", "T15_I9_Thr", "T21_I9_Thr", "T27_I9_Thr", "N21_I15_Thr", "N15_I15_Thr", 
              "N9_I15_Thr", "N3_I15_Thr", "T3_I15_Thr", "T9_I15_Thr", "T15_I15_Thr", "T21_I15_Thr", 
              "N15_I21_Thr", "N9_I21_Thr", "N3_I21_Thr", "T3_I21_Thr", "T9_I21_Thr", "T15_I21_Thr", 
              "N9_I27_Thr", "N3_I27_Thr", "T3_I27_Thr", "T9_I27_Thr", 
              "STATPAC_STATUS", "LOW_PATIENT_RELIABILITY_STATUS", "GHT", "MD", "MD_PROBABILITY", 
              "PSD", "PSD_PROBABILITY", "CPSD", "CPSD_PROBABILITY", "SF_PROBABILITY", "VFI", 
              "FOVEAL_THRESHOLD_PROBABILITY", "NUM_TOTAL_DEV_VALUE_POINTS",
              "N9_S27_TD", "N3_S27_TD", "T3_S27_TD", "T9_S27_TD", "N15_S21_TD", "N9_S21_TD", 
              "N3_S21_TD", "T3_S21_TD", "T9_S21_TD", "T15_S21_TD", "N21_S15_TD", "N15_S15_TD", 
              "N9_S15_TD", "N3_S15_TD", "T3_S15_TD", "T9_S15_TD", "T15_S15_TD", "T21_S15_TD", 
              "N27_S9_TD", "N21_S9_TD", "N15_S9_TD", "N9_S9_TD", "N3_S9_TD", "T3_S9_TD", 
              "T9_S9_TD", "T15_S9_TD", "T21_S9_TD", "T27_S9_TD", "N27_S3_TD", "N21_S3_TD", 
              "N15_S3_TD", "N9_S3_TD", "N3_S3_TD", "T3_S3_TD", "T9_S3_TD", "T15_S3_TD", 
              "T21_S3_TD", "T27_S3_TD", "N27_I3_TD", "N21_I3_TD", "N15_I3_TD", "N9_I3_TD", 
              "N3_I3_TD", "T3_I3_TD", "T9_I3_TD", "T15_I3_TD", "T21_I3_TD", "T27_I3_TD", 
              "N27_I9_TD", "N21_I9_TD", "N15_I9_TD", "N9_I9_TD", "N3_I9_TD", "T3_I9_TD", 
              "T9_I9_TD", "T15_I9_TD", "T21_I9_TD", "T27_I9_TD", "N21_I15_TD", "N15_I15_TD", 
              "N9_I15_TD", "N3_I15_TD", "T3_I15_TD", "T9_I15_TD", "T15_I15_TD", "T21_I15_TD", 
              "N15_I21_TD", "N9_I21_TD", "N3_I21_TD", "T3_I21_TD", "T9_I21_TD", "T15_I21_TD", 
              "N9_I27_TD", "N3_I27_TD", "T3_I27_TD", "T9_I27_TD",
              "NUM_PATTERN_DEV_VALUE_POINTS",
              "N9_S27_PD", "N3_S27_PD", "T3_S27_PD", "T9_S27_PD", "N15_S21_PD", "N9_S21_PD", 
              "N3_S21_PD", "T3_S21_PD", "T9_S21_PD", "T15_S21_PD", "N21_S15_PD", "N15_S15_PD", 
              "N9_S15_PD", "N3_S15_PD", "T3_S15_PD", "T9_S15_PD", "T15_S15_PD", "T21_S15_PD", 
              "N27_S9_PD", "N21_S9_PD", "N15_S9_PD", "N9_S9_PD", "N3_S9_PD", "T3_S9_PD", 
              "T9_S9_PD", "T15_S9_PD", "T21_S9_PD", "T27_S9_PD", "N27_S3_PD", "N21_S3_PD", 
              "N15_S3_PD", "N9_S3_PD", "N3_S3_PD", "T3_S3_PD", "T9_S3_PD", "T15_S3_PD", 
              "T21_S3_PD", "T27_S3_PD", "N27_I3_PD", "N21_I3_PD", "N15_I3_PD", "N9_I3_PD", 
              "N3_I3_PD", "T3_I3_PD", "T9_I3_PD", "T15_I3_PD", "T21_I3_PD", "T27_I3_PD", 
              "N27_I9_PD", "N21_I9_PD", "N15_I9_PD", "N9_I9_PD", "N3_I9_PD", "T3_I9_PD", 
              "T9_I9_PD", "T15_I9_PD", "T21_I9_PD", "T27_I9_PD", "N21_I15_PD", "N15_I15_PD", 
              "N9_I15_PD", "N3_I15_PD", "T3_I15_PD", "T9_I15_PD", "T15_I15_PD", "T21_I15_PD", 
              "N15_I21_PD", "N9_I21_PD", "N3_I21_PD", "T3_I21_PD", "T9_I21_PD", "T15_I21_PD", 
              "N9_I27_PD", "N3_I27_PD", "T3_I27_PD", "T9_I27_PD",
              "NUM_TOTAL_DEV_PROB_POINTS",
              "N9_S27_TDP", "N3_S27_TDP", "T3_S27_TDP", "T9_S27_TDP", "N15_S21_TDP", "N9_S21_TDP", 
              "N3_S21_TDP", "T3_S21_TDP", "T9_S21_TDP", "T15_S21_TDP", "N21_S15_TDP", "N15_S15_TDP", 
              "N9_S15_TDP", "N3_S15_TDP", "T3_S15_TDP", "T9_S15_TDP", "T15_S15_TDP", "T21_S15_TDP", 
              "N27_S9_TDP", "N21_S9_TDP", "N15_S9_TDP", "N9_S9_TDP", "N3_S9_TDP", "T3_S9_TDP", 
              "T9_S9_TDP", "T15_S9_TDP", "T21_S9_TDP", "T27_S9_TDP", "N27_S3_TDP", "N21_S3_TDP", 
              "N15_S3_TDP", "N9_S3_TDP", "N3_S3_TDP", "T3_S3_TDP", "T9_S3_TDP", "T15_S3_TDP", 
              "T21_S3_TDP", "T27_S3_TDP", "N27_I3_TDP", "N21_I3_TDP", "N15_I3_TDP", "N9_I3_TDP", 
              "N3_I3_TDP", "T3_I3_TDP", "T9_I3_TDP", "T15_I3_TDP", "T21_I3_TDP", "T27_I3_TDP", 
              "N27_I9_TDP", "N21_I9_TDP", "N15_I9_TDP", "N9_I9_TDP", "N3_I9_TDP", "T3_I9_TDP", 
              "T9_I9_TDP", "T15_I9_TDP", "T21_I9_TDP", "T27_I9_TDP", "N21_I15_TDP", "N15_I15_TDP", 
              "N9_I15_TDP", "N3_I15_TDP", "T3_I15_TDP", "T9_I15_TDP", "T15_I15_TDP", "T21_I15_TDP", 
              "N15_I21_TDP", "N9_I21_TDP", "N3_I21_TDP", "T3_I21_TDP", "T9_I21_TDP", "T15_I21_TDP", 
              "N9_I27_TDP", "N3_I27_TDP", "T3_I27_TDP", "T9_I27_TDP",
              "NUM_PATTERN_DEV_PROB_POINTS",
              "N9_S27_PDP", "N3_S27_PDP", "T3_S27_PDP", "T9_S27_PDP", "N15_S21_PDP", "N9_S21_PDP", 
              "N3_S21_PDP", "T3_S21_PDP", "T9_S21_PDP", "T15_S21_PDP", "N21_S15_PDP", "N15_S15_PDP", 
              "N9_S15_PDP", "N3_S15_PDP", "T3_S15_PDP", "T9_S15_PDP", "T15_S15_PDP", "T21_S15_PDP", 
              "N27_S9_PDP", "N21_S9_PDP", "N15_S9_PDP", "N9_S9_PDP", "N3_S9_PDP", "T3_S9_PDP", 
              "T9_S9_PDP", "T15_S9_PDP", "T21_S9_PDP", "T27_S9_PDP", "N27_S3_PDP", "N21_S3_PDP", 
              "N15_S3_PDP", "N9_S3_PDP", "N3_S3_PDP", "T3_S3_PDP", "T9_S3_PDP", "T15_S3_PDP", 
              "T21_S3_PDP", "T27_S3_PDP", "N27_I3_PDP", "N21_I3_PDP", "N15_I3_PDP", "N9_I3_PDP", 
              "N3_I3_PDP", "T3_I3_PDP", "T9_I3_PDP", "T15_I3_PDP", "T21_I3_PDP", "T27_I3_PDP", 
              "N27_I9_PDP", "N21_I9_PDP", "N15_I9_PDP", "N9_I9_PDP", "N3_I9_PDP", "T3_I9_PDP", 
              "T9_I9_PDP", "T15_I9_PDP", "T21_I9_PDP", "T27_I9_PDP", "N21_I15_PDP", "N15_I15_PDP", 
              "N9_I15_PDP", "N3_I15_PDP", "T3_I15_PDP", "T9_I15_PDP", "T15_I15_PDP", "T21_I15_PDP", 
              "N15_I21_PDP", "N9_I21_PDP", "N3_I21_PDP", "T3_I21_PDP", "T9_I21_PDP", "T15_I21_PDP", 
              "N9_I27_PDP", "N3_I27_PDP", "T3_I27_PDP", "T9_I27_PDP")
    nn302 <- c(nn302, XMLS[i])
    pID   <- jFetch("Patient ID")
    pID2  <- jFetch("Patient’s Name")
    pID2x <- gsub("^", " ", pID2, fixed = TRUE)
    sSer  <- jFetch("Device Serial Number")
    
    ## Series Values
    xCr  <- as.numeric(sapply(flatNode[grep("Visual Field Test Point X-Coordinate", flatNode) + 1], 
                              extractValue))
    yCr  <- as.numeric(sapply(flatNode[grep("Visual Field Test Point Y-Coordinate", flatNode) + 1], 
                              extractValue))
    sens <- sapply(flatNode[grep("-Sensitivity Value-", flatNode) + 1], extractValue)
    tds  <- sapply(flatNode[grep("Age Corrected Sensitivity Deviation Value", flatNode) + 1], 
                   extractValue)
    pds  <- sapply(flatNode[grep("Generalized Defect Corrected Sensitivity Deviation Value", 
                                 flatNode) + 1], 
                   extractValue)
    tdp  <- sapply(flatNode[grep("Age Corrected Sensitivity Deviation Probability Value", 
                                 flatNode) + 1], 
                   extractValue)
    pdp  <- sapply(flatNode[grep("Generalized Defect Corrected Sensitivity Deviation Probability Value", 
                                 flatNode) + 1], 
                   extractValue)
    
    EYE <- jFetch("Laterality")
    if (EYE == "R") {
      llab <- paste(ifelse(xCr < 0, "N", "T"), abs(xCr), "_", 
                    ifelse(yCr < 0, "I", "S"), abs(yCr), "_", "Thr", sep = "")
    }
    
    if (EYE == "L") {
      llab <- paste(ifelse(xCr < 0, "T", "N"), abs(xCr), "_", 
                    ifelse(yCr < 0, "I", "S"), abs(yCr), "_", "Thr", sep = "")
    }
    
    jOrd <- match(n302[78:153], llab)
    sens <- sens[jOrd]
    tds  <- tds[jOrd]
    pds  <- pds[jOrd]
    tdp  <- tdp[jOrd]
    pdp  <- pdp[jOrd]
    llab <- llab[jOrd]
    
    MTYPE <- try(jFetch("Model Name-", skippr = 2), silent = TRUE)
    if (class(MTYPE) == "try-error") {
      MTYPE <- jFetch("Model Name-", skippr = 1)
    }
    
    ## Fixation Monitor Bit
    ff1   <- jFetch("Fixation Monitoring Code Sequence", OFFSET = 8)
    ff2   <- jFetch("Fixation Monitoring Code Sequence", OFFSET = 16)
    testr <- paste(sort(c(ff1, ff2)), collapse = "; ")
    
    if (testr == "Automated Optical; Blind Spot Monitoring") {
      fixfill <- "Gaze/Blind Spot"
    } else {
      fixfill <- ff1
    }
    
    fixfill <- jReplacer(fixfill, c("Automated Optical", "Blind Spot Monitoring"),
                         c("Gaze", "Blind Spot"))
    
    ## Make Line
    linr <- c(
      R_VERSION,
      XML_TIMESTAMP,
      strsplit(jFetch("Software Version"), "\\\\")[[1]][3],
      jFetch("czm_xml_version"),
      #extractValue(flatNode[1], pattern = "n=\\\"\\s*(.*?)\\s*\\\""),
      pID, ## Patient_ID
      pID2x, ## Full Name
      strsplit(pID2, "^", fixed = TRUE)[[1]][2],    ## "given" name
      "",    ## Middle
      strsplit(pID2, "^", fixed = TRUE)[[1]][1],    ## Last_Name
      rep("", 2),
      jFetch("Patient’s Birth Date"),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), jFetch("Performed Procedure Step Start Date-"), 
             jFetch("Referenced SOP Instance UID-")),
      rep("", 2),
      jFetch("Modality"),
      EYE,
      MTYPE,
      jFetch("Manufacturer-"),
      sSer,
      sSer,
      jFetch("Software Version"),
      rep("", 2),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), 
             jFetch("Performed Procedure Step Start Time-"), ""),
      rep("", 3),
      jFetch("Spherical Lens Power"), 
      jFetch("Cylinder Lens Power"), 
      jFetch("Cylinder Axis"),
      rep("", 3),
      jFetch("Pupil Size"),
      rep("", 5),
      jFetch("00400260", OFFSET = 21),
      rep("", 2),
      gsub(" Test Pattern", "", gsub("Visual Field ", "", TYPE, fixed = TRUE), fixed = TRUE),
      gsub(" Test Strategy", "", gsub("Visual Field ", "", jFetch("Strategy", OFFSET = 0), 
                                      fixed = TRUE), fixed = TRUE),
      jFetch("Stimulus Color Code Sequence", OFFSET = 8),
      jFetch("00240025", OFFSET = 0),
      jFetch("Background Illumination Color Code Sequence", OFFSET = 8),
      jFetch("Visual Field Test Duration"),
      jFetch("fixation_target_type"),
      fixfill,
      jFetch("Blind Spot X-Coordinate"),
      jFetch("Blind Spot Y-Coordinate"),
      rep("", 1),
      jFetch("00240086", OFFSET = 0),
      jFetch("00240087", OFFSET = 0),
      jFetch("Screening Baseline Value"),
      rep("", 2),
      jFetch("Visual Field Horizontal Extent"),
      rep("", 1),
      jReplacer(jFetch("00240045", OFFSET = 0), c("YES", "NO"), c("1", "0")),
      "", ## jFetch("Negative Catch Trials Quantity"),
      jFetch("False Negatives Quantity"),
      jFetch("False Negatives Estimate-"),
      jReplacer(jFetch("00240053", OFFSET = 0), c("YES", "NO"), c("1", "0")),
      jFetch("Positive Catch Trials Quantity"),
      jFetch("False Positives Quantity"),
      jFetch("False Positives Estimate-"),
      jFetch("Fixation Checked Quantity"),
      jFetch("Patient Not Properly Fixated Quantity"),
      rep("", 3),
      jReplacer(jFetch("00240074", OFFSET = 0), c("YES", "NO"), c("On", "Off")),
      jFetch("00240075", OFFSET = 0),
      length(sens),
      sens,
      rep("", 1),
      jFetch("Patient Reliability Indicator-"),
      jFetch("Glaucoma Hemifield Test Analysis", OFFSET = 11),
      jFetch("Global Deviation From Normal"),
      jFetch("Global Deviation Probability-"),
      jFetch("Localized Deviation from Normal"),
      jFetch("Localized Deviation Probability-"),
      rep("", 3),
      jFetch("Visual Field Index", OFFSET = 4),
      jFetch("00240118", OFFSET = 0),
      74,
      tds,
      74,
      pds,
      74,
      tdp,
      74,
      pdp
    )
    
    l302[[k302]] <- linr
    k302 <- k302 + 1
  }
  
  if (TYPE == "Visual Field 24-2 Test Pattern") {
    n242 <- c("HGCParserVersion", "XMLConversionTimeStamp", "HFA2_SOFTWARE_VERSION", "HFA_XML_VERSION",
              "PATIENT_ID", "FULL_NAME", "GIVEN_NAME", "MIDDLE_NAME", "LAST_NAME", "NAME_PREFIX",
              "NAME_SUFFIX", "BIRTH_DATE", "VISIT_DATE", "STUDY_UID", "SERIES_DATE_TIME", "MODALITY",
              "SITE", "INSTRUMENT_NAME", "INSTRUMENT_MANUFACTURER", "INSTRUMENT_MODEL_NUMBER",
              "INSTRUMENT_SERIAL_NUMBER", "INSTRUMENT_SOFTWARE_VERSION", "DISPLAY_NAME",
              "CLINICAL_NOTES", "EXAM_TIME", "CD_HORIZONTAL", "CD_VERTICAL", "IOP", "TRIAL_RX_SPHERE",
              "TRIAL_RX_CYLINDER", "TRIAL_RX_AXIS", "DISTANCE_RX_SPHERE", "DISTANCE_RX_CYLINDER",
              "DISTANCE_RX_AXIS", "PUPIL_DIAMETER", "PUPIL_DIAMETER_AUTO", "DIAGNOSIS_CODE",
              "PROCEDURE_CODE", "VA", "VA_STRING", "TEST_TYPE", "IMAGE_TYPE", "IMAGE_FILE_NAME",
              "TEST_PATTERN", "TEST_STRATEGY", "STIMULUS_COLOR", "STIMULUS_SIZE", "BACKGROUND_COLOR",
              "EXAM_DURATION", "FIXATION_TARGET", "FIXATION_MONITOR", "BLIND_SPOT_X", "BLIND_SPOT_Y",
              "BLIND_SPOT_STIMULUS_SIZE", "FOVEAL_RESULT", "FOVEAL_THRESHOLD", "CENTRAL_REF_LEVEL",
              "THROWN_OUT_POINTS", "MINIMUM_STIMULUS", "FIELD_SIZE", "LANGUAGE",
              "FALSE_NEGATIVE_METHOD", "FALSE_NEGATIVE_TRIALS", "FALSE_NEGATIVE_ERRORS",
              "FALSE_NEGATIVE_PERCENT", "FALSE_POSITIVE_METHOD", "FALSE_POSITIVE_TRIALS",
              "FALSE_POSITIVE_ERRORS", "FALSE_POSITIVE_PERCENT", "FIXATION_CHECK_TRIALS",
              "FIXATION_CHECK_ERRORS", "QUESTIONS_ASKED", "REFERENCE_TEST_DATE",
              "REFERENCE_TEST_CODE", "SF_STATUS", "SF", "NUM_THRESHOLD_POINTS",
              "N9_S27_Thr", "N3_S27_Thr", "T3_S27_Thr", "T9_S27_Thr", "N15_S21_Thr", "N9_S21_Thr",
              "N3_S21_Thr", "T3_S21_Thr", "T9_S21_Thr", "T15_S21_Thr", "N21_S15_Thr", "N15_S15_Thr",
              "N9_S15_Thr", "N3_S15_Thr", "T3_S15_Thr", "T9_S15_Thr", "T15_S15_Thr", "T21_S15_Thr",
              "N27_S9_Thr", "N21_S9_Thr", "N15_S9_Thr", "N9_S9_Thr", "N3_S9_Thr", "T3_S9_Thr",
              "T9_S9_Thr", "T15_S9_Thr", "T21_S9_Thr", "T27_S9_Thr", "N27_S3_Thr", "N21_S3_Thr",
              "N15_S3_Thr", "N9_S3_Thr", "N3_S3_Thr", "T3_S3_Thr", "T9_S3_Thr", "T15_S3_Thr",
              "T21_S3_Thr", "T27_S3_Thr", "N27_I3_Thr", "N21_I3_Thr", "N15_I3_Thr", "N9_I3_Thr",
              "N3_I3_Thr", "T3_I3_Thr", "T9_I3_Thr", "T15_I3_Thr", "T21_I3_Thr", "T27_I3_Thr",
              "N27_I9_Thr", "N21_I9_Thr", "N15_I9_Thr", "N9_I9_Thr", "N3_I9_Thr", "T3_I9_Thr",
              "T9_I9_Thr", "T15_I9_Thr", "T21_I9_Thr", "T27_I9_Thr", "N21_I15_Thr", "N15_I15_Thr",
              "N9_I15_Thr", "N3_I15_Thr", "T3_I15_Thr", "T9_I15_Thr", "T15_I15_Thr", "T21_I15_Thr",
              "N15_I21_Thr", "N9_I21_Thr", "N3_I21_Thr", "T3_I21_Thr", "T9_I21_Thr", "T15_I21_Thr",
              "N9_I27_Thr", "N3_I27_Thr", "T3_I27_Thr", "T9_I27_Thr",
              "STATPAC_STATUS", "LOW_PATIENT_RELIABILITY_STATUS", "GHT", "MD", "MD_PROBABILITY",
              "PSD", "PSD_PROBABILITY", "CPSD", "CPSD_PROBABILITY", "SF_PROBABILITY", "VFI",
              "FOVEAL_THRESHOLD_PROBABILITY", "NUM_TOTAL_DEV_VALUE_POINTS",
              "N9_S27_TD", "N3_S27_TD", "T3_S27_TD", "T9_S27_TD", "N15_S21_TD", "N9_S21_TD",
              "N3_S21_TD", "T3_S21_TD", "T9_S21_TD", "T15_S21_TD", "N21_S15_TD", "N15_S15_TD",
              "N9_S15_TD", "N3_S15_TD", "T3_S15_TD", "T9_S15_TD", "T15_S15_TD", "T21_S15_TD",
              "N27_S9_TD", "N21_S9_TD", "N15_S9_TD", "N9_S9_TD", "N3_S9_TD", "T3_S9_TD",
              "T9_S9_TD", "T15_S9_TD", "T21_S9_TD", "T27_S9_TD", "N27_S3_TD", "N21_S3_TD",
              "N15_S3_TD", "N9_S3_TD", "N3_S3_TD", "T3_S3_TD", "T9_S3_TD", "T15_S3_TD",
              "T21_S3_TD", "T27_S3_TD", "N27_I3_TD", "N21_I3_TD", "N15_I3_TD", "N9_I3_TD",
              "N3_I3_TD", "T3_I3_TD", "T9_I3_TD", "T15_I3_TD", "T21_I3_TD", "T27_I3_TD",
              "N27_I9_TD", "N21_I9_TD", "N15_I9_TD", "N9_I9_TD", "N3_I9_TD", "T3_I9_TD",
              "T9_I9_TD", "T15_I9_TD", "T21_I9_TD", "T27_I9_TD", "N21_I15_TD", "N15_I15_TD",
              "N9_I15_TD", "N3_I15_TD", "T3_I15_TD", "T9_I15_TD", "T15_I15_TD", "T21_I15_TD",
              "N15_I21_TD", "N9_I21_TD", "N3_I21_TD", "T3_I21_TD", "T9_I21_TD", "T15_I21_TD",
              "N9_I27_TD", "N3_I27_TD", "T3_I27_TD", "T9_I27_TD",
              "NUM_PATTERN_DEV_VALUE_POINTS",
              "N9_S27_PD", "N3_S27_PD", "T3_S27_PD", "T9_S27_PD", "N15_S21_PD", "N9_S21_PD",
              "N3_S21_PD", "T3_S21_PD", "T9_S21_PD", "T15_S21_PD", "N21_S15_PD", "N15_S15_PD",
              "N9_S15_PD", "N3_S15_PD", "T3_S15_PD", "T9_S15_PD", "T15_S15_PD", "T21_S15_PD",
              "N27_S9_PD", "N21_S9_PD", "N15_S9_PD", "N9_S9_PD", "N3_S9_PD", "T3_S9_PD",
              "T9_S9_PD", "T15_S9_PD", "T21_S9_PD", "T27_S9_PD", "N27_S3_PD", "N21_S3_PD",
              "N15_S3_PD", "N9_S3_PD", "N3_S3_PD", "T3_S3_PD", "T9_S3_PD", "T15_S3_PD",
              "T21_S3_PD", "T27_S3_PD", "N27_I3_PD", "N21_I3_PD", "N15_I3_PD", "N9_I3_PD",
              "N3_I3_PD", "T3_I3_PD", "T9_I3_PD", "T15_I3_PD", "T21_I3_PD", "T27_I3_PD",
              "N27_I9_PD", "N21_I9_PD", "N15_I9_PD", "N9_I9_PD", "N3_I9_PD", "T3_I9_PD",
              "T9_I9_PD", "T15_I9_PD", "T21_I9_PD", "T27_I9_PD", "N21_I15_PD", "N15_I15_PD",
              "N9_I15_PD", "N3_I15_PD", "T3_I15_PD", "T9_I15_PD", "T15_I15_PD", "T21_I15_PD",
              "N15_I21_PD", "N9_I21_PD", "N3_I21_PD", "T3_I21_PD", "T9_I21_PD", "T15_I21_PD",
              "N9_I27_PD", "N3_I27_PD", "T3_I27_PD", "T9_I27_PD",
              "NUM_TOTAL_DEV_PROB_POINTS",
              "N9_S27_TDP", "N3_S27_TDP", "T3_S27_TDP", "T9_S27_TDP", "N15_S21_TDP", "N9_S21_TDP",
              "N3_S21_TDP", "T3_S21_TDP", "T9_S21_TDP", "T15_S21_TDP", "N21_S15_TDP", "N15_S15_TDP",
              "N9_S15_TDP", "N3_S15_TDP", "T3_S15_TDP", "T9_S15_TDP", "T15_S15_TDP", "T21_S15_TDP",
              "N27_S9_TDP", "N21_S9_TDP", "N15_S9_TDP", "N9_S9_TDP", "N3_S9_TDP", "T3_S9_TDP",
              "T9_S9_TDP", "T15_S9_TDP", "T21_S9_TDP", "T27_S9_TDP", "N27_S3_TDP", "N21_S3_TDP",
              "N15_S3_TDP", "N9_S3_TDP", "N3_S3_TDP", "T3_S3_TDP", "T9_S3_TDP", "T15_S3_TDP",
              "T21_S3_TDP", "T27_S3_TDP", "N27_I3_TDP", "N21_I3_TDP", "N15_I3_TDP", "N9_I3_TDP",
              "N3_I3_TDP", "T3_I3_TDP", "T9_I3_TDP", "T15_I3_TDP", "T21_I3_TDP", "T27_I3_TDP",
              "N27_I9_TDP", "N21_I9_TDP", "N15_I9_TDP", "N9_I9_TDP", "N3_I9_TDP", "T3_I9_TDP",
              "T9_I9_TDP", "T15_I9_TDP", "T21_I9_TDP", "T27_I9_TDP", "N21_I15_TDP", "N15_I15_TDP",
              "N9_I15_TDP", "N3_I15_TDP", "T3_I15_TDP", "T9_I15_TDP", "T15_I15_TDP", "T21_I15_TDP",
              "N15_I21_TDP", "N9_I21_TDP", "N3_I21_TDP", "T3_I21_TDP", "T9_I21_TDP", "T15_I21_TDP",
              "N9_I27_TDP", "N3_I27_TDP", "T3_I27_TDP", "T9_I27_TDP",
              "NUM_PATTERN_DEV_PROB_POINTS",
              "N9_S27_PDP", "N3_S27_PDP", "T3_S27_PDP", "T9_S27_PDP", "N15_S21_PDP", "N9_S21_PDP",
              "N3_S21_PDP", "T3_S21_PDP", "T9_S21_PDP", "T15_S21_PDP", "N21_S15_PDP", "N15_S15_PDP",
              "N9_S15_PDP", "N3_S15_PDP", "T3_S15_PDP", "T9_S15_PDP", "T15_S15_PDP", "T21_S15_PDP",
              "N27_S9_PDP", "N21_S9_PDP", "N15_S9_PDP", "N9_S9_PDP", "N3_S9_PDP", "T3_S9_PDP",
              "T9_S9_PDP", "T15_S9_PDP", "T21_S9_PDP", "T27_S9_PDP", "N27_S3_PDP", "N21_S3_PDP",
              "N15_S3_PDP", "N9_S3_PDP", "N3_S3_PDP", "T3_S3_PDP", "T9_S3_PDP", "T15_S3_PDP",
              "T21_S3_PDP", "T27_S3_PDP", "N27_I3_PDP", "N21_I3_PDP", "N15_I3_PDP", "N9_I3_PDP",
              "N3_I3_PDP", "T3_I3_PDP", "T9_I3_PDP", "T15_I3_PDP", "T21_I3_PDP", "T27_I3_PDP",
              "N27_I9_PDP", "N21_I9_PDP", "N15_I9_PDP", "N9_I9_PDP", "N3_I9_PDP", "T3_I9_PDP",
              "T9_I9_PDP", "T15_I9_PDP", "T21_I9_PDP", "T27_I9_PDP", "N21_I15_PDP", "N15_I15_PDP",
              "N9_I15_PDP", "N3_I15_PDP", "T3_I15_PDP", "T9_I15_PDP", "T15_I15_PDP", "T21_I15_PDP",
              "N15_I21_PDP", "N9_I21_PDP", "N3_I21_PDP", "T3_I21_PDP", "T9_I21_PDP", "T15_I21_PDP",
              "N9_I27_PDP", "N3_I27_PDP", "T3_I27_PDP", "T9_I27_PDP",
              "BACKUP_TIME", "BACKUP_DATE")
    pID   <- jFetch("Patient ID")
    sSer  <- jFetch("Device Serial Number")

    ## Series Values
    xCr  <- as.numeric(sapply(flatNode[grep("Visual Field Test Point X-Coordinate", flatNode) + 1],
                              extractValue))
    yCr  <- as.numeric(sapply(flatNode[grep("Visual Field Test Point Y-Coordinate", flatNode) + 1],
                              extractValue))
    sens <- sapply(flatNode[grep("-Sensitivity Value-", flatNode) + 1], extractValue)
    tds  <- sapply(flatNode[grep("Age Corrected Sensitivity Deviation Value", flatNode) + 1],
                   extractValue)
    pds  <- sapply(flatNode[grep("Generalized Defect Corrected Sensitivity Deviation Value",
                                 flatNode) + 1],
                   extractValue)
    tdp  <- sapply(flatNode[grep("Age Corrected Sensitivity Deviation Probability Value",
                                 flatNode) + 1],
                   extractValue)
    pdp  <- sapply(flatNode[grep("Generalized Defect Corrected Sensitivity Deviation Probability Value",
                                 flatNode) + 1],
                   extractValue)

    EYE <- jFetch("Laterality")
    if (EYE == "R") {
      llab <- paste(ifelse(xCr < 0, "N", "T"), abs(xCr), "_",
                    ifelse(yCr < 0, "I", "S"), abs(yCr), "_", "Thr", sep = "")
    }

    if (EYE == "L") {
      llab <- paste(ifelse(xCr < 0, "T", "N"), abs(xCr), "_",
                    ifelse(yCr < 0, "I", "S"), abs(yCr), "_", "Thr", sep = "")
    }

    jOrd <- match(n242[78:153], llab)
    sens <- sens[jOrd]
    tds  <- tds[jOrd]
    pds  <- pds[jOrd]
    tdp  <- tdp[jOrd]
    pdp  <- pdp[jOrd]
    llab <- llab[jOrd]

    MTYPE <- jFetch("Model Name-", skippr = 2)
    BACKUP_DATE <- jFetch("Study Date-")
    BACKUP_TIME <- jFetch("Study Time-")
    
    ## Make Line
    linr <- c(
      R_VERSION,
      XML_TIMESTAMP,
      jFetch("Software Version"),
      jFetch("czm_xml_version"),
      pID,
      pID,
      rep("", 2),
      pID,
      rep("", 2),
      jFetch("Patient’s Birth Date"),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), 
             jFetch("Performed Procedure Step Start Date-"),
             jFetch("Referenced SOP Instance UID-")),
      rep("", 2),
      jFetch("Modality"),
      EYE,
      MTYPE,
      jFetch("Manufacturer-"),
      sSer,
      sSer,
      jFetch("Software Version"),
      rep("", 2),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), 
             jFetch("Performed Procedure Step Start Time-"), ""),
      rep("", 9),
      jFetch("Pupil Size"),
      rep("", 8),
      TYPE,
      jFetch("Strategy", OFFSET = 0),
      jFetch("Stimulus Color Code Sequence", OFFSET = 8),
      "",
      jFetch("Background Illumination Color Code Sequence", OFFSET = 8),
      jFetch("Visual Field Test Duration"),
      jFetch("fixation_target_type"),
      "",
      jFetch("Blind Spot X-Coordinate"),
      jFetch("Blind Spot Y-Coordinate"),
      rep("", 3),
      jFetch("Screening Baseline Value"),
      rep("", 2),
      jFetch("Visual Field Horizontal Extent"),
      rep("", 4),
      jFetch("False Negatives Quantity-"),
      rep("", 3),
      jFetch("False Positives Quantity-"),
      jFetch("Fixation Checked Quantity"),
      jFetch("Patient Not Properly Fixated Quantity"),
      rep("", 5),
      length(sens[!is.na(sens)]),
      sens,
      rep("", 2),
      jFetch("Glaucoma Hemifield Test Analysis", OFFSET = 11),
      jFetch("Global Deviation From Normal"),
      jFetch("Global Deviation Probability-"),
      jFetch("Localized Deviation from Normal"),
      jFetch("Localized Deviation Probability-"),
      rep("", 3),
      jFetch("Visual Field Index", OFFSET = 4),
      "",
      52,
      tds,
      52,
      pds,
      52,
      tdp,
      52,
      pdp,
      BACKUP_TIME,
      BACKUP_DATE
    )
    linr <- as.character(linr)

    l242[[k242]] <- linr
    k242 <- k242 + 1
  }
  
  TOT <- k604 + k302 + k242
  if (TOT %% 100 == 0) {
    cat(TOT, "files processed...\n")
  }
}
cat("...done!\n")

## Post-process 60-4
if (length(l604) > 0) {
  o604 <- do.call("rbind", l604)
  o604 <- matrix(sapply(o604, function(x) ifelse(is.null(x), "", x)), nrow = nrow(o604))
  o604 <- data.frame(o604)
  for (j in 1:ncol(o604)) {
    if (class(o604[, j]) == "list") {
      o604[, j] <- unlist(o604[, j])
    }
  }
  colnames(o604) <- n604
  o604$BIRTH_DATE <- jDate(o604$BIRTH_DATE)
  
  ## Date and time for HFA2
  o604$EXAM_TIME[o604$INSTRUMENT_NAME == "HFA II-i"] <- 
    jTime(unlist(lapply(strsplit(o604$VISIT_DATE[o604$INSTRUMENT_NAME == "HFA II-i"], ".", 
                                 fixed = TRUE), 
                        function (x) substr(x[11], 9, 14))))
  o604$VISIT_DATE[o604$INSTRUMENT_NAME == "HFA II-i"] <- 
    jDate(unlist(lapply(strsplit(o604$VISIT_DATE[o604$INSTRUMENT_NAME == "HFA II-i"], ".", 
                                 fixed = TRUE), 
                        function (x) substr(x[11], 1, 8))))
  
  ## Date and time for HFA3
  o604$EXAM_TIME[o604$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")] <- 
    jTime(o604$EXAM_TIME[o604$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")])
  o604$VISIT_DATE[o604$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")] <- 
    jDate(o604$VISIT_DATE[o604$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")])
  
  o604$SERIES_DATE_TIME <- paste(o604$VISIT_DATE, o604$EXAM_TIME, sep = "T")
  
  o604$EXAM_DURATION <- gsub("0 00:", "", dhms(as.numeric(o604$EXAM_DURATION)), fixed = TRUE)
  o604$SITE <- jReplacer(o604$SITE, c("R", "L"), c("OD", "OS"))
  o604$MODEL_NUMBER  <- unlist(lapply(strsplit(o604$MODEL_NUMBER, "-"), function (x) x[1]))
  o604$SERIAL_NUMBER <- unlist(lapply(strsplit(o604$SERIAL_NUMBER, "-"), function (x) x[2]))
  o604 <- o604[, -which(names(o604) == "LANGUAGE")]
  
  o604$TEST_TYPE     <- jReplacer(o604$TEST_TYPE, "Diagnostic", "Threshold")
  o604$TEST_STRATEGY <- jReplacer(o604$TEST_STRATEGY, "SITA-Fast", "SITA Fast")
  o604$STIMULUS_SIZE <- jReplacer(o604$STIMULUS_SIZE, "0.14748032", "III")
  
  o604[o604 == "XXXXXXNOTFOUND"] <- ""
  o604$TEST_PATTERN <- jReplacer(o604$TEST_PATTERN, "60-4", "P60-2")
  write.csv(o604, file.path(XML_DIRECTORY, "test60-4.csv"), row.names = FALSE,
            na = "")
  cat("...60-4 file written (", nrow(o604), " files).\n", sep = "")
}

## Post-process 30-2
if (length(l302) > 0) {
  o302 <- do.call("rbind", l302)
  o302 <- matrix(sapply(o302, function(x) ifelse(is.null(x), "", x)), nrow = nrow(o302))
  o302 <- data.frame(o302)
  for (j in 1:ncol(o302)) {
    if (class(o302[, j]) == "list") {
      o302[, j] <- unlist(o302[, j])
    }
  }
  
  colnames(o302) <- n302
  o302$BIRTH_DATE <- jDate(o302$BIRTH_DATE)
  
  ## Date and time for HFA2
  o302$EXAM_TIME[o302$INSTRUMENT_NAME == "HFA II-i"] <- 
    jTime(unlist(lapply(strsplit(o302$VISIT_DATE[o302$INSTRUMENT_NAME == "HFA II-i"], ".", 
                                        fixed = TRUE), 
                        function (x) substr(x[11], 9, 14))))
  o302$VISIT_DATE[o302$INSTRUMENT_NAME == "HFA II-i"] <- 
    jDate(unlist(lapply(strsplit(o302$VISIT_DATE[o302$INSTRUMENT_NAME == "HFA II-i"], ".", 
                                 fixed = TRUE), 
                        function (x) substr(x[11], 1, 8))))
  
  ## Date and time for HFA3
  o302$EXAM_TIME[o302$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")] <- 
    jTime(o302$EXAM_TIME[o302$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")])
  o302$VISIT_DATE[o302$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")] <- 
    jDate(o302$VISIT_DATE[o302$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")])
  
  o302$SERIES_DATE_TIME <- paste(o302$VISIT_DATE, o302$EXAM_TIME, sep = "T")
  o302$SITE <- jReplacer(o302$SITE, c("R", "L"), c("OD", "OS"))
  o302$INSTRUMENT_MODEL_NUMBER  <- unlist(lapply(strsplit(o302$INSTRUMENT_MODEL_NUMBER, "-"), 
                                                 function (x) x[1]))
  o302$INSTRUMENT_SERIAL_NUMBER <- unlist(lapply(strsplit(o302$INSTRUMENT_SERIAL_NUMBER, "-"), 
                                                 function (x) x[2]))
  
  ## PDP / TDP Fix
  o302[, c(grep("TDP", names(o302)), grep("PDP", names(o302)))] <-
    jReplacer(o302[, c(grep("TDP", names(o302)), grep("PDP", names(o302)))],
              c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
              c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  o302[, "MD_PROBABILITY"] <- jReplacer(o302[, "MD_PROBABILITY"],
                                        c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                        c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  
  ## "Blind Spot" Fix
  o302[, c("T15_S3_TD", "T15_I3_TD", "T15_S3_TDP", "T15_I3_TDP",
           "T15_S3_PD", "T15_I3_PD", "T15_S3_PDP", "T15_I3_PDP")] <- ""
  
  ## Other Fixes
  #o302$DISPLAY_NAME  <- "SF-30-2 Thr"
  o302$TEST_TYPE     <- jReplacer(o302$TEST_TYPE, "Diagnostic", "Threshold")
  #o302$TEST_PATTERN  <- "C30-2"
  o302$TEST_STRATEGY <- jReplacer(o302$TEST_STRATEGY, "SITA-Fast", "SITA Fast")
  o302$STIMULUS_SIZE <- jReplacer(o302$STIMULUS_SIZE, "0.14748032", "III")
  
  o302$FOVEAL_THRESHOLD_PROBABILITY <- jReplacer(o302$FOVEAL_THRESHOLD_PROBABILITY,
                                                 c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                                 c("Not Significant", "< 0.5%", "< 1%", "< 2%", 
                                                   "< 5%", "< 10%"))
  o302$PSD_PROBABILITY <- jReplacer(o302$PSD_PROBABILITY,
                                    c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                    c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  #o302$LOW_PATIENT_RELIABILITY_STATUS <- jReplacer(o302$LOW_PATIENT_RELIABILITY_STATUS,
  #                                                 c("*** Low Test Reliability ***", ""),
  #                                                 c("Unreliable", "Reliable"))
  o302$EXAM_DURATION <- gsub("0 00:", "", dhms(as.numeric(o302$EXAM_DURATION)), fixed = TRUE)
  
  o302[o302 == "XXXXXXNOTFOUND"] <- ""
  o302$TEST_PATTERN <- jReplacer(o302$TEST_PATTERN, "30-2", "C30-2")
  
  ## Writing out part 1
  write.csv(o302, file.path(XML_DIRECTORY, "test30-2.csv"), row.names = FALSE,
            na = "")
  cat("...30-2 file written (", nrow(o302), " files).\n", sep = "")
  
  ## "Calculated" version - WIP as no fields are actually filled in
  o302$XMLSourceFileName <- nn302
  cNames302 <- c('HFA2_SOFTWARE_VERSION', 'HFA_XML_VERSION', 'HGCParserVersion', 'XMLSourceFileName', 
                 'XMLConversionTimeStamp', 'PATIENT_ID', 'FULL_NAME', 'GIVEN_NAME', 'MIDDLE_NAME', 
                 'LAST_NAME', 'NAME_PREFIX', 'NAME_SUFFIX', 'BIRTH_DATE', 'VISIT_DATE', 'STUDY_UID', 
                 'SERIES_DATE_TIME', 'MODALITY', 'SITE', 'INSTRUMENT_NAME', 'INSTRUMENT_MANUFACTURER', 
                 'INSTRUMENT_MODEL_NUMBER', 'INSTRUMENT_SERIAL_NUMBER', 'INSTRUMENT_SOFTWARE_VERSION',
                 'DISPLAY_NAME', 'CLINICAL_NOTES', 'EXAM_TIME', 'CD_HORIZONTAL', 'CD_VERTICAL', 'IOP', 
                 'TRIAL_RX_SPHERE', 'TRIAL_RX_CYLINDER', 'TRIAL_RX_AXIS', 'DISTANCE_RX_SPHERE', 
                 'DISTANCE_RX_CYLINDER', 'DISTANCE_RX_AXIS', 'PUPIL_DIAMETER', 'PUPIL_DIAMETER_AUTO', 
                 'DIAGNOSIS_CODE', 'PROCEDURE_CODE', 'VA', 'VA_STRING', 'TEST_TYPE', 'IMAGE_TYPE', 
                 'IMAGE_FILE_NAME', 'TEST_PATTERN', 'TEST_STRATEGY', 'STIMULUS_COLOR', 
                 'STIMULUS_SIZE', 'BACKGROUND_COLOR', 'EXAM_DURATION', 'FIXATION_TARGET', 
                 'FIXATION_MONITOR', 'BLIND_SPOT_X', 'BLIND_SPOT_Y', 'BLIND_SPOT_STIMULUS_SIZE', 
                 'FOVEAL_RESULT', 'FOVEAL_THRESHOLD', 'CENTRAL_REF_LEVEL', 'THROWN_OUT_POINTS', 
                 'MINIMUM_STIMULUS', 'FIELD_SIZE', 'LANGUAGE', 'FALSE_NEGATIVE_TRIALS', 
                 'FALSE_NEGATIVE_ERRORS', 'FALSE_NEGATIVE_PERCENT', 'FALSE_POSITIVE_TRIALS', 
                 'FALSE_POSITIVE_ERRORS', 'FALSE_POSITIVE_PERCENT', 'FIXATION_CHECK_TRIALS', 
                 'FIXATION_CHECK_ERRORS', 'QUESTIONS_ASKED', 'REFERENCE_TEST_DATE', 
                 'REFERENCE_TEST_CODE', 'SF_STATUS', 'SF', 'NUM_THRESHOLD_POINTS', 
                 'N9_S27_Thr', 'N3_S27_Thr', 'T3_S27_Thr', 'T9_S27_Thr', 'N15_S21_Thr', 'N9_S21_Thr', 
                 'N3_S21_Thr', 'T3_S21_Thr', 'T9_S21_Thr', 'T15_S21_Thr', 'N21_S15_Thr', 
                 'N15_S15_Thr', 'N9_S15_Thr', 'N3_S15_Thr', 'T3_S15_Thr', 'T9_S15_Thr', 'T15_S15_Thr', 
                 'T21_S15_Thr', 'N27_S9_Thr', 'N21_S9_Thr', 'N15_S9_Thr', 'N9_S9_Thr', 'N3_S9_Thr', 
                 'T3_S9_Thr', 'T9_S9_Thr', 'T15_S9_Thr', 'T21_S9_Thr', 'T27_S9_Thr', 'N27_S3_Thr', 
                 'N21_S3_Thr', 'N15_S3_Thr', 'N9_S3_Thr', 'N3_S3_Thr', 'T3_S3_Thr', 'T9_S3_Thr', 
                 'T15_S3_Thr', 'T21_S3_Thr', 'T27_S3_Thr', 'N27_I3_Thr', 'N21_I3_Thr', 'N15_I3_Thr', 
                 'N9_I3_Thr', 'N3_I3_Thr', 'T3_I3_Thr', 'T9_I3_Thr', 'T15_I3_Thr', 'T21_I3_Thr', 
                 'T27_I3_Thr', 'N27_I9_Thr', 'N21_I9_Thr', 'N15_I9_Thr', 'N9_I9_Thr', 'N3_I9_Thr', 
                 'T3_I9_Thr', 'T9_I9_Thr', 'T15_I9_Thr', 'T21_I9_Thr', 'T27_I9_Thr', 'N21_I15_Thr', 
                 'N15_I15_Thr', 'N9_I15_Thr', 'N3_I15_Thr', 'T3_I15_Thr', 'T9_I15_Thr', 'T15_I15_Thr', 
                 'T21_I15_Thr', 'N15_I21_Thr', 'N9_I21_Thr', 'N3_I21_Thr', 'T3_I21_Thr', 'T9_I21_Thr', 
                 'T15_I21_Thr', 'N9_I27_Thr', 'N3_I27_Thr', 'T3_I27_Thr', 'T9_I27_Thr', 
                 'STATPAC_STATUS', 'LOW_PATIENT_RELIABILITY_STATUS', 'SWAPFT_GENERAL_HEIGHT', 'GHT', 
                 'MD', 'MD_PROBABILITY', 'PSD', 'PSD_PROBABILITY', 'CPSD', 'CPSD_PROBABILITY', 
                 'SF_PROBABILITY', 'VFI', 'FOVEAL_THRESHOLD_PROBABILITY', 'FLAG_ASSESSMENT', 
                 'FLAG_SEVERITY', 'NUM_TOTAL_DEV_VALUE_POINTS', 'N9_S27_TD', 'N3_S27_TD', 'T3_S27_TD', 
                 'T9_S27_TD', 'N15_S21_TD', 'N9_S21_TD', 'N3_S21_TD', 'T3_S21_TD', 'T9_S21_TD', 
                 'T15_S21_TD', 'N21_S15_TD', 'N15_S15_TD', 'N9_S15_TD', 'N3_S15_TD', 'T3_S15_TD', 
                 'T9_S15_TD', 'T15_S15_TD', 'T21_S15_TD', 'N27_S9_TD', 'N21_S9_TD', 'N15_S9_TD', 
                 'N9_S9_TD', 'N3_S9_TD', 'T3_S9_TD', 'T9_S9_TD', 'T15_S9_TD', 'T21_S9_TD', 
                 'T27_S9_TD', 'N27_S3_TD', 'N21_S3_TD', 'N15_S3_TD', 'N9_S3_TD', 'N3_S3_TD', 
                 'T3_S3_TD', 'T9_S3_TD', 'T15_S3_TD', 'T21_S3_TD', 'T27_S3_TD', 'N27_I3_TD', 
                 'N21_I3_TD', 'N15_I3_TD', 'N9_I3_TD', 'N3_I3_TD', 'T3_I3_TD', 'T9_I3_TD', 
                 'T15_I3_TD', 'T21_I3_TD', 'T27_I3_TD', 'N27_I9_TD', 'N21_I9_TD', 'N15_I9_TD', 
                 'N9_I9_TD', 'N3_I9_TD', 'T3_I9_TD', 'T9_I9_TD', 'T15_I9_TD', 'T21_I9_TD', 
                 'T27_I9_TD', 'N21_I15_TD', 'N15_I15_TD', 'N9_I15_TD', 'N3_I15_TD', 'T3_I15_TD', 
                 'T9_I15_TD', 'T15_I15_TD', 'T21_I15_TD', 'N15_I21_TD', 'N9_I21_TD', 'N3_I21_TD', 
                 'T3_I21_TD', 'T9_I21_TD', 'T15_I21_TD', 'N9_I27_TD', 'N3_I27_TD', 'T3_I27_TD', 
                 'T9_I27_TD', 'NUM_PATTERN_DEV_VALUE_POINTS', 'N9_S27_PD', 'N3_S27_PD', 'T3_S27_PD', 
                 'T9_S27_PD', 'N15_S21_PD', 'N9_S21_PD', 'N3_S21_PD', 'T3_S21_PD', 'T9_S21_PD', 
                 'T15_S21_PD', 'N21_S15_PD', 'N15_S15_PD', 'N9_S15_PD', 'N3_S15_PD', 'T3_S15_PD', 
                 'T9_S15_PD', 'T15_S15_PD', 'T21_S15_PD', 'N27_S9_PD', 'N21_S9_PD', 'N15_S9_PD', 
                 'N9_S9_PD', 'N3_S9_PD', 'T3_S9_PD', 'T9_S9_PD', 'T15_S9_PD', 'T21_S9_PD', 
                 'T27_S9_PD', 'N27_S3_PD', 'N21_S3_PD', 'N15_S3_PD', 'N9_S3_PD', 'N3_S3_PD', 
                 'T3_S3_PD', 'T9_S3_PD', 'T15_S3_PD', 'T21_S3_PD', 'T27_S3_PD', 'N27_I3_PD', 
                 'N21_I3_PD', 'N15_I3_PD', 'N9_I3_PD', 'N3_I3_PD', 'T3_I3_PD', 'T9_I3_PD', 
                 'T15_I3_PD', 'T21_I3_PD', 'T27_I3_PD', 'N27_I9_PD', 'N21_I9_PD', 'N15_I9_PD', 
                 'N9_I9_PD', 'N3_I9_PD', 'T3_I9_PD', 'T9_I9_PD', 'T15_I9_PD', 'T21_I9_PD', 
                 'T27_I9_PD', 'N21_I15_PD', 'N15_I15_PD', 'N9_I15_PD', 'N3_I15_PD', 'T3_I15_PD', 
                 'T9_I15_PD', 'T15_I15_PD', 'T21_I15_PD', 'N15_I21_PD', 'N9_I21_PD', 'N3_I21_PD', 
                 'T3_I21_PD', 'T9_I21_PD', 'T15_I21_PD', 'N9_I27_PD', 'N3_I27_PD', 'T3_I27_PD', 
                 'T9_I27_PD', 'NUM_TOTAL_DEV_PROB_POINTS', 'N9_S27_TDP', 'N3_S27_TDP', 'T3_S27_TDP', 
                 'T9_S27_TDP', 'N15_S21_TDP', 'N9_S21_TDP', 'N3_S21_TDP', 'T3_S21_TDP', 'T9_S21_TDP', 
                 'T15_S21_TDP', 'N21_S15_TDP', 'N15_S15_TDP', 'N9_S15_TDP', 'N3_S15_TDP', 'T3_S15_TDP', 
                 'T9_S15_TDP', 'T15_S15_TDP', 'T21_S15_TDP', 'N27_S9_TDP', 'N21_S9_TDP', 'N15_S9_TDP', 
                 'N9_S9_TDP', 'N3_S9_TDP', 'T3_S9_TDP', 'T9_S9_TDP', 'T15_S9_TDP', 'T21_S9_TDP', 
                 'T27_S9_TDP', 'N27_S3_TDP', 'N21_S3_TDP', 'N15_S3_TDP', 'N9_S3_TDP', 'N3_S3_TDP', 
                 'T3_S3_TDP', 'T9_S3_TDP', 'T15_S3_TDP', 'T21_S3_TDP', 'T27_S3_TDP', 'N27_I3_TDP', 
                 'N21_I3_TDP', 'N15_I3_TDP', 'N9_I3_TDP', 'N3_I3_TDP', 'T3_I3_TDP', 'T9_I3_TDP', 
                 'T15_I3_TDP', 'T21_I3_TDP', 'T27_I3_TDP', 'N27_I9_TDP', 'N21_I9_TDP', 'N15_I9_TDP', 
                 'N9_I9_TDP', 'N3_I9_TDP', 'T3_I9_TDP', 'T9_I9_TDP', 'T15_I9_TDP', 'T21_I9_TDP', 
                 'T27_I9_TDP', 'N21_I15_TDP', 'N15_I15_TDP', 'N9_I15_TDP', 'N3_I15_TDP', 'T3_I15_TDP', 
                 'T9_I15_TDP', 'T15_I15_TDP', 'T21_I15_TDP', 'N15_I21_TDP', 'N9_I21_TDP', 'N3_I21_TDP', 
                 'T3_I21_TDP', 'T9_I21_TDP', 'T15_I21_TDP', 'N9_I27_TDP', 'N3_I27_TDP', 'T3_I27_TDP', 
                 'T9_I27_TDP', 'NUM_PATTERN_DEV_PROB_POINTS', 'N9_S27_PDP', 'N3_S27_PDP', 'T3_S27_PDP', 
                 'T9_S27_PDP', 'N15_S21_PDP', 'N9_S21_PDP', 'N3_S21_PDP', 'T3_S21_PDP', 'T9_S21_PDP', 
                 'T15_S21_PDP', 'N21_S15_PDP', 'N15_S15_PDP', 'N9_S15_PDP', 'N3_S15_PDP', 'T3_S15_PDP', 
                 'T9_S15_PDP', 'T15_S15_PDP', 'T21_S15_PDP', 'N27_S9_PDP', 'N21_S9_PDP', 'N15_S9_PDP', 
                 'N9_S9_PDP', 'N3_S9_PDP', 'T3_S9_PDP', 'T9_S9_PDP', 'T15_S9_PDP', 'T21_S9_PDP', 
                 'T27_S9_PDP', 'N27_S3_PDP', 'N21_S3_PDP', 'N15_S3_PDP', 'N9_S3_PDP', 'N3_S3_PDP', 
                 'T3_S3_PDP', 'T9_S3_PDP', 'T15_S3_PDP', 'T21_S3_PDP', 'T27_S3_PDP', 'N27_I3_PDP', 
                 'N21_I3_PDP', 'N15_I3_PDP', 'N9_I3_PDP', 'N3_I3_PDP', 'T3_I3_PDP', 'T9_I3_PDP', 
                 'T15_I3_PDP', 'T21_I3_PDP', 'T27_I3_PDP', 'N27_I9_PDP', 'N21_I9_PDP', 'N15_I9_PDP', 
                 'N9_I9_PDP', 'N3_I9_PDP', 'T3_I9_PDP', 'T9_I9_PDP', 'T15_I9_PDP', 'T21_I9_PDP', 
                 'T27_I9_PDP', 'N21_I15_PDP', 'N15_I15_PDP', 'N9_I15_PDP', 'N3_I15_PDP', 'T3_I15_PDP', 
                 'T9_I15_PDP', 'T15_I15_PDP', 'T21_I15_PDP', 'N15_I21_PDP', 'N9_I21_PDP', 'N3_I21_PDP', 
                 'T3_I21_PDP', 'T9_I21_PDP', 'T15_I21_PDP', 'N9_I27_PDP', 'N3_I27_PDP', 'T3_I27_PDP', 
                 'T9_I27_PDP', 'SUPERIOR_SCORE', 'INFERIOR_SCORE', 'NASAL_SCORE', 'AGIS_SCORE', 
                 'GHSupThrSum', 'GHSupThrMean', 'GHSupThrStd', 'GHSupTDSum', 'GHSupTDMean', 
                 'GHSupTDStd', 'GHSupPDSum', 'GHSupPDMean', 'GHSupPDStd', 'GHSupPDCntLT50p', 
                 'GHSupPDCntLT10p', 'GHSupNasThrSum', 'GHSupNasThrMean', 'GHSupNasThrStd', 
                 'GHSupNasTDSum', 'GHSupNasTDMean', 'GHSupNasTDStd', 'GHSupNasPDSum', 
                 'GHSupNasPDMean', 'GHSupNasPDStd', 'GHSupNasPDCntLT50p', 'GHSupNasPDCntLT10p', 
                 'GHInfThrSum', 'GHInfThrMean', 'GHInfThrStd', 'GHInfTDSum', 'GHInfTDMean', 
                 'GHInfTDStd', 'GHInfPDSum', 'GHInfPDMean', 'GHInfPDStd', 'GHInfPDCntLT50p', 
                 'GHInfPDCntLT10p', 'GHInfNasThrSum', 'GHInfNasThrMean', 'GHInfNasThrStd', 
                 'GHInfNasTDSum', 'GHInfNasTDMean', 'GHInfNasTDStd', 'GHInfNasPDSum', 'GHInfNasPDMean', 
                 'GHInfNasPDStd', 'GHInfNasPDCntLT50p', 'GHInfNasPDCntLT10p', 'GHCentralThrSum', 
                 'GHCentralThrMean', 'GHCentralThrStd', 'GHCentralTDSum', 'GHCentralTDMean', 
                 'GHCentralTDStd', 'GHCentralPDSum', 'GHCentralPDMean', 'GHCentralPDStd', 
                 'GHCentralPDCntLT50p', 'GHCentralPDCntLT10p', 'GHTemporalThrSum', 'GHTemporalThrMean', 
                 'GHTemporalThrStd', 'GHTemporalTDSum', 'GHTemporalTDMean', 'GHTemporalTDStd', 
                 'GHTemporalPDSum', 'GHTemporalPDMean', 'GHTemporalPDStd', 'GHTemporalPDCntLT50p', 
                 'GHTemporalPDCntLT1', 'GHTemporalPDCntLT10p')
  o302 <- o302[, which(names(o302) %in% cNames302)]
  for (j in cNames302[!cNames302 %in% names(o302)]) {
    o302[, j] <- ""
  }
  o302 <- o302[, cNames302]
  
  write.csv(o302, file.path(XML_DIRECTORY, "test30-2_FMP.csv"), row.names = FALSE,
            na = "")
}

## Post-process 24-2
if (length(l242) > 0) {
  o242 <- do.call("rbind", l242)
  o242 <- matrix(sapply(o242, function(x) ifelse(is.null(x), "", x)), nrow = nrow(o242))
  o242 <- data.frame(o242)
  for (j in 1:ncol(o242)) {
    if (class(o242[, j]) == "list") {
      o242[, j] <- unlist(o242[, j])
    }
  }
  colnames(o242) <- n242
  o242$BIRTH_DATE <- jDate(o242$BIRTH_DATE)

  ## Date and time for HFA2
  o242$EXAM_TIME[unlist(o242$INSTRUMENT_NAME) == "HFA II-i"] <-
    jTime(unlist(lapply(strsplit(o242$VISIT_DATE[unlist(o242$INSTRUMENT_NAME) == "HFA II-i"], ".",
                                 fixed = TRUE),
                        function (x) substr(x[11], 9, 14))))
  o242$VISIT_DATE[o242$INSTRUMENT_NAME == "HFA II-i"] <-
    jDate(unlist(lapply(strsplit(o242$VISIT_DATE[o242$INSTRUMENT_NAME == "HFA II-i"], ".",
                                 fixed = TRUE),
                        function (x) substr(x[11], 1, 8))))

  ## Date and time for HFA3
  o242$EXAM_TIME[o242$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")] <-
    jTime(o242$EXAM_TIME[o242$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")])
  o242$VISIT_DATE[o242$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")] <-
    jDate(o242$VISIT_DATE[o242$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")])

  o242$SITE <- jReplacer(o242$SITE, c("R", "L"), c("OD", "OS"))
  o242$INSTRUMENT_MODEL_NUMBER  <- unlist(lapply(strsplit(o242$INSTRUMENT_MODEL_NUMBER, "-"),
                                                 function (x) x[1]))
  o242$INSTRUMENT_SERIAL_NUMBER <- unlist(lapply(strsplit(o242$INSTRUMENT_SERIAL_NUMBER, "-"),
                                                 function (x) x[2]))

  ## PDP / TDP Fix
  o242[, c(grep("TDP", names(o242)), grep("PDP", names(o242)))] <-
    jReplacer(o242[, c(grep("TDP", names(o242)), grep("PDP", names(o242)))],
              c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
              c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  o242[, "MD_PROBABILITY"] <- jReplacer(o242[, "MD_PROBABILITY"],
                                        c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                        c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  o242[, "PSD_PROBABILITY"] <- jReplacer(o242[, "PSD_PROBABILITY"],
                                         c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                         c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  ## "Blind Spot" Fix
  o242[, c("T15_S3_TD", "T15_I3_TD", "T15_S3_TDP", "T15_I3_TDP",
           "T15_S3_PD", "T15_I3_PD", "T15_S3_PDP", "T15_I3_PDP")] <- ""

  ## Bandaid for Date / Time
  if (any(o242$VISIT_DATE == "NA-NA-NA")) {
    o242[which(o242$VISIT_DATE == "NA-NA-NA"), c("VISIT_DATE", "EXAM_TIME")] <-
      cbind(jDate(o242$BACKUP_DATE[which(o242$VISIT_DATE == "NA-NA-NA")]),
            jTime(o242$BACKUP_TIME[which(o242$VISIT_DATE == "NA-NA-NA")]))
  }
  
  o242$SERIES_DATE_TIME <- paste(o242$VISIT_DATE, o242$EXAM_TIME, sep = "T")
  o242 <- o242[, -which(names(o242) %in% c("BACKUP_TIME", "BACKUP_DATE"))]
  
  o242[o242 == "XXXXXXNOTFOUND"] <- ""
  
  write.csv(o242, file.path(XML_DIRECTORY, "test24-2.csv"), row.names = FALSE,
            na = "")
  cat("...24-2 file written (", nrow(o242), " files).\n", sep = "")
}

kprs   <- which(c("o604", "o302", "o242") %in% ls())
checkr <- 0
for (i in kprs) {
  checkr <- checkr + nrow(get(c("o604", "o302", "o242")[i]))
}

if (checkr < NFILES) {
  cat(NFILES - checkr, "unsupported XML's not processed.")
}
