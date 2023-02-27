####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
## Parsing Forum XML's VERSION 1.21                                                              ##
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

## Version 1.0: Support for 10-2, 24-2, 24-2C, 30-2 and 60-4
## Directory with the XML files / where the new file will be saved
XML_DIRECTORY <- file.path("/Users/nicolebrye/Desktop/HGC/Data_Management/RF_VFxmlParse/",
                            "VO0024")

## Version of R File
R_VERSION <- "v1.22"







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

search_tag <- function(x, XML = xml1) {
  path <- paste("//attr[@tag='", x, "']", sep = "")
  ns   <- getNodeSet(XML, path)
  
  info <- "XXXXXXNOTFOUND"
  if (length(ns) > 0) {
    info <- xmlValue(ns[[1]])
  }
  return(info)
}

jFetch <- function(x, XML = xml1, skippr = 1, all = FALSE) {
  path <- paste("//comment()[(.='", x, "')]/following-sibling::attr[1]", sep = "")
  path <- paste(path, collapse = "")
  ns   <- getNodeSet(XML, path)
  
  if (all) {
    info <- sapply(ns, function(x) xmlValue(x))
    return(info)
  }
  
  info <- "XXXXXXNOTFOUND"
  if (length(ns) > 0) {
    info <- xmlValue(ns[[skippr]])
  }
  return(info)
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

jTime3 <- function(x) {
  x <- as.numeric(x)
  paste(sprintf("%02d", floor(x %% 3600 / 60)),
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
l604 <- l302 <- l242 <- l242C <- l102 <- list()
k604 <- k302 <- k242 <- k242C <- k102 <-  1
XMLS <- list.files(XML_DIRECTORY, pattern = "\\.xml$")
NFILES <- length(XMLS)
cat("Processing", NFILES, "XML's...\n")
nn302 <- nn242 <- nn242C <- nn102 <- NULL

for (i in 1:length(XMLS)) {
  
  ##################################################################################################
  ## Extracting basic info                                                                        ##
  ##################################################################################################
  
  # Read in the xml and determine the type
  xml1 <- xmlParse(file.path(XML_DIRECTORY, XMLS[i]))
  TYPE <- jFetch(c("Performed Protocol Code Sequence", "Code Meaning"))
  
  # Patient ID and Device Serial Number
  pID   <- jFetch("Patient ID")
  sSer  <- jFetch("Device Serial Number")
  
  ## Threshold Values
  xCr  <- as.numeric(jFetch("Visual Field Test Point X-Coordinate", all = TRUE))
  yCr  <- as.numeric(jFetch("Visual Field Test Point Y-Coordinate", all = TRUE))
  sens <- as.numeric(jFetch("Sensitivity Value", all = TRUE))
  
  # Determine the column names for each value
  EYE <- jFetch("Laterality")
  if (EYE == "R") {
    llab <- paste(ifelse(xCr < 0, "N", "T"), abs(xCr), "_", 
                  ifelse(yCr < 0, "I", "S"), abs(yCr), "_", "Thr", sep = "")
  }
  if (EYE == "L") {
    llab <- paste(ifelse(xCr < 0, "T", "N"), abs(xCr), "_", 
                  ifelse(yCr < 0, "I", "S"), abs(yCr), "_", "Thr", sep = "")
  }
  
  # Extract model name
  MTYPE <- jFetch("Manufacturer’s Model Name", skippr = 2)
  if (class(MTYPE) == "try-error") {
    MTYPE <- jFetch("Model Name")
  }
  
  ## Fixation Monitor Bit
  ff1   <- try(jFetch(c("Fixation Monitoring Code Sequence", "Code Meaning")))
  ff2   <- try(jFetch(c("Fixation Monitoring Code Sequence", "Code Meaning"), skippr = 2),
               silent = TRUE)
  testr <- paste(sort(c(ff1, ff2)), collapse = "; ")
  
  if (testr == "Automated Optical; Blind Spot Monitoring") {
    fixfill <- "Gaze/Blind Spot"
  } else {
    fixfill <- ff1
  }
  fixfill <- jReplacer(fixfill, c("Automated Optical", "Blind Spot Monitoring"),
                       c("Gaze", "Blind Spot"))
  
  ##################################################################################################
  ## 60-4 Tests                                                                                   ##
  ##################################################################################################
  
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

    # Separating first and last name
    pID2  <- jFetch("Patient’s Name")
    pID2x <- gsub("^", " ", pID2, fixed = TRUE)
    
    jOrd <- match(n604[70:129], llab)
    sens <- sens[jOrd]
    llab <- llab[jOrd]
    
    # Make line
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
             jFetch("Performed Procedure Step Start Date"),
             jFetch("Referenced SOP Instance UID")),
      "",
      jFetch("Modality"),
      EYE,
      MTYPE,
      jFetch("Manufacturer"),
      sSer,
      sSer,
      strsplit(jFetch("Software Version(s)"), "\\\\")[[1]][3],
      rep("", 2),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"),
             jFetch("Performed Procedure Step Start Time"), ""),
      rep("", 3),
      jFetch("Spherical Lens Power"),
      jFetch("Cylinder Lens Power"),
      jFetch("Cylinder Axis"),
      rep("", 3),
      jFetch("Pupil Size"),
      rep("", 5),
      jFetch(c("Performed Protocol Code Sequence", "Protocol Context Sequence", 
               "Concept Name Code Sequence", "Code Meaning")),
      rep("", 2),
      gsub(" Test Pattern", "", gsub("Visual Field ", "", TYPE, fixed = TRUE), fixed = TRUE),
      gsub(" Test Strategy", "", gsub("Visual Field ", "",
                                      jFetch(c("Performed Protocol Code Sequence", "Code Meaning"), 
                                             skippr = 3), fixed = TRUE), fixed = TRUE), 
      jFetch(c("Stimulus Color Code Sequence", "Code Meaning")),
      jFetch("Stimulus Area"), 
      jFetch(c("Background Illumination Color Code Sequence", "Code Meaning")), 
      jFetch("Visual Field Test Duration"), 
      jFetch("fixation_target_type"), 
      fixfill,
      jFetch("Blind Spot X-Coordinate"), 
      jFetch("Blind Spot Y-Coordinate"), 
      rep("", 1),
      jReplacer(jFetch(c("Visual Field Catch Trial Sequence", "False Negatives Estimate Flag")), 
                c("YES", "NO"), c("1", "0")), 
      jFetch("False Negatives Estimate"), 
      jReplacer(jFetch(c("Visual Field Catch Trial Sequence", "False Positives Estimate Flag")), 
                c("YES", "NO"), c("1", "0")), 
      jFetch("False Positives Estimate"), 
      jFetch("Fixation Checked Quantity"), 
      jFetch("Patient Not Properly Fixated Quantity"), 
      jFetch("Foveal Sensitivity Measured"), 
      search_tag("00240087"), 
      jFetch("Screening Baseline Value"), 
      rep("", 2),
      jFetch("Visual Field Horizontal Extent"), 
      rep("", 2),
      jReplacer(jFetch("Short Term Fluctuation Calculated"), 
                c("YES", "NO"), c("On", "Off")), 
      search_tag("00240075"), 
      "60",
      sens, 
      "",
      rep("", length(sens)),
      "60")
    
    l604[[k604]] <- linr
    k604 <- k604 + 1
  }
  
  ##################################################################################################
  ## 30-2 Tests                                                                                   ##
  ##################################################################################################
  
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

    # Separate First and Last name
    pID2  <- jFetch("Patient’s Name")
    pID2x <- gsub("^", " ", pID2, fixed = TRUE)
    
    ## Series Values
    tds  <- as.numeric(jFetch("Age Corrected Sensitivity Deviation Value", all = TRUE))
    pds  <- as.numeric(jFetch("Generalized Defect Corrected Sensitivity Deviation Value", all = TRUE))
    tdp  <- jFetch("Age Corrected Sensitivity Deviation Probability Value", all = TRUE)
    pdp  <- jFetch("Generalized Defect Corrected Sensitivity Deviation Probability Value", all = TRUE)
    
    jOrd <- match(n302[78:153], llab)
    sens <- sens[jOrd]
    tds  <- tds[jOrd]
    pds  <- pds[jOrd]
    tdp  <- tdp[jOrd]
    pdp  <- pdp[jOrd]
    llab <- llab[jOrd]
    
    # Make line
    linr <- c(
      R_VERSION,
      XML_TIMESTAMP,
      strsplit(jFetch("Software Version(s)"), "\\\\")[[1]][3],
      jFetch("czm_xml_version"),
      pID, ## Patient_ID 
      pID2x, ## Full Name 
      strsplit(pID2, "^", fixed = TRUE)[[1]][2],  ## "given" name
      "",    ## Middle 
      strsplit(pID2, "^", fixed = TRUE)[[1]][1],  ## Last_Name
      rep("", 2),
      jFetch("Patient’s Birth Date"),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"),
             jFetch("Performed Procedure Step Start Date"),
             jFetch("Referenced SOP Instance UID")),
      rep("", 2),
      jFetch("Modality"),
      EYE,
      MTYPE,
      jFetch("Manufacturer"),
      sSer,
      sSer,
      jFetch("Software Version(s)"),
      rep("", 2),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"),
             jFetch("Performed Procedure Step Start Time"), ""),
      rep("", 3),
      jFetch("Spherical Lens Power"),
      jFetch("Cylinder Lens Power"),
      jFetch("Cylinder Axis"),
      rep("", 3),
      jFetch("Pupil Size"),
      rep("", 5),
      jFetch(c("Performed Protocol Code Sequence", "Protocol Context Sequence", 
               "Concept Name Code Sequence", "Code Meaning")),
      rep("", 2),
      gsub(" Test Pattern", "", gsub("Visual Field ", "", TYPE, fixed = TRUE), fixed = TRUE),
      gsub(" Test Strategy", "", gsub("Visual Field ", "",
                                      jFetch(c("Performed Protocol Code Sequence", "Code Meaning"), 
                                             skippr = 3), fixed = TRUE), fixed = TRUE),
      jFetch(c("Stimulus Color Code Sequence", "Code Meaning")),
      jFetch("Stimulus Area"),
      jFetch(c("Background Illumination Color Code Sequence", "Code Meaning")),
      jFetch("Visual Field Test Duration"),
      jFetch("fixation_target_type"),
      fixfill,
      jFetch("Blind Spot X-Coordinate"),
      jFetch("Blind Spot Y-Coordinate"),
      rep("", 1),
      jFetch("Foveal Sensitivity Measured"),
      search_tag("00240087"),
      jFetch("Screening Baseline Value"),
      rep("", 2),
      jFetch("Visual Field Horizontal Extent"),
      rep("", 1),
      jReplacer(jFetch(c("Visual Field Catch Trial Sequence", "False Negatives Estimate Flag")), 
                c("YES", "NO"), c("1", "0")), 
      "", ## jFetch("Negative Catch Trials Quantity"),
      jFetch("False Negatives Quantity"),
      jFetch("False Negatives Estimate"),
      jReplacer(jFetch(c("Visual Field Catch Trial Sequence", "False Positives Estimate Flag")), 
                c("YES", "NO"), c("1", "0")), 
      jFetch("Positive Catch Trials Quantity"),
      jFetch("False Positives Quantity"),
      jFetch("False Positives Estimate"),
      jFetch("Fixation Checked Quantity"),
      jFetch("Patient Not Properly Fixated Quantity"),
      rep("", 3),
      jReplacer(jFetch("Short Term Fluctuation Calculated"), 
                c("YES", "NO"), c("On", "Off")),
      search_tag("00240075"),
      length(sens),
      sens,
      rep("", 1),
      jFetch("Patient Reliability Indicator"),
      jFetch(c("Concept Code Sequence", "Code Meaning")),
      jFetch("Global Deviation From Normal"),
      jFetch("Global Deviation Probability"),
      jFetch("Localized Deviation from Normal"),
      jFetch("Localized Deviation Probability"),
      rep("", 3),
      jFetch(c("Visual Field Global Results Index Sequence", "Data Observation Sequence", 
               "Numeric Value")),
      jFetch("Foveal Point Probability Value"),
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
  
  ##################################################################################################
  ## 24-2 Tests                                                                                   ##
  ##################################################################################################
  
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
    
    nn242 <- c(nn242, XMLS[i])
    
    ## Series Values
    tds  <- as.numeric(jFetch("Age Corrected Sensitivity Deviation Value", all = TRUE))
    pds  <- as.numeric(jFetch("Generalized Defect Corrected Sensitivity Deviation Value", all = TRUE))
    tdp  <- jFetch("Age Corrected Sensitivity Deviation Probability Value", all = TRUE)
    pdp  <- jFetch("Generalized Defect Corrected Sensitivity Deviation Probability Value", all = TRUE)
    
    jOrd <- match(n242[78:153], llab)
    sens <- sens[jOrd]
    tds  <- tds[jOrd]
    pds  <- pds[jOrd]
    tdp  <- tdp[jOrd]
    pdp  <- pdp[jOrd]
    llab <- llab[jOrd]
    
    BACKUP_DATE <- jFetch("Study Date")
    BACKUP_TIME <- jFetch("Study Time")
    
    ## Make Line
    linr <- c(
      R_VERSION,
      XML_TIMESTAMP,
      jFetch("Software Version(s)"),
      jFetch("czm_xml_version"),
      pID,
      pID,
      rep("", 2),
      pID,
      rep("", 2),
      jFetch("Patient’s Birth Date"),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), 
             jFetch("Performed Procedure Step Start Date"),
             jFetch("Referenced SOP Instance UID")),
      rep("", 2),
      jFetch("Modality"),
      EYE,
      MTYPE,
      jFetch("Manufacturer"),
      sSer,
      sSer,
      jFetch("Software Version(s)"),
      rep("", 2),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), 
             jFetch("Performed Procedure Step Start Time"), ""),
      rep("", 3),
      jFetch("Spherical Lens Power"),
      jFetch("Cylinder Lens Power"),
      jFetch("Cylinder Axis"),
      rep("", 3),
      jFetch("Pupil Size"),
      rep("", 5),
      jFetch(c("Performed Protocol Code Sequence", "Protocol Context Sequence", 
               "Concept Name Code Sequence", "Code Meaning")),
      rep("", 2),
      gsub(" Test Pattern", "", gsub("Visual Field ", "", TYPE, fixed = TRUE), fixed = TRUE),
      gsub(" Test Strategy", "", gsub("Visual Field ", "",
                                      jFetch(c("Performed Protocol Code Sequence", "Code Meaning"), 
                                             skippr = 3), fixed = TRUE), fixed = TRUE),
      jFetch(c("Stimulus Color Code Sequence", "Code Meaning")),
      jFetch("Stimulus Area"),
      jFetch(c("Background Illumination Color Code Sequence", "Code Meaning")),
      jFetch("Visual Field Test Duration"),
      jFetch("fixation_target_type"),
      fixfill,
      jFetch("Blind Spot X-Coordinate"),
      jFetch("Blind Spot Y-Coordinate"),
      rep("", 1),
      jFetch("Foveal Sensitivity Measured"),
      search_tag("00240087"),
      jFetch("Screening Baseline Value"),
      rep("", 2),
      jFetch("Visual Field Horizontal Extent"),
      rep("", 4),
      jFetch("False Negatives Estimate"),
      rep("", 3),
      jFetch("False Positives Estimate"),
      jFetch("Fixation Checked Quantity"),
      jFetch("Patient Not Properly Fixated Quantity"),
      rep("", 3),
      jReplacer(jFetch("Short Term Fluctuation Calculated"), 
                c("YES", "NO"), c("On", "Off")),
      search_tag("00240075"),
      length(sens[!is.na(sens)]),
      sens,
      rep("", 1),
      jFetch("Patient Reliability Indicator"),
      jFetch(c("Concept Code Sequence", "Code Meaning")),
      jFetch("Global Deviation From Normal"),
      jFetch("Global Deviation Probability"),
      jFetch("Localized Deviation from Normal"),
      jFetch("Localized Deviation Probability"),
      rep("", 3),
      jFetch(c("Visual Field Global Results Index Sequence", "Data Observation Sequence", 
               "Numeric Value")),
      jFetch("Foveal Point Probability Value"),
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
  
  ##################################################################################################
  ## 24-2C Tests                                                                                   ##
  ##################################################################################################
  
  if (TYPE == "Visual Field 24-2C Test Pattern") {
    n242C <- c("HGCParserVersion", "XMLConversionTimeStamp", "HFA2_SOFTWARE_VERSION", "HFA_XML_VERSION",
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
               "N9_I27_Thr", "N3_I27_Thr", "T3_I27_Thr", "T9_I27_Thr", "T7_S5_Thr", "T5_S7_Thr",
               "T1_S5_Thr", "N1_S5_Thr", "N5_S1_Thr", "N7_I1_Thr", "N7_I5_Thr", "N1_I5_Thr", 
               "T1_I9_Thr", "T5_I7_Thr",
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
               "N9_I27_TD", "N3_I27_TD", "T3_I27_TD", "T9_I27_TD", "T7_S5_TD", "T5_S7_TD", 
               "T1_S5_TD", "N1_S5_TD", "N5_S1_TD", "N7_I1_TD", "N7_I5_TD", "N1_I5_TD", 
               "T1_I9_TD", "T5_I7_TD",
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
               "N9_I27_PD", "N3_I27_PD", "T3_I27_PD", "T9_I27_PD", "T7_S5_PD", "T5_S7_PD", 
               "T1_S5_PD", "N1_S5_PD", "N5_S1_PD", "N7_I1_PD", "N7_I5_PD", "N1_I5_PD", 
               "T1_I9_PD", "T5_I7_PD",
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
               "N9_I27_TDP", "N3_I27_TDP", "T3_I27_TDP", "T9_I27_TDP", "T7_S5_TDP", "T5_S7_TDP", 
               "T1_S5_TDP", "N1_S5_TDP", "N5_S1_TDP", "N7_I1_TDP", "N7_I5_TDP", "N1_I5_TDP", 
               "T1_I9_TDP", "T5_I7_TDP",
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
               "N9_I27_PDP", "N3_I27_PDP", "T3_I27_PDP", "T9_I27_PDP", "T7_S5_PDP", "T5_S7_PDP", 
               "T1_S5_PDP", "N1_S5_PDP", "N5_S1_PDP", "N7_I1_PDP", "N7_I5_PDP", "N1_I5_PDP", 
               "T1_I9_PDP", "T5_I7_PDP", "BACKUP_TIME", "BACKUP_DATE")
    
    nn242C <- c(nn242C, XMLS[i])
    
    ## Series Values
    tds  <- as.numeric(jFetch("Age Corrected Sensitivity Deviation Value", all = TRUE))
    pds  <- as.numeric(jFetch("Generalized Defect Corrected Sensitivity Deviation Value", all = TRUE))
    tdp  <- jFetch("Age Corrected Sensitivity Deviation Probability Value", all = TRUE)
    pdp  <- jFetch("Generalized Defect Corrected Sensitivity Deviation Probability Value", all = TRUE)
    
    jOrd <- match(n242C[78:163], llab)
    sens <- sens[jOrd]
    tds  <- tds[jOrd]
    pds  <- pds[jOrd]
    tdp  <- tdp[jOrd]
    pdp  <- pdp[jOrd]
    llab <- llab[jOrd]
    
    BACKUP_DATE <- jFetch("Study Date")
    BACKUP_TIME <- jFetch("Study Time")
    
    ## Make Line
    linr <- c(
      R_VERSION,
      XML_TIMESTAMP,
      jFetch("Software Version(s)"),
      jFetch("czm_xml_version"),
      pID,
      pID,
      rep("", 2),
      pID,
      rep("", 2),
      jFetch("Patient’s Birth Date"),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), 
             jFetch("Performed Procedure Step Start Date"),
             jFetch("Referenced SOP Instance UID")),
      rep("", 2),
      jFetch("Modality"),
      EYE,
      MTYPE,
      jFetch("Manufacturer"),
      sSer,
      sSer,
      jFetch("Software Version(s)"),
      rep("", 2),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), 
             jFetch("Performed Procedure Step Start Time"), ""),
      rep("", 3),
      jFetch("Spherical Lens Power"),
      jFetch("Cylinder Lens Power"),
      jFetch("Cylinder Axis"),
      rep("", 3),
      jFetch("Pupil Size"),
      rep("", 5),
      jFetch(c("Performed Protocol Code Sequence", "Protocol Context Sequence", 
               "Concept Name Code Sequence", "Code Meaning")),
      rep("", 2),
      gsub(" Test Pattern", "", gsub("Visual Field ", "", TYPE, fixed = TRUE), fixed = TRUE),
      gsub(" Test Strategy", "", gsub("Visual Field ", "",
                                      jFetch(c("Performed Protocol Code Sequence", "Code Meaning"), 
                                             skippr = 3), fixed = TRUE), fixed = TRUE),
      jFetch(c("Stimulus Color Code Sequence", "Code Meaning")),
      jFetch("Stimulus Area"),
      jFetch(c("Background Illumination Color Code Sequence", "Code Meaning")),
      jFetch("Visual Field Test Duration"),
      jFetch("fixation_target_type"),
      fixfill,
      jFetch("Blind Spot X-Coordinate"),
      jFetch("Blind Spot Y-Coordinate"),
      rep("", 1),
      jFetch("Foveal Sensitivity Measured"),
      search_tag("00240087"),
      jFetch("Screening Baseline Value"),
      rep("", 2),
      jFetch("Visual Field Horizontal Extent"),
      rep("", 4),
      jFetch("False Negatives Estimate"),
      rep("", 3),
      jFetch("False Positives Estimate"),
      jFetch("Fixation Checked Quantity"),
      jFetch("Patient Not Properly Fixated Quantity"),
      rep("", 3),
      jReplacer(jFetch("Short Term Fluctuation Calculated"), 
                c("YES", "NO"), c("On", "Off")),
      search_tag("00240075"),
      length(sens[!is.na(sens)]),
      sens,
      rep("", 1),
      jFetch("Patient Reliability Indicator"),
      jFetch(c("Concept Code Sequence", "Code Meaning")),
      jFetch("Global Deviation From Normal"),
      jFetch("Global Deviation Probability"),
      jFetch("Localized Deviation from Normal"),
      jFetch("Localized Deviation Probability"),
      rep("", 3),
      jFetch(c("Visual Field Global Results Index Sequence", "Data Observation Sequence", 
               "Numeric Value")),
      jFetch("Foveal Point Probability Value"),
      64,
      tds,
      64,
      pds,
      64,
      tdp,
      64,
      pdp,
      BACKUP_TIME,
      BACKUP_DATE
    )
    linr <- as.character(linr)
    
    l242C[[k242C]] <- linr
    k242C <- k242C + 1
  }
  
  ##################################################################################################
  ## 10-2 Tests                                                                                   ##
  ##################################################################################################
  
  if (TYPE == "Visual Field 10-2 Test Pattern") {
    n102 <- c('HFA2_SOFTWARE_VERSION', 'HFA_XML_VERSION', 'HGCParserVersion', 'XMLSourceFileName', 
              'XMLConversionTimeStamp', 'PATIENT_ID', 'FULL_NAME', 'GIVEN_NAME', 'MIDDLE_NAME', 
              'LAST_NAME', 'NAME_PREFIX', 'NAME_SUFFIX', 'BIRTH_DATE', 'VISIT_DATE', 'STUDY_UID',
              'SERIES_DATE_TIME', 'MODALITY', 'SITE', 'INSTRUMENT_NAME', 
              'INSTRUMENT_MANUFACTURER', 'INSTRUMENT_MODEL_NUMBER', 'INSTRUMENT_SERIAL_NUMBER', 
              'INSTRUMENT_SOFTWARE_VERSION', 'DISPLAY_NAME', 'CLINICAL_NOTES', 'EXAM_TIME', 
              'CD_HORIZONTAL', 'CD_VERTICAL', 'IOP', 'TRIAL_RX_SPHERE', 'TRIAL_RX_CYLINDER', 
              'TRIAL_RX_AXIS', 'DISTANCE_RX_SPHERE', 'DISTANCE_RX_CYLINDER', 'DISTANCE_RX_AXIS', 
              'PUPIL_DIAMETER', 'PUPIL_DIAMETER_AUTO', 'DIAGNOSIS_CODE', 'PROCEDURE_CODE', 'VA', 
              'VA_STRING', 'TEST_TYPE', 'IMAGE_TYPE', 'IMAGE_FILE_NAME', 'TEST_PATTERN', 
              'TEST_STRATEGY', 'STIMULUS_COLOR', 'STIMULUS_SIZE', 'BACKGROUND_COLOR', 
              'EXAM_DURATION', 'FIXATION_TARGET', 'FIXATION_MONITOR', 'BLIND_SPOT_X', 
              'BLIND_SPOT_Y', 'BLIND_SPOT_STIMULUS_SIZE', 'FOVEAL_RESULT', 'FOVEAL_THRESHOLD', 
              'CENTRAL_REF_LEVEL', 'THROWN_OUT_POINTS', 'MINIMUM_STIMULUS', 'FIELD_SIZE', 
              'LANGUAGE', 'FALSE_NEGATIVE_TRIALS', 'FALSE_NEGATIVE_ERRORS', 
              'FALSE_NEGATIVE_PERCENT', 'FALSE_POSITIVE_TRIALS', 'FALSE_POSITIVE_ERRORS', 
              'FALSE_POSITIVE_PERCENT', 'FIXATION_CHECK_TRIALS', 'FIXATION_CHECK_ERRORS', 
              'QUESTIONS_ASKED', 'REFERENCE_TEST_DATE', 'REFERENCE_TEST_CODE', 'SF_STATUS', 
              'SF', 'NUM_THRESHOLD_POINTS', 'N1_S9_Thr', 'T1_S9_Thr', 'N5_S7_Thr', 'N3_S7_Thr', 
              'N1_S7_Thr', 'T1_S7_Thr', 'T3_S7_Thr', 'T5_S7_Thr', 'N7_S5_Thr', 'N5_S5_Thr', 
              'N3_S5_Thr', 'N1_S5_Thr', 'T1_S5_Thr', 'T3_S5_Thr', 'T5_S5_Thr', 'T7_S5_Thr', 
              'N7_S3_Thr', 'N5_S3_Thr', 'N3_S3_Thr', 'N1_S3_Thr', 'T1_S3_Thr', 'T3_S3_Thr', 
              'T5_S3_Thr', 'T7_S3_Thr', 'N9_S1_Thr', 'N7_S1_Thr', 'N5_S1_Thr', 'N3_S1_Thr', 
              'N1_S1_Thr', 'T1_S1_Thr', 'T3_S1_Thr', 'T5_S1_Thr', 'T7_S1_Thr', 'T9_S1_Thr', 
              'N9_I1_Thr', 'N7_I1_Thr', 'N5_I1_Thr', 'N3_I1_Thr', 'N1_I1_Thr', 'T1_I1_Thr', 
              'T3_I1_Thr', 'T5_I1_Thr', 'T7_I1_Thr', 'T9_I1_Thr', 'N7_I3_Thr', 'N5_I3_Thr',
              'N3_I3_Thr', 'N1_I3_Thr', 'T1_I3_Thr', 'T3_I3_Thr', 'T5_I3_Thr', 'T7_I3_Thr', 
              'N7_I5_Thr', 'N5_I5_Thr', 'N3_I5_Thr', 'N1_I5_Thr', 'T1_I5_Thr', 'T3_I5_Thr', 
              'T5_I5_Thr', 'T7_I5_Thr', 'N5_I7_Thr', 'N3_I7_Thr', 'N1_I7_Thr', 'T1_I7_Thr', 
              'T3_I7_Thr', 'T5_I7_Thr', 'N1_I9_Thr', 'T1_I9_Thr', 'STATPAC_STATUS', 
              'LOW_PATIENT_RELIABILITY_STATUS', 'SWAPFT_GENERAL_HEIGHT', 'GHT', 'MD', 
              'MD_PROBABILITY', 'PSD', 'PSD_PROBABILITY', 'CPSD', 'CPSD_PROBABILITY', 
              'SF_PROBABILITY', 'VFI', 'FOVEAL_THRESHOLD_PROBABILITY', 'FLAG_ASSESSMENT', 
              'FLAG_SEVERITY', 'NUM_TOTAL_DEV_VALUE_POINTS', 'N1_S9_TD', 'T1_S9_TD', 'N5_S7_TD', 
              'N3_S7_TD', 'N1_S7_TD', 'T1_S7_TD', 'T3_S7_TD', 'T5_S7_TD', 'N7_S5_TD', 'N5_S5_TD',
              'N3_S5_TD', 'N1_S5_TD', 'T1_S5_TD', 'T3_S5_TD', 'T5_S5_TD', 'T7_S5_TD', 'N7_S3_TD', 
              'N5_S3_TD', 'N3_S3_TD', 'N1_S3_TD', 'T1_S3_TD', 'T3_S3_TD', 'T5_S3_TD', 'T7_S3_TD', 
              'N9_S1_TD', 'N7_S1_TD', 'N5_S1_TD', 'N3_S1_TD', 'N1_S1_TD', 'T1_S1_TD', 'T3_S1_TD', 
              'T5_S1_TD', 'T7_S1_TD', 'T9_S1_TD', 'N9_I1_TD', 'N7_I1_TD', 'N5_I1_TD', 'N3_I1_TD', 
              'N1_I1_TD', 'T1_I1_TD', 'T3_I1_TD', 'T5_I1_TD', 'T7_I1_TD', 'T9_I1_TD', 'N7_I3_TD', 
              'N5_I3_TD', 'N3_I3_TD', 'N1_I3_TD', 'T1_I3_TD', 'T3_I3_TD', 'T5_I3_TD', 'T7_I3_TD', 
              'N7_I5_TD', 'N5_I5_TD', 'N3_I5_TD', 'N1_I5_TD', 'T1_I5_TD', 'T3_I5_TD', 'T5_I5_TD', 
              'T7_I5_TD', 'N5_I7_TD', 'N3_I7_TD', 'N1_I7_TD', 'T1_I7_TD', 'T3_I7_TD', 'T5_I7_TD', 
              'N1_I9_TD', 'T1_I9_TD', 'NUM_PATTERN_DEV_VALUE_POINTS', 'N1_S9_PD', 'T1_S9_PD', 
              'N5_S7_PD', 'N3_S7_PD', 'N1_S7_PD', 'T1_S7_PD', 'T3_S7_PD', 'T5_S7_PD', 'N7_S5_PD', 
              'N5_S5_PD', 'N3_S5_PD', 'N1_S5_PD', 'T1_S5_PD', 'T3_S5_PD', 'T5_S5_PD', 'T7_S5_PD', 
              'N7_S3_PD', 'N5_S3_PD', 'N3_S3_PD', 'N1_S3_PD', 'T1_S3_PD', 'T3_S3_PD', 'T5_S3_PD', 
              'T7_S3_PD', 'N9_S1_PD', 'N7_S1_PD', 'N5_S1_PD', 'N3_S1_PD', 'N1_S1_PD', 'T1_S1_PD', 
              'T3_S1_PD', 'T5_S1_PD', 'T7_S1_PD', 'T9_S1_PD', 'N9_I1_PD', 'N7_I1_PD', 'N5_I1_PD', 
              'N3_I1_PD', 'N1_I1_PD', 'T1_I1_PD', 'T3_I1_PD', 'T5_I1_PD', 'T7_I1_PD', 'T9_I1_PD', 
              'N7_I3_PD', 'N5_I3_PD', 'N3_I3_PD', 'N1_I3_PD', 'T1_I3_PD', 'T3_I3_PD', 'T5_I3_PD', 
              'T7_I3_PD', 'N7_I5_PD', 'N5_I5_PD', 'N3_I5_PD', 'N1_I5_PD', 'T1_I5_PD', 'T3_I5_PD', 
              'T5_I5_PD', 'T7_I5_PD', 'N5_I7_PD', 'N3_I7_PD', 'N1_I7_PD', 'T1_I7_PD', 'T3_I7_PD', 
              'T5_I7_PD', 'N1_I9_PD', 'T1_I9_PD', 'NUM_TOTAL_DEV_PROB_POINTS', 'N1_S9_TDP', 
              'T1_S9_TDP', 'N5_S7_TDP', 'N3_S7_TDP', 'N1_S7_TDP', 'T1_S7_TDP', 'T3_S7_TDP', 
              'T5_S7_TDP', 'N7_S5_TDP', 'N5_S5_TDP', 'N3_S5_TDP', 'N1_S5_TDP', 'T1_S5_TDP', 
              'T3_S5_TDP', 'T5_S5_TDP', 'T7_S5_TDP', 'N7_S3_TDP', 'N5_S3_TDP', 'N3_S3_TDP', 
              'N1_S3_TDP', 'T1_S3_TDP', 'T3_S3_TDP', 'T5_S3_TDP', 'T7_S3_TDP', 'N9_S1_TDP', 
              'N7_S1_TDP', 'N5_S1_TDP', 'N3_S1_TDP', 'N1_S1_TDP', 'T1_S1_TDP', 'T3_S1_TDP', 
              'T5_S1_TDP', 'T7_S1_TDP', 'T9_S1_TDP', 'N9_I1_TDP', 'N7_I1_TDP', 'N5_I1_TDP', 
              'N3_I1_TDP', 'N1_I1_TDP', 'T1_I1_TDP', 'T3_I1_TDP', 'T5_I1_TDP', 'T7_I1_TDP', 
              'T9_I1_TDP', 'N7_I3_TDP', 'N5_I3_TDP', 'N3_I3_TDP', 'N1_I3_TDP', 'T1_I3_TDP', 
              'T3_I3_TDP', 'T5_I3_TDP', 'T7_I3_TDP', 'N7_I5_TDP', 'N5_I5_TDP', 'N3_I5_TDP', 
              'N1_I5_TDP', 'T1_I5_TDP', 'T3_I5_TDP', 'T5_I5_TDP', 'T7_I5_TDP', 'N5_I7_TDP', 
              'N3_I7_TDP', 'N1_I7_TDP', 'T1_I7_TDP', 'T3_I7_TDP', 'T5_I7_TDP', 'N1_I9_TDP', 
              'T1_I9_TDP', 'NUM_PATTERN_DEV_PROB_POINTS', 'N1_S9_PDP', 'T1_S9_PDP', 'N5_S7_PDP', 
              'N3_S7_PDP', 'N1_S7_PDP', 'T1_S7_PDP', 'T3_S7_PDP', 'T5_S7_PDP', 'N7_S5_PDP', 
              'N5_S5_PDP', 'N3_S5_PDP', 'N1_S5_PDP', 'T1_S5_PDP', 'T3_S5_PDP', 'T5_S5_PDP', 
              'T7_S5_PDP', 'N7_S3_PDP', 'N5_S3_PDP', 'N3_S3_PDP', 'N1_S3_PDP', 'T1_S3_PDP', 
              'T3_S3_PDP', 'T5_S3_PDP', 'T7_S3_PDP', 'N9_S1_PDP', 'N7_S1_PDP', 'N5_S1_PDP', 
              'N3_S1_PDP', 'N1_S1_PDP', 'T1_S1_PDP', 'T3_S1_PDP', 'T5_S1_PDP', 'T7_S1_PDP', 
              'T9_S1_PDP', 'N9_I1_PDP', 'N7_I1_PDP', 'N5_I1_PDP', 'N3_I1_PDP', 'N1_I1_PDP', 
              'T1_I1_PDP', 'T3_I1_PDP', 'T5_I1_PDP', 'T7_I1_PDP', 'T9_I1_PDP', 'N7_I3_PDP', 
              'N5_I3_PDP', 'N3_I3_PDP', 'N1_I3_PDP', 'T1_I3_PDP', 'T3_I3_PDP', 'T5_I3_PDP', 
              'T7_I3_PDP', 'N7_I5_PDP', 'N5_I5_PDP', 'N3_I5_PDP', 'N1_I5_PDP', 'T1_I5_PDP',
              'T3_I5_PDP', 'T5_I5_PDP', 'T7_I5_PDP', 'N5_I7_PDP', 'N3_I7_PDP', 'N1_I7_PDP',
              'T1_I7_PDP', 'T3_I7_PDP', 'T5_I7_PDP', 'N1_I9_PDP', 'T1_I9_PDP',
              "BACKUP_TIME", "BACKUP_DATE")
    
    nn102 <- c(nn102, XMLS[i])
    
    ## Series Values
    tds  <- as.numeric(jFetch("Age Corrected Sensitivity Deviation Value", all = TRUE))
    pds  <- as.numeric(jFetch("Generalized Defect Corrected Sensitivity Deviation Value", all = TRUE))
    tdp  <- jFetch("Age Corrected Sensitivity Deviation Probability Value", all = TRUE)
    pdp  <- jFetch("Generalized Defect Corrected Sensitivity Deviation Probability Value", all = TRUE)
    
    jOrd <- match(n102[77:144], llab)
    sens <- sens[jOrd]
    tds  <- tds[jOrd]
    pds  <- pds[jOrd]
    tdp  <- tdp[jOrd]
    pdp  <- pdp[jOrd]
    llab <- llab[jOrd]
    
    BACKUP_DATE <- jFetch("Study Date")
    BACKUP_TIME <- jFetch("Study Time")
    
    ## Make Line
    linr <- c(
      jFetch("Software Version(s)"),
      jFetch("czm_xml_version"),
      R_VERSION,
      paste(XML_DIRECTORY, XMLS[i], sep = "/"),
      XML_TIMESTAMP,
      pID,
      pID,
      rep("", 2),
      pID,
      rep("", 2),
      jFetch("Patient’s Birth Date"),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), 
             jFetch("Performed Procedure Step Start Date"),
             jFetch("Referenced SOP Instance UID")),
      rep("", 2),
      jFetch("Modality"),
      EYE,
      MTYPE,
      jFetch("Manufacturer"),
      sSer,
      sSer,
      jFetch("Software Version(s)"),
      rep("", 2),
      ifelse(MTYPE %in% c("HFA 3", "Humphrey Field Analyzer 3"), 
             jFetch("Performed Procedure Step Start Time"), ""),
      rep("", 3),
      jFetch("Spherical Lens Power"),
      jFetch("Cylinder Lens Power"),
      jFetch("Cylinder Axis"),
      rep("", 3),
      jFetch("Pupil Size"),
      rep("", 5),
      jFetch(c("Performed Protocol Code Sequence", "Protocol Context Sequence", 
               "Concept Name Code Sequence", "Code Meaning")),
      rep("", 2),
      gsub(" Test Pattern", "", gsub("Visual Field ", "", TYPE, fixed = TRUE), fixed = TRUE),
      gsub(" Test Strategy", "", gsub("Visual Field ", "",
                                      jFetch(c("Performed Protocol Code Sequence", "Code Meaning"), 
                                             skippr = 3), fixed = TRUE), fixed = TRUE),
      jFetch(c("Stimulus Color Code Sequence", "Code Meaning")),
      "",
      jFetch(c("Background Illumination Color Code Sequence", "Code Meaning")),
      jFetch("Visual Field Test Duration"),
      jFetch("fixation_target_type"),
      fixfill,
      jFetch("Blind Spot X-Coordinate"),
      jFetch("Blind Spot Y-Coordinate"),
      rep("", 1),
      jFetch("Foveal Sensitivity Measured"),
      search_tag("00240087"),
      jFetch("Screening Baseline Value"),
      rep("", 2),
      jFetch("Visual Field Horizontal Extent"),
      rep("", 3),
      jFetch("False Negatives Estimate"),
      rep("", 2),
      jFetch("False Positives Estimate"),
      jFetch("Fixation Checked Quantity"),
      jFetch("Patient Not Properly Fixated Quantity"),
      rep("", 3),
      jReplacer(jFetch("Short Term Fluctuation Calculated"), 
                c("YES", "NO"), c("On", "Off")),
      search_tag("00240075"),
      length(sens[!is.na(sens)]),
      sens,
      "",
      jFetch("Patient Reliability Indicator"),
      jFetch(c("Concept Code Sequence", "Code Meaning")),
      "",
      jFetch("Global Deviation From Normal"),
      jFetch("Global Deviation Probability"),
      jFetch("Localized Deviation from Normal"),
      jFetch("Localized Deviation Probability"),
      rep("", 4),
      jFetch("Foveal Point Probability Value"),
      rep("", 2),
      68,
      tds,
      68,
      pds,
      68,
      tdp,
      68,
      pdp,
      BACKUP_TIME,
      BACKUP_DATE
    )
    
    linr <- as.character(linr)
    
    l102[[k102]] <- linr
    k102 <- k102 + 1
  }
  
  TOT <- k604 + k302 + k242 + k102
  if (TOT %% 100 == 0) {
    cat(TOT, "files processed...\n")
  }
}
cat("...done!\n")


####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
## Post-processing                                                                                ##
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################


####################################################################################################
## Post-process 60-4                                                                              ##
####################################################################################################
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

####################################################################################################
## Post-process 30-2                                                                              ##
####################################################################################################

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
  o302[, cNames302[!cNames302 %in% names(o302)]] <- ""
  o302 <- o302[, cNames302]
  
  write.csv(o302, file.path(XML_DIRECTORY, "test30-2_FMP.csv"), row.names = FALSE,
            na = "")
}

####################################################################################################
## Post-process 24-2                                                                              ##
####################################################################################################

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
  
  ## Exam Duration Fix
  o242$EXAM_DURATION <- jTime3(o242$EXAM_DURATION)
  
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
  
  # Other Fixes
  o242$TEST_TYPE     <- jReplacer(o242$TEST_TYPE, "Diagnostic", "Threshold")
  o242$TEST_PATTERN  <- jReplacer(o242$TEST_PATTERN, "24-2", "Central 24-2")
  #o302$TEST_PATTERN  <- "C30-2"
  #o242$TEST_STRATEGY <- jReplacer(o242$TEST_STRATEGY, "SITA-Fast", "SITA Fast")
  o242$TEST_STRATEGY <- gsub("-", " ", o242$TEST_STRATEGY, fixed = TRUE)
  o242$STIMULUS_SIZE <- jReplacer(o242$STIMULUS_SIZE, "0.14748032", "III")
  
  o242$FOVEAL_THRESHOLD_PROBABILITY <- jReplacer(o242$FOVEAL_THRESHOLD_PROBABILITY,
                                                 c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                                 c("Not Significant", "< 0.5%", "< 1%", "< 2%", 
                                                   "< 5%", "< 10%"))
  o242$PSD_PROBABILITY <- jReplacer(o242$PSD_PROBABILITY,
                                    c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                    c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  
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
  
  ## "Calculated" version - WIP as no fields are actually filled in
  o242$XMLSourceFileName <- nn242
  cNames242 <- c('HFA2_SOFTWARE_VERSION', 'HFA_XML_VERSION', 'HGCParserVersion', 'XMLSourceFileName', 
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
  
  o242_FMP <- o242[, which(names(o242) %in% cNames242)]
  o242_FMP[, cNames242[!cNames242 %in% names(o242_FMP)]] <- ""
  o242_FMP <- o242_FMP[, cNames242]
  
  write.csv(o242_FMP, file.path(XML_DIRECTORY, "test24-2_FMP.csv"), row.names = FALSE,
            na = "")
  
  # QC Version
  qcNames242 <- c('Batch_UID', 'Exam_UID', 'InstrumentModel', 'InstrumentSerialNumber',                 #
                  'InstrumentSoftwareVersion', 'PatientID', 'StudyCode', 'aeDOB', 'Eye', 'SeriesDateTime', 
                  'aeExamDate', 'aeExamTime', 'ExamDuration', 'aeIsShileyClinicHFAExam', 'aeDIGSTestType', 
                  'TestType', 'TestPattern', 'TestStrategy', 'StimulusColor', 'StimulusSize', 
                  'BackgroundColor', 'FixationTarget', 'FixationMonitor', 'TrialRXSphereRaw', 
                  'TrialRXCylRaw', 'TrialRXAxisRaw', 'PupilDiameter', 'VAType', 'BlindSpotX', 
                  'BlindSpotY', 'BlindSpotStimulusSize', 'FalseNegativePercent', 'FalsePositivePercent',
                  'aeFixationCheckPercentage', 'FovealResult', 'FovealThreshold', 'ClinicalNotes', 
                  'SFStatus', 'SF', 'SFProb', 'SWAPFTGeneralHeight', 'GHTType', 'MD', 'MDProb', 'PSD', 
                  'PSDProb', 'CPSD', 'CPSDProb', 'FovealThresholdProb', 'aePDPCen4LT5Count', 
                  'aeHasHighRawThreshold', 'FLAGAssessment', 'FLAGSeverity', 'AGISScore', 'AGISNas', 
                  'AGISInf', 'AGISSup', 'GHSupThrSum', 'GHSupThrMean', 'GHSupThrStd', 'GHSupTDSum', 
                  'GHSupTDMean', 'GHSupTDStd', 'GHSupPDSum', 'GHSupPDMean', 'GHSupPDStd', 
                  'GHSupPDCntLT50p', 'GHSupPDCntLT10p', 'GHSupNasThrSum', 'GHSupNasThrMean', 
                  'GHSupNasThrStd', 'GHSupNasTDSum', 'GHSupNasTDMean', 'GHSupNasTDStd', 'GHSupNasPDSum', 
                  'GHSupNasPDMean', 'GHSupNasPDStd', 'GHSupNasPDCntLT50p', 'GHSupNasPDCntLT10p', 
                  'GHInfThrSum', 'GHInfThrMean', 'GHInfThrStd', 'GHInfTDSum', 'GHInfTDMean',
                  'GHInfTDStd', 'GHInfPDSum', 'GHInfPDMean', 'GHInfPDStd', 'GHInfPDCntLT50p', 
                  'GHInfPDCntLT10p', 'GHInfNasThrSum', 'GHInfNasThrMean', 'GHInfNasThrStd', 'GHInfNasTDSum',
                  'GHInfNasTDMean', 'GHInfNasTDStd', 'GHInfNasPDSum', 'GHInfNasPDMean', 'GHInfNasPDStd', 
                  'GHInfNasPDCntLT50p', 'GHInfNasPDCntLT10p', 'GHCentralThrSum', 'GHCentralThrMean',
                  'GHCentralThrStd', 'GHCentralTDSum', 'GHCentralTDMean', 'GHCentralTDStd', 
                  'GHCentralPDSum', 'GHCentralPDMean', 'GHCentralPDStd', 'GHCentralPDCntLT50p', 
                  'GHCentralPDCntLT10p', 'GHTemporalThrSum', 'GHTemporalThrMean', 'GHTemporalThrStd', 
                  'GHTemporalTDSum', 'GHTemporalTDMean', 'GHTemporalTDStd', 'GHTemporalPDSum', 
                  'GHTemporalPDMean', 'GHTemporalPDStd', 'GHTemporalPDCntLT50p', 'GHTemporalPDCntLT10p', 
                  'N9_S27_Thr', 'N3_S27_Thr', 'T3_S27_Thr', 'T9_S27_Thr', 'N15_S21_Thr', 'N9_S21_Thr', 
                  'N3_S21_Thr', 'T3_S21_Thr', 'T9_S21_Thr', 'T15_S21_Thr', 'N21_S15_Thr', 'N15_S15_Thr',
                  'N9_S15_Thr', 'N3_S15_Thr', 'T3_S15_Thr', 'T9_S15_Thr', 'T15_S15_Thr', 'T21_S15_Thr', 
                  'N27_S9_Thr', 'N21_S9_Thr', 'N15_S9_Thr', 'N9_S9_Thr', 'N3_S9_Thr', 'T3_S9_Thr', 
                  'T9_S9_Thr', 'T15_S9_Thr', 'T21_S9_Thr', 'T27_S9_Thr', 'N27_S3_Thr', 'N21_S3_Thr', 
                  'N15_S3_Thr', 'N9_S3_Thr', 'N3_S3_Thr', 'T3_S3_Thr', 'T9_S3_Thr', 'T21_S3_Thr', 
                  'T27_S3_Thr', 'N27_I3_Thr', 'N21_I3_Thr', 'N15_I3_Thr', 'N9_I3_Thr', 'N3_I3_Thr', 
                  'T3_I3_Thr', 'T9_I3_Thr', 'T21_I3_Thr', 'T27_I3_Thr', 'N27_I9_Thr', 'N21_I9_Thr', 
                  'N15_I9_Thr', 'N9_I9_Thr', 'N3_I9_Thr', 'T3_I9_Thr', 'T9_I9_Thr', 'T15_I9_Thr', 
                  'T21_I9_Thr', 'T27_I9_Thr', 'N21_I15_Thr', 'N15_I15_Thr', 'N9_I15_Thr', 'N3_I15_Thr', 
                  'T3_I15_Thr', 'T9_I15_Thr', 'T15_I15_Thr', 'T21_I15_Thr', 'N15_I21_Thr', 'N9_I21_Thr', 
                  'N3_I21_Thr', 'T3_I21_Thr', 'T9_I21_Thr', 'T15_I21_Thr', 'N9_I27_Thr', 'N3_I27_Thr', 
                  'T3_I27_Thr', 'T9_I27_Thr', 'N9_S27_TD', 'N3_S27_TD', 'T3_S27_TD', 'T9_S27_TD', 
                  'N15_S21_TD', 'N9_S21_TD', 'N3_S21_TD', 'T3_S21_TD', 'T9_S21_TD', 'T15_S21_TD', 
                  'N21_S15_TD', 'N15_S15_TD', 'N9_S15_TD', 'N3_S15_TD', 'T3_S15_TD', 'T9_S15_TD', 
                  'T15_S15_TD', 'T21_S15_TD', 'N27_S9_TD', 'N21_S9_TD', 'N15_S9_TD', 'N9_S9_TD', 
                  'N3_S9_TD', 'T3_S9_TD', 'T9_S9_TD', 'T15_S9_TD', 'T21_S9_TD', 'T27_S9_TD', 'N27_S3_TD', 
                  'N21_S3_TD', 'N15_S3_TD', 'N9_S3_TD', 'N3_S3_TD', 'T3_S3_TD', 'T9_S3_TD', 'T21_S3_TD', 
                  'T27_S3_TD', 'N27_I3_TD', 'N21_I3_TD', 'N15_I3_TD', 'N9_I3_TD', 'N3_I3_TD', 'T3_I3_TD', 
                  'T9_I3_TD', 'T21_I3_TD', 'T27_I3_TD', 'N27_I9_TD', 'N21_I9_TD', 'N15_I9_TD', 'N9_I9_TD',
                  'N3_I9_TD', 'T3_I9_TD', 'T9_I9_TD', 'T15_I9_TD', 'T21_I9_TD', 'T27_I9_TD', 'N21_I15_TD', 
                  'N15_I15_TD', 'N9_I15_TD', 'N3_I15_TD', 'T3_I15_TD', 'T9_I15_TD', 'T15_I15_TD', 
                  'T21_I15_TD', 'N15_I21_TD', 'N9_I21_TD', 'N3_I21_TD', 'T3_I21_TD', 'T9_I21_TD', 
                  'T15_I21_TD', 'N9_I27_TD', 'N3_I27_TD', 'T3_I27_TD', 'T9_I27_TD', 'N9_S27_PD', 
                  'N3_S27_PD', 'T3_S27_PD', 'T9_S27_PD', 'N15_S21_PD', 'N9_S21_PD', 'N3_S21_PD', 
                  'T3_S21_PD', 'T9_S21_PD', 'T15_S21_PD', 'N21_S15_PD', 'N15_S15_PD', 'N9_S15_PD', 
                  'N3_S15_PD', 'T3_S15_PD', 'T9_S15_PD', 'T15_S15_PD', 'T21_S15_PD', 'N27_S9_PD', 
                  'N21_S9_PD', 'N15_S9_PD', 'N9_S9_PD', 'N3_S9_PD', 'T3_S9_PD', 'T9_S9_PD', 'T15_S9_PD', 
                  'T21_S9_PD', 'T27_S9_PD', 'N27_S3_PD', 'N21_S3_PD', 'N15_S3_PD', 'N9_S3_PD', 'N3_S3_PD',
                  'T3_S3_PD', 'T9_S3_PD', 'T21_S3_PD', 'T27_S3_PD', 'N27_I3_PD', 'N21_I3_PD', 'N15_I3_PD',
                  'N9_I3_PD', 'N3_I3_PD', 'T3_I3_PD', 'T9_I3_PD', 'T21_I3_PD', 'T27_I3_PD', 'N27_I9_PD', 
                  'N21_I9_PD', 'N15_I9_PD', 'N9_I9_PD', 'N3_I9_PD', 'T3_I9_PD', 'T9_I9_PD', 'T15_I9_PD', 
                  'T21_I9_PD', 'T27_I9_PD', 'N21_I15_PD', 'N15_I15_PD', 'N9_I15_PD', 'N3_I15_PD', 
                  'T3_I15_PD', 'T9_I15_PD', 'T15_I15_PD', 'T21_I15_PD', 'N15_I21_PD', 'N9_I21_PD', 
                  'N3_I21_PD', 'T3_I21_PD', 'T9_I21_PD', 'T15_I21_PD', 'N9_I27_PD', 'N3_I27_PD', 
                  'T3_I27_PD', 'T9_I27_PD', 'N9_S27_TDP', 'N3_S27_TDP', 'T3_S27_TDP', 'T9_S27_TDP', 
                  'N15_S21_TDP', 'N9_S21_TDP', 'N3_S21_TDP', 'T3_S21_TDP', 'T9_S21_TDP', 'T15_S21_TDP', 
                  'N21_S15_TDP', 'N15_S15_TDP', 'N9_S15_TDP', 'N3_S15_TDP', 'T3_S15_TDP', 'T9_S15_TDP', 
                  'T15_S15_TDP', 'T21_S15_TDP', 'N27_S9_TDP', 'N21_S9_TDP', 'N15_S9_TDP', 'N9_S9_TDP', 
                  'N3_S9_TDP', 'T3_S9_TDP', 'T9_S9_TDP', 'T15_S9_TDP', 'T21_S9_TDP', 'T27_S9_TDP', 
                  'N27_S3_TDP', 'N21_S3_TDP', 'N15_S3_TDP', 'N9_S3_TDP', 'N3_S3_TDP', 'T3_S3_TDP', 
                  'T9_S3_TDP', 'T21_S3_TDP', 'T27_S3_TDP', 'N27_I3_TDP', 'N21_I3_TDP', 'N15_I3_TDP', 
                  'N9_I3_TDP', 'N3_I3_TDP', 'T3_I3_TDP', 'T9_I3_TDP', 'T21_I3_TDP', 'T27_I3_TDP', 
                  'N27_I9_TDP', 'N21_I9_TDP', 'N15_I9_TDP', 'N9_I9_TDP', 'N3_I9_TDP', 'T3_I9_TDP', 
                  'T9_I9_TDP', 'T15_I9_TDP', 'T21_I9_TDP', 'T27_I9_TDP', 'N21_I15_TDP', 'N15_I15_TDP', 
                  'N9_I15_TDP', 'N3_I15_TDP', 'T3_I15_TDP', 'T9_I15_TDP', 'T15_I15_TDP', 'T21_I15_TDP', 
                  'N15_I21_TDP', 'N9_I21_TDP', 'N3_I21_TDP', 'T3_I21_TDP', 'T9_I21_TDP', 'T15_I21_TDP',
                  'N9_I27_TDP', 'N3_I27_TDP', 'T3_I27_TDP', 'T9_I27_TDP', 'N9_S27_PDP', 'N3_S27_PDP',
                  'T3_S27_PDP', 'T9_S27_PDP', 'N15_S21_PDP', 'N9_S21_PDP', 'N3_S21_PDP', 'T3_S21_PDP', 
                  'T9_S21_PDP', 'T15_S21_PDP', 'N21_S15_PDP', 'N15_S15_PDP', 'N9_S15_PDP', 'N3_S15_PDP',
                  'T3_S15_PDP', 'T9_S15_PDP', 'T15_S15_PDP', 'T21_S15_PDP', 'N27_S9_PDP', 'N21_S9_PDP',
                  'N15_S9_PDP', 'N9_S9_PDP', 'N3_S9_PDP', 'T3_S9_PDP', 'T9_S9_PDP', 'T15_S9_PDP', 
                  'T21_S9_PDP', 'T27_S9_PDP', 'N27_S3_PDP', 'N21_S3_PDP', 'N15_S3_PDP', 'N9_S3_PDP', 
                  'N3_S3_PDP', 'T3_S3_PDP', 'T9_S3_PDP', 'T21_S3_PDP', 'T27_S3_PDP', 'N27_I3_PDP', 
                  'N21_I3_PDP', 'N15_I3_PDP', 'N9_I3_PDP', 'N3_I3_PDP', 'T3_I3_PDP', 'T9_I3_PDP', 
                  'T21_I3_PDP', 'T27_I3_PDP', 'N27_I9_PDP', 'N21_I9_PDP', 'N15_I9_PDP', 'N9_I9_PDP', 
                  'N3_I9_PDP', 'T3_I9_PDP', 'T9_I9_PDP', 'T15_I9_PDP', 'T21_I9_PDP', 'T27_I9_PDP', 
                  'N21_I15_PDP', 'N15_I15_PDP', 'N9_I15_PDP', 'N3_I15_PDP', 'T3_I15_PDP', 'T9_I15_PDP',
                  'T15_I15_PDP', 'T21_I15_PDP', 'N15_I21_PDP', 'N9_I21_PDP', 'N3_I21_PDP', 'T3_I21_PDP', 
                  'T9_I21_PDP', 'T15_I21_PDP', 'N9_I27_PDP', 'N3_I27_PDP', 'T3_I27_PDP', 'T9_I27_PDP',
                  'cAutoQCStatus', 'QCFieldUsable', 'QCReliable', 'cQCFN33Status', 'cQCAHSManualStatus',
                  'cQCRimArtifactStatus', 'cQCInattentionStatus', 'cQCLearningEffectStatus', 
                  'cQCFatigueStatus', 'cQCFixationStatus', 'cQCOtherDefectStatus', 
                  'cQCUnreliableByTechnicianStatus', 'cQCUnaccPupilSizeStatus', 'VFI', 
                  'kPrevUsable_ExamTimeStamp', 'aeExamTimeStamp', 'kNextUsable_ExamTimeStamp', 
                  'kPrevUsable_FLAGAssessment', 'kNextUsable_FLAGAssessment', 
                  'cFLAG_Confirmation_Status_ByTestType', 'cIsABNORMAL_FLAG_Confirmed_ByTestType', 
                  'cIsNORMAL_FLAG_Confirmed_ByTestType', 'cCnt_TDP_LessThan5', 'cCnt_PDP_LessThan5',
                  'cCnt_TDP_LessThan2', 'cCnt_PDP_LessThan2', 'cCnt_TDP_LessThan1', 'cCnt_PDP_LessThan1',
                  'cCnt_TDP_LessThan05', 'cCnt_PDP_LessThan05', 'kUsedADAGESBL09', 'sFLAGAbn3ConsecConfirmed',
                  'sFLAGAbn3ConsecUnconfirmed', 'sFLAGNorm3ConsecConfirmed', 'sFLAGNorm3ConsecUnconfirmed', 
                  'LowPatientReliabilityStatus')
  
  reg_cols <- c('birthdate', 'visitdate', 'site', 'instrumentmodelnumber', 
                'examtime', 'trialrxsphere', 'trialrxcylinder', 'trialrxaxis', 
                'ght', 'mdprobability', 'psdprobability', 'cpsdprobability', 'sfprobability', 
                'fovealthresholdprobability')
  
  odd_cols <- c('aedob', 'aeexamdate', 'eye', 'instrumentmodel', 
                'aeexamtime', 'trialrxsphereraw', 'trialrxcylraw', 'trialrxaxisraw', 
                'ghttype', 'mdprob', 'psdprob', 'cpsdprob', 'sfprob', 
                'fovealthresholdprob')
  
  # Create similar column names
  cols1 <- tolower(gsub("_", "", names(o242), fixed = TRUE))
  cols1 <- jReplacer(cols1, reg_cols, odd_cols)
  cols2 <- tolower(gsub("_", "", qcNames242, fixed = TRUE))
  
  fill  <- which(cols1 %in% cols2)
  
  # Populate the new data frame with the known values
  o242_QC <- o242[, fill]
  colnames(o242_QC) <- cols1[fill]
  
  # Fill in the unknown values with an empty string
  o242_QC[, cols2[which(!cols2 %in% names(o242_QC))]] <- ""
  
  # Rearrange and rename the columns
  o242_QC <- o242_QC[, cols2]
  colnames(o242_QC) <- qcNames242

  o242_QC[ , 'aeFixationCheckPercentage'] <- (as.numeric(o242[, 'FIXATION_CHECK_ERRORS']) / 
                                                as.numeric(o242[, 'FIXATION_CHECK_TRIALS'])) *100
  
  write.csv(o242_QC, file.path(XML_DIRECTORY, "test24-2_QC.csv"), row.names = FALSE,
            na = "")
}

####################################################################################################
## Post-process 24-2C                                                                             ##
####################################################################################################

if (length(l242C) > 0) {
  o242C <- do.call("rbind", l242C)
  o242C <- matrix(sapply(o242C, function(x) ifelse(is.null(x), "", x)), nrow = nrow(o242C))
  o242C <- data.frame(o242C)
  for (j in 1:ncol(o242C)) {
    if (class(o242C[, j]) == "list") {
      o242C[, j] <- unlist(o242C[, j])
    }
  }
  colnames(o242C) <- n242C
  o242C$BIRTH_DATE <- jDate(o242C$BIRTH_DATE)
  
  ## Date and time for HFA2
  o242C$EXAM_TIME[unlist(o242C$INSTRUMENT_NAME) == "HFA II-i"] <-
    jTime(unlist(lapply(strsplit(o242C$VISIT_DATE[unlist(o242C$INSTRUMENT_NAME) == "HFA II-i"], ".",
                                 fixed = TRUE),
                        function (x) substr(x[11], 9, 14))))
  o242C$VISIT_DATE[o242C$INSTRUMENT_NAME == "HFA II-i"] <-
    jDate(unlist(lapply(strsplit(o242C$VISIT_DATE[o242C$INSTRUMENT_NAME == "HFA II-i"], ".",
                                 fixed = TRUE),
                        function (x) substr(x[11], 1, 8))))
  
  ## Date and time for HFA3
  o242C$EXAM_TIME[o242C$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")] <-
    jTime(o242C$EXAM_TIME[o242C$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")])
  o242C$VISIT_DATE[o242C$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")] <-
    jDate(o242C$VISIT_DATE[o242C$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")])
  
  o242C$SITE <- jReplacer(o242C$SITE, c("R", "L"), c("OD", "OS"))
  o242C$INSTRUMENT_MODEL_NUMBER  <- unlist(lapply(strsplit(o242C$INSTRUMENT_MODEL_NUMBER, "-"),
                                                 function (x) x[1]))
  o242C$INSTRUMENT_SERIAL_NUMBER <- unlist(lapply(strsplit(o242C$INSTRUMENT_SERIAL_NUMBER, "-"),
                                                 function (x) x[2]))
  
  ## Exam Duration Fix
  o242C$EXAM_DURATION <- jTime3(o242C$EXAM_DURATION)
  
  ## PDP / TDP Fix
  o242C[, c(grep("TDP", names(o242C)), grep("PDP", names(o242C)))] <-
    jReplacer(o242C[, c(grep("TDP", names(o242C)), grep("PDP", names(o242C)))],
              c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
              c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  o242C[, "MD_PROBABILITY"] <- jReplacer(o242C[, "MD_PROBABILITY"],
                                        c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                        c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  o242C[, "PSD_PROBABILITY"] <- jReplacer(o242C[, "PSD_PROBABILITY"],
                                         c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                         c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  ## "Blind Spot" Fix
  o242C[, c("T15_S3_TD", "T15_I3_TD", "T15_S3_TDP", "T15_I3_TDP",
           "T15_S3_PD", "T15_I3_PD", "T15_S3_PDP", "T15_I3_PDP")] <- ""
  
  # Other Fixes
  o242C$TEST_TYPE     <- jReplacer(o242C$TEST_TYPE, "Diagnostic", "Threshold")
  #o242C$TEST_PATTERN  <- jReplacer(o242C$TEST_PATTERN, "24-2", "Central 24-2")
  #o302$TEST_PATTERN  <- "C30-2"
  #o242$TEST_STRATEGY <- jReplacer(o242$TEST_STRATEGY, "SITA-Fast", "SITA Fast")
  o242C$TEST_STRATEGY <- gsub("-", " ", o242C$TEST_STRATEGY, fixed = TRUE)
  o242C$STIMULUS_SIZE <- jReplacer(o242C$STIMULUS_SIZE, "0.14748032", "III")
  
  o242C$FOVEAL_THRESHOLD_PROBABILITY <- jReplacer(o242C$FOVEAL_THRESHOLD_PROBABILITY,
                                                 c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                                 c("Not Significant", "< 0.5%", "< 1%", "< 2%", 
                                                   "< 5%", "< 10%"))
  o242C$PSD_PROBABILITY <- jReplacer(o242C$PSD_PROBABILITY,
                                    c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                    c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  
  ## Bandaid for Date / Time
  if (any(o242C$VISIT_DATE == "NA-NA-NA")) {
    o242C[which(o242C$VISIT_DATE == "NA-NA-NA"), c("VISIT_DATE", "EXAM_TIME")] <-
      cbind(jDate(o242C$BACKUP_DATE[which(o242C$VISIT_DATE == "NA-NA-NA")]),
            jTime(o242C$BACKUP_TIME[which(o242C$VISIT_DATE == "NA-NA-NA")]))
  }
  
  o242C$SERIES_DATE_TIME <- paste(o242C$VISIT_DATE, o242C$EXAM_TIME, sep = "T")
  o242C <- o242C[, -which(names(o242C) %in% c("BACKUP_TIME", "BACKUP_DATE"))]
  
  o242C[o242C == "XXXXXXNOTFOUND"] <- ""
  
  write.csv(o242C, file.path(XML_DIRECTORY, "test24-2C.csv"), row.names = FALSE,
            na = "")
  cat("...24-2 file written (", nrow(o242C), " files).\n", sep = "")
  
  ## "Calculated" version - WIP as no fields are actually filled in
  o242C$XMLSourceFileName <- nn242C
  cNames242C <- c('HFA2_SOFTWARE_VERSION', 'HFA_XML_VERSION', 'HGCParserVersion', 'XMLSourceFileName', 
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
                 'T15_I21_Thr', 'N9_I27_Thr', 'N3_I27_Thr', 'T3_I27_Thr', 'T9_I27_Thr', "T7_S5_Thr", 
                 "T5_S7_Thr", "T1_S5_Thr", "N1_S5_Thr", "N5_S1_Thr", "N7_I1_Thr", "N7_I5_Thr", 
                 "N1_I5_Thr", "T1_I9_Thr", "T5_I7_Thr",
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
                 'T9_I27_TD', "T7_S5_TD", "T5_S7_TD", "T1_S5_TD", "N1_S5_TD", "N5_S1_TD", 
                 "N7_I1_TD", "N7_I5_TD", "N1_I5_TD", "T1_I9_TD", "T5_I7_TD",
                 'NUM_PATTERN_DEV_VALUE_POINTS', 'N9_S27_PD', 'N3_S27_PD', 'T3_S27_PD', 
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
                 'T9_I27_PD', "T7_S5_PD", "T5_S7_PD", "T1_S5_PD", "N1_S5_PD", "N5_S1_PD", 
                 "N7_I1_PD", "N7_I5_PD", "N1_I5_PD", "T1_I9_PD", "T5_I7_PD",
                 'NUM_TOTAL_DEV_PROB_POINTS', 'N9_S27_TDP', 'N3_S27_TDP', 'T3_S27_TDP', 
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
                 'T9_I27_TDP', "T7_S5_TDP", "T5_S7_TDP", "T1_S5_TDP", "N1_S5_TDP", "N5_S1_TDP", 
                 "N7_I1_TDP", "N7_I5_TDP", "N1_I5_TDP", "T1_I9_TDP", "T5_I7_TDP",
                 'NUM_PATTERN_DEV_PROB_POINTS', 'N9_S27_PDP', 'N3_S27_PDP', 'T3_S27_PDP', 
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
                 'T9_I27_PDP', "T7_S5_PDP", "T5_S7_PDP", "T1_S5_PDP", "N1_S5_PDP", "N5_S1_PDP", 
                 "N7_I1_PDP", "N7_I5_PDP", "N1_I5_PDP", "T1_I9_PDP", "T5_I7_PDP",
                 'SUPERIOR_SCORE', 'INFERIOR_SCORE', 'NASAL_SCORE', 'AGIS_SCORE', 
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
  
  o242C_FMP <- o242C[, which(names(o242C) %in% cNames242C)]
  o242C_FMP[, cNames242C[!cNames242C %in% names(o242C_FMP)]] <- ""
  o242C_FMP <- o242C_FMP[, cNames242C]
  
  write.csv(o242C_FMP, file.path(XML_DIRECTORY, "test24-2C_FMP.csv"), row.names = FALSE,
            na = "")
  
  # QC Version
  qcNames242C <- c('Batch_UID', 'Exam_UID', 'InstrumentModel', 'InstrumentSerialNumber',                 #
                  'InstrumentSoftwareVersion', 'PatientID', 'StudyCode', 'aeDOB', 'Eye', 'SeriesDateTime', 
                  'aeExamDate', 'aeExamTime', 'ExamDuration', 'aeIsShileyClinicHFAExam', 'aeDIGSTestType', 
                  'TestType', 'TestPattern', 'TestStrategy', 'StimulusColor', 'StimulusSize', 
                  'BackgroundColor', 'FixationTarget', 'FixationMonitor', 'TrialRXSphereRaw', 
                  'TrialRXCylRaw', 'TrialRXAxisRaw', 'PupilDiameter', 'VAType', 'BlindSpotX', 
                  'BlindSpotY', 'BlindSpotStimulusSize', 'FalseNegativePercent', 'FalsePositivePercent',
                  'aeFixationCheckPercentage', 'FovealResult', 'FovealThreshold', 'ClinicalNotes', 
                  'SFStatus', 'SF', 'SFProb', 'SWAPFTGeneralHeight', 'GHTType', 'MD', 'MDProb', 'PSD', 
                  'PSDProb', 'CPSD', 'CPSDProb', 'FovealThresholdProb', 'aePDPCen4LT5Count', 
                  'aeHasHighRawThreshold', 'FLAGAssessment', 'FLAGSeverity', 'AGISScore', 'AGISNas', 
                  'AGISInf', 'AGISSup', 'GHSupThrSum', 'GHSupThrMean', 'GHSupThrStd', 'GHSupTDSum', 
                  'GHSupTDMean', 'GHSupTDStd', 'GHSupPDSum', 'GHSupPDMean', 'GHSupPDStd', 
                  'GHSupPDCntLT50p', 'GHSupPDCntLT10p', 'GHSupNasThrSum', 'GHSupNasThrMean', 
                  'GHSupNasThrStd', 'GHSupNasTDSum', 'GHSupNasTDMean', 'GHSupNasTDStd', 'GHSupNasPDSum', 
                  'GHSupNasPDMean', 'GHSupNasPDStd', 'GHSupNasPDCntLT50p', 'GHSupNasPDCntLT10p', 
                  'GHInfThrSum', 'GHInfThrMean', 'GHInfThrStd', 'GHInfTDSum', 'GHInfTDMean',
                  'GHInfTDStd', 'GHInfPDSum', 'GHInfPDMean', 'GHInfPDStd', 'GHInfPDCntLT50p', 
                  'GHInfPDCntLT10p', 'GHInfNasThrSum', 'GHInfNasThrMean', 'GHInfNasThrStd', 'GHInfNasTDSum',
                  'GHInfNasTDMean', 'GHInfNasTDStd', 'GHInfNasPDSum', 'GHInfNasPDMean', 'GHInfNasPDStd', 
                  'GHInfNasPDCntLT50p', 'GHInfNasPDCntLT10p', 'GHCentralThrSum', 'GHCentralThrMean',
                  'GHCentralThrStd', 'GHCentralTDSum', 'GHCentralTDMean', 'GHCentralTDStd', 
                  'GHCentralPDSum', 'GHCentralPDMean', 'GHCentralPDStd', 'GHCentralPDCntLT50p', 
                  'GHCentralPDCntLT10p', 'GHTemporalThrSum', 'GHTemporalThrMean', 'GHTemporalThrStd', 
                  'GHTemporalTDSum', 'GHTemporalTDMean', 'GHTemporalTDStd', 'GHTemporalPDSum', 
                  'GHTemporalPDMean', 'GHTemporalPDStd', 'GHTemporalPDCntLT50p', 'GHTemporalPDCntLT10p', 
                  'N9_S27_Thr', 'N3_S27_Thr', 'T3_S27_Thr', 'T9_S27_Thr', 'N15_S21_Thr', 'N9_S21_Thr', 
                  'N3_S21_Thr', 'T3_S21_Thr', 'T9_S21_Thr', 'T15_S21_Thr', 'N21_S15_Thr', 'N15_S15_Thr',
                  'N9_S15_Thr', 'N3_S15_Thr', 'T3_S15_Thr', 'T9_S15_Thr', 'T15_S15_Thr', 'T21_S15_Thr', 
                  'N27_S9_Thr', 'N21_S9_Thr', 'N15_S9_Thr', 'N9_S9_Thr', 'N3_S9_Thr', 'T3_S9_Thr', 
                  'T9_S9_Thr', 'T15_S9_Thr', 'T21_S9_Thr', 'T27_S9_Thr', 'N27_S3_Thr', 'N21_S3_Thr', 
                  'N15_S3_Thr', 'N9_S3_Thr', 'N3_S3_Thr', 'T3_S3_Thr', 'T9_S3_Thr', 'T21_S3_Thr', 
                  'T27_S3_Thr', 'N27_I3_Thr', 'N21_I3_Thr', 'N15_I3_Thr', 'N9_I3_Thr', 'N3_I3_Thr', 
                  'T3_I3_Thr', 'T9_I3_Thr', 'T21_I3_Thr', 'T27_I3_Thr', 'N27_I9_Thr', 'N21_I9_Thr', 
                  'N15_I9_Thr', 'N9_I9_Thr', 'N3_I9_Thr', 'T3_I9_Thr', 'T9_I9_Thr', 'T15_I9_Thr', 
                  'T21_I9_Thr', 'T27_I9_Thr', 'N21_I15_Thr', 'N15_I15_Thr', 'N9_I15_Thr', 'N3_I15_Thr', 
                  'T3_I15_Thr', 'T9_I15_Thr', 'T15_I15_Thr', 'T21_I15_Thr', 'N15_I21_Thr', 'N9_I21_Thr', 
                  'N3_I21_Thr', 'T3_I21_Thr', 'T9_I21_Thr', 'T15_I21_Thr', 'N9_I27_Thr', 'N3_I27_Thr', 
                  'T3_I27_Thr', 'T9_I27_Thr', "T7_S5_Thr", "T5_S7_Thr", "T1_S5_Thr", "N1_S5_Thr", 
                  "N5_S1_Thr", "N7_I1_Thr", "N7_I5_Thr", "N1_I5_Thr", "T1_I9_Thr", "T5_I7_Thr",
                  'N9_S27_TD', 'N3_S27_TD', 'T3_S27_TD', 'T9_S27_TD', 
                  'N15_S21_TD', 'N9_S21_TD', 'N3_S21_TD', 'T3_S21_TD', 'T9_S21_TD', 'T15_S21_TD', 
                  'N21_S15_TD', 'N15_S15_TD', 'N9_S15_TD', 'N3_S15_TD', 'T3_S15_TD', 'T9_S15_TD', 
                  'T15_S15_TD', 'T21_S15_TD', 'N27_S9_TD', 'N21_S9_TD', 'N15_S9_TD', 'N9_S9_TD', 
                  'N3_S9_TD', 'T3_S9_TD', 'T9_S9_TD', 'T15_S9_TD', 'T21_S9_TD', 'T27_S9_TD', 'N27_S3_TD', 
                  'N21_S3_TD', 'N15_S3_TD', 'N9_S3_TD', 'N3_S3_TD', 'T3_S3_TD', 'T9_S3_TD', 'T21_S3_TD', 
                  'T27_S3_TD', 'N27_I3_TD', 'N21_I3_TD', 'N15_I3_TD', 'N9_I3_TD', 'N3_I3_TD', 'T3_I3_TD', 
                  'T9_I3_TD', 'T21_I3_TD', 'T27_I3_TD', 'N27_I9_TD', 'N21_I9_TD', 'N15_I9_TD', 'N9_I9_TD',
                  'N3_I9_TD', 'T3_I9_TD', 'T9_I9_TD', 'T15_I9_TD', 'T21_I9_TD', 'T27_I9_TD', 'N21_I15_TD', 
                  'N15_I15_TD', 'N9_I15_TD', 'N3_I15_TD', 'T3_I15_TD', 'T9_I15_TD', 'T15_I15_TD', 
                  'T21_I15_TD', 'N15_I21_TD', 'N9_I21_TD', 'N3_I21_TD', 'T3_I21_TD', 'T9_I21_TD', 
                  'T15_I21_TD', 'N9_I27_TD', 'N3_I27_TD', 'T3_I27_TD', 'T9_I27_TD', "T7_S5_TD", 
                  "T5_S7_TD", "T1_S5_TD", "N1_S5_TD", "N5_S1_TD", "N7_I1_TD", "N7_I5_TD", "N1_I5_TD", 
                  "T1_I9_TD", "T5_I7_TD",'N9_S27_PD', 
                  'N3_S27_PD', 'T3_S27_PD', 'T9_S27_PD', 'N15_S21_PD', 'N9_S21_PD', 'N3_S21_PD', 
                  'T3_S21_PD', 'T9_S21_PD', 'T15_S21_PD', 'N21_S15_PD', 'N15_S15_PD', 'N9_S15_PD', 
                  'N3_S15_PD', 'T3_S15_PD', 'T9_S15_PD', 'T15_S15_PD', 'T21_S15_PD', 'N27_S9_PD', 
                  'N21_S9_PD', 'N15_S9_PD', 'N9_S9_PD', 'N3_S9_PD', 'T3_S9_PD', 'T9_S9_PD', 'T15_S9_PD', 
                  'T21_S9_PD', 'T27_S9_PD', 'N27_S3_PD', 'N21_S3_PD', 'N15_S3_PD', 'N9_S3_PD', 'N3_S3_PD',
                  'T3_S3_PD', 'T9_S3_PD', 'T21_S3_PD', 'T27_S3_PD', 'N27_I3_PD', 'N21_I3_PD', 'N15_I3_PD',
                  'N9_I3_PD', 'N3_I3_PD', 'T3_I3_PD', 'T9_I3_PD', 'T21_I3_PD', 'T27_I3_PD', 'N27_I9_PD', 
                  'N21_I9_PD', 'N15_I9_PD', 'N9_I9_PD', 'N3_I9_PD', 'T3_I9_PD', 'T9_I9_PD', 'T15_I9_PD', 
                  'T21_I9_PD', 'T27_I9_PD', 'N21_I15_PD', 'N15_I15_PD', 'N9_I15_PD', 'N3_I15_PD', 
                  'T3_I15_PD', 'T9_I15_PD', 'T15_I15_PD', 'T21_I15_PD', 'N15_I21_PD', 'N9_I21_PD', 
                  'N3_I21_PD', 'T3_I21_PD', 'T9_I21_PD', 'T15_I21_PD', 'N9_I27_PD', 'N3_I27_PD', 
                  'T3_I27_PD', 'T9_I27_PD', "T7_S5_PD", "T5_S7_PD", "T1_S5_PD", "N1_S5_PD", "N5_S1_PD", 
                  "N7_I1_PD", "N7_I5_PD", "N1_I5_PD", "T1_I9_PD", "T5_I7_PD",
                  'N9_S27_TDP', 'N3_S27_TDP', 'T3_S27_TDP', 'T9_S27_TDP', 
                  'N15_S21_TDP', 'N9_S21_TDP', 'N3_S21_TDP', 'T3_S21_TDP', 'T9_S21_TDP', 'T15_S21_TDP', 
                  'N21_S15_TDP', 'N15_S15_TDP', 'N9_S15_TDP', 'N3_S15_TDP', 'T3_S15_TDP', 'T9_S15_TDP', 
                  'T15_S15_TDP', 'T21_S15_TDP', 'N27_S9_TDP', 'N21_S9_TDP', 'N15_S9_TDP', 'N9_S9_TDP', 
                  'N3_S9_TDP', 'T3_S9_TDP', 'T9_S9_TDP', 'T15_S9_TDP', 'T21_S9_TDP', 'T27_S9_TDP', 
                  'N27_S3_TDP', 'N21_S3_TDP', 'N15_S3_TDP', 'N9_S3_TDP', 'N3_S3_TDP', 'T3_S3_TDP', 
                  'T9_S3_TDP', 'T21_S3_TDP', 'T27_S3_TDP', 'N27_I3_TDP', 'N21_I3_TDP', 'N15_I3_TDP', 
                  'N9_I3_TDP', 'N3_I3_TDP', 'T3_I3_TDP', 'T9_I3_TDP', 'T21_I3_TDP', 'T27_I3_TDP', 
                  'N27_I9_TDP', 'N21_I9_TDP', 'N15_I9_TDP', 'N9_I9_TDP', 'N3_I9_TDP', 'T3_I9_TDP', 
                  'T9_I9_TDP', 'T15_I9_TDP', 'T21_I9_TDP', 'T27_I9_TDP', 'N21_I15_TDP', 'N15_I15_TDP', 
                  'N9_I15_TDP', 'N3_I15_TDP', 'T3_I15_TDP', 'T9_I15_TDP', 'T15_I15_TDP', 'T21_I15_TDP', 
                  'N15_I21_TDP', 'N9_I21_TDP', 'N3_I21_TDP', 'T3_I21_TDP', 'T9_I21_TDP', 'T15_I21_TDP',
                  'N9_I27_TDP', 'N3_I27_TDP', 'T3_I27_TDP', 'T9_I27_TDP', "T7_S5_TDP", "T5_S7_TDP", 
                  "T1_S5_TDP", "N1_S5_TDP", "N5_S1_TDP", "N7_I1_TDP", "N7_I5_TDP", "N1_I5_TDP", 
                  "T1_I9_TDP", "T5_I7_TDP",
                  'T3_S27_PDP', 'T9_S27_PDP', 'N15_S21_PDP', 'N9_S21_PDP', 'N3_S21_PDP', 'T3_S21_PDP', 
                  'T9_S21_PDP', 'T15_S21_PDP', 'N21_S15_PDP', 'N15_S15_PDP', 'N9_S15_PDP', 'N3_S15_PDP',
                  'T3_S15_PDP', 'T9_S15_PDP', 'T15_S15_PDP', 'T21_S15_PDP', 'N27_S9_PDP', 'N21_S9_PDP',
                  'N15_S9_PDP', 'N9_S9_PDP', 'N3_S9_PDP', 'T3_S9_PDP', 'T9_S9_PDP', 'T15_S9_PDP', 
                  'T21_S9_PDP', 'T27_S9_PDP', 'N27_S3_PDP', 'N21_S3_PDP', 'N15_S3_PDP', 'N9_S3_PDP', 
                  'N3_S3_PDP', 'T3_S3_PDP', 'T9_S3_PDP', 'T21_S3_PDP', 'T27_S3_PDP', 'N27_I3_PDP', 
                  'N21_I3_PDP', 'N15_I3_PDP', 'N9_I3_PDP', 'N3_I3_PDP', 'T3_I3_PDP', 'T9_I3_PDP', 
                  'T21_I3_PDP', 'T27_I3_PDP', 'N27_I9_PDP', 'N21_I9_PDP', 'N15_I9_PDP', 'N9_I9_PDP', 
                  'N3_I9_PDP', 'T3_I9_PDP', 'T9_I9_PDP', 'T15_I9_PDP', 'T21_I9_PDP', 'T27_I9_PDP', 
                  'N21_I15_PDP', 'N15_I15_PDP', 'N9_I15_PDP', 'N3_I15_PDP', 'T3_I15_PDP', 'T9_I15_PDP',
                  'T15_I15_PDP', 'T21_I15_PDP', 'N15_I21_PDP', 'N9_I21_PDP', 'N3_I21_PDP', 'T3_I21_PDP', 
                  'T9_I21_PDP', 'T15_I21_PDP', 'N9_I27_PDP', 'N3_I27_PDP', 'T3_I27_PDP', 'T9_I27_PDP',
                  "T7_S5_PDP", "T5_S7_PDP", "T1_S5_PDP", "N1_S5_PDP", "N5_S1_PDP", "N7_I1_PDP", 
                  "N7_I5_PDP", "N1_I5_PDP", "T1_I9_PDP", "T5_I7_PDP",
                  'cAutoQCStatus', 'QCFieldUsable', 'QCReliable', 'cQCFN33Status', 'cQCAHSManualStatus',
                  'cQCRimArtifactStatus', 'cQCInattentionStatus', 'cQCLearningEffectStatus', 
                  'cQCFatigueStatus', 'cQCFixationStatus', 'cQCOtherDefectStatus', 
                  'cQCUnreliableByTechnicianStatus', 'cQCUnaccPupilSizeStatus', 'VFI', 
                  'kPrevUsable_ExamTimeStamp', 'aeExamTimeStamp', 'kNextUsable_ExamTimeStamp', 
                  'kPrevUsable_FLAGAssessment', 'kNextUsable_FLAGAssessment', 
                  'cFLAG_Confirmation_Status_ByTestType', 'cIsABNORMAL_FLAG_Confirmed_ByTestType', 
                  'cIsNORMAL_FLAG_Confirmed_ByTestType', 'cCnt_TDP_LessThan5', 'cCnt_PDP_LessThan5',
                  'cCnt_TDP_LessThan2', 'cCnt_PDP_LessThan2', 'cCnt_TDP_LessThan1', 'cCnt_PDP_LessThan1',
                  'cCnt_TDP_LessThan05', 'cCnt_PDP_LessThan05', 'kUsedADAGESBL09', 'sFLAGAbn3ConsecConfirmed',
                  'sFLAGAbn3ConsecUnconfirmed', 'sFLAGNorm3ConsecConfirmed', 'sFLAGNorm3ConsecUnconfirmed', 
                  'LowPatientReliabilityStatus')
  
  reg_cols <- c('birthdate', 'visitdate', 'site', 'instrumentmodelnumber', 
                'examtime', 'trialrxsphere', 'trialrxcylinder', 'trialrxaxis', 
                'ght', 'mdprobability', 'psdprobability', 'cpsdprobability', 'sfprobability', 
                'fovealthresholdprobability')
  
  odd_cols <- c('aedob', 'aeexamdate', 'eye', 'instrumentmodel', 
                'aeexamtime', 'trialrxsphereraw', 'trialrxcylraw', 'trialrxaxisraw', 
                'ghttype', 'mdprob', 'psdprob', 'cpsdprob', 'sfprob', 
                'fovealthresholdprob')
  
  # Create similar column names
  cols1 <- tolower(gsub("_", "", names(o242C), fixed = TRUE))
  cols1 <- jReplacer(cols1, reg_cols, odd_cols)
  cols2 <- tolower(gsub("_", "", qcNames242C, fixed = TRUE))
  
  fill  <- which(cols1 %in% cols2)
  
  # Populate the new data frame with the known values
  o242C_QC <- o242C[, fill]
  colnames(o242C_QC) <- cols1[fill]
  
  # Fill in the unknown values with an empty string
  o242C_QC[, cols2[which(!cols2 %in% names(o242C_QC))]] <- ""
  
  # Rearrange and rename the columns
  o242C_QC <- o242C_QC[, cols2]
  colnames(o242C_QC) <- qcNames242C
  
  o242C_QC[ , 'aeFixationCheckPercentage'] <- (as.numeric(o242C[, 'FIXATION_CHECK_ERRORS']) / 
                                                as.numeric(o242C[, 'FIXATION_CHECK_TRIALS'])) *100
  
  write.csv(o242C_QC, file.path(XML_DIRECTORY, "test24-2C_QC.csv"), row.names = FALSE,
            na = "")
}

####################################################################################################
## Post-process 10-2                                                                              ##
####################################################################################################

if (length(l102) > 0) {
  o102 <- do.call("rbind", l102)
  o102 <- matrix(sapply(o102, function(x) ifelse(is.null(x), "", x)), nrow = nrow(o102))
  o102 <- data.frame(o102)
  for (j in 1:ncol(o102)) {
    if (class(o102[, j]) == "list") {
      o102[, j] <- unlist(o102[, j])
    }
  }
  colnames(o102) <- n102
  o102$BIRTH_DATE <- jDate(o102$BIRTH_DATE)
  
  ## Date and time for HFA2
  o102$EXAM_TIME[unlist(o102$INSTRUMENT_NAME) == "HFA II-i"] <-
    jTime(unlist(lapply(strsplit(o102$VISIT_DATE[unlist(o102$INSTRUMENT_NAME) == "HFA II-i"], ".",
                                 fixed = TRUE),
                        function (x) substr(x[11], 9, 14))))
  o102$VISIT_DATE[o102$INSTRUMENT_NAME == "HFA II-i"] <-
    jDate(unlist(lapply(strsplit(o102$VISIT_DATE[o102$INSTRUMENT_NAME == "HFA II-i"], ".",
                                 fixed = TRUE),
                        function (x) substr(x[11], 1, 8))))
  
  ## Date and time for HFA3
  o102$EXAM_TIME[o102$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")] <-
    jTime(o102$EXAM_TIME[o102$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")])
  o102$VISIT_DATE[o102$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")] <-
    jDate(o102$VISIT_DATE[o102$INSTRUMENT_NAME %in% c("HFA 3", "Humphrey Field Analyzer 3")])
  
  o102$SITE <- jReplacer(o102$SITE, c("R", "L"), c("OD", "OS"))
  o102$INSTRUMENT_MODEL_NUMBER  <- unlist(lapply(strsplit(o102$INSTRUMENT_MODEL_NUMBER, "-"),
                                                 function (x) x[1]))
  o102$INSTRUMENT_SERIAL_NUMBER <- unlist(lapply(strsplit(o102$INSTRUMENT_SERIAL_NUMBER, "-"),
                                                 function (x) x[2]))
  
  ## Exam Duration Fix
  o102$EXAM_DURATION <- jTime3(o102$EXAM_DURATION)
  
  ## PDP / TDP Fix
  o102[, c(grep("TDP", names(o102)), grep("PDP", names(o102)))] <-
    jReplacer(o102[, c(grep("TDP", names(o102)), grep("PDP", names(o102)))],
              c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
              c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  o102[, "MD_PROBABILITY"] <- jReplacer(o102[, "MD_PROBABILITY"],
                                        c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                        c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  o102[, "PSD_PROBABILITY"] <- jReplacer(o102[, "PSD_PROBABILITY"],
                                         c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                         c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  ## "Blind Spot" Fix
  o102[, c("T15_S3_TD", "T15_I3_TD", "T15_S3_TDP", "T15_I3_TDP",
           "T15_S3_PD", "T15_I3_PD", "T15_S3_PDP", "T15_I3_PDP")] <- ""
  
  # Other Fixes
  o102$TEST_TYPE     <- jReplacer(o102$TEST_TYPE, "Diagnostic", "Threshold")
  o102$TEST_PATTERN  <- jReplacer(o102$TEST_PATTERN, "10-2", "C10-2")
  o102$TEST_STRATEGY <- gsub("-", " ", o102$TEST_STRATEGY, fixed = TRUE)
  o102$STIMULUS_SIZE <- jReplacer(o102$STIMULUS_SIZE, "0.14748032", "III")
  
  o102$FOVEAL_THRESHOLD_PROBABILITY <- jReplacer(o102$FOVEAL_THRESHOLD_PROBABILITY,
                                                 c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                                 c("Not Significant", "< 0.5%", "< 1%", "< 2%", 
                                                   "< 5%", "< 10%"))
  o102$PSD_PROBABILITY <- jReplacer(o102$PSD_PROBABILITY,
                                    c("0.0", "0.5", "1.0", "2.0", "5.0", "10.0"),
                                    c("Not Significant", "< 0.5%", "< 1%", "< 2%", "< 5%", "< 10%"))
  
  # Bandaid for Date / Time
  if (any(o102$VISIT_DATE == "NA-NA-NA")) {
    o102[which(o102$VISIT_DATE == "NA-NA-NA"), c("VISIT_DATE", "EXAM_TIME")] <-
      cbind(jDate(o102$BACKUP_DATE[which(o102$VISIT_DATE == "NA-NA-NA")]),
            jTime(o102$BACKUP_TIME[which(o102$VISIT_DATE == "NA-NA-NA")]))
  }
  
  o102$SERIES_DATE_TIME <- paste(o102$VISIT_DATE, o102$EXAM_TIME, sep = "T")
  o102 <- o102[, -which(names(o102) %in% c("BACKUP_TIME", "BACKUP_DATE"))]
  
  o102[o102 == "XXXXXXNOTFOUND"] <- ""
  
  write.csv(o102, file.path(XML_DIRECTORY, "test10-2_FMP.csv"), row.names = FALSE,
            na = "")
  cat("...10-2 file written (", nrow(o102), " files).\n", sep = "")
  
  #QC Version
  qcNames102 <- c('Batch_UID', 'Exam_UID', 'INSTRUMENT_MODEL_NUMBER', 'INSTRUMENT_SERIAL_NUMBER',
                  'INSTRUMENT_SOFTWARE_VERSION', 'PatientID', 'StudyCode', 'aeDOB', 'Eye', 
                  'SERIES_DATE_TIME', 'aeExamDate', 'aeExamTime', 'EXAM_DURATION', 'aeDIGSTestType',
                  'TEST_TYPE', 'TEST_PATTERN', 'TEST_STRATEGY', 'STIMULUS_COLOR', 'STIMULUS_SIZE', 
                  'BACKGROUND_COLOR', 'FIXATION_TARGET', 'FIXATION_MONITOR', 'TRIAL_RX_SPHERE', 
                  'TRIAL_RX_CYLINDER', 'TRIAL_RX_AXIS', 'PUPIL_DIAMETER', 'VA', 'BLIND_SPOT_X', 
                  'BLIND_SPOT_Y', 'BLIND_SPOT_STIMULUS_SIZE', 'FALSE_NEGATIVE_PERCENT', 
                  'FALSE_POSITIVE_PERCENT', 'aeFixationCheckPercentage', 'FOVEAL_RESULT', 
                  'FOVEAL_THRESHOLD', 'CLINICAL_NOTES', 'SF_STATUS', 'SF', 'SF_PROBABILITY', 
                  'GHT', 'MD', 'MD_PROBABILITY', 'PSD', 'PSD_PROBABILITY', 'CPSD', 'CPSD_PROBABILITY', 
                  'FOVEAL_THRESHOLD_PROBABILITY', 'N1_S9_Thr', 'T1_S9_Thr', 'N5_S7_Thr', 'N3_S7_Thr', 
                  'N1_S7_Thr', 'T1_S7_Thr', 'T3_S7_Thr', 'T5_S7_Thr', 'N7_S5_Thr', 'N5_S5_Thr', 
                  'N3_S5_Thr', 'N1_S5_Thr', 'T1_S5_Thr', 'T3_S5_Thr', 'T5_S5_Thr', 'T7_S5_Thr', 
                  'N7_S3_Thr', 'N5_S3_Thr', 'N3_S3_Thr', 'N1_S3_Thr', 'T1_S3_Thr', 'T3_S3_Thr', 
                  'T5_S3_Thr', 'T7_S3_Thr', 'N9_S1_Thr', 'N7_S1_Thr', 'N5_S1_Thr', 'N3_S1_Thr', 
                  'N1_S1_Thr', 'T1_S1_Thr', 'T3_S1_Thr', 'T5_S1_Thr', 'T7_S1_Thr', 'T9_S1_Thr', 
                  'N9_I1_Thr', 'N7_I1_Thr', 'N5_I1_Thr', 'N3_I1_Thr', 'N1_I1_Thr', 'T1_I1_Thr', 
                  'T3_I1_Thr', 'T5_I1_Thr', 'T7_I1_Thr', 'T9_I1_Thr', 'N7_I3_Thr', 'N5_I3_Thr', 
                  'N3_I3_Thr', 'N1_I3_Thr', 'T1_I3_Thr', 'T3_I3_Thr', 'T5_I3_Thr', 'T7_I3_Thr', 
                  'N7_I5_Thr', 'N5_I5_Thr', 'N3_I5_Thr', 'N1_I5_Thr', 'T1_I5_Thr', 'T3_I5_Thr', 
                  'T5_I5_Thr', 'T7_I5_Thr', 'N5_I7_Thr', 'N3_I7_Thr', 'N1_I7_Thr', 'T1_I7_Thr',
                  'T3_I7_Thr', 'T5_I7_Thr', 'N1_I9_Thr', 'T1_I9_Thr', 'N1_S9_TD', 'T1_S9_TD', 
                  'N5_S7_TD', 'N3_S7_TD', 'N1_S7_TD', 'T1_S7_TD', 'T3_S7_TD', 'T5_S7_TD', 'N7_S5_TD', 
                  'N5_S5_TD', 'N3_S5_TD', 'N1_S5_TD', 'T1_S5_TD', 'T3_S5_TD', 'T5_S5_TD', 'T7_S5_TD', 
                  'N7_S3_TD', 'N5_S3_TD', 'N3_S3_TD', 'N1_S3_TD', 'T1_S3_TD', 'T3_S3_TD', 'T5_S3_TD', 
                  'T7_S3_TD', 'N9_S1_TD', 'N7_S1_TD', 'N5_S1_TD', 'N3_S1_TD', 'N1_S1_TD', 'T1_S1_TD', 
                  'T3_S1_TD', 'T5_S1_TD', 'T7_S1_TD', 'T9_S1_TD', 'N9_I1_TD', 'N7_I1_TD', 'N5_I1_TD', 
                  'N3_I1_TD', 'N1_I1_TD', 'T1_I1_TD', 'T3_I1_TD', 'T5_I1_TD', 'T7_I1_TD', 'T9_I1_TD', 
                  'N7_I3_TD', 'N5_I3_TD', 'N3_I3_TD', 'N1_I3_TD', 'T1_I3_TD', 'T3_I3_TD', 'T5_I3_TD', 
                  'T7_I3_TD', 'N7_I5_TD', 'N5_I5_TD', 'N3_I5_TD', 'N1_I5_TD', 'T1_I5_TD', 'T3_I5_TD', 
                  'T5_I5_TD', 'T7_I5_TD', 'N5_I7_TD', 'N3_I7_TD', 'N1_I7_TD', 'T1_I7_TD', 'T3_I7_TD', 
                  'T5_I7_TD', 'N1_I9_TD', 'T1_I9_TD', 'N1_S9_PD', 'T1_S9_PD', 'N5_S7_PD', 'N3_S7_PD', 
                  'N1_S7_PD', 'T1_S7_PD', 'T3_S7_PD', 'T5_S7_PD', 'N7_S5_PD', 'N5_S5_PD', 'N3_S5_PD', 
                  'N1_S5_PD', 'T1_S5_PD', 'T3_S5_PD', 'T5_S5_PD', 'T7_S5_PD', 'N7_S3_PD', 'N5_S3_PD', 
                  'N3_S3_PD', 'N1_S3_PD', 'T1_S3_PD', 'T3_S3_PD', 'T5_S3_PD', 'T7_S3_PD', 'N9_S1_PD', 
                  'N7_S1_PD', 'N5_S1_PD', 'N3_S1_PD', 'N1_S1_PD', 'T1_S1_PD', 'T3_S1_PD', 'T5_S1_PD', 
                  'T7_S1_PD', 'T9_S1_PD', 'N9_I1_PD', 'N7_I1_PD', 'N5_I1_PD', 'N3_I1_PD', 'N1_I1_PD', 
                  'T1_I1_PD', 'T3_I1_PD', 'T5_I1_PD', 'T7_I1_PD', 'T9_I1_PD', 'N7_I3_PD', 'N5_I3_PD', 
                  'N3_I3_PD', 'N1_I3_PD', 'T1_I3_PD', 'T3_I3_PD', 'T5_I3_PD', 'T7_I3_PD', 'N7_I5_PD', 
                  'N5_I5_PD', 'N3_I5_PD', 'N1_I5_PD', 'T1_I5_PD', 'T3_I5_PD', 'T5_I5_PD', 'T7_I5_PD', 
                  'N5_I7_PD', 'N3_I7_PD', 'N1_I7_PD', 'T1_I7_PD', 'T3_I7_PD', 'T5_I7_PD', 'N1_I9_PD', 
                  'T1_I9_PD', 'N1_S9_TDP', 'T1_S9_TDP', 'N5_S7_TDP', 'N3_S7_TDP', 'N1_S7_TDP', 
                  'T1_S7_TDP', 'T3_S7_TDP', 'T5_S7_TDP', 'N7_S5_TDP', 'N5_S5_TDP', 'N3_S5_TDP', 
                  'N1_S5_TDP', 'T1_S5_TDP', 'T3_S5_TDP', 'T5_S5_TDP', 'T7_S5_TDP', 'N7_S3_TDP', 
                  'N5_S3_TDP', 'N3_S3_TDP', 'N1_S3_TDP', 'T1_S3_TDP', 'T3_S3_TDP', 'T5_S3_TDP', 
                  'T7_S3_TDP', 'N9_S1_TDP', 'N7_S1_TDP', 'N5_S1_TDP', 'N3_S1_TDP', 'N1_S1_TDP', 
                  'T1_S1_TDP', 'T3_S1_TDP', 'T5_S1_TDP', 'T7_S1_TDP', 'T9_S1_TDP', 'N9_I1_TDP', 
                  'N7_I1_TDP', 'N5_I1_TDP', 'N3_I1_TDP', 'N1_I1_TDP', 'T1_I1_TDP', 'T3_I1_TDP', 
                  'T5_I1_TDP', 'T7_I1_TDP', 'T9_I1_TDP', 'N7_I3_TDP', 'N5_I3_TDP', 'N3_I3_TDP', 
                  'N1_I3_TDP', 'T1_I3_TDP', 'T3_I3_TDP', 'T5_I3_TDP', 'T7_I3_TDP', 'N7_I5_TDP', 
                  'N5_I5_TDP', 'N3_I5_TDP', 'N1_I5_TDP', 'T1_I5_TDP', 'T3_I5_TDP', 'T5_I5_TDP', 
                  'T7_I5_TDP', 'N5_I7_TDP', 'N3_I7_TDP', 'N1_I7_TDP', 'T1_I7_TDP', 'T3_I7_TDP', 
                  'T5_I7_TDP', 'N1_I9_TDP', 'T1_I9_TDP', 'N1_S9_PDP', 'T1_S9_PDP', 'N5_S7_PDP', 
                  'N3_S7_PDP', 'N1_S7_PDP', 'T1_S7_PDP', 'T3_S7_PDP', 'T5_S7_PDP', 'N7_S5_PDP', 
                  'N5_S5_PDP', 'N3_S5_PDP', 'N1_S5_PDP', 'T1_S5_PDP', 'T3_S5_PDP', 'T5_S5_PDP', 
                  'T7_S5_PDP', 'N7_S3_PDP', 'N5_S3_PDP', 'N3_S3_PDP', 'N1_S3_PDP', 'T1_S3_PDP', 
                  'T3_S3_PDP', 'T5_S3_PDP', 'T7_S3_PDP', 'N9_S1_PDP', 'N7_S1_PDP', 'N5_S1_PDP', 
                  'N3_S1_PDP', 'N1_S1_PDP', 'T1_S1_PDP', 'T3_S1_PDP', 'T5_S1_PDP', 'T7_S1_PDP', 
                  'T9_S1_PDP', 'N9_I1_PDP', 'N7_I1_PDP', 'N5_I1_PDP', 'N3_I1_PDP', 'N1_I1_PDP', 
                  'T1_I1_PDP', 'T3_I1_PDP', 'T5_I1_PDP', 'T7_I1_PDP', 'T9_I1_PDP', 'N7_I3_PDP', 
                  'N5_I3_PDP', 'N3_I3_PDP', 'N1_I3_PDP', 'T1_I3_PDP', 'T3_I3_PDP', 'T5_I3_PDP', 
                  'T7_I3_PDP', 'N7_I5_PDP', 'N5_I5_PDP', 'N3_I5_PDP', 'N1_I5_PDP', 'T1_I5_PDP', 
                  'T3_I5_PDP', 'T5_I5_PDP', 'T7_I5_PDP', 'N5_I7_PDP', 'N3_I7_PDP', 'N1_I7_PDP', 
                  'T1_I7_PDP', 'T3_I7_PDP', 'T5_I7_PDP', 'N1_I9_PDP', 'T1_I9_PDP', 'cAutoQCStatus', 
                  'QCFieldUsable', 'QCReliable', 'cQCFN33Status', 'cQCAHSManualStatus', 
                  'cQCRimArtifactStatus', 'cQCInattentionStatus', 'cQCLearningEffectStatus', 
                  'cQCFatigueStatus', 'cQCFixationStatus', 'cQCOtherDefectStatus', 
                  'cQCUnreliableByTechnicianStatus', 'cQCUnaccPupilSizeStatus', 
                  'LOW_PATIENT_RELIABILITY_STATUS')
  
  reg_cols <- c('birthdate', 'visitdate', 'site', 'examtime')
  
  odd_cols <- c('aedob', 'aeexamdate', 'eye', 'aeexamtime')
  
  # Create similar column names
  cols1 <- tolower(gsub("_", "", names(o102), fixed = TRUE))
  cols1 <- jReplacer(cols1, reg_cols, odd_cols)
  cols2 <- tolower(gsub("_", "", qcNames102, fixed = TRUE))
  
  fill  <- which(cols1 %in% cols2)
  
  # Populate the new data frame with the known values
  o102_QC <- o102[, fill]
  colnames(o102_QC) <- cols1[fill]
  
  # Fill in the unknown values with an empty string
  o102_QC[, cols2[which(!cols2 %in% names(o102_QC))]] <- ""
  
  # Rearrange and rename the columns
  o102_QC <- o102_QC[, cols2]
  colnames(o102_QC) <- qcNames102

  o102_QC[ , 'aeFixationCheckPercentage'] <- (as.numeric(o102[, 'FIXATION_CHECK_ERRORS']) / 
                                                as.numeric(o102[, 'FIXATION_CHECK_TRIALS'])) *100
  
  write.csv(o102_QC, file.path(XML_DIRECTORY, "test10-2_QC.csv"), row.names = FALSE,
            na = "")
}

kprs   <- which(c("o604", "o302", "o242", "o242C", "o102") %in% ls())
checkr <- 0
for (i in kprs) {
  checkr <- checkr + nrow(get(c("o604", "o302", "o242", "o242C", "o102")[i]))
}

if (checkr < NFILES) {
  cat(NFILES - checkr, "unsupported XML's not processed.")
}

