#' determine file path to data to be imported
#'
#' @param path character string; path to SUF data provided by user
#' @param SC character string; starting cohort ("SCx")
#' @param domain charachter string; competence domain (e.g., "RE", "MA")
#' @param school logical; indicator whether the data to be imported is school
#' identifiers (not competence data0)
#'
#' @return path to file as a character string
#' @noRd

#_______________________________Editted by RaelK________________________________
determine_file_path <- function(path, SC, domain, school = FALSE) {
  files <- list.files(path = path)
  
  if (school) {
    tc_files <- files[grep("CohortProfile", files)]
    if (length(tc_files) > 1) {
      versions <- as.numeric(sub(".*_D_(\\d+)-0-0.*", "\\1", tc_files))
      latest_index <- which.max(versions)
      selected_file <- tc_files[latest_index]
    } else if (length(tc_files) == 1) {
      selected_file <- tc_files[1]
    } else {
      stop("[", SC, "] No CohortProfile file found in path.")
    }
    message("[", SC, "] Selected school identifier file: ", selected_file)
    return(paste0(path, selected_file))
  }
  
  if (SC == "SC5" & domain == "BA") {
    #selected_file <- files[grep("xEcoCAPI", files)]
    tc_files <- files[grep("xEcoCAPI", files)]
    if (length(tc_files) > 1) {
      versions <- as.numeric(sub(".*_D_(\\d+)-0-0.*", "\\1", tc_files))
      latest_index <- which.max(versions)
      selected_file <- tc_files[latest_index]
    } else if (length(tc_files) == 1) {
      selected_file <- tc_files[1]
    } else {
      stop("[", SC, "] No xEcoCAPI file found in path.")
    }
  } else if (SC == "SC1" & domain == "CD") {
    #selected_file <- files[grep("xDirectMeasures", files)]
    tc_files <- files[grep("xDirectMeasures", files)]
    
    if (length(tc_files) > 1) {
      versions <- as.numeric(sub(".*_D_(\\d+)-0-0.*", "\\1", tc_files))
      latest_index <- which.max(versions)
      selected_file <- tc_files[latest_index]
    } else if (length(tc_files) == 1) {
      selected_file <- tc_files[1]
    } else {
      stop("[", SC, "] No xDirectMeasures file found in path.")
    }
  } else {
    tc_files <- files[grep("xTargetCompetencies", files)]
    
    if (length(tc_files) > 1) {
      versions <- as.numeric(sub(".*_D_(\\d+)-0-0.*", "\\1", tc_files))
      latest_index <- which.max(versions)
      selected_file <- tc_files[latest_index]
    } else if (length(tc_files) == 1) {
      selected_file <- tc_files[1]
    } else {
      stop("[", SC, "] No xTargetCompetencies file found in path.")
    }
  }
  
  message("[", SC, "] Selected competence file: ", selected_file)
  filepath <- paste0(path, selected_file)
  return(filepath)
}
#_______________________________________________________________________________

#' create appropriate error message for data import
#'
#' @param filepath character string; path to SUF data completed by the package
#' @param filetype character string; file format of SUF data ("SPSS" or "Stata")
#' @param school logical; indicator whether the data to be imported is school
#' identifiers (not competence data0)
#'
#' @return error message for school identifier or competence data
#' @noRd
create_error_msg <- function(filepath, filetype, school = FALSE) {
  if (school) {
    return(paste0(
      "* Path '", filepath, "' may not contain CohortProfile.\n",
      "* File format: '", filetype, "' might be wrong"
    ))
  }
  paste0(
    "* Path '", filepath, "' may not lead to competence files.\n",
    "* File format: '", filetype, "' might be wrong"
  )
}

#' import data
#'
#' @param filepath character string; path to SUF data completed by the package
#' @param filetype character string; file format of SUF data ("SPSS" or "Stata")
#' @param error_msg character string; error message for data import
#' @param school logical; indicator whether the data to be imported is school
#' identifiers (not competence data0)
#'
#' @return data.frame of competence (with custom values for missing values) OR
#' school identifier data (with missing values set to NA)
#' @noRd
import_data <- function(filetype, filepath, error_msg, school = FALSE) {
  if (filetype == "sav") {
    data <-
      tryCatch(
        haven::read_spss(file = filepath, user_na = TRUE),
        error = function(cnd) {
          stop(error_msg, call. = FALSE)
        }
      )
  } else if (filetype == "dta") {
    data <-
      tryCatch(
        haven::read_dta(file = filepath),
        error = function(cnd) {
          stop(error_msg, call. = FALSE)
        }
      )
  } else {
    stop(error_msg, call. = FALSE)
  }
  # sjlabelled because of problems with labelled_spss and tibble class
  data <- sjlabelled::remove_all_labels(data)
  # user defined missings not needed for school id data
  if (school) {
    data[data < -15] <- NA
  }
  data
}
