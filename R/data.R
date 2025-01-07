#' Download and load data files
#'
#' @description Various functions for downloading and loading the data files
#' used by the package. See **Details**.
#'
#' @export
#' @param states (character vector) The states to download data for. The default
#'   (`NULL`) is to download for all states. States should be provided as
#'   two-letter state [abbreviations][datasets::state.abb].
#' @param ... Optional arguments (other than `url` and `destfile`) passed to
#'   [utils::download.file()].
#'
#' @details
#' * `download_data()`: Download the required data files for the specified
#' states. The files will be stored internally in a package-specific data
#' [directory][tools::R_user_dir].
#' * `load_data()`: Load the data for a particular state. This is only necessary
#' if you want to work with the full data files directly. When using the
#' functions provided by this package (e.g. [race_probabilities()]) the data
#' will be loaded internally automatically.
#' * `data_dir()`: Get the path to the internal data directory.
#' * `available_data()`: List the names of the available data files.
#' * `delete_all_data()`: Delete all the data files stored internally.
#'
#' @return * `download_data()`: (character vector) The path(s) to the downloaded
#'   data file(s), invisibly.
#'
download_data <- function(states = NULL, ...) {
  if (is.null(states)) {
    states <- all_states()
  } else {
    validate_states(states)
  }
  message("Downloading data for states: ", paste(states, collapse = ", "))
  succesfully_downloaded <- character()
  for (state in states) {
    file_path <- data_path(state)
    if (file.exists(file_path)) {
      message(basename(file_path), " already exists, skipping.")
    } else {
      message("Downloading data file ", data_download_url(state, year = 2020))
      # return_code <- utils::download.file(
      #   url = data_download_url(state, year = 2020),
      #   destfile = file_path,
      #   ...
      # )
      data <- readr::read_csv(
        data_download_url(state, year = 2020),
        show_col_types = FALSE,
        progress = FALSE
      )
      saveRDS(as.data.frame(data), file_path)
      return_code <- 0
      if (return_code == 0) {
        message("Download complete.")
        succesfully_downloaded <- c(succesfully_downloaded, file_path)
      } else {
        message("Download failed.")
      }
    }
  }

  invisible(succesfully_downloaded)
}

#' @rdname download_data
#' @export
#' @param state (string) For `load_data()`, the state to load.
#' @return * `load_data()`: (data frame) A data frame of the data for the
#'   specified state.
#'
load_data <- function(state) {
  year <- 2020
  data_name <- paste0(tolower(state), "_", year)
  ensure_data_available(state, year)
  readRDS(data_path(state, year))
}

#' @rdname download_data
#' @export
#' @return * `data_dir()`: (string) The path to the data directory.
#'
data_dir <- function() {
  dir <- tools::R_user_dir("pisg", "data")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  dir
}

#' @rdname download_data
#' @export
#' @return * `available_data()`: (character vector) The names of the available
#'   data files.
available_data <- function() {
  list.files(data_dir(), full.names = FALSE)
}

#' @rdname download_data
#' @export
#'
delete_all_data <- function() {
  unlink(data_dir(), recursive = TRUE)
}


# internal ----------------------------------------------------------------

#' Load the data for a particular state into the internal data environment
#' unless it's already loaded
#'
#' @noRd
#' @param state (string) The state to check.
#' @return (data frame) A data frame of the data for the specified state.
#'
load_data_internal <- function(state, year = 2020) {
  data_name <- paste0(tolower(state), "_", year)
  if (!exists(data_name, envir = .internal_data_env)) {
    ensure_data_available(state, year)
    .internal_data_env[[data_name]] <- readRDS(data_path(state, year))
  }
  .internal_data_env[[data_name]]
}

#' Check if data for a particular state is available
#'
#' @noRd
#' @param state (string) The state to check. This should be provided as a
#'   two-letter state abbreviation.
#' @return (logical) `TRUE`, invisibly, if no error.
#'
ensure_data_available <- function(state, year = 2020) {
  if (!file.exists(data_path(state, year))) {
    stop(
      "Data file for ", state, ", ", year, " not found. ",
      "Use `download_data()` to download it."
    )
  }
  invisible(TRUE)
}

#' Get the path to the data file for a particular state-year pair
#'
#' @noRd
#' @param state (string) The state.
#' @return (string) The path to the data file.
#'
data_path <- function(state, year = 2020) {
  file.path(data_dir(), paste0(tolower(state), "_", year, ".rds"))
}

#' Get the download URL for the data file for a particular state
#'
#' @noRd
#' @param state (string) The state.
#' @param year (integer) The year of the data.
#' @return (string) The URL to download the data file.
#'
data_download_url <- function(state, year = 2020) {
  paste0("/Users/jgabry/Desktop/tmp/voter_bisg/", tolower(state), "_", year, ".csv")
}

#' List the states that are currently available for download
#'
#' @noRd
#' @return (character vector) The two-letter state abbreviations of the
#'   available states.
#'
all_states <- function() {
  c("NC", "WA")
}

#' List the years that are currently available for download
#'
#' @noRd
#' @return (integer vector) The years of the available data.
all_years <- function() {
  as.integer(c(2020))
}

#' Validate that user-specified states are available
#'
#' @noRd
#' @param states (character vector) The states to validate.
#' @return (logical) `TRUE`, invisibly, if no error.
#'
validate_states <- function(states) {
  if (!all(states %in% all_states())) {
    stop(
      "Invalid state(s) specified. Available states are: ",
      paste(all_states(), collapse = ", ")
    )
  }
  invisible(TRUE)
}

#' Validate that user-specified years are available
#'
#' @noRd
#' @param states (integer vector) The years to validate.
#' @return (logical) `TRUE`, invisibly, if no error.
#'
validate_years <- function(years) {
  if (!all(years %in% all_years())) {
    stop(
      "Invalid year(s) specified. Available years are: ",
      paste(all_years(), collapse = ", ")
    )
  }
  invisible(TRUE)
}
