#' Download and load caliBISG data files
#'
#' @description Various functions for downloading and loading the caliBISG data
#'   files used by the package. See **Details**.
#'
#' @export
#' @param states (character vector) The states to download data for. The default
#'   (`NULL`) is to download for all states. States should be provided as
#'   uppercase two-letter state [abbreviations][datasets::state.abb].
#' @param years (integer vector) The years to download data for. The default is
#'   to download all years. Currently only 2020 is available but more years will
#'   be added eventually.
#' @param ... Optional arguments (other than `url` and `destfile`) passed to
#'   [utils::download.file()].
#'
#' @details
#' * `download_data()`: Download the required data files for the specified
#' states and years. The files will be stored internally in a package-specific
#' data [directory][tools::R_user_dir].
#' * `load_data()`: Load the data for a particular `state`-`year`. This is only
#' necessary if you want to work with the full data files directly. When using
#' the functions provided by this package (e.g. [race_probabilities()]) the data
#' will be loaded internally automatically.
#' * `data_dir()`: Get the path to the internal data directory.
#' * `available_data()`: List the names of the data files that have already been
#'  downloaded and are available for use.
#' * `delete_all_data()`: Delete all the data files stored internally.
#'
#' @return * `download_data()`: (logical) `TRUE`, invisibly, if no error.
#'
download_data <- function(states = NULL, years = 2020, ...) {
  if (is.null(states)) {
    states <- .all_states()
  }
  if (is.null(years)) {
    years <- .all_years()
  }
  .validate_states_years(states, years)

  for (st in states) {
    for (yr in years) {
      rds_name <- paste0(st, "-", yr, ".rds")
      rds_path <- file.path(data_dir(), rds_name)

      if (file.exists(rds_path)) {
        message("* ", rds_name, " already exists. Skipping.")
        next
      }

      csv_url  <- .make_csv_url(st, yr)
      message("* Reading file: ", csv_url)
      df <-  readr::read_csv(csv_url, show_col_types = FALSE, progress = FALSE)

      # Add year and state and order the columns
      df <- as.data.frame(df)
      df$year <- yr
      df$state <- st
      df <- .rename_data(df)
      df <- .reorder_data(df)

      message("* Saving data to: ", rds_path)
      saveRDS(df, file = rds_path)
    }
  }

  invisible(TRUE)
}

#' @rdname download_data
#' @export
#' @param state (string) For `load_data()`, the state to load.
#' @param year (integer) For `load_data()`, the year to load.
#' @return * `load_data()`: (data frame) A data frame of the data for the
#'   specified state.
#'
load_data <- function(state, year = 2020) {
  stopifnot(
    is.character(state), length(state) == 1,
    is.numeric(year), length(year) == 1
  )
  .ensure_data_available(state, year)
  readRDS(.data_path(state, year))
}

#' @rdname download_data
#' @export
#' @return * `data_dir()`: (string) The path to the data directory.
#'
data_dir <- function() {
  dir <- tools::R_user_dir("caliBISG", "data")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  dir
}

#' @rdname download_data
#' @export
#' @return * `available_data()`: (character vector) The names of the data files
#'   that have already been downloaded.
available_data <- function() {
  list.files(data_dir(), full.names = FALSE)
}

#' @rdname download_data
#' @export
#'
delete_all_data <- function() {
  message("Deleting data files: ", paste(available_data(), collapse = ", "))
  unlink(data_dir(), recursive = TRUE)
}


# internal ----------------------------------------------------------------

#' Load the data for a particular state into the internal data environment
#' unless it's already loaded
#'
#' @noRd
#' @param error_if_missing (logical) Whether to error if the data is not available.
#' @return (data frame) A data frame of the data for the specified state or, if
#' `error_is_missing = FALSE`, `NULL` if the data is not available.
#'
.load_data_internal <- function(state, year, error_if_missing = TRUE) {
  data_name <- paste0(tolower(state), "_", year)
  if (!exists(data_name, envir = .internal_data_env)) {
    if (!.is_data_available(state, year)) {
      if (!error_if_missing) {
        return(NULL)
      } else { # error
        .ensure_data_available(state, year)
      }
    }
    .internal_data_env[[data_name]] <- readRDS(.data_path(state, year))
  }
  .internal_data_env[[data_name]]
}

#' Check if data for a particular state-year has been downloaded
#' @noRd
#' @param state (string) The state to check.
#' @param year (numeric) The year to check.
#' @return (logical) `TRUE` if the data is available, `FALSE` otherwise.
.is_data_available <- function(state, year) {
  file.exists(.data_path(state, year))
}

#' Error if data for a particular state-year is not available
#' @noRd
#' @param state (string) The state to check.
#' @param year (numeric) The year to check.
#' @return (logical) `TRUE`, invisibly, if no error.
.ensure_data_available <- function(state, year) {
  if (!.is_data_available(state, year)) {
    stop(
      "Data file for ", state, ", ", year, " not found. ",
      "Use `download_data()` to download it.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Get the path to the data file for a particular state-year pair
#' @noRd
#' @param state (string) The state to check.
#' @param year (numeric) The year to check.
#' @return (string) The path to the data file.
.data_path <- function(state, year) {
  file.path(data_dir(), paste0(tolower(state), "-", year, ".rds"))
}

#' List the states that are currently available for download
#'
#' @noRd
#' @return (character vector) The two-letter state abbreviations of the
#'   available states.
#'
.all_states <- function() {
  c("FL", "GA", "NC", "NY", "OH", "OK", "VT", "WA")
}

#' List the years that are currently available for download
#'
#' @noRd
#' @return (integer vector) The years of the available data.
.all_years <- function() {
  as.integer(c(2020))
}

#' Validate that user-specified states/years are available
#'
#' @noRd
#' @param states (character vector) The states to validate.
#' @param years (integer vector) The years to validate.
#' @return (logical) `TRUE`, invisibly, if no error.
#'
.validate_states_years <- function(states, years) {
  valid_states  <- .all_states()
  valid_years <- .all_years()

  bad_states <- setdiff(states, valid_states)
  if (length(bad_states) > 0) {
    stop(
      "Invalid states requested: ", paste(bad_states, collapse = ", "),
      "\n  Available states are: ", paste(valid_states, collapse = ", "),
      call. = FALSE
    )
  }

  bad_years  <- setdiff(years, valid_years)
  if (length(bad_years) > 0) {
    stop(
      "Invalid years requested: ", paste(bad_years, collapse = ", "),
      "\n  Available years are: ", paste(valid_years, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Rename columns in the imported data if necessary
#'
#' @noRd
#' @param data (data frame) The data frame to process.
#' @return (data frame) The updated data frame.
#'
.rename_data <- function(data) {
  # eventually we should rename these columns in the files before uploading them
  colnames(data) <- gsub("nh_aian", "aian", colnames(data))
  colnames(data) <- gsub("nh_api", "api", colnames(data))
  colnames(data) <- gsub("nh_black", "black_nh", colnames(data))
  colnames(data) <- gsub("nh_white", "white_nh", colnames(data))
  colnames(data) <- gsub("in_cen_surs", "in_census", colnames(data))
  data
}

#' Reorder columns in the imported data if necessary
#'
#' @noRd
#' @param data (data frame) The data frame to process.
#' @return (data frame) The updated data frame.
#'
.reorder_data <- function(data) {
  col_order <- c(
    .demographic_columns(),
    .calibisg_columns(),
    "in_census"
  )
  data[, col_order]
}


#' Construct the URL for downloading the CSV file for a particular state-year
#'
#' @noRd
#' @param state (string) The state to use.
#' @param year (numeric) The year to use.
#' @return (string) The URL for the CSV file.
.make_csv_url <- function(state, year) {
  # replace this with actual URL once data is online
  .temporary_local_calibisg_download_path(state, year)
}



# remove once data is online ----------------------------------------------

#' Set and get the temporarly local path to the data files. Will replace this
#' with downloading them eventually.
#'
#' @noRd
#' @export
set_temporary_local_directory <- function(dir = NULL) {
  .internal_data_env$temporary_local_directory <- dir
}
.get_temporary_local_directory <- function() {
  dir <- .internal_data_env$temporary_local_directory
  if (is.null(dir)) {
    dir <- "/Users/jgabry/Desktop/tmp/caliBISG-data/"
  }
  dir
}
.temporary_local_calibisg_download_path <- function(state, year) {
  paste0(.get_temporary_local_directory(), "calibisg_", tolower(state), year, ".csv")
}
