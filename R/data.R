#' Download and load data files
#'
#' @description Various functions for downloading and loading the data files
#' used by the package. See **Details**.
#'
#' @export
#' @param states (character vector) The states to download data for. The default
#'   (`NULL`) is to download for all states. States should be provided as
#'   two-letter state [abbreviations][datasets::state.abb].
#' @param years (integer vector) The years to download data for. The default
#' is to download all years. Currently only 2020 is available.
#' @param ... Optional arguments (other than `url` and `destfile`) passed to
#'   [utils::download.file()].
#'
#' @details
#' * `download_data()`: Download the required data files for the specified
#' states and years. The files will be stored internally in a package-specific
#' data [directory][tools::R_user_dir].
#' * `load_data()`: Load the data for a particular state-year. This is only
#' necessary if you want to work with the full data files directly. When using
#' the functions provided by this package (e.g. [race_probabilities()]) the data
#' will be loaded internally automatically.
#' * `data_dir()`: Get the path to the internal data directory.
#' * `available_data()`: List the names of the available data files.
#' * `delete_all_data()`: Delete all the data files stored internally.
#'
#' @return * `download_data()`: (logical) `TRUE`, invisibly, if no error.
#'
download_data <- function(states = NULL, years = 2020) {
  if (is.null(states)) {
    states <- .all_states()
  }
  if (is.null(years)) {
    years <- .all_years()
  }
  .validate_states_years(states, years)

  data_dir <- data_dir()

  for (st in states) {
    for (yr in years) {
      rds_name <- paste0(st, "-", yr, ".rds")
      rds_path <- file.path(data_dir, rds_name)

      if (file.exists(rds_path)) {
        message(rds_name, " already exists. Skipping.")
        next
      }

      csv_name <- paste0(st, "-", yr, ".csv")
      csv_url  <- paste0("https://mydata.org/race/", st, "-", yr, ".csv")  # placeholder
      csv_path <- file.path(tempdir(), csv_name)

      message("Downloading: ", csv_name, " from ", csv_url)
      # res <- utils::download.file(csv_url, csv_path, mode = "wb", quiet = TRUE)

      # temporarily read data from local file
      res <- 0
      csv_path <- .temporary_local_path(st, yr)

      # Check if download was successful
      if (res != 0) {
        warning(
          "Download of ", csv_url, " failed with code ", res,
          ". Skipping this file."
        )
        next
      }

      # Attempt to read the CSV
      df <- tryCatch(
        readr::read_csv(
          csv_path,
          show_col_types = FALSE,
          progress = FALSE
        ),
        error = function(e) {
          warning("Error reading CSV: ", conditionMessage(e))
          return(NULL)
        }
      )

      # If we couldn't read, remove the CSV file and skip
      if (is.null(df)) {
        if (file.exists(csv_path)) file.remove(csv_path)
        next
      }

      # Save to RDS
      saveRDS(as.data.frame(df), file = rds_path)
      message("Saved data as: ", rds_path)

      # Cleanup
      # Re-enable this if we're actually downloading files
      # if (file.exists(csv_path)) file.remove(csv_path)
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
  stopifnot(length(state) == 1, length(year) == 1)
  .ensure_data_available(state, year)
  readRDS(.data_path(state, year))
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
.load_data_internal <- function(state, year) {
  data_name <- paste0(tolower(state), "_", year)
  if (!exists(data_name, envir = .internal_data_env)) {
    .ensure_data_available(state, year)
    .internal_data_env[[data_name]] <- readRDS(.data_path(state, year))
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
.ensure_data_available <- function(state, year) {
  if (!file.exists(.data_path(state, year))) {
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
.data_path <- function(state, year) {
  file.path(data_dir(), paste0(tolower(state), "-", year, ".rds"))
}

#' Get the download URL for the data file for a particular state
#'
#' @noRd
#' @param state (string) The state.
#' @param year (integer) The year of the data.
#' @return (string) The URL to download the data file.
#'
.temporary_local_path <- function(state, year) {
  paste0("/Users/jgabry/Desktop/tmp/voter_bisg/", tolower(state), "-", year, ".csv")
}

#' List the states that are currently available for download
#'
#' @noRd
#' @return (character vector) The two-letter state abbreviations of the
#'   available states.
#'
.all_states <- function() {
  c("NC", "WA")
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
  valid_s  <- .all_states()
  valid_yr <- .all_years()

  bad_states <- setdiff(states, valid_s)
  if (length(bad_states) > 0) {
    stop("Invalid states requested: ", paste(bad_states, collapse = ", "))
  }

  bad_years  <- setdiff(years, valid_yr)
  if (length(bad_years) > 0) {
    stop("Invalid years requested: ", paste(bad_years, collapse = ", "))
  }

  invisible(TRUE)
}

