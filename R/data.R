#' Download caliBISG data files
#'
#' @name caliBISG-data
#' @aliases calibisg-data
#'
#' @description Currently caliBISG (Greengard and Gelman, 2025) is available for
#'   the following states and years:
#'
#'   * States: FL, GA, NC, NY, OH, OK, VT, WA
#'   * Years: 2020
#'
#'   Each state-year combination has a separate data file that is used
#'   internally by the package when the user requests caliBISG estimates for
#'   that state and year. Because most of the files are hundreds of megabytes
#'   each, they are not included with the package by default and must instead be
#'   downloaded from GitHub. See **Details**.
#'
#'   We are working on adding additional states and years. When caliBISG is
#'   unavailable we still provide traditional BISG, which does not require
#'   downloading any files.
#'
#' @template calibisg-ref
#'
#' @examples
#' \dontrun{
#' # Download data just for Oklahoma and Vermont for 2020
#' download_data(state = c("OK", "VT"), year = 2020)
#'
#' # Download all available data
#' download_data()
#'
#' # List the data files that have already been downloaded
#' available_data()
#'
#' # Delete the downloaded data for OK
#' delete_data("OK")
#'
#' # Delete all downloaded data
#' delete_data()
#' }
NULL


#' @rdname caliBISG-data
#' @export
#'
#' @details
#' * `download_data()`: Download the CSV data files from GitHub for the
#' specified states and years. The downloaded CSV files are converted to data
#' frames and stored as [RDS][base::readRDS] files internally in a
#' package-specific data [directory][tools::R_user_dir]. A GitHub personal
#' access token (PAT) is not required but can be used to increase rate limits.
#' If the **gitcreds** package is installed and has a credential for
#' `github.com`, the token provided by [gitcreds::gitcreds_get()] will be used.
#' Otherwise the environment variables `GITHUB_PAT` and `GITHUB_TOKEN` will be
#' checked in that order. If no token is found, the request will be made
#' anonymously.
#'
#' @param state (character vector) For `download_data()` or `delete_data()`, the
#'   states to download or delete. The default is all available states. If
#'   specifying particular states, they should be provided as two-letter
#'   abbreviations. For `load_data()`, a single state to load.
#' @param year (integer vector) For `download_data()` or `delete_data()`, the
#'   years to download or delete. The default is all available years. For
#'   `load_data()`, a single year to load. For `available_data()`, an optional
#'   single year to filter the available data files.
#' @param progress (logical) Whether to show a progress bar while downloading
#'   the data. The default is `TRUE`.
#' @param overwrite (logical) For `download_data()`, whether to overwrite
#'   existing files if files with the same names already exist. The default is
#'   `FALSE`.
#'
#' @return
#' * `download_data()`: (logical) `TRUE`, invisibly, if no error.
#'
download_data <- function(state, year, progress = TRUE, overwrite = FALSE) {
  states <- if (missing(state)) .all_calibisg_states() else toupper(state)
  years <- if (missing(year)) .all_calibisg_years() else as.integer(year)
  .validate_calibisg_states_years(states, years)
  for (st in states) {
    for (yr in years) {
      rds_name <- paste0(st, "-", yr, ".rds")
      rds_path <- file.path(.data_dir(), rds_name)
      if (file.exists(rds_path) && !overwrite) {
        message("\n", rds_name, " already exists. Skipping.")
        next
      }
      message("\nDownloading, reading, and saving caliBISG file for: ", st, ", ", yr)
      temp_csv <- .download_calibisg_csv(st, yr, progress)
      df <-  readr::read_csv(temp_csv, progress = FALSE, show_col_types = FALSE)
      file.remove(temp_csv)
      saveRDS(as.data.frame(df), file = rds_path)
    }
  }
  invisible(TRUE)
}

#' @rdname caliBISG-data
#' @export
#'
#' @details
#' * `delete_data()`: Delete all or a subset of the data files stored internally.
#'
#' @return
#' * `delete_data()`: (logical) Invisibly, a vector indicating if each specified
#' file has been deleted successfully.
#'
delete_data <- function(state, year) {
  if (missing(state) && missing(year)) {
    to_delete <- available_data()
  } else {
    states <- if (missing(state)) .all_calibisg_states() else toupper(state)
    years <- if (missing(year)) .all_calibisg_years() else as.integer(year)
    .validate_calibisg_states_years(states, years)
    to_delete <- intersect(paste0(states, "-", years, ".rds"), available_data())
  }
  if (length(to_delete)) {
    message("Deleting data files: ", paste(to_delete, collapse = ", "))
  } else {
    message("No files found for deletion.")
  }
  invisible(file.remove(file.path(.data_dir(), to_delete)))
}

#' @rdname caliBISG-data
#' @export
#'
#' @details
#' * `available_data()`: List the names of the data files that have already been
#'  downloaded and are available for use.
#'
#' @return * `available_data()`: (character vector) The names of the data files
#'   that have already been downloaded.
#'
available_data <- function(year) {
  files <- list.files(.data_dir(), full.names = FALSE)
  if (!missing(year)) {
    files <- files[grepl(paste0("-", year, ".rds$"), files)]
  }
  files
}

#' @rdname caliBISG-data
#' @export
#'
#' @details
#' * `load_data()`: Load the data for a particular state and  year. This is only
#' necessary if you want to work with the full data files directly. When using
#' the functions provided by this package (e.g. [race_probabilities()]) the data
#' will be loaded internally automatically.
#'
#' @return
#' * `load_data()`: (data frame) The caliBISG data for the specified state and year.
#'
load_data <- function(state, year = 2020) {
  stopifnot(
    is.character(state),
    length(state) == 1,
    is.numeric(year),
    length(year) == 1
  )
  .ensure_data_available(state, year)
  readRDS(.data_path(state, year))
}


# internal ----------------------------------------------------------------

#' Load the data for a particular state into the internal data environment
#' unless it's already loaded
#'
#' @noRd
#' @param error_if_missing (logical) Whether to error if the data is not available.
#' @return (data frame) The data for the specified state or, if
#'   `error_is_missing = FALSE`, `NULL` if the data is not available.
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
#'
#' @noRd
#' @return (logical) `TRUE` if the data is available, `FALSE` otherwise.
#'
.is_data_available <- function(state, year) {
  file.exists(.data_path(state, year))
}

#' Error if data for a particular state-year is not available
#'
#' @noRd
#' @return (logical) `TRUE`, invisibly, if no error.
#'
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

#' Get the path to the internal data directory.
#'
#' @noRd
#' @return (string) The path to the directory.
#'
.data_dir <- function() {
  dir <- tools::R_user_dir("caliBISG", "data")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  dir
}

#' Get the path to the data file for a particular state-year pair
#'
#' @noRd
#' @return (string) The path to the data file.
#'
.data_path <- function(state, year) {
  file.path(.data_dir(), paste0(toupper(state), "-", year, ".rds"))
}

#' List the states for which caliBISG data is currently available for download
#'
#' @noRd
#' @return (character vector) The two-letter state abbreviations of the
#'   available states.
#'
.all_calibisg_states <- function() {
  c("FL", "GA", "NC", "NY", "OH", "OK", "VT", "WA")
}

#' List the years that are currently available for download
#'
#' @noRd
#' @return (integer vector) The years of the available data.
#'
.all_calibisg_years <- function() {
  years <- c(2020)
  as.integer(years)
}

#' Validate that caliBISG data is available for user-specified states-years
#'
#' @noRd
#' @return (logical) `TRUE`, invisibly, if no error.
#'
.validate_calibisg_states_years <- function(state, year) {
  valid_states  <- .all_calibisg_states()
  valid_years <- .all_calibisg_years()

  bad_states <- setdiff(state, valid_states)
  if (length(bad_states) > 0) {
    stop(
      "Invalid states requested: ", paste(bad_states, collapse = ", "),
      "\n  Available states are: ", paste(valid_states, collapse = ", "),
      call. = FALSE
    )
  }

  bad_years  <- setdiff(year, valid_years)
  if (length(bad_years) > 0) {
    stop(
      "Invalid years requested: ", paste(bad_years, collapse = ", "),
      "\n  Available years are: ", paste(valid_years, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Download a CSV file from assets of a specific GitHub release
#'
#' @noRd
#' @description Fetches release metadata from the GitHub API, finds the CSV that
#'   matches the requested state and year, and streams it straight to a
#'   temporary file.  We keep error-handling intentionally simple: **any** HTTP
#'   error triggers the same generic message that reminds the user to set a
#'   personal access token if the failure is due to rate-limiting.
#'
#' @param state,year,progress Same as above.
#' @param version The version of the caliBISG package release.
#' @return (string) The path to the local temporary file containing the
#'   downloaded CSV file.
#'
.download_calibisg_csv <- function(state,
                                   year,
                                   progress = TRUE,
                                   version = NULL) {
  # Resolve GitHub personal access token:
  # gitcreds -> GITHUB_PAT -> GITHUB_TOKEN -> anonymous
  token <- NULL
  if (requireNamespace("gitcreds", quietly = TRUE)) {
    cred <- try(gitcreds::gitcreds_get(), silent = TRUE)
    if (!inherits(cred, "try-error") &&
        !is.null(cred$password) &&
        identical(cred$host, "github.com")) {
      token <- cred$password
    }
  }
  if (is.null(token) || !nzchar(token)) {
    token <- Sys.getenv("GITHUB_PAT", unset = Sys.getenv("GITHUB_TOKEN", ""))
  }

  # Attach Authorization header when we have a token
  add_auth_header <- function(request) {
    if (nzchar(token)) {
      request <- httr2::req_headers(request, Authorization = paste("token", token))
    }
    request
  }

  # Build the releases endpoint
  owner <- "jgabry"
  repo <- "caliBISG"
  release_url <- if (is.null(version)) {
    sprintf("https://api.github.com/repos/%s/%s/releases/latest", owner, repo)
  } else {
    tag <- if (grepl("^v", version, ignore.case = TRUE)) version else paste0("v", version)
    sprintf("https://api.github.com/repos/%s/%s/releases/tags/%s", owner, repo, tag)
  }

  # Request release metadata
  req  <- httr2::request(release_url)
  req  <- add_auth_header(req)
  resp <- httr2::req_perform(req)

  if (httr2::resp_is_error(resp)) {
    status <- httr2::resp_status(resp)
    stop(
      "Failed to fetch release info (HTTP ", status, ").\n",
      "If this is due to rate-limiting, set GITHUB_PAT (or GITHUB_TOKEN) and try again.",
      call. = FALSE
    )
  }

  # Find the right asset
  release_info <- httr2::resp_body_json(resp)
  file_name <- sprintf("calibisg_%s%s.csv", tolower(state), year)
  assets <- release_info$assets
  idx <- which(vapply(assets, `[[`, "", "name") == file_name)
  if (length(idx) == 0) {
    stop(
      "Asset '", file_name, "' not found in release '", release_info$tag_name, "'.",
      call. = FALSE
    )
  }
  asset_id <- assets[[idx]]$id
  asset_url_api <- sprintf(
    "https://api.github.com/repos/%s/%s/releases/assets/%s",
    owner, repo, asset_id
  )

  # Download the asset
  temp_csv_file <- tempfile(fileext = ".csv")
  req2 <- httr2::request(asset_url_api)
  req2 <- httr2::req_headers(req2, Accept = "application/octet-stream")
  req2 <- add_auth_header(req2)
  if (isTRUE(progress)) {
    req2 <- httr2::req_progress(req2)
  }
  resp2 <- httr2::req_perform(req2, path = temp_csv_file)

  if (httr2::resp_is_error(resp2)) {
    status2 <- httr2::resp_status(resp2)
    file.remove(temp_csv_file)
    stop(
      "Failed to download asset (HTTP ", status2, ").\n",
      "If this is due to rate-limiting, set GITHUB_PAT (or GITHUB_TOKEN) and try again.",
      call. = FALSE
    )
  }

  temp_csv_file
}
