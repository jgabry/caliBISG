#' Download caliBISG data files
#'
#' @name caliBISG-data
#' @aliases calibisg-data
#'
#' @description Currently caliBISG is available for the following states and
#'   years:
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
#' @examples
#' \dontrun{
#' # Download data just for Oklahoma and Washington for 2020
#' download_data(state = c("OK", "WA"), year = 2020)
#'
#' # Download all available data
#' download_data()
#'
#' # List the data files that have already been downloaded
#' available_data()
#' }
NULL


#' @rdname caliBISG-data
#' @export
#'
#' @details
#' * `download_data()`: Download the CSV data files from GitHub for the
#' specified states and years. The downloaded files are converted to data frames
#' and stored as [RDS][base::readRDS] files internally in a package-specific
#' data [directory][tools::R_user_dir]. A GitHub personal access token (PAT) can
#' be used to increase rate limits. If the **gitcreds** package is installed and
#' has a credential for `github.com`, the token provided by
#' [gitcreds::gitcreds_get()] will be used. Otherwise the environment variables
#' `GITHUB_PAT` and `GITHUB_TOKEN` will be checked in that order. If no token is
#' found, the request will be made anonymously, in which case you may run into
#' GitHub's unauthenticated rate limit if you are trying to download files many
#' times within one hour.
#'
#' @param state (character vector) For `download_data()`, the states to
#'   download. The default is to download caliBISG data for all available
#'   states. If specifying particular states, they should be provided as
#'   two-letter abbreviations. For `load_data()`, a single state to load.
#' @param year (integer vector) For `download_data()`, the years to download.
#'   The default is to download caliBISG data for all available years. For
#'   `load_data()`, a single year to load.
#' @param progress (logical) Whether to show a progress bar while downloading
#'   the data. The default is `TRUE`.
#'
#' @return
#' * `download_data()`: (logical) `TRUE`, invisibly, if no error.
#'
download_data <- function(state, year, progress = TRUE) {
  if (missing(state)) {
    states <- .all_calibisg_states()
  } else {
    states <- toupper(state)
  }
  if (missing(year)) {
    years <- .all_calibisg_years()
  } else {
    years <- as.integer(year)
  }
  .validate_calibisg_states_years(states, years)

  for (st in states) {
    for (yr in years) {
      rds_name <- paste0(st, "-", yr, ".rds")
      rds_path <- file.path(.data_dir(), rds_name)
      if (file.exists(rds_path)) {
        message("* ", rds_name, " already exists. Skipping.")
        next
      }

      message("* Downloading, reading, and saving file for: ", st, ", ", yr)
      temp_csv <- .download_calibisg_csv(st, yr, progress)
      df <-  readr::read_csv(temp_csv, progress = FALSE, show_col_types = FALSE)
      file.remove(temp_csv)

      message("  (Saving ", rds_path, ")")
      saveRDS(as.data.frame(df), file = rds_path)
    }
  }

  invisible(TRUE)
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
    is.character(state), length(state) == 1,
    is.numeric(year), length(year) == 1
  )
  .ensure_data_available(state, year)
  readRDS(.data_path(state, year))
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
available_data <- function() {
  list.files(.data_dir(), full.names = FALSE)
}

#' @rdname caliBISG-data
#' @export
#'
#' @details
#' * `delete_all_data()`: Delete all the data files stored internally.
#'
#' @return
#' * `delete_all_data()`: (logical) `TRUE` or `FALSE`, invisibly,
#' indicating success or failure.
#'
delete_all_data <- function() {
  message("Deleting data files: ", paste(available_data(), collapse = ", "))
  out <- unlink(.data_dir(), recursive = TRUE)
  invisible(as.logical(out))
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
#' @param version The version of the caliBISG package release.
#' @return (string) The path to the local temporary file containing the
#'   downloaded CSV file.
#'
.download_calibisg_csv <- function(state,
                                   year,
                                   progress = TRUE,
                                   version = NULL) {

  # Resolve token: gitcreds -> GITHUB_PAT -> GITHUB_TOKEN -> anonymous
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

  # Build auth config if we have a token
  configs <- if (nzchar(token)) {
    list(httr::add_headers(Authorization = paste("token", token)))
  } else {
    list()
  }

  owner <- "jgabry"
  repo  <- "caliBISG"
  state <- tolower(state)

  # Fetch release JSON (latest or specific tag)
  release_url <- if (is.null(version)) {
    sprintf("https://api.github.com/repos/%s/%s/releases/latest", owner, repo)
  } else {
    tag <- if (grepl("^v", version, ignore.case = TRUE)) version else paste0("v", version)
    sprintf("https://api.github.com/repos/%s/%s/releases/tags/%s", owner, repo, tag)
  }
  resp <- do.call(httr::GET, c(list(release_url), configs))
  status <- httr::status_code(resp)

  # Rate-limit check
  if (status == 403) {
    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    if (grepl("rate limit exceeded", body, ignore.case = TRUE)) {
      stop(
        "GitHub API rate limit exceeded.\n",
        "Please set GITHUB_PAT (or GITHUB_TOKEN) to raise the limit.",
        call. = FALSE
      )
    }
  }
  if (httr::http_error(resp)) {
    stop(
      "Failed to fetch release info: HTTP ", status,
      call. = FALSE
    )
  }

  release_info <- httr::content(resp)

  # Locate the right asset by name
  file_name <- sprintf("calibisg_%s%s.csv", state, year)
  assets <- release_info$assets
  idx <- which(vapply(assets, `[[`, "", "name") == file_name)
  if (length(idx) == 0) {
    stop(
      "Asset '", file_name, "' not found in release '", release_info$tag_name, "'.",
      call. = FALSE
    )
  }
  asset_id <- assets[[idx]]$id

  # Download the asset via the API
  asset_url_api <- sprintf(
    "https://api.github.com/repos/%s/%s/releases/assets/%s",
    owner, repo, asset_id
  )
  message("  (Downloading ", file_name, " from caliBISG release ", release_info$tag_name, ")")

  temp_csv_file <- tempfile(fileext = ".csv")
  resp2 <- do.call(httr::GET, c(
    list(
      asset_url_api,
      httr::add_headers(Accept = "application/octet-stream"),
      httr::write_disk(temp_csv_file, overwrite = TRUE),
      if (progress) httr::progress()
    ),
    configs
  ))
  if (httr::http_error(resp2)) {
    stop(
      "Failed to download asset: HTTP ", httr::status_code(resp2),
      call. = FALSE
    )
  }

  temp_csv_file
}

