#' Download and load caliBISG data files
#'
#' @description Various functions for downloading and loading the caliBISG data
#'   files used by the package. See **Details**.
#'
#'   Currently caliBISG is available for the following states and years:
#'
#'   * States: FL, GA, NC, NY, OH, OK, VT, WA
#'   * Years: 2020
#'
#'   We are working on adding additional states and years. When caliBISG is
#'   unavailable we still provide traditional BISG. Traditional BISG does not
#'   require downloading any files.
#'
#' @export
#' @param states (character vector) The states to download. The default is to
#'   download caliBISG data for all available states. If specifying particular
#'   states, they should be provided as two-letter abbreviations.
#' @param years (integer vector) The years to download. The default is to
#'   download caliBISG data for all available years.
#' @param token (string) Optionally, a GitHub personal access token (PAT) for
#'   authentication. If `NULL` we check the `GITHUB_PAT` and then `GITHUB_TOKEN`
#'   environment variables. If you do not have a PAT, you can create one in your
#'   GitHub account settings under "Developer settings" -> "Personal access
#'   tokens". This is used to increase rate limits when downloading the data,
#'   but is typically not necessary unless you are downloading a large number of
#'   files within one hour.
#'
#' @details
#' * `download_data()`: Download the required data files for the specified
#' states and years. The downloaded CSV files are converted to data frames and
#' stored as [RDS][base::readRDS] files internally in a package-specific data
#' [directory][tools::R_user_dir].
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
download_data <- function(states, years, token = NULL) {
  if (missing(states)) {
    states <- .all_states()
  } else {
    states <- toupper(states)
  }
  if (missing(years)) {
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

      message("* Downloading, reading, and saving file for: ", st, ", ", yr)
      temp_csv <- .download_calibisg_csv(st, yr, version = NULL, token = token)
      df <-  readr::read_csv(temp_csv, show_col_types = FALSE, progress = FALSE)
      file.remove(temp_csv)

      # Add year and state and order the columns
      df <- as.data.frame(df)
      df$year <- yr
      df$state <- st
      df <- .rename_data(df)
      df <- .reorder_data(df)

      message("  (Saving ", rds_path, ")")
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



#' Download a CSV file asset from a specific GitHub release
#'
#' @noRd
#' @param state,year A single state and year.
#' @param version The version of the caliBISG package release.
#' @param token A GitHub personal access token (PAT) for authentication.
#' @return The path to a local temporary file containing the downloaded CSV.
#'
.download_calibisg_csv <- function(state,
                                   year,
                                   version = NULL,
                                   token = NULL) {

  if (!is.null(token) && !is.character(token)) {
    stop("`token` must be a character string or NULL.", call. = FALSE)
  }
  if (is.null(token)) {
    token <- Sys.getenv("GITHUB_PAT", unset = Sys.getenv("GITHUB_TOKEN", ""))
  }

  owner <- "jgabry"
  repo  <- "caliBISG"
  state <- tolower(state)

  # Build auth config if we have a token
  configs <- if (nzchar(token)) {
    list(httr::add_headers(Authorization = paste("token", token)))
  } else {
    list()
  }

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
      httr::progress()
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

