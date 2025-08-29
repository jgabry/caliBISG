#' Predict race given surname and location
#'
#' @name caliBISG-predict
#' @aliases calibisg-predict
#'
#' @description Predict race given surname and location using the calibrated
#'   BISG (caliBISG) method from Greengard and Gelman (2025). Traditional BISG
#'   estimates are also provided. For some state, county, and surname
#'   combinations the caliBISG estimate will not be available. In those cases we
#'   still provide traditional BISG estimates as long as the state and county
#'   are valid.
#'
#'   Before caliBISG is available, the data files for the relevant states and
#'   years must be downloaded using [download_data()].
#'
#'   The first query to `most_probable_race()` or `race_probabilities()` for a
#'   particular state and year may take a few seconds to first load the relevant
#'   caliBISG data internally. Subsequent queries for the same state and year
#'   will be faster.
#'
#'   If the `state` argument to `most_probable_race()` or `race_probabilities()`
#'   contains multiple states, they can be processed in parallel via the
#'   \pkg{future} package by setting a [future::plan]. However, this will only
#'   be faster if each state subset requires enough computation time so that the
#'   cost of starting workers and transferring data is outweighed by concurrent
#'   execution.
#'
#' @template calibisg-ref
#'
#' @examples
#' \dontrun{
#' download_data(c("VT", "OK"), 2020)
#'
#' most_probable_race("Smith", "OK", "Tulsa")
#' most_probable_race(
#'   name = c("Lopez", "Jackson"),
#'   state = c("VT", "OK"),
#'   county = c("Chittenden", "Tulsa")
#' )
#'
#' race_probabilities("Smith", "OK", "Tulsa")
#' race_probabilities("Lopez", "VT", "Chittenden")
#' probs2 <- race_probabilities(
#'   name = c("Lopez", "Smith"),
#'   state = c("VT", "OK"),
#'   county = c("Chittenden", "Tulsa")
#' )
#' str(probs2)
#' print(probs2, digits = 3)
#'
#' # caliBISG is not yet available for RI but we can still get
#' # regular BISG if we input a valid county
#' valid_counties("RI")
#' race_probabilities("Jones", "RI", "Providence")
#'
#' # using FIPS codes
#' # we can specify Albany County either by name or by its code 36001
#' most_probable_race(
#'  name = "Chan",
#'  state = "NY",
#'  county = "Albany"
#' )
#' most_probable_race(
#'  name = "Chan",
#'  state = "NY",
#'  county = fips_to_county("36001")
#' )
#' }
#'
#'
NULL

#' @rdname caliBISG-predict
#' @export
#'
#' @details
#' * `most_probable_race()`: Get the single most probable race given
#' surname and location.
#'
#' @param name (character vector) A vector of surnames. Coerced to lowercase
#'   internally.
#' @param state (character vector) A vector of state abbreviations. Coerced to
#'   uppercase internally. If a single state is provided, it is recycled to
#'   match the length of `name`.
#' @param county (character vector) A vector of counties. Coerced to lowercase
#'   internally. If a single county is provided, it is recycled to match the
#'   length of `name`. To use FIPS codes instead of county names, use the
#'   `fips_to_county()` function to convert FIPS codes to county names first.
#' @param year (integer) The year of the data to use to compute the estimates.
#'   The default is `2020`, which is currently the only available year. This
#'   default may change in the future when more years become available.
#'
#' @return
#' * `most_probable_race()`: (data frame) A data frame with number of rows equal
#' to the length of the input vectors and the following columns:
#'      - `name` (string): The surname.
#'      - `year` (integer): The year of the data used to compute the estimates.
#'      - `state` (string): The state.
#'      - `county` (string): The county.
#'      - `calibisg_race` (string): The most probable race according to caliBISG.
#'      - `bisg_race` (string): The most probable race according to traditional BISG.
#'      - `in_census` (logical): Whether the surname is found in the list of
#'         names that appear at least 100 times in the census.
#'
most_probable_race <- function(name, state, county, year = 2020) {
  prediction <- race_probabilities(name, state, county, year)
  class(prediction) <- "data.frame"

  calibisg_cols <- .calibisg_columns()
  bisg_cols <- .bisg_columns()
  calibisg_labels <- sub("^calibisg_", "", calibisg_cols)
  bisg_labels <- sub("^bisg_", "", bisg_cols)

  calibisg_idx <- max.col(prediction[, calibisg_cols, drop = FALSE])
  bisg_idx <- max.col(prediction[, bisg_cols, drop = FALSE])
  prediction$calibisg_race <- calibisg_labels[calibisg_idx]
  prediction$bisg_race <- bisg_labels[bisg_idx]

  col_order <- c(
    .demographic_columns(),
    "calibisg_race",
    "bisg_race",
    "in_census"
  )
  prediction[, col_order]
}


#' @rdname caliBISG-predict
#' @export
#'
#' @details
#' * `race_probabilities()`: Get probabilities for all of the races given
#' surname and location, rather than the single most probable race.
#'
#' @return
#' * `race_probabilities()`: (data frame) A data frame with number of rows equal
#' to the length of the input vectors and the same columns as
#' `most_probable_race()`, except the `calibisg_race` and `bisg_race` columns
#' are each replaced by multiple columns giving the probabilities of the various
#' races, not just the single most probable race. Those columns are:
#'      - `calibisg_aian` (numeric): The caliBISG estimate for American Indian and Alaskan Native.
#'      - `bisg_aian` (numeric): The traditional BISG estimate for American Indian and Alaskan Native.
#'      - `calibisg_api` (numeric): The caliBISG estimate for Asian and Pacific Islander.
#'      - `bisg_api` (numeric): The traditional BISG estimate for Asian and Pacific Islander.
#'      - `calibisg_black_nh` (numeric): The caliBISG estimate for non-Hispanic Black.
#'      - `bisg_black_nh` (numeric): The traditional BISG estimate for non-Hispanic Black.
#'      - `calibisg_hispanic` (numeric): The caliBISG estimate for Hispanic.
#'      - `bisg_hispanic` (numeric): The traditional BISG estimate for Hispanic.
#'      - `calibisg_white_nh` (numeric): The caliBISG estimate for non-Hispanic White.
#'      - `bisg_white_nh` (numeric): The traditional BISG estimate for non-Hispanic
#'      - `calibisg_other` (numeric): The caliBISG estimate for other.
#'      - `bisg_other` (numeric): The traditional BISG estimate for other.
#'
#'     The data frame also has class `"compare_calibisg"`, which enables defining a
#'     custom `print()` method.
#'
#' @import data.table
race_probabilities <- function(name, state, county, year = 2020) {
  .validate_inputs(name, state, county, year)
  name <- tolower(name)
  state <- .recycle(toupper(state), size = length(name))
  county <- .recycle(tolower(county), size = length(name))
  .warn_if_not_downloaded(state, year)

  input_dt <- data.table::data.table(
    .id = seq_along(name),
    name = name,
    state = state,
    county = county,
    year = year
  )
  input_split <- split(input_dt, list(input_dt$state, input_dt$year), drop = TRUE)
  calibisg_list <- future.apply::future_lapply(input_split, function(sub_dt) {
    df <- .load_data_internal(sub_dt$state[1], sub_dt$year[1], error_if_missing = FALSE)
    if (is.null(df)) {
      out <- as.data.frame(sub_dt)
      for (col in .calibisg_columns()) {
        out[[col]] <- NA
      }
      out$in_census <- NA
      out$.found <- FALSE
      return(out)
    }
    dt <- data.table::as.data.table(df)
    dt[, state := NULL]
    merged <- merge(sub_dt, dt, by = c("name", "county", "year"), all.x = TRUE, sort = FALSE)
    merged$.found <- !is.na(merged$in_census)
    as.data.frame(merged)
  })
  calibisg_out <- data.table::rbindlist(calibisg_list, use.names = TRUE, fill = TRUE)
  calibisg_out <- calibisg_out[order(.id)]
  calibisg_out[, .id := NULL]
  calibisg_out <- as.data.frame(calibisg_out)
  not_found_count_calibisg <- sum(!calibisg_out$.found)
  if (not_found_count_calibisg > 0) {
    warning(
      "caliBISG is not available for ",
      not_found_count_calibisg,
      " input(s). Returning NA estimates for those cases.",
      call. = FALSE
    )
  }

  bisg_out <- bisg(
    name = calibisg_out$name,
    state = calibisg_out$state,
    county = calibisg_out$county,
    year = unique(calibisg_out$year)
  )
  not_found_count_bisg <- sum(!bisg_out$.found)
  if (not_found_count_bisg > 0) {
    warning(
      "Traditional BISG is not available for ",
      not_found_count_bisg,
      " input(s). Returning NA estimates for those cases.",
      call. = FALSE
    )
  }

  out <- cbind(
    calibisg_out[, c(.demographic_columns(), .calibisg_columns(), "in_census")],
    bisg_out[, .bisg_columns(), drop = FALSE]
  )
  col_order <- c(
    .demographic_columns(),
    # interleave calibisg and bisg columns for easier visual comparison
    as.vector(rbind(.calibisg_columns(), .bisg_columns())),
    "in_census"
  )
  structure(out[, col_order], class = c("compare_calibisg", class(out)))
}
utils::globalVariables(".id") # to avoid 'no visible binding for global variable' in R CMD check

#' @rdname caliBISG-predict
#' @export
#'
#' @details
#' * `print()`: Pretty print the output of `race_probabilities()`, making it
#' easier to compare caliBISG and BISG estimates. It prints a separate
#' table for each row in the returned data frame up to `max_print` rows.
#'
#' @param x (compare_calibisg) For `print()`, the object returned by
#'   `race_probabilities()`, which is a data frame with subclass
#'   `"compare_calibisg"`.
#' @param ... Currently unused.
#' @param digits (integer) For `print()`, the number of digits to display in the
#'   output. The default is to use two digits unless the global option
#'   `calibisg.digits` has been set.
#' @param max_print (integer) For `print()`, the maximum number of rows to
#'   print. Because the tables take up a lot of space in the console, the
#'   default is to print at most four tables unless the global option
#'   `calibisg.max_print` has been set.
#'
#' @return
#' * `print()`: The input, invisibly.
#'
print.compare_calibisg <- function(x,
                                   ...,
                                   digits = getOption("calibisg.digits", 2),
                                   max_print = getOption("calibisg.max_print", 4)) {
  if (!is.numeric(digits) || length(digits) != 1L || digits < 0) {
    stop("`digits` must be a single non-negative integer.", call. = FALSE)
  }
  if (!is.numeric(max_print) || length(max_print) != 1L || max_print < 0) {
    stop("`max_print` must be a single non-negative integer.", call. = FALSE)
  }

  n_print <- min(max_print, nrow(x))
  for (j in seq_len(n_print)) {
    cat(
      sprintf(
        "Surname:  %-10s\nState:    %-10s\nCounty:   %-10s\nYear:     %-10s\n",
        .capitalize(x$name[j]),
        toupper(x$state[j]),
        .capitalize(x$county[j]),
        x$year[j]
      )
    )
    .print_comparison_table(x[j, ], digits)
    cat("\n")
  }
  if (n_print < nrow(x)) {
    cat("Only the first" , n_print, "of", nrow(x), "rows printed.\n")
    cat(
      "Use `print(max_print = ...)` or `options(calibisg.max_print = ...)`",
      "to print more rows.\n"
    )
  }

  invisible(x)
}

#' @rdname caliBISG-predict
#' @export
#'
#' @details
#' * `valid_counties()`: List the valid county names for a given state and year.
#' @return
#' * `valid_counties()`: (character vector) County names.
#'
valid_counties <- function(state, year = 2020) {
  .validate_state(state, allow_multiple = FALSE)
  .validate_year(year)
  counties <- .race_x_county_data(state, year)$county
  sort(unique(counties))
}

#' @rdname caliBISG-predict
#' @export
#'
#' @param fips (character vector) A vector of 5-digit FIPS codes.
#' @details
#' * `fips_to_county()`: Convert 5-digit FIPS codes to county names.
#' @return
#' * `fips_to_county()`: (character vector) County names.
#'
fips_to_county <- function(fips, year = 2020) {
  .validate_year(year)
  if (!is.character(fips) ||
      any(nchar(fips) != 5) ||
      any(!grepl("^[0-9]{5}$", fips))) {
    stop(
      "`fips` must be a character vector of 5-digit FIPS codes.",
      call. = FALSE
    )
  }
  df <- .fips_x_county_data(year)
  county <- df$county[match(fips, df$fips)]
  if (anyNA(county)) {
    stop(
      "The following FIPS codes could not be converted: ",
      paste(fips[is.na(county)], collapse = ", "),
      call. = FALSE
    )
  }
  county
}


# internal ----------------------------------------------------------------

.column_order <- function() {
  c("aian", "api", "black_nh", "hispanic", "white_nh", "other")
}
.calibisg_columns <- function() {
  paste0("calibisg_", .column_order())
}
.bisg_columns <- function() {
  paste0("bisg_", .column_order())
}
.demographic_columns <- function() {
  c("name", "year", "state", "county")
}

#' Input validation
#'
#' @noRd
#' @param name,state,county,year The inputs to validate.
#' @return (logical) `TRUE`, invisibly, if no error is thrown.
#'
.validate_inputs <- function(name, state, county, year) {
  .validate_year(year)
  .validate_state(state, allow_multiple = TRUE)

  # these two don't check if the name/county are legit just that they are
  # character vectors
  .validate_county(county)
  .validate_name(name)

  if (length(state) != 1L && length(state) != length(name)) {
    stop("`state` must be length 1 or the same length as `name`.", call. = FALSE)
  }
  if (length(county) != 1L && length(county) != length(name)) {
    stop("`county` must be length 1 or the same length as `name`.", call. = FALSE)
  }

  invisible(TRUE)
}
.validate_year <- function(year) {
  if (length(year) != 1L || !is.numeric(year) || is.na(year)) {
    stop("`year` must be a single numeric value.", call. = FALSE)
  }
  if (!year %in% .all_calibisg_years()) {
    stop(
      "`year` must be one of the available years: ",
      paste(.all_calibisg_years(), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
.validate_state <- function(state, allow_multiple = TRUE) {
  if (!is.character(state)) {
    stop("`state` must be a character vector.", call. = FALSE)
  }
  if (anyNA(state)) {
    stop("`state` must not contain NA values.", call. = FALSE)
  }
  if (length(state) > 1 && !allow_multiple) {
    stop("`state` must be a single two-letter state abbreviation.", call. = FALSE)
  }
  invalid_states <- setdiff(toupper(state), datasets::state.abb)
  if (length(invalid_states) > 0L) {
    stop(
      "Invalid state abbreviations: ",
      paste(unique(invalid_states), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
.validate_county <- function(county) {
  if (!is.character(county)) {
    stop("`county` must be a character vector.", call. = FALSE)
  }
  if (anyNA(county)) {
    stop("`county` must not contain NA values.", call. = FALSE)
  }
  invisible(TRUE)
}
.validate_name <- function(name) {
  if (!is.character(name)) {
    stop("`name` must be a character vector.", call. = FALSE)
  }
  if (anyNA(name)) {
    stop("`name` must not contain NA values.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Warn if the caliBISG data for the given states and year haven't been downloaded
#'
#' @noRd
#' @param state (character vector) A vector of state abbreviations.
#' @param year (integer) The year of the data to check.
#'
.warn_if_not_downloaded <- function(state, year) {
  states <- unique(state)
  calibisg_states <- states[states %in% .all_calibisg_states()]
  downloaded_states <- substr(available_data(year), 1, 2)
  not_downloaded_states <- calibisg_states[!calibisg_states %in% downloaded_states]
  if (length(not_downloaded_states)) {
    warning(
      "The caliBISG files for these states have not been downloaded: ",
      paste(not_downloaded_states, collapse = ", "),
      ". Please run `download_data()` first.",
      call. = FALSE
    )
  }
}

#' Access internal data for converting fips to county
#' @noRd
.fips_x_county_data <- function(year) {
  get(paste0(".fips_x_county_df_", year))
}


#' Load the state-year data, filter by a single county and surname
#'
#' @noRd
#' @param name (string) A single lowercase name.
#' @param state (string) A single uppercase state abbreviation.
#' @param county (string) A single lowercase county name.
#' @param year (integer) A single year.
#' @return (data frame) Data with all available columns plus a column `.found`
#'   indicating if the requested record was found.
#'
.get_single_calibisg_record <- function(name, state, county, year) {
  df <- .load_data_internal(state, year, error_if_missing = FALSE)
  subset_df <- df[df$name == name & df$county == county & df$year == year, ]
  rownames(subset_df) <- NULL
  if (NROW(subset_df) == 0) {
    out <- data.frame(
      name = name,
      state = state,
      county = county,
      year = year
    )
    for (col in .calibisg_columns()) {
      out[[col]] <- NA
    }
    out$in_census <- NA
    out$.found <- FALSE
    return(out)
  }
  subset_df$.found <- TRUE
  subset_df
}

#' Recycle to a certain size if length 1
#'
#' @param x (vector) A vector that is either length 1 or length `size`
#'   (this will have already been verified by `.validate_inputs()`), typically
#'   `state` or `county`.
#' @param size (integer) The desired length, typically `length(name)`.
#' @param return (vector) The maybe recycled vector `x`.
#' @noRd
#'
.recycle <- function(x, size) {
  if (length(x) == 1L) rep(x, size) else x
}

#' Capitalize the first letter of each string
#'
#' @noRd
#' @param x (character vector) The input strings.
#' @return (character vector) The input strings with the first letter capitalized.
#'
.capitalize <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

#' Pretty print the table of estimates
#'
#' @noRd
#' @param data (data frame) The single-row data frame with the estimates.
#' @param digits (integer) The number of digits to display in the output.
#' @return (data frame) The input data, invisibly.
#'
.print_comparison_table <- function(data, digits) {
  calibisg_vals <- sapply(.calibisg_columns(), function(col) data[[col]])
  bisg_vals <- sapply(.bisg_columns(), function(col) data[[col]])

  fmt <- paste0("%.", digits, "f")
  calibisg_vals <- sprintf(fmt, calibisg_vals)
  bisg_vals <- sprintf(fmt, bisg_vals)

  row_labels <- c("AIAN", "API", "Black", "Hispanic", "White", "Other")
  cat(sprintf(
    "\n%-10s %-12s %-10s\n",
    "Race", "Pr_calibisg", "Pr_bisg"
  ))
  cat(strrep("-", 40), "\n")
  for (i in seq_along(row_labels)) {
    cat(sprintf(
      "%-10s %-12s %-10s\n",
      row_labels[i],
      calibisg_vals[i],
      bisg_vals[i]
    ))
  }
  cat(strrep("-", 40), "\n")
  invisible(data)
}
