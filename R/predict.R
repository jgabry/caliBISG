#' Predict race given surname and location
#'
#' @description Predict race given surname and location using the calibrated
#'   BISG (caliBISG) method from Greengard and Gelman (2025). Traditional BISG
#'   estimates are also provided. See **Details**.
#'
#'   Before caliBISG is available, the data files for the relevant states and
#'   years must be downloaded using [download_data()].
#'
#' @export
#' @param name (character vector) A vector of surnames. Coerced to lowercase
#'   internally.
#' @param state (character vector) A vector of state abbreviations. Coerced to
#'   uppercase internally.
#' @param county (character vector) A vector of counties. Coerced to lowercase
#'   internally.
#' @param year (integer) The year of the data to use to compute the estimates.
#'   The default is 2020, which is currently the only available year. This
#'   default may change in the future when more years become available.
#'
#' @details
#' The `most_probable_race()` function finds the single most probable race given
#' surname and location.
#'
#' The `race_probabilities()` function provides probabilities for all of the
#' races rather than the single most probable race.
#'
#' The `print()` method pretty prints the output of `race_probabilities()`,
#' making it easier to compare caliBISG and BISG estimates. It prints a separate
#' table for each row in the returned data frame up to `max_print` rows.
#'
#' The first query for a particular `state` and `year` may take a few seconds
#' to first load the caliBISG data internally. Subsequent calls for the same
#' `state` and `year` will be faster.
#'
#' For some state, county, and surname combinations the caliBISG estimate
#' will not be available. In those cases we still provide traditional BISG
#' estimates as long as the state and county are valid.
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
#'     The data frame also has class `"compare_bisg"`, which enables defining a
#'     custom `print()` method.
#'
#' @references Philip Greengard and Andrew Gelman (2025). A calibrated BISG for
#'   inferring race from surname and geolocation.
#'   *Journal of the Royal Statistical Society Series A: Statistics in Society*.
#'   \url{https://doi.org/10.1093/jrsssa/qnaf003}.
#'
#' @examples
#' \dontrun{
#' download_data(c("VT", "WA"), 2020)
#'
#' most_probable_race("smith", "wa", "king")
#' most_probable_race(
#'   name = c("Lopez", "Jackson"),
#'   state = c("VT", "WA"),
#'   county = c("Chittenden", "King")
#' )
#'
#' race_probabilities("smith", "wa", "king")
#' race_probabilities("lopez", "vt", "chittenden")
#' probs2 <- race_probabilities(
#'   name = c("Lopez", "Smith"),
#'   state = c("VT", "WA"),
#'   county = c("Chittenden", "King")
#' )
#' str(probs2)
#' print(probs2, digits = 3)
#' }
#'
most_probable_race <- function(name, state, county, year = 2020) {
  prediction <- as.data.frame(race_probabilities(name, state, county, year))
  prediction$calibisg_race <- apply(
    prediction[, .calibisg_columns()], 1, function(probs) {
    if (all(is.na(probs))) {
      NA_character_
    } else {
      # For each row, find the race with the highest probability
      idx <- which.max(probs)
      sub("^calibisg_", "", names(probs)[idx])
    }
  })
  prediction$bisg_race <- apply(
    prediction[, .bisg_columns()], 1, function(probs) {
      if (all(is.na(probs))) {
        NA_character_
      } else {
        idx <- which.max(probs)
        sub("^bisg_", "", names(probs)[idx])
      }
  })
  prediction[, c(.demographic_columns(), "calibisg_race", "bisg_race", "in_census")]
}

#' @rdname most_probable_race
#' @export
#'
race_probabilities <- function(name, state, county, year = 2020) {
  .validate_inputs(name, state, county, year)
  calibisg_out_list <- lapply(seq_along(name), function(i) {
    .get_single_calibisg_record(name[i], state[i], county[i], year)
  })
  calibisg_out <- do.call(rbind, calibisg_out_list)
  bisg_out <- bisg(
    name = calibisg_out$name,
    state = calibisg_out$state,
    county = calibisg_out$county,
    year = unique(calibisg_out$year)
  )

  not_found_count_calibisg <- sum(!calibisg_out$.found)
  if (not_found_count_calibisg > 0) {
    warning(
      "caliBISG is not available for ",
      not_found_count_calibisg,
      " input(s). Returning NA estimates for those cases.",
      call. = FALSE
    )
  }

  not_found_count_bisg <- sum(!bisg_out$.found)
  if (not_found_count_bisg > 0) {
    warning(
      "Traditional BISG is not available for ",
      not_found_count_bisg,
      " input(s). Returning NA estimates for those cases.",
      call. = FALSE
    )
  }

  out <- merge(
    calibisg_out,
    bisg_out,
    by = c("name", "state", "county", "year"),
    all = TRUE,
    sort = FALSE
  )

  col_order <- c(
    .demographic_columns(),
    # interleave calibisg and bisg columns for easier visual comparison
    as.vector(rbind(
      paste0("calibisg_", .race_column_order()),
      paste0("bisg_", .race_column_order())
    )),
    "in_census"
  )
  structure(out[, col_order], class = c("compare_bisg", class(out)))
}

#' @rdname most_probable_race
#' @export
#' @param x (compare_bisg) For `print()`, the object returned by
#'   `race_probabilities()`, which is a data frame with subclass
#'   `"compare_bisg"`.
#' @param ... Currently unused.
#' @param digits (integer) For `print()`, the number of digits to display in the
#'   output. The default is to use two digits unless the global option
#'   `calibisg.digits` has been set.
#' @param max_print (integer) For `print()`, the maximum number of rows to
#'   print. Because the tables take up a lot of space in the console, the
#'   default is to print at most four tables unless the global option
#'   `calibisg.max_print` has been set.
#'
print.compare_bisg <- function(x,
                               ...,
                               digits = getOption("calibisg.digits", 2),
                               max_print = getOption("calibisg.max_print", 4)) {
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
    .print_table(x[j, ], digits)
    cat("\n")
  }

  #  tell the user how many rows weren't printed
  if (n_print < nrow(x)) {
    cat("Only the first" , n_print, "of", nrow(x), "rows printed.\n")
    cat("Use `print(max_print = ...)` or `options(calibisg.max_print = ...)`",
        "to print more rows.\n")
  }

  invisible(x)
}


# internal ----------------------------------------------------------------

.races <- function() {
  c("AIAN", "API", "Black", "Hispanic", "White", "Other")
}
.race_column_order <- function() {
  # Must match the suffixes that appear after "bisg_" or "calibisg_".
  c("aian", "api", "black_nh", "hispanic", "white_nh", "other")
}
.calibisg_columns <- function() {
  paste0("calibisg_", .race_column_order())
}
.bisg_columns <- function() {
  paste0("bisg_", .race_column_order())
}
.demographic_columns <- function() {
  c("name", "year", "state", "county")
}

#' Validate inputs for `race_probabilities()`, `most_probable_race()` and `bisg()`.
#' @noRd
.validate_inputs <- function(name, state, county, year) {
  if (length(year) != 1L || !is.numeric(year)) {
    stop("`year` must be a single numeric value.", call. = FALSE)
  }
  if (!is.character(name) || !is.character(county) || !is.character(state)) {
    stop("`name`, `state`, and `county` must be character vectors.", call. = FALSE)
  }
  lengths <- c(length(name), length(state), length(county))
  if (length(unique(lengths)) != 1L) {
    stop("`name`, `state`, and `county` must all have the same length.", call. = FALSE)
  }
  invalid_states <- setdiff(toupper(state), state.abb)
  if (length(invalid_states) > 0L) {
    stop(
      "Invalid state abbreviations: ",
      paste(unique(invalid_states), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Load the state-year data, filter by county and surname
#'
#' @noRd
#' @param name,state,county,year Same as above except scalars not vectors.
#' @return A data frame with all available columns plus a column `.found`
#'   indicating if the requested record was found.
.get_single_calibisg_record <- function(name, state, county, year) {
  stopifnot(
    is.character(name), length(name) == 1,
    is.character(county), length(county) == 1,
    is.character(state), length(state) == 1, nchar(state) == 2,
    is.numeric(year), length(year) == 1
  )
  name <- tolower(name)
  county <- tolower(county)
  state <- toupper(state)

  df <- .load_data_internal(state, year, error_if_missing = FALSE)
  subset_df <- df[df$name == name & df$county == county & df$year == year, ]
  rownames(subset_df) <- NULL

  if (NROW(subset_df) == 0) {
    out <- data.frame(
      name   = name,
      state  = state,
      county = county,
      year   = year,
      stringsAsFactors = FALSE
    )
    for (col in .calibisg_columns()) {
      out[[col]] <- NA
    }
    out$in_census <- NA
    out$.found <- FALSE
    return(out)
  }

  if (nrow(subset_df) > 1) {
    warning(
      "Multiple rows found for caliBISG for (name=", name,
      ", state=", state, ", county=", county, ", year=", year, "). ",
      "Returning the first match.",
      call. = FALSE
    )
    subset_df <- subset_df[1, ]
  }

  subset_df$.found <- TRUE
  subset_df
}

#' Capitalize the first letter of each string
#'
#' @noRd
#' @param x (string) The input string.
#' @return (string) The input string with the first letter capitalized.
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
.print_table <- function(data, digits) {
  calibisg_vals <- sapply(.calibisg_columns(), function(col) data[[col]])
  bisg_vals <- sapply(.bisg_columns(), function(col) data[[col]])

  # Format them with the desired number of digits
  fmt <- paste0("%.", digits, "f")
  calibisg_vals <- sprintf(fmt, calibisg_vals)
  bisg_vals <- sprintf(fmt, bisg_vals)

  row_labels <- .races()
  cat(sprintf("\n%-10s %-12s %-10s\n",
              "Race", "Pr_calibisg", "Pr_bisg"))
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
