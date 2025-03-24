#' Predict race given surname and location
#'
#' @description Predict race given surname and location using the raking-based
#'   Calibrated BISG (caliBISG) method from Greengard and Gelman (2024). See
#'   **Details**.
#'
#'   Before these functions can be used, the data files for the relevant states
#'   must be downloaded using [download_data()].
#'
#'   These functions are just convenience functions for querying the large data
#'   files. You can also [load][load_data()] and use the data files directly.
#'
#'   NOTE: the first query for a particular `state` and `year` may take a few
#'   seconds to first load the data internally. Subsequent calls for the same
#'   `state` and `year` will be faster.
#'
#' @export
#' @param name (character vector) A vector of surnames. Coerced to lowercase
#'   internally.
#' @param state (character vector) A vector of state abbreviations. Coerced to
#'   uppercase internally.
#' @param county (character vector) A vector of counties. Coerced to lowercase
#'   internally.
#' @param year (integer) The year of the data used to compute the estimates.
#'   Currently only 2020 is available.
#'
#' @details
#' * `most_probable_race()`: The single most probable race according to the
#' caliBISG method.
#' * `race_probabilities()`: Probabilities for all of the races based on the
#' caliBISG method.
#' * `compare_race_probabilities()`: Same as `race_probabilities()` but also
#' includes traditional BISG estimates for comparison purposes.
#' * `print_comparison_tables()`: Pretty print the output of
#' `compare_race_probabilities()`. Prints a separate table for each input
#' record, so this is most useful when only a small number of records were
#' requested.
#'
#' @return
#' * `most_probable_race()`: (data frame) A data frame with the following columns:
#'      - `name` (string): The surname.
#'      - `year` (integer): The year of the data used to compute the estimates.
#'      - `state` (string): The state.
#'      - `county` (string): The county.
#'      - `race` (string): The most probable race based on the caliBISG estimates.
#'      - `in_census` (logical): Whether the surname is found in the list of
#'         names that appear at least 100 times in the census.
#'
#' * `race_probabilities()`: (data frame) A data frame with the same columns
#' as `most_probable_race()`, except the `race` column is replaced with the
#' following columns that give the raking-based probabilities:
#'      - `calibisg_aian` (numeric): The caliBISG estimate for American Indian and Alaskan Native.
#'      - `calibisg_api` (numeric): The caliBISG estimate for Asian and Pacific Islander.
#'      - `calibisg_black_nh` (numeric): The caliBISG estimate for non-Hispanic Black.
#'      - `calibisg_hispanic` (numeric): The caliBISG estimate for Hispanic.
#'      - `calibisg_white_nh` (numeric): The caliBISG estimate for non-Hispanic White.
#'      - `calibisg_other` (numeric): The caliBISG estimate for other.
#'
#' * `compare_race_probabilities`: (data frame) Same as `race_probabilities()`
#' but with additional columns for each race containing the traditional BISG
#' estimates (prefixed with `bisg_`).
#'
#' @references Philip Greengard and Andrew Gelman (2024). An improved BISG for
#'   inferring race from surname and geolocation.
#'   \url{https://arxiv.org/abs/2304.09126}.
#'
#' @examples
#' \dontrun{
#' most_probable_race("smith", "wa", "king")
#' most_probable_race(
#'   name = c("Lopez", "Jackson"),
#'   state = c("NC", "WA"),
#'   county = c("Burke", "King")
#' )
#'
#' race_probabilities("smith", "wa", "king")
#' race_probabilities("lopez", "nc", "burke")
#' probs2 <- race_probabilities(
#'   name = c("Smith", "Lopez"),
#'   state = c("NC", "WA"),
#'   county = c("Burke", "King")
#' )
#' str(probs2)
#'
#' comp1 <- compare_race_probabilities("smith", "wa", "king")
#' str(comp1)
#' print_comparison_tables(comp1)
#'
#' comp2 <- compare_race_probabilities(
#'   name = c("Lopez", "Jackson"),
#'   state = c("NC", "WA"),
#'   county = c("Burke", "King")
#' )
#' str(comp2)
#' print_comparison_tables(comp2)
#' print_comparison_tables(comp2, digits = 2)
#' }
#'
most_probable_race <- function(name, state, county, year = 2020) {
  prediction <- race_probabilities(name, state, county, year)
  prediction$race <- apply(
    prediction[, .calibisg_columns()], 1, function(probs) {
    if (all(is.na(probs))) {
      NA_character_
    } else {
      # For each row, find the race with the highest probability
      idx <- which.max(probs)
      sub("^calibisg_", "", names(probs)[idx])
    }
  })
  prediction[, c(.demographic_columns(), "race", "in_census")]
}

#' @rdname most_probable_race
#' @export
race_probabilities <- function(name, state, county, year = 2020) {
  stopifnot(is.numeric(year), length(year) == 1)
  if (!(length(state) == length(name) && length(county) == length(name))) {
    stop("`name`, `state`, and `county` must all have the same length.")
  }
  out_list <- lapply(seq_along(name), function(i) {
    .get_single_record(name[i], state[i], county[i], year, quiet = TRUE)
  })
  out <- do.call(rbind, out_list)
  not_found_count <- sum(!out$.found)
  if (not_found_count > 0) {
    warning(
      "No record found for ",
      not_found_count,
      " input(s). Returning NAs for those cases.",
      call. = FALSE
    )
  }
  out[, c(.demographic_columns(), .calibisg_columns(), "in_census")]
}


#' @rdname most_probable_race
#' @export
compare_race_probabilities <- function(name, state, county, year = 2020) {
  stopifnot(is.numeric(year), length(year) == 1)
  if (!(length(state) == length(name) && length(county) == length(name))) {
    stop("`name`, `state`, and `county` must all have the same length.")
  }
  out_list <- lapply(seq_along(name), function(i) {
    .get_single_record(name[i], state[i], county[i], year, quiet = TRUE)
  })
  out <- do.call(rbind, out_list)
  not_found_count <- sum(!out$.found)
  if (not_found_count > 0) {
    warning(
      "No record found for ",
      not_found_count,
      " input(s). Returning NAs for those cases.",
      call. = FALSE
    )
  }
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
#' @param x For `print_comparison_tables()`, the object returned by
#'   `compare_race_probabilities()`.
#' @param ... Currently unused.
#' @param digits (integer) For `print_comparison_tables()`, the number of digits
#'   to display in the output.
print_comparison_tables <- function(x, ..., digits = 4) {
  if (!inherits(x, "compare_bisg")) {
    stop("Input must be an object returned by compare_race_probabilities().",
         call. = FALSE)
  }
  for (j in seq_len(nrow(x))) {
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


#' Load the state-year data, filter by county and surname
#'
#' @noRd
#' @param name,state,county,year Same as above.
#' @param quiet Whether to suppress warnings.
#' @return A data frame with all available columns (both BISG and raking
#'   estimates) plus a column `.found` indicating if the requested record was
#'   found.
.get_single_record <- function(name, state, county, year, quiet = FALSE) {
  stopifnot(
    is.character(name), length(name) == 1,
    is.character(county), length(county) == 1,
    is.character(state), length(state) == 1, nchar(state) == 2,
    is.numeric(year), length(year) == 1
  )
  name <- tolower(name)
  county <- tolower(county)
  state <- toupper(state)

  df <- .load_data_internal(state, year)
  subset_df <- df[df$name == name & df$county == county & df$year == year, ]
  rownames(subset_df) <- NULL

  if (nrow(subset_df) == 0) {
    out <- data.frame(
      name   = name,
      state  = state,
      county = county,
      year   = year,
      stringsAsFactors = FALSE
    )
    for (col in c(.calibisg_columns(), .bisg_columns())) {
      out[[col]] <- NA_real_
    }
    out$in_census <- NA
    out$.found <- FALSE
    if (!quiet) {
      warning(
        "No record found for (name=", name,
        ", state=", state,
        ", county=", county,
        ", year=", year,
        "). Returning NAs.",
        call. = FALSE
      )
    }
    return(out)
  }

  if (nrow(subset_df) > 1) {
    warning(
      "Multiple rows found for (name=", name,
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
