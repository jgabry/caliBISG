#' Predict race given surname and location
#'
#' @description Predict race given surname and location using the improved
#'   raking-based BISG method from Greengard and Gelman (2024). See **Details**.
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
#' @param ... Currently unused.
#' @param digits (integer) For `print_comparison_tables()`, the number of digits
#'   to display in the output.
#'
#' @details
#' * `most_probable_race()`: The single most probable race according to the
#' raking-based approach.
#' * `race_probabilities()`: Raking-based probabilities for all of the races.
#' * `compare_race_probabilities()`: Same as `race_probabilities()` but also
#' includes "Improved BISG" estimates. Here, "Improved BISG" refers to
#' traditional BISG but including an adjustment to those predictions that
#' accounts for the fact that we’re making predictions on registered voters, not
#' the general population. It uses the registered voter status x race
#' distribution and an application of Bayes rule.
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
#'      - `race` (string): The most probable race based on the raking estimates.
#'
#' * `race_probabilities()`: (data frame) A data frame with the following columns:
#'      - `name` (string): The surname.
#'      - `year` (integer): The year of the data used to compute the estimates.
#'      - `state` (string): The state.
#'      - `county` (string): The county.
#'      - `rake_nh_aian` (numeric): The raking estimate for non-Hispanic American Indian and Alaskan Native.
#'      - `rake_nh_api` (numeric): The raking estimate for non-Hispanic Asian and Pacific Islander.
#'      - `rake_nh_black` (numeric): The raking estimate for non-Hispanic Black.
#'      - `rake_hispanic` (numeric): The raking estimate for Hispanic.
#'      - `rake_nh_white` (numeric): The raking estimate for non-Hispanic White.
#'      - `rake_other` (numeric): The raking estimate for other.
#'
#' * `compare_race_probabilities`: (data frame) Same as `race_probabilities()`
#' but with the following additional columns:
#'      - `bisg_nh_aian` (numeric): The improved BISG estimate for non-Hispanic American Indian and Alaskan Native.
#'      - `bisg_nh_api` (numeric): The improved BISG estimate for non-Hispanic Asian and Pacific Islander.
#'      - `bisg_nh_black` (numeric): The improved BISG estimate for non-Hispanic Black.
#'      - `bisg_hispanic` (numeric): The improved BISG estimate for Hispanic.
#'      - `bisg_nh_white` (numeric): The improved BISG estimate for non-Hispanic White.
#'      - `bisg_other` (numeric): The improved BISG estimate for other.
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
    prediction[, .rake_columns()], 1, function(probs) {
    if (all(is.na(probs))) {
      NA_character_
    } else {
      # For each row, find the race with the highest probability
      idx <- which.max(probs)
      sub("^rake_", "", names(probs)[idx])
    }
  })
  prediction[, c(.demographic_columns(), "race")]
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
  out[, c(.demographic_columns(), .rake_columns())]
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
    # interleave rake and bisg columns for easier visual comparison
    as.vector(rbind(
      paste0("rake_", .race_column_order()),
      paste0("bisg_", .race_column_order())
    ))
  )
  structure(
    out[, col_order, drop = FALSE],
    class = c("raking_bisg", class(out))
  )
}


#' @rdname most_probable_race
#' @export
print_comparison_tables <- function(x, ..., digits = 4) {
  if (!inherits(x, "raking_bisg")) {
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
  # Must match the suffixes that appear after "bisg_" or "rake_".
  c("nh_aian", "nh_api", "nh_black", "hispanic", "nh_white", "other")
}
.rake_columns <- function() {
  paste0("rake_", .race_column_order())
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
  state <- tolower(state)

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
    for (col in c(.rake_columns(), .bisg_columns())) {
      out[[col]] <- NA_real_
    }
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
#' @param data (data frame) The data frame with the estimates.
#' @param digits (integer) The number of digits to display in the output.
#' @return The input data, invisibly.
#'
.print_table <- function(data, digits) {
  raking_values <- sapply(.rake_columns(), function(var) data[[var]])
  bisg_values <- sapply(.bisg_columns(), function(var) data[[var]])

  # Ensure values are formatted to the correct number of digits
  format_string <- paste0("%.", digits, "f")
  raking_values <- sprintf(format_string, raking_values)
  bisg_values <- sprintf(format_string, bisg_values)

  row_labels <- .races()
  cat(sprintf("\n%-10s %-10s %-10s\n", "Race", "Pr_raking", "Pr_bisg"))
  cat(strrep("-", 30), "\n")
  for (i in seq_along(row_labels)) {
    cat(sprintf("%-10s %-10s %-10s\n", row_labels[i], raking_values[i], bisg_values[i]))
  }
  cat(strrep("-", 30), "\n")
  invisible(data)
}
