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
#' @export
#' @param name (character vector) For `most_probable_race()` and
#'   `race_probabilities()`, a single surname. Can be a vector of surnames for
#'   `compare_race_probabilities()`. Coerced to lowercase internally.
#' @param state (character vector) For `most_probable_race()` and
#'   `race_probabilities()`, a single state abbreviation. Can be a vector of state
#'   abbreviations for `compare_race_probabilities()`.
#' @param county (character vector) For `most_probable_race()` and
#'   `race_probabilities()`, a single county. Can be a vector of counties for
#'   `compare_race_probabilities()`. Coerced to lowercase internally.
#' @param year (integer) The year of the data used to compute the estimates.
#'   Currently only 2020 is available.
#' @param ... Currently unused.
#' @param digits (integer) For printing the output of
#'   `compare_race_probabilities()`, the number of digits to display in the
#'   output.
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
#'
#' @return
#' * `most_probable_race()`: (data frame) A data frame with the following columns:
#'      - `name` (string): The surname.
#'      - `year` (integer): The year of the data used to compute the estimates.
#'      - `state` (string): The state.
#'      - `county` (string): The county.
#'      - `race` (string): The most probable race based on the raking estimates.
#'
#' * `race_probabilities()`: (data frame) A data frame with one
#' row and the following columns:
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
#' most_probable_race("lopez", "nc", "burke")
#'
#' race_probabilities("smith", "wa", "king")
#' race_probabilities("lopez", "nc", "burke")
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
#' }
#'
race_probabilities <- function(name, state, county, year = 2020) {
  rec <- .get_single_record(name, state, county, year)
  rec[, c(.demographic_columns(), .rake_columns()), drop = FALSE]
}

#' @rdname race_probabilities
#' @export
most_probable_race <- function(name, state, county, year = 2020) {
  prediction <- race_probabilities(name, state, county, year)
  if (anyNA(prediction)) {
    return(cbind(prediction[, .demographic_columns()], race = NA))
  }
  most_probable <- which.max(prediction[, .rake_columns()])
  prediction$race <- gsub("rake_", "", names(most_probable))
  prediction <- prediction[, c(.demographic_columns(), "race")]
  rownames(prediction) <- NULL
  prediction
}


#' @rdname race_probabilities
#' @export
compare_race_probabilities <- function(name, state, county, year = 2020) {
  if (!(length(state) == length(name) && length(county) == length(name))) {
    stop("`name`, `state`, and `county` must all have the same length.")
  }
  out_list <- lapply(seq_along(name), function(i) {
    .get_single_record(name[i], state[i], county[i], year)
  })
  out <- do.call(rbind, out_list)
  structure(out, class = c("raking_bisg", class(out)))
}


#' @rdname race_probabilities
#' @export
print_comparison_tables <- function(x, ..., digits = 4) {
  if (!inherits(x, "raking_bisg")) {
    stop("Input must be an object returned by compare_race_probabilities().")
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
    .pretty_table(x[j, ], digits)
    cat("\n\n")
  }
  invisible(x)
}


# internal ----------------------------------------------------------------


.demographic_columns <- function() {
  c("name", "year", "state", "county")
}
.rake_columns <- function() {
  c("rake_nh_aian", "rake_nh_api", "rake_nh_black", "rake_hispanic",
    "rake_nh_white", "rake_other")
}
.bisg_columns <- function() {
  c("bisg_nh_aian", "bisg_nh_api", "bisg_nh_black", "bisg_hispanic",
    "bisg_nh_white", "bisg_other")
}
.races <- function() {
  c("AIAN", "API", "Black", "Hispanic", "White", "Other")
}


#' Load the state-year data, filter by county and surname
#'
#' @noRd
#' @param name,state,county,year Same as above.
#' @return A data frame with all available columns (both BISG and raking
#'   estimates)
.get_single_record <- function(name, state, county, year) {
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
    warning(
      "No record found for (name=", name,
      ", state=", state,
      ", county=", county,
      ", year=", year,
      "). Returning NAs."
    )
    return(out)
  }

  if (nrow(subset_df) > 1) {
    warning(
      "Multiple rows found for (name=", name,
      ", state=", state, ", county=", county, ", year=", year, "). ",
      "Returning the first match."
    )
    subset_df <- subset_df[1, ]
  }

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
.pretty_table <- function(data, digits) {
  row_labels <- .races()
  raking_values <- NULL
  bisg_values <- NULL
  has_raking_estimates <- any(grepl("rake_", colnames(data)))
  has_bisg_estimates <- any(grepl("bisg_", colnames(data)))
  if (has_raking_estimates) {
    raking_values <- sapply(.rake_columns(), function(var) data[[var]])
  }
  if (has_bisg_estimates) {
    bisg_values <- sapply(.bisg_columns(), function(var) data[[var]])
  }

  # Ensure values are formatted to the correct number of digits
  format_string <- paste0("%.", digits, "f")
  if (!is.null(raking_values)) {
    raking_values <- sprintf(format_string, raking_values)
  }
  if (!is.null(bisg_values)) {
    bisg_values <- sprintf(format_string, bisg_values)
  }

  # Pretty print the table based on available estimates
  if (has_raking_estimates && has_bisg_estimates) {
    cat(sprintf("\n%-10s %-10s %-10s\n", "Race", "Pr_raking", "Pr_bisg"))
    cat(strrep("-", 30), "\n")
    for (i in seq_along(row_labels)) {
      cat(sprintf("%-10s %-10s %-10s\n", row_labels[i], raking_values[i], bisg_values[i]))
    }
  } else if (has_raking_estimates) {
    cat(sprintf("\n%-10s %-10s\n", "Race", "Pr_raking"))
    cat(strrep("-", 20), "\n")
    for (i in seq_along(row_labels)) {
      cat(sprintf("%-10s %-10s\n", row_labels[i], raking_values[i]))
    }
  } else if (has_bisg_estimates) {
    cat(sprintf("\n%-10s %-10s\n", "Race", "Pr_bisg"))
    cat(strrep("-", 20), "\n")
    for (i in seq_along(row_labels)) {
      cat(sprintf("%-10s %-10s\n", row_labels[i], bisg_values[i]))
    }
  }
  invisible(data)
}
