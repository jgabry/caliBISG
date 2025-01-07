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
#' @param name (string) A surname. Will be coerced to lowercase internally.
#' @param state (string) A two-letter state abbreviation.
#' @param county (string) A county within `state`. Will be coerced to lowercase
#'   internally.
#' @param year (integer) The year of the data used to compute the estimates.
#'   Currently only 2020 is available.
#' @param ... Currently unused.
#' @param digits (integer) For printing the output of
#'   `compare_race_probabilities()`, the number of digits to display in the
#'   output.
#'
#' @details
#' * `race_probabilities()`: Loads the data internally for the specified
#' state-year, then looks up the surname and county and returns the
#' raking-based probabilities for all of the races.
#' * `compare_race_probabilities()`: Same as `race_probabilities()` but also
#' includes "Improved BISG" estimates and has a custom print method that
#' produces a table comparing the two sets of estimates. Here, "Improved BISG"
#' refers to traditional BISG but including an adjustment to those predictions
#' that accounts for the fact that we’re making predictions on registered
#' voters, not the general population. It uses the registered voter status x
#' race distribution and an application of Bayes rule.
#' * `predict_race()`: Same as `race_probabilities()` but only includes the
#' single most probable race, not probabilities for all the races.
#'
#' @return
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
#' but the data frame has a custom class and print method and includes the
#' following additional columns:
#'      - `bisg_nh_aian` (numeric): The improved BISG estimate for non-Hispanic American Indian and Alaskan Native.
#'      - `bisg_nh_api` (numeric): The improved BISG estimate for non-Hispanic Asian and Pacific Islander.
#'      - `bisg_nh_black` (numeric): The improved BISG estimate for non-Hispanic Black.
#'      - `bisg_hispanic` (numeric): The improved BISG estimate for Hispanic.
#'      - `bisg_nh_white` (numeric): The improved BISG estimate for non-Hispanic White.
#'      - `bisg_other` (numeric): The improved BISG estimate for other.
#'
#' * `predict_race()`: (data frame) A data frame with the following columns:
#'      - `name` (string): The surname.
#'      - `year` (integer): The year of the data used to compute the estimates.
#'      - `state` (string): The state.
#'      - `county` (string): The county.
#'      - `race` (string): The most probable race based on the raking estimates.
#'
#' @references Philip Greengard and Andrew Gelman (2024). An improved BISG for
#'   inferring race from surname and geolocation.
#'   \url{https://arxiv.org/abs/2304.09126}.
#'
#' @examples
#' \dontrun{
#' predict_race("smith", "wa", "king")
#' predict_race("lopez", "nc", "burke")
#'
#' race_probabilities("smith", "wa", "king")
#' race_probabilities("lopez", "nc", "burke")
#'
#' compare_race_probabilities("smith", "wa", "king")
#' compare_race_probabilities("lopez", "nc", "burke")
#'
#' # Only use 2 digits when printing the comparison table
#' print(
#'  compare_race_probabilities("lopez", "nc", "burke"),
#'  digits = 2
#' )
#' }
#'
race_probabilities <- function(name, state, county, year = 2020) {
  out <- all_probabilities(name, state, county, year)
  out[, !grepl("bisg_", colnames(out))]
}

#' @rdname race_probabilities
#' @export
compare_race_probabilities <- function(name, state, county, year = 2020) {
  out <- all_probabilities(name, state, county, year)
  structure(
    out,
    class = c("raking_bisg", class(out))
  )
}

#' @rdname race_probabilities
#' @export
predict_race <- function(name, state, county, year = 2020) {
  prediction <- race_probabilities(name, state, county, year)
  raking_columns <- grepl("rake_", colnames(prediction))
  most_likely <- which.max(prediction[, raking_columns])
  prediction$race <- gsub("rake_", "", names(most_likely))
  prediction <- prediction[, c("name", "year", "state", "county", "race")]
  rownames(prediction) <- NULL
  prediction
}



#' @rdname race_probabilities
#' @export
print.raking_bisg <- function(x, ..., digits = 4) {
  cat(
    sprintf(
      "Surname:  %-10s\nState:    %-10s\nCounty:   %-10s\nYear:     %-10s\n",
      capitalize(x$name[1]),
      toupper(x$state[1]),
      capitalize(x$county[1]),
      x$year[1]
    )
  )
  pretty_table(x, digits)
  invisible(x)
}



# internal ----------------------------------------------------------------

#' Load the state-year data, filter by county and surname
#'
#' @noRd
#' @param name,state,county,year Same as above.
#' @return A data frame with all available columns (both BISG and raking
#'   estimates)
all_probabilities <- function(name, state, county, year = 2020) {
  stopifnot(
    is.character(name), length(name) == 1,
    is.character(county), length(county) == 1,
    is.character(state), length(state) == 1, nchar(state) == 2,
    is.numeric(year), length(year) == 1, year == 2020
  )
  name <- tolower(name)
  county <- tolower(county)
  state <- tolower(state)

  state_data <- load_data_internal(state, year = 2020)
  if (!county %in% state_data$county) {
    stop(
      "No data found for ", capitalize(county),
      ", ", toupper(state), "."
    )
  }
  county_data <- state_data[state_data$county == county, ]
  if (!name %in% county_data$name) {
    stop(
      "No data found for surname ", dQuote(capitalize(name)),
      " in ", capitalize(county),
      ", ", toupper(state), "."
    )
  }
  name_data <- county_data[county_data$name == name, ]
  rownames(name_data) <- NULL
  name_data
}

#' Get just the BISG estimates for all the races
#'
#' @noRd
#' @param name,state,county,year Same as above.
#' @return * `race_probabilities_bisg()`: (data frame) The same as
#'   `race_probabilities()` but with BISG estimates instead of raking-based
#'   estimates.
race_probabilities_bisg <- function(name, state, county, year = 2020) {
  out <- race_probabilities(name, state, county, year)
  out[, !grepl("raking_", colnames(out))]
}


#' Capitalize the first letter of each string
#'
#' @noRd
#' @param x (string) The input string.
#' @return (string) The input string with the first letter capitalized.
#'
capitalize <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

#' Pretty print the table of estimates
#'
#' @noRd
#' @param data (data frame) The data frame with the estimates.
#' @param digits (integer) The number of digits to display in the output.
#' @return The input data, invisibly.
#'
pretty_table <- function(data, digits) {
  races <- c("nh_aian", "nh_api", "nh_black", "hispanic", "nh_white", "other")
  raking_vars <- paste0("rake_", races)
  bisg_vars <- paste0("bisg_", races)
  row_labels <- c("AIAN", "API", "Black", "Hispanic", "White", "Other")

  raking_values <- NULL
  bisg_values <- NULL
  has_raking_estimates <- any(grepl("rake_", colnames(data)))
  has_bisg_estimates <- any(grepl("bisg_", colnames(data)))
  if (has_raking_estimates) {
    raking_values <- sapply(raking_vars, function(var) data[[var]])
  }
  if (has_bisg_estimates) {
    bisg_values <- sapply(bisg_vars, function(var) data[[var]])
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
