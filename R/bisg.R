#' Compute traditional BISG
#'
#' @description Compute Bayesian Improved Surname Geocoding (BISG)
#'   race‐probability estimates using census surname and county reference
#'   tables.
#'
#' @keywords internal
#' @export
#' @param name (character vector) A vector of surnames. Coerced to lowercase
#'   internally.
#' @param county (character vector) A vector of counties. Coerced to lowercase
#'   internally.
#' @param state (character vector) A vector of state abbreviations. Coerced to
#'   uppercase internally.
#' @param year (integer) The year of the data to use to compute the estimates.
#'   Currently only 2020 is available.
#'
#' @return (data frame) A data frame with colums `name`, `year`, `state`,
#'   `county`, plus six BISG probability columns:
#'   * `bisg_aian`
#'   * `bisg_api`
#'   * `bisg_black_nh`
#'   * `bisg_hispanic`
#'   * `bisg_white_nh`
#'   * `bisg_other`
#'
#' @examples
#' \dontrun{
#' bisg(
#'   name = c("Lopez", "Jackson", "Smith"),
#'   county = c("King", "King", "Chittenden"),
#'   state = c("WA", "WA", "VT"),
#'   year = 2020
#' )
#' }
#'
bisg <- function(name, state, county, year = 2020) {
  .validate_inputs(name, state, county, year)
  county <- tolower(county)
  name <- tolower(name)
  state <- toupper(state)

  # reference tables
  df_surnames <- .race_x_surname_data()
  df_national <- .race_x_usa_data(year)

  # collect county tables for each unique state once
  county_data_list <-  lapply(unique(state), function(st) .race_x_county_data(st, year))
  df_counties <- do.call(rbind, county_data_list)

  # column name sanity checks
  cen_cols  <- paste0("cen_r_given_sur_", .race_column_order())
  prob_cols <- paste0("prob_", .race_column_order())
  bisg_cols <- .bisg_columns()
  stopifnot(
    identical(colnames(df_surnames[-1]), cen_cols),
    identical(colnames(df_national), prob_cols),
    identical(
      colnames(df_counties)[!(colnames(df_counties) %in% c("state", "county")) ],
      prob_cols
    )
  )

  # working data frame in original order
  df <- data.frame(
    id = seq_along(name), # used later to preserve original order
    name = name,
    county = county,
    state = state,
    stringsAsFactors = FALSE
  )

  # merge surname distributions
  df <- merge(
    df,
    df_surnames[, c("name", cen_cols)],
    by = "name",
    all.x = TRUE,
    sort = FALSE
  )

  # use "all other names" distribution for unmatched surnames
  missing_name <- is.na(df[[cen_cols[1]]])
  if (any(missing_name)) {
    other_vals_row <- which(df_surnames$name == "all other names")
    other_vals <- df_surnames[other_vals_row, cen_cols]
    df[missing_name, cen_cols] <- other_vals[rep(1, sum(missing_name)), ]
  }

  # merge county distributions
  df <- merge(
    df,
    df_counties[, c("state", "county", prob_cols)],
    by = c("state", "county"),
    all.x = TRUE,
    sort = FALSE
  )

  # BISG calculation
  sur_mat <- as.matrix(df[, cen_cols])
  geo_mat <- as.matrix(df[, prob_cols])
  bisg_mat <- sur_mat * geo_mat
  nat_vec  <- as.numeric(df_national[1, prob_cols])
  bisg_mat <- sweep(bisg_mat, 2, nat_vec, FUN = "/")
  bisg_mat <- bisg_mat / rowSums(bisg_mat)

  out <- data.frame(
    name = df$name,
    year = year,
    state = df$state,
    county = df$county,
    bisg_mat,
    stringsAsFactors = FALSE
  )
  names(out)[-(1:4)] <- bisg_cols
  out <- out[order(df$id), ] # restore original order
  rownames(out) <- NULL
  out$.found <- !.is_bisg_na(out)
  out
}

# internal ----------------------------------------------------------------

#' Access internal data files for use in computing regular BISG probabilities
#' @return (data frame) The requested data frame.
#' @noRd
.race_x_county_data <- function(state, year) {
  get(paste0(".race_x_county_list_", year))[[tolower(state)]]
}
#' @noRd
.race_x_surname_data <- function() {
  .race_x_surname_df
}
#' @noRd
.race_x_usa_data <- function(year) {
  get(paste0(".race_x_usa_df_", year))
}


#' Check if all BISG columns are NA
#' @noRd
#' @param data (data frame) The data frame to check.
#' @return (logical vector) A vector with each element indicating if the corresponding
#' row in `data` has NAs for all the BISG columns.
.is_bisg_na <- function(data) {
  rowSums(is.na(data[, .bisg_columns()])) == length(.bisg_columns())
}

