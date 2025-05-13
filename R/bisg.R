#' Compute traditional BISG
#'
#' @description Compute Bayesian Improved Surname Geocoding (BISG) race‐probability
#' estimates using census surname and county reference tables.
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
#'      * `bisg_aian`
#'      * `bisg_api`
#'      * `bisg_black_nh`
#'      * `bisg_hispanic`
#'      * `bisg_white_nh`
#'      * `bisg_other`
#'
#' @examples
#' bisg(
#'   name = c("Lopez", "Jackson", "Smith"),
#'   county = c("King", "King", "Chittenden"),
#'   state = c("WA", "WA", "VT"),
#'   year = 2020
#' )
#' }
#'
bisg <- function(name, county, state, year = 2020) {
  if (!is.character(name)   || !is.character(county)) {
    stop("`name` and `county` must both be character vectors.", call. = FALSE)
  }
  if (length(name) != length(county)) {
    stop("`name` and `county` must be the same length.", call. = FALSE)
  }
  if (!is.character(state)) {
    stop("`state` must be a character vector.", call. = FALSE)
  }
  if (length(state) == 1L) {
    state <- rep(state, length(name))
  } else if (length(state) != length(name)) {
    stop("`state` must be length-1 or the same length as `name`.", call. = FALSE)
  }
  if (length(year) != 1L || !is.numeric(year)) {
    stop("`year` must be a single numeric value.", call. = FALSE)
  }
  county <- tolower(county)
  name <- tolower(name)

  # reference tables
  df_surnames <- race_x_surname_data()
  df_national  <- race_x_usa_data(year)

  # collect county tables for *each* unique state once
  states <- toupper(state)
  unique_states <- unique(states)
  df_counties <- do.call(
    rbind,
    lapply(unique_states, function(st) {
      tmp <- race_x_county_data(st, year)
      if (!"state" %in% names(tmp))  # add state column if absent
        tmp$state <- st
      tmp
    })
  )

  # column-name sets & sanity checks
  race_suffixes <- .race_column_order()
  cen_cols  <- paste0("cen_r_given_sur_", race_suffixes)
  prob_cols <- paste0("prob_", race_suffixes)
  bisg_cols <- paste0("bisg_", race_suffixes)
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
    state = states,
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

  # fill unmatched surnames with "all other names"
  missing_name <- is.na(df[[cen_cols[1]]])
  if (any(missing_name)) {
    other_vals_row <- which(df_surnames$name == "all other names")
    other_vals <- df_surnames[other_vals_row, cen_cols]
    if (nrow(other_vals) != 1) {
      stop("Could not find a unique 'all other names' row in surname table.", call. = FALSE)
    }
    df[missing_name, cen_cols] <- other_vals[rep(1, sum(missing_name)), ]
  }

  # merge county distributions
  df <- merge(
    df,
    df_counties[, c("state", "county", prob_cols)],
    by      = c("state", "county"),
    all.x   = TRUE,
    sort    = FALSE
  )

  # BISG calculation
  sur_mat <- as.matrix(df[, cen_cols])
  geo_mat <- as.matrix(df[, prob_cols])
  bisg_mat <- sur_mat * geo_mat
  nat_vec  <- as.numeric(df_national[1, prob_cols])
  bisg_mat <- sweep(bisg_mat, 2, nat_vec, FUN = "/")
  bisg_mat <- bisg_mat / rowSums(bisg_mat)

  out <- data.frame(
    name   = df$name,
    county = df$county,
    state  = df$state,
    year   = year,
    bisg_mat,
    stringsAsFactors = FALSE
  )
  names(out)[-(1:4)] <- bisg_cols

  # restore original order
  out <- out[order(df$id), ]
  rownames(out) <- NULL
  out
}

# internal ----------------------------------------------------------------

#' Access internal data files for .race_x_county, .race_x_surname, .race_x_usa
#' data frames used to computed regular BISG probabilities
#' @return A data frame / tibble
#' @noRd
race_x_county_data <- function(state, year) {
  get(paste0(".race_x_county_", tolower(state), "_", year))
}
#' @noRd
race_x_surname_data <- function() {
  .race_x_surname
}
#' @noRd
race_x_usa_data <- function(year) {
  get(paste0(".race_x_usa_", year))
}




# Version for a single state
# bisg <- function(name,
#                          county,
#                          state,
#                          year = 2020) {
#
#   if (!is.character(name) || !is.character(county)) {
#     stop("`name` and `county` must both be character vectors.", call. = FALSE)
#   }
#   if (length(name) != length(county)) {
#     stop("`name` and `county` must be the same length.", call. = FALSE)
#   }
#   if (length(state) != 1 || !is.character(state)) {
#     stop("`state` must be a single character string.", call. = FALSE)
#   }
#   if (length(year) != 1 || !is.numeric(year)) {
#     stop("`year` must be a single numeric value.", call. = FALSE)
#   }
#
#   df_surnames <- race_x_surname_data()
#   df_national <- race_x_usa_data(year)
#   df_counties <- race_x_county_data(state, year)
#
#   # column-name sets
#   cen_cols  <- paste0("cen_r_given_sur_", .race_column_order())  # surname × race
#   prob_cols <- paste0("prob_", .race_column_order())  # county × race
#   bisg_cols <- paste0("bisg_", .race_column_order())  # output names
#   stopifnot(colnames(df_surnames[-1]) == cen_cols,
#             colnames(df_national) == prob_cols,
#             colnames(df_counties[-1]) == prob_cols)
#
#   # build working data frame in original order
#   df <- data.frame(
#     id = seq_along(name),   # keep order after merges
#     name = name,
#     county = county,
#     stringsAsFactors = FALSE
#   )
#
#   # merge surname distributions
#   df <- merge(
#     df,
#     df_surnames[, c("name", cen_cols)],
#     by  = "name",
#     all.x   = TRUE,
#     sort    = FALSE
#   )
#
#   # if any names didn't match then all the probabilities for that name with be NA
#   # fill them in with the "all other names" distribution
#   missing_name <- is.na(df[[cen_cols[1]]])
#   if (any(missing_name)) {
#     other_vals_row <- which(df_surnames$name == "all other names")
#     other_vals <- df_surnames[other_vals_row, cen_cols]
#     if (nrow(other_vals) != 1) {
#       stop("Could not find a unique '", "all other names",
#            "' row in `df_surnames`.", call. = FALSE)
#     }
#     df[missing_name, cen_cols] <- other_vals[rep(1, sum(missing_name)), ]
#   }
#
#   # merge county distributions
#   df <- merge(
#     df,
#     df_counties[, c("county", prob_cols)],
#     by  = "county",
#     all.x   = TRUE,
#     sort    = FALSE
#   )
#
#   # BISG calculation
#   sur_mat <- as.matrix(df[, cen_cols])
#   geo_mat <- as.matrix(df[, prob_cols])
#
#   bisg_mat <- sur_mat * geo_mat                     # surname × county
#   nat_vec  <- as.numeric(df_national[1, prob_cols])  # USA marginals
#
#   bisg_mat <- sweep(bisg_mat, 2, nat_vec, FUN = "/")    # divide by nat. race
#   bisg_mat <- bisg_mat / rowSums(bisg_mat)
#
#   ## --- assemble result ----------------------------------------------------
#   out <- data.frame(
#     name   = df$name,
#     year = year,
#     state = toupper(state),
#     county = df$county,
#     bisg_mat,
#     stringsAsFactors = FALSE
#   )
#   names(out)[-(1:4)] <- bisg_cols
#
#   # restore original order
#   out <- out[order(df$id), ]
#
#   rownames(out) <- NULL
#   out
# }
