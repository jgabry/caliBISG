test_that("race_probabilities() warns if caliBISG files not downloaded", {
  delete_data(c("FL", "NC"), 2020)

  # FL and NC not downloaded, VA is not available
  expect_warning(
    expect_warning(
      race_probabilities(
        name = c("Chan", "Chan", "Chan", "Chan"),
        state = c("FL", "VA", "NC", "NC"),
        county = c("miami-dade", "fairfax", "burke", "burke")
      ),
      "The caliBISG files for these states have not been downloaded: FL, NC"
    ),
    "caliBISG is not available for 4 input(s). Returning NA estimates for those cases.",
    fixed = TRUE
  )
})

# download the rest of the states
suppressMessages(download_data(year = 2020, progress = FALSE))

test_that("race_probabilities() output hasn't changed", {
  expect_snapshot_value(
    race_probabilities(
      name   = c("lopez", "jackson", "smith", "chan", "xxx1", "lopez", "xxx2"),
      state  = c("VT", "OK", "WA", "NC", "NC", "VT", "WA"),
      county = c("Chittenden", "Tulsa", "King", "Wake", "Wake", "xxx", "King"),
      year   = 2020
    ),
    style = "deparse"
  )
})

test_that("race_probabilities() recycles state and county", {
  # both state and county are length 1
  expect_no_error(
    out <- race_probabilities(
      name   = c("smith", "lopez"),
      state  = "VT",
      county = "Chittenden",
      year   = 2020
    )
  )
  expect_s3_class(out, c("compare_calibisg", "data.frame"))
  expect_equal(nrow(out), 2)
  expect_equal(out$name, c("smith", "lopez"))
  expect_equal(out$state, c("VT", "VT"))
  expect_equal(out$county, c("chittenden", "chittenden"))

  # state is length 1, county is length 2
  expect_no_error(
    out <- race_probabilities(
      name   = c("smith", "lopez"),
      state  = "VT",
      county = c("Chittenden", "Windsor"),
      year   = 2020
    )
  )
  expect_s3_class(out, c("compare_calibisg", "data.frame"))
  expect_equal(nrow(out), 2)
  expect_equal(out$name, c("smith", "lopez"))
  expect_equal(out$state, c("VT", "VT"))
  expect_equal(out$county, c("chittenden", "windsor"))

  # state is length 2, county is length 1
  expect_no_error(
    out <- race_probabilities(
      name   = c("smith", "lopez"),
      state  = c("VT", "VT"),
      county = "Chittenden",
      year   = 2020
    )
  )
  expect_s3_class(out, c("compare_calibisg", "data.frame"))
  expect_equal(nrow(out), 2)
  expect_equal(out$name, c("smith", "lopez"))
  expect_equal(out$state, c("VT", "VT"))
  expect_equal(out$county, c("chittenden", "chittenden"))

  # state longer than name
  expect_error(
    race_probabilities(
      name   = c("smith", "lopez"),
      state  = rep("VT", 3),
      county = "Chittenden",
      year   = 2020
    ),
    "`state` must be length 1 or the same length as `name`"
  )

  # county longer than name
  expect_error(
    race_probabilities(
      name   = c("smith", "lopez"),
      state  = "VT",
      county = rep("Chittenden", 3),
      year   = 2020
    ),
    "`county` must be length 1 or the same length as `name`"
  )
})

test_that("race_probabilities() errors if invalid state abbreviation is used", {
  expect_error(
    race_probabilities(
      name   = "lopez",
      state  = "XX",
      county = "Chittenden",
      year   = 2020
    ),
    "Invalid state abbreviations: XX"
  )
})

test_that("race_probabilities() errors if invalid year is used", {
  expect_error(
    race_probabilities(
      name   = "lopez",
      state  = "VT",
      county = "Chittenden",
      year   = 2021
    ),
    "`year` must be one of the available years: 2020"
  )
})

test_that("race_probabilities() errors for multiple years", {
  expect_error(
    race_probabilities(
      name   = "lopez",
      state  = "VT",
      county = "Chittenden",
      year   = c(2020, 2022)
    ),
    "`year` must be a single numeric value"
  )
})

test_that("race_probabilities() errors if inputs are wrong type", {
  expect_error(
    race_probabilities(
      name   = "lopez",
      state  = 10,
      county = "Chittenden",
      year   = 2020
    ),
    "`state` must be a character vector"
  )
  expect_error(
    race_probabilities(
      name   = 10,
      state  = "VT",
      county = "Chittenden",
      year   = 2020
    ),
    "`name` must be a character vector"
  )
  expect_error(
    race_probabilities(
      name   = "lopez",
      state  = "VT",
      county = 10,
      year   = 2020
    ),
    "`county` must be a character vector"
  )
})

test_that("race_probabilities() errors if inputs contain NAs", {
  expect_error(
    race_probabilities(
      name   = c("johnson", NA),
      state  = "VT",
      county = "Chittenden",
      year   = 2020
    ),
    "`name` must not contain NA values"
  )
  expect_error(
    race_probabilities(
      name   = c("lopez", "lopez"),
      state  = c("VT", NA),
      county = "Chittenden",
      year   = 2020
    ),
    "`state` must not contain NA values"
  )
  expect_error(
    race_probabilities(
      name   = "lopez",
      state  = "VT",
      county = c("Chittenden", NA),
      year   = 2020
    ),
    "`county` must not contain NA values"
  )
  expect_error(
    race_probabilities(
      name   = "lopez",
      state  = "VT",
      county = "Chittenden",
      year   = NA
    ),
    "`year` must be a single numeric value"
  )
})

test_that("race_probabilities() returns correct columns for valid inputs", {
  out <- race_probabilities(
    name   = "lopez",
    state  = "VT",
    county = "Chittenden",
    year   = 2020
  )
  expect_s3_class(out, c("compare_calibisg", "data.frame"))
  expect_equal(nrow(out), 1)
  expect_named(out, c(.demographic_columns(),
                      as.vector(rbind(.calibisg_columns(), .bisg_columns())),
                      "in_census"))
})

test_that("race_probabilities() handles multiple inputs, including a non-match", {
  # Multiple inputs:
  # - 1st input: both calibisg and bisg available
  # - 2nd input: neither calibisg nor bisg available
  # - 3rd input: calibisg not available, bisg available

  expect_warning(
    expect_warning(
      out <- race_probabilities(
        name   = c("lopez", "noname", "noname2"),
        state  = c("VT", "VT", "VT"),
        county = c("Chittenden", "dummy", "Chittenden"),
        year   = 2020
      ),
      regexp = "caliBISG is not available for 2 input(s)",
      fixed = TRUE
    ),
    regexp = "Traditional BISG is not available for 1 input(s)",
    fixed = TRUE
  )
  expect_s3_class(out, c("compare_calibisg", "data.frame"))
  expect_equal(nrow(out), 3)

  # 1st row: non-NA calibisg columns
  # other rows: all calibisg columns = NA
  expect_false(all(is.na(out[1, .calibisg_columns()])))
  expect_true(all(is.na(out[2:3, .calibisg_columns()])))

  # 1st and 3rd rows: non-NA bisg columns
  # 2nd row: all bisg columns = NA
  expect_false(all(is.na(out[c(1,3), .bisg_columns()])))
  expect_true(all(is.na(out[2, .bisg_columns()])))
})

test_that("race_probabilities() handles duplicates correctly", {
  out <- race_probabilities(
    name = c("lopez", "lopez", "jackson"),
    state = "VT",
    county = rep("Chittenden", 3),
    year = 2020
  )
  expect_s3_class(out, c("compare_calibisg", "data.frame"))
  expect_equal(nrow(out), 3)
  expect_equal(out$name, c("lopez", "lopez", "jackson"))
  expect_equal(out$state, c("VT", "VT", "VT"))
  expect_equal(out$county, rep("chittenden", 3))
})

test_that("best_guess() prioritizes caliBISG values", {
  fake_probs <- data.frame(
    name = c("smith", "lopez"),
    year = c(2020L, 2020L),
    state = c("WA", "VT"),
    county = c("king", "chittenden"),
    calibisg_aian = c(0.10, NA_real_),
    calibisg_api = c(0.20, 0.25),
    calibisg_black_nh = c(NA_real_, 0.30),
    calibisg_hispanic = c(0.15, 0.30),
    calibisg_white_nh = c(0.20, NA_real_),
    calibisg_other = c(0.05, 0.45),
    bisg_aian = c(0.12, 0.05),
    bisg_api = c(0.18, 0.30),
    bisg_black_nh = c(0.25, 0.20),
    bisg_hispanic = c(0.20, 0.25),
    bisg_white_nh = c(0.15, 0.15),
    bisg_other = c(0.10, 0.05)
  )
  class(fake_probs) <- c("compare_calibisg", class(fake_probs))

  out <- best_guess(probs = fake_probs)
  expect_s3_class(out, "data.frame")
  expect_named(out, c(.demographic_columns(), paste0("best_guess_", .column_order())))
  expected <- data.frame(
    name = c("smith", "lopez"),
    year = c(2020L, 2020L),
    state = c("WA", "VT"),
    county = c("king", "chittenden"),
    best_guess_aian = c(0.10, 0.05),
    best_guess_api = c(0.20, 0.25),
    best_guess_black_nh = c(0.25, 0.30),
    best_guess_hispanic = c(0.15, 0.30),
    best_guess_white_nh = c(0.20, 0.15),
    best_guess_other = c(0.05, 0.45)
  )
  expect_equal(out, expected)
})

test_that("best_guess() validates the probs input", {
  fake_probs <- data.frame(
    name = "smith",
    year = 2020,
    state = "VT",
    county = "chittenden",
    calibisg_aian = 0.1,
    calibisg_api = 0.2,
    calibisg_black_nh = 0.3,
    calibisg_hispanic = 0.1,
    calibisg_white_nh = 0.2,
    calibisg_other = 0.1,
    bisg_aian = 0.2,
    bisg_api = 0.2,
    bisg_black_nh = 0.2,
    bisg_hispanic = 0.2,
    bisg_white_nh = 0.1,
    bisg_other = 0.1
  )
  class(fake_probs) <- c("compare_calibisg", class(fake_probs))

  expect_error(
    best_guess(name = "smith", probs = fake_probs),
    "`probs` cannot be supplied together with the other arguments.",
    fixed = TRUE
  )
  expect_error(
    best_guess(probs = as.vector(fake_probs)),
    "`probs` must be a data frame",
    fixed = TRUE
  )
  fake_missing <- fake_probs
  fake_missing$calibisg_other <- NULL
  expect_error(
    best_guess(probs = fake_missing),
    "`probs` is missing the following columns: calibisg_other",
    fixed = TRUE
  )
  fake_missing <- fake_probs
  fake_missing$name <- NULL
  expect_error(
    best_guess(probs = fake_missing),
    "`probs` is missing the following columns: name",
    fixed = TRUE
  )
})

test_that("best_guess() reuses race_probabilities() when probs provided", {
  names <- c("lopez", "jackson", "smith")
  states <- c("VT", "OK", "WA")
  counties <- c("Chittenden", "Tulsa", "King")

  probs <- race_probabilities(
    name = names,
    state = states,
    county = counties,
    year = 2020
  )
  direct <- best_guess(
    name = names,
    state = states,
    county = counties,
    year = 2020
  )
  from_probs <- best_guess(probs = probs)
  expect_equal(direct, from_probs)

  column_order <- .column_order()
  calibisg_cols <- paste0("calibisg_", column_order)
  bisg_cols <- paste0("bisg_", column_order)
  manual <- as.data.frame(probs)[, calibisg_cols]
  for (j in seq_along(column_order)) {
    cali_col <- calibisg_cols[j]
    bisg_col <- bisg_cols[j]
    missing_idx <- is.na(manual[[cali_col]])
    if (any(missing_idx)) {
      manual[[cali_col]][missing_idx] <- probs[[bisg_col]][missing_idx]
    }
  }
  names(manual) <- paste0("best_guess_", column_order)
  manual <- cbind(
    probs[, .demographic_columns(), drop = FALSE],
    manual
  )
  expect_equal(direct, manual)
})

test_that("print.compare_calibisg() prints correctly", {
  out <- race_probabilities(
    name   = c("lopez", "jackson", "smith"),
    state  = c("VT", "VT", "WA"),
    county = c("Chittenden", "Windsor", "King")
  )
  expect_snapshot(print(out))
  expect_snapshot(print(out, digits = 4))

  options(calibisg.digits = 3)
  expect_snapshot(print(out))

  options(calibisg.max_print = 1)
  expect_snapshot(print(out))
  expect_snapshot(print(out, max_print = 2, digits = 5))
})

test_that("print.compare_calibisg() handles edge cases", {
  out <- race_probabilities("Smith", "WA", "King")
  expect_snapshot(print(out, max_print = 0))
  expect_snapshot(print(out, digits = 0))

  expect_error(print(out, max_print = -1), "`max_print` must be a single non-negative integer")
  expect_error(print(out, max_print = c(1, 2)), "`max_print` must be a single non-negative integer")
  expect_error(print(out, max_print = "A"), "`max_print` must be a single non-negative integer")
  expect_error(print(out, digits = -1), "`digits` must be a single non-negative integer")
  expect_error(print(out, digits = c(1, 2)), "`digits` must be a single non-negative integer")
  expect_error(print(out, digits = "A"), "`digits` must be a single non-negative integer")
})


test_that("most_probable_race() output hasn't changed", {
  expect_snapshot_value(
    suppressWarnings(most_probable_race(
      name   = c("lopez", "jackson", "smith", "chan", "noname", "thomas"),
      state  = c("VT", "OK", "WA", "NC", "WA", "OK"),
      county = c("Chittenden", "Tulsa", "King", "Wake", "King", "noname"),
      year   = 2020
    )),
    style = "deparse"
  )
})

test_that("most_probable_race() returns correct columns", {
  out <- most_probable_race("lopez", "VT", "Chittenden", 2020)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_named(out, c(.demographic_columns(), "calibisg_race", "bisg_race", "in_census"))
  expect_equal(out$name, "lopez")
  expect_equal(out$state, "VT")
  expect_equal(out$county, "chittenden")
  expect_equal(out$year, 2020)
  expect_equal(out$calibisg_race, "hispanic")
  expect_equal(out$bisg_race, "hispanic")
  expect_equal(out$in_census, TRUE)
})

test_that("most_probable_race() handles multiple inputs", {
  out <- most_probable_race(
    name   = c("lopez", "jackson", "smith"),
    state  = c("VT", "VT", "WA"),
    county = c("Chittenden", "Windsor", "King"),
    year   = 2020
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
  expect_named(out, c(.demographic_columns(), "calibisg_race", "bisg_race", "in_census"))
  expect_equal(out$name, c("lopez", "jackson", "smith"))
  expect_equal(out$state, c("VT", "VT", "WA"))
  expect_equal(out$county, c("chittenden", "windsor", "king"))
  expect_equal(out$year, c(2020, 2020, 2020))
  expect_equal(out$calibisg_race, c("hispanic", "white_nh" , "white_nh"))
  expect_equal(out$bisg_race, c("hispanic", "white_nh" , "white_nh"))
  expect_equal(out$in_census, c(TRUE, TRUE, TRUE))
})

test_that("most_probable_race() handles missing records for name and county correctly", {
  # Multiple inputs (1 found, 1 not found)
  # because both name and county can't be found even regular bisg will be NA
  # we expect two warnings in this case
  expect_warning(
    expect_warning(
      out <- most_probable_race(
        name   = c("lopez", "noname"),
        state  = c("VT", "VT"),
        county = c("Chittenden", "dummy"),
        year   = 2020
      ),
      regexp = "caliBISG is not available for 1 input"
    ),
    regexp = "Traditional BISG is not available for 1 input"
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  # The second row should have NA for the race columns
  expect_false(is.na(out$calibisg_race[1]))
  expect_false(is.na(out$bisg_race[1]))
  expect_true(is.na(out$calibisg_race[2]))
  expect_true(is.na(out$bisg_race[2]))


  # Same scenario but using a real county
  # because only name can't be found, calibisg will be NA but bisg will be not
  expect_warning(
    out <- most_probable_race(
      name   = c("lopez", "noname"),
      state  = c("VT", "WA"),
      county = c("Chittenden", "King"),
      year   = 2020
    ),
    regexp = "caliBISG is not available for 1 input"
  )
  # The second row should have NA only for the calibisg race column
  expect_false(is.na(out$calibisg_race[1]))
  expect_false(is.na(out$bisg_race[1]))
  expect_true(is.na(out$calibisg_race[2]))
  expect_false(is.na(out$bisg_race[2]))
})

test_that("most_probable_race() handles missing caliBISG state correctly", {
  # Currently caliBISG not available for RI but BISG is
  expect_warning(
    out <- most_probable_race("Jones", "RI", "Providence", 2020),
    "caliBISG is not available for 1 input"
  )
  expect_s3_class(out, "data.frame")
  expect_true(is.na(out$calibisg_race))
  expect_false(is.na(out$bisg_race))
})

test_that("most_probable_race() gives same answer with county or fips", {
  expect_identical(
    most_probable_race(
      name = c("Chan", "Jones"),
      state = "NY",
      county = fips_to_county("36001")
    ),
    most_probable_race(
      name = c("Chan", "Jones"),
      state = "NY",
      county = "Albany"
    )
  )
})


test_that("valid_counties() errors with invalid inputs", {
  expect_error(
    valid_counties("XX", 2020),
    "Invalid state abbreviations: XX"
  )
  expect_error(
    valid_counties(10, 2020),
    "`state` must be a character vector"
  )
  expect_error(
    valid_counties(c("WA", "NC"), 2020),
    "`state` must be a single two-letter state abbreviation"
  )
  expect_error(
    valid_counties("VT", 2021),
    "`year` must be one of the available years: 2020"
  )
  expect_error(
    valid_counties("VT", c(2020, 2023)),
    "`year` must be a single numeric value"
  )
})

test_that("valid_counties() returns correct values", {
  for (st in datasets::state.abb) {
    out <- valid_counties(st, 2020)
    counties <- sort(unique(.race_x_county_data(st, 2020)$county))
    expect_equal(out, counties, info = paste("State =", st))
  }
})

test_that("valid_counties() matches caliBISG counties", {
  calibisg_states <- substr(available_data(), 1, 2)
  for (st in calibisg_states) {
    out <- valid_counties(st, 2020)
    counties <- sort(unique(load_data(st, 2020)$county))
    expect_equal(out, counties, info = paste("State =", st))
  }
})

test_that("fips_to_county() errors with invalid inputs", {
  expect_error(
    fips_to_county(12345),
    "`fips` must be a character vector of 5-digit FIPS codes"
  )
  expect_error(
    fips_to_county("001"),
    "`fips` must be a character vector of 5-digit FIPS codes"
  )
  expect_error(
    fips_to_county("1XXX5"),
    "`fips` must be a character vector of 5-digit FIPS codes"
  )
  expect_error(
    fips_to_county("36001", year = 1950),
    "`year` must be one of the available years"
  )
  expect_error(
    fips_to_county(c("36001", "00000", "36003", "99999")),
    "The following FIPS codes could not be converted: 00000, 99999",
    fixed = TRUE
  )
})

test_that("fips_to_county() handles multiple inputs", {
  fips <- c(
    "01001", "02013", "06037", "08013", "12086",
    "17031", "21037", "36061", "41007", "53033"
  )
  county <- c(
    "autauga", "aleutians east borough", "los angeles",
    "boulder", "miami-dade", "cook", "campbell",
    "new york", "clatsop", "king"
  )
  expect_identical(fips_to_county(fips), county)
})

