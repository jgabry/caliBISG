suppressMessages(download_data(year = 2020, progress = FALSE))

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
    expect_equal(out, counties, info = st)
  }
})

test_that("valid_counties() matches caliBISG counties", {
  calibisg_states <- substr(available_data(), 1, 2)
  for (st in calibisg_states) {
    out <- valid_counties(st, 2020)
    counties <- sort(unique(load_data(st, 2020)$county))
    expect_equal(out, counties, info = st)
  }
})

test_that("race_probabilities() errors if lengths are mismatched", {
  expect_error(
    race_probabilities(
      name   = c("smith", "lopez"),
      state  = "VT",
      county = "Chittenden",
      year   = 2020
    ),
    "`name`, `state`, and `county` must all have the same length."
  )

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


test_that("race_probabilities() returns correct columns for valid inputs", {
  out <- race_probabilities(
    name   = "lopez",
    state  = "VT",
    county = "Chittenden",
    year   = 2020
  )
  expect_s3_class(out, c("compare_bisg", "data.frame"))
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
  expect_s3_class(out, c("compare_bisg", "data.frame"))
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

test_that("print.compare_bisg() prints correctly", {
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


test_that("most_probable_race() returns correct columns", {
  # Single input
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

