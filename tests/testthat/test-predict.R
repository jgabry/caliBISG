suppressMessages(download_data())

test_that("most_probable_race() returns correct columns and handles multiple inputs", {
  # Single input
  single_out <- most_probable_race("lopez", "VT", "Chittenden", 2020)
  expect_s3_class(single_out, "data.frame")
  expect_equal(nrow(single_out), 1)
  expect_named(single_out, c(.demographic_columns(), "race", "in_census"))
  # expect_named(single_out, c(.demographic_columns(), "calibisg_race", "bisg_race", "in_census"))

  # Multiple inputs (1 found, 1 not found)
  expect_warning(
    multi_out <- most_probable_race(
      name   = c("lopez", "noname"),
      state  = c("VT", "VT"),
      county = c("Chittenden", "dummy"),
      year   = 2020
    ),
    regexp = "No record found for 1 input"
  )
  expect_s3_class(multi_out, "data.frame")
  expect_equal(nrow(multi_out), 2)
  # The second row should have race = NA
  expect_false(is.na(multi_out$race[1]))
  expect_true(is.na(multi_out$race[2]))
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
})

test_that("race_probabilities() returns correct columns for valid inputs", {
  out <- race_probabilities(
    name   = "lopez",
    state  = "VT",
    county = "Chittenden",
    year   = 2020
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_named(out, c(.demographic_columns(),
                      .calibisg_columns(),
                      #.bisg_columns()
                      "in_census"))
})

test_that("race_probabilities() handles multiple inputs, including a non-match", {
  # Suppose "lopez, VT, Chittenden" is a match, but the other two are not
  # We expect one aggregated warning, not two separate ones
  expect_warning(
    out <- race_probabilities(
      name   = c("lopez", "noname", "noname2"),
      state  = c("VT", "VT", "VT"),
      county = c("Chittenden", "dummy", "dummy"),
      year   = 2020
    ),
    regexp = "No record found for 2 input(s)",
    fixed = TRUE
  )
  expect_s3_class(out, "data.frame")
  expect_s3_class(out, "compare_bisg")
  expect_equal(nrow(out), 3)

  # 1st row: non-NA calibisg columns
  # other rows: all calibisg columns = NA
  calibisg_cols <- .calibisg_columns()
  expect_false(all(is.na(out[1, calibisg_cols])))
  expect_true(all(is.na(out[2, calibisg_cols])))
})

test_that("race_probabilities() aggregates warnings for non-matches", {
  expect_warning(
    out <- race_probabilities(
      name   = c("lopez", "noname", "noname2"),
      state  = c("VT", "VT", "VT"),
      county = c("Chittenden", "dummy", "dummy"),
      year   = 2020
    ),
    regexp = "No record found for 2 input(s)",
    fixed = TRUE
  )
  expect_equal(nrow(out), 3)

  # For the row with no match, check for NAs
  expect_true(all(is.na(out[2:3, c(.calibisg_columns())])))
  # expect_true(all(is.na(out[2:3, c(.bisg_columns(), .calibisg_columns())])))
})

