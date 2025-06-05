# precomputed values created by data-raw/test-compute-bisg.R
precomputed_bisg <- dget(test_path("bisg-test-answers", "precomputed-bisg.R"))

test_that("bisg() returns correct values", {
  out <- bisg(
    name = precomputed_bisg$name,
    state = precomputed_bisg$state,
    county = precomputed_bisg$county,
    year = 2020
  )
  expect_equal(
    out[, !colnames(out) == ".found"],
    precomputed_bisg
  )
})

test_that("BISG can be computed for all 50 states with known name", {
  for (st in datasets::state.abb) {
    out <- bisg(
      name = "Smith",
      state = st,
      county = .race_x_county_data(st, 2020)$county[1],
      year = 2020
    )
    expect_false(is.na(out$bisg_aian[1]), info = st)
  }
})

test_that("BISG can be computed for all 50 states with unknown name", {
  for (st in datasets::state.abb) {
    out <- bisg(
      name = "NOT_A_NAME",
      state = st,
      county = .race_x_county_data(st, 2020)$county[1],
      year = 2020
    )
    expect_false(is.na(out$bisg_aian[1]), info = st)
  }
})

test_that("race x surname reference table has a single 'all other names' row", {
  df <- .race_x_surname_data()
  expect_length(which(df$name == 'all other names'), 1)
})
