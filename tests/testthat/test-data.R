test_that("data_dir() returns a valid directory", {
  expect_true(dir.exists(data_dir()))
})

test_that("load_data() errors if not downloaded", {
  expect_error(
    load_data("VT", 2020),
    "Data file for VT, 2020 not found. Use `download_data()` to download it.",
    fixed = TRUE
  )
})

test_that("download_data() works as expected", {
  expect_message({
    res <- download_data("VT", 2020)
    expect_true(res)
  }, "calibisg_vt2020.csv")

  expect_message(
    expect_message({
      res <- download_data(c("VT", "WA"), 2020)
      expect_true(res)
    }, "VT-2020.rds already exists. Skipping."),
    "calibisg_wa2020.csv"
  )

  expect_error(
    download_data("CO", 2020),
    "Invalid states requested: CO",
  )
  expect_error(
    download_data("WA", 2021:2023),
    "Invalid years requested: 2021, 2022, 2023"
  )
})

test_that("available_data() returns a character vector", {
  expect_equal(available_data(), c("VT-2020.rds", "WA-2020.rds"))
})

test_that("load_data() works as expected", {
  wa_2020_data <- load_data("WA", 2020)
  expect_true(is.data.frame(wa_2020_data))
  expect_named(
    wa_2020_data,
    c(.demographic_columns(), .calibisg_columns(), "in_census")
  )

  vt_2020_data <- load_data("VT", 2020)
  expect_true(is.data.frame(vt_2020_data))
  expect_named(
    vt_2020_data,
    c(.demographic_columns(), .calibisg_columns(), "in_census")
  )
})
