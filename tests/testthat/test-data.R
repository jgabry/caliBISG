suppressMessages(delete_all_data())

test_that("load_data() errors if not downloaded", {
  expect_error(
    load_data("NC", 2020),
    "Data file for NC, 2020 not found. Use `download_data()` to download it.",
    fixed = TRUE
  )
})

test_that("data_dir() returns a valid directory", {
  dir_path <- data_dir()
  expect_true(dir.exists(dir_path))
})

test_that("download_data() works as expected", {
  expect_message({
    res <- download_data("NC", 2020)
    expect_true(isTRUE(res))
  }, "Downloading: NC-2020.csv")

  expect_message(
    expect_message({
      res <- download_data(c("NC", "WA"), 2020)
      expect_true(isTRUE(res))
    }, "NC-2020.rds already exists. Skipping."),
    "Downloading: WA-2020.csv"
  )

  expect_error(
    download_data("NY", 2020),
    "Invalid states requested"
  )
  expect_error(
    download_data("WA", 2021),
    "Invalid years requested"
  )
})

test_that("available_data() returns a character vector", {
  expect_equal(available_data(), c("NC-2020.rds", "WA-2020.rds"))
})

test_that("load_data() works as expected", {
  wa_2020_data <- load_data("WA", 2020)
  expect_true(is.data.frame(wa_2020_data))
  expect_named(
    wa_2020_data,
    c(.demographic_columns(), .rake_columns(), .voter_bisg_columns(), .bisg_columns(), "in_census")
  )
})
