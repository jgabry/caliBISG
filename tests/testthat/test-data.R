test_that("available_data() returns empty when no data downloaded", {
  expect_equal(available_data(), character(0))
})

test_that("load_data() errors if not downloaded", {
  expect_error(
    load_data("VT", 2020),
    "Data file for VT, 2020 not found. Use `download_data()` to download it.",
    fixed = TRUE
  )
})

test_that("download_data() throws expected messages and errors", {
  expect_message({
    res <- download_data("VT", 2020, progress = FALSE)
    expect_true(res)
  }, regexp = "Downloading, reading, and saving caliBISG file for: VT, 2020")

  expect_message(expect_message({
    res <- download_data(c("VT", "WA"), 2020, progress = FALSE)
    expect_true(res)
  }, regexp = "VT-2020.rds already exists. Skipping."),
  regexp = "Downloading, reading, and saving caliBISG file for: WA, 2020")

  expect_error(
    download_data("CO", 2020),
    "Invalid states requested: CO",
  )
  expect_error(
    download_data("WA", 2021:2023),
    "Invalid years requested: 2021, 2022, 2023"
  )
})

test_that("download_data() can overwrite existing files", {
  expect_message(
    download_data("VT"),
    "VT-2020.rds already exists. Skipping."
  )

  expect_message({
    res <- download_data("VT", 2020, progress = FALSE, overwrite = TRUE)
    expect_true(res)
  }, regexp = "Downloading, reading, and saving caliBISG file for: VT, 2020")
})

test_that("available_data() recognizes downloaded data", {
  expect_equal(available_data(), c("VT-2020.rds", "WA-2020.rds"))
})

test_that("delete_data() deletes downloaded data", {
  suppressMessages(download_data("OK", progress = FALSE))
  expect_equal(available_data(), c("OK-2020.rds", "VT-2020.rds", "WA-2020.rds"))
  expect_message(
    expect_true(delete_data("OK", 2020)),
    "Deleting data file(s): OK-2020.rds",
    fixed = TRUE
  )
  expect_equal(available_data(), c("VT-2020.rds", "WA-2020.rds"))

  expect_message(
    expect_equal(delete_data("OK"), logical()),
    "No files found for deletion"
  )

  suppressMessages(download_data("OK", progress = FALSE))
  expect_message(
    expect_true(all(delete_data(c("OK", "VT")))),
    "Deleting data file(s): OK-2020.rds, VT-2020.rds",
    fixed = TRUE
  )
  expect_equal(available_data(), "WA-2020.rds")
  suppressMessages(download_data("VT", progress = FALSE))
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

test_that("the rest of the states can be downloaded", {
  expect_no_error(
    suppressMessages(download_data(year = 2020, progress = FALSE))
  )
  expect_equal(
    sort(available_data()),
    sort(paste0(.all_calibisg_states(), "-2020.rds"))
  )
})

test_that("all rows of caliBISG data frames are unique", {
  for (st in .all_calibisg_states()) {
    df <- load_data(st, 2020)
    expect_equal(anyDuplicated(df), 0, info = paste("State =", st))
  }
})

