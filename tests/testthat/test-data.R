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
  expect_message(
    expect_message(
      expect_message({
        res <- download_data("VT", 2020, progress = FALSE)
        expect_true(res)
      },
      regexp = "Downloading, reading, and saving file for: VT, 2020"),
      regexp = "Downloading calibisg_vt2020.csv"
    ),
    regexp = "VT-2020.rds"
  )

  expect_message(
    expect_message(
      expect_message(
        expect_message({
          res <- download_data(c("VT", "WA"), 2020, progress = FALSE)
          expect_true(res)
        },
        regexp = "VT-2020.rds already exists. Skipping."
        ),
        regexp = "Downloading, reading, and saving file for: WA, 2020",
      ),
      regexp = "Downloading calibisg_wa2020.csv"
    ),
    regexp = "WA-2020.rds"
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

test_that("downloaded data recognized by available_data()", {
  expect_equal(available_data(), c("VT-2020.rds", "WA-2020.rds"))
})

test_that("available_data() uses uppercase state codes", {
  expect_true(all(grepl("^[A-Z]{2}-[0-9]{4}\\.rds$", available_data())))
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
