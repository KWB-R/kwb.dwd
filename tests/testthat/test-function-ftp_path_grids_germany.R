test_that("ftp_path_grids_germany() works", {

  f <- kwb.dwd:::ftp_path_grids_germany

  expect_error(f())
  expect_error(f("unknown-resolution"))

  check <- function(x) {
    expect_type(x, "character")
    expect_length(x, 1L)
    expect_true(startsWith(x, "ftp://"))
    expect_true(grepl(x, x))
  }

  check(f("monthly"))
  check(f("daily"))
})
