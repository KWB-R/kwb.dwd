test_that("last_month_as_yyyymm() works", {

  f <- kwb.dwd:::last_month

  test_properties <- function(x, pattern) {
    expect_type(x, "character")
    expect_length(x, 1L)
    expect_true(startsWith(x, substr(Sys.Date(), 1L, 4L)))
    expect_true(grepl(pattern, x))
  }

  test_properties(f(), "\\d{6}")
  test_properties(f(format = "%Y-%m"), "\\d{4}-\\d{2}")
})
