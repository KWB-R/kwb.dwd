test_that("filter_by_month_range() works", {

  f <- kwb.dwd:::filter_by_month_range

  expect_error(f())
  expect_error(f(1:3))
  expect_error(f(c("a", "b")))

  expect_identical(f(character(0)), character(0))

  expect_identical(f("202201"), "202201")

  from <- "202201"
  to <-   "202207"

  expect_identical(f("file-202203.xyz", from, to), "file-202203.xyz")
  expect_identical(f("file-202208.xyz", from, to), character(0))
})
