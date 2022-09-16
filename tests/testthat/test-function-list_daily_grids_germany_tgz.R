test_that("list_daily_grids_germany_tgz() works", {

  f <- kwb.dwd:::list_daily_grids_germany_tgz

  expect_error(f())

})
