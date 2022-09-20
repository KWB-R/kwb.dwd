test_that("find_description_files() works", {

  f <- kwb.dwd:::find_description_files

  expect_error(f())

  expect_output(result <- f(kwb.dwd:::ftp_path_daily_grids("evapo_p/any-file")))

  expect_true("DESCRIPTION_gridsgermany_daily_evapo_p_en.pdf" %in% result)
})
