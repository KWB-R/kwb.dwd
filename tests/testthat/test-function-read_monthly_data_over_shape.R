#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2022-09-20 04:44:47.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("read_monthly_data_over_shape() works", {

  f <- kwb.dwd:::read_monthly_data_over_shape

  expect_error(
    kwb.dwd:::read_monthly_data_over_shape()
    # argument "variable" is missing, with no default
  )

})
