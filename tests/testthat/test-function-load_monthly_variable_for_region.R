#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2022-09-16 16:40:56.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("load_monthly_variable_for_region() works", {

  f <- kwb.dwd:::load_monthly_variable_for_region

  expect_error(
    f()
    # argument "variable" is missing, with no default
  )

})
