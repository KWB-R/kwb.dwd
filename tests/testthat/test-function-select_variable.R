#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2022-09-16 16:43:27.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("select_variable() works", {

  f <- kwb.dwd:::select_variable

  expect_error(
    f()
    # argument "shape" is missing, with no default
  )

})
