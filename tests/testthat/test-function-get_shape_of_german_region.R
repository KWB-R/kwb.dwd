#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2022-09-16 16:40:56.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("get_shape_of_german_region() works", {

  f <- kwb.dwd:::get_shape_of_german_region

  expect_error(
    f()
    # argument "name" is missing, with no default
  )

})
