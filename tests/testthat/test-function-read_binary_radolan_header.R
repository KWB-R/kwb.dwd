#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2022-09-16 16:43:26.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("read_binary_radolan_header() works", {

  f <- kwb.dwd:::read_binary_radolan_header

  expect_error(
    f()
    # argument "path" is missing, with no default
  )

})
