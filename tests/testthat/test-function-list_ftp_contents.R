test_that("list_ftp_contents() works", {

  f <- kwb.dwd:::list_ftp_contents

  result <- f()

  expect_true(is.data.frame(result))
  expect_identical(names(result), c("file", "isdir"))
})
