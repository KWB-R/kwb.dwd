test_that("get_radolan_url() works", {

  f <- kwb.dwd:::get_radolan_url

  expect_error(f())
  expect_error(f("no-such-frequency"))
  expect_error(f("hourly", "wrong-date-format"), "six numeric characters")
  expect_error(f("hourly", "200001"), "first available year")
})
