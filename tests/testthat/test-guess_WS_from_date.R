test_that("guess works for past", {
  expect_identical(guess_WS_from_date("2018-01-01"), "WS262")
})


test_that("guess works for 2008", {
  expect_identical(guess_WS_from_date("2008-04-01"), "WS188")
})

test_that("guess works for future", {
  expect_identical(guess_WS_from_date("2028-01-01"), "WS306")
})
