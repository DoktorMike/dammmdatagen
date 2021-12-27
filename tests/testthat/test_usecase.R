context("Top level API checks")

ret <- generateRetailData()

test_that("we get three list items back", {
  expect_equal(length(ret), 3)
})
