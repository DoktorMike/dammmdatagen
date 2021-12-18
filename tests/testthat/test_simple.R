# library(stringr)
context("Dimensions check on data")

offonsize <- structure(c(366L, 4L, 366L, 4L, 366L, 4L),
  .Dim = 2:3,
  .Dimnames = list(NULL, c("net", "impression", "cpm"))
)

test_that("we get the correct dimensions", {
  expect_equal(dim(generateMacroData()), c(366, 4))
  expect_equal(dim(generateEventData()), c(366, 3))
  expect_equal(dim(generateWeatherData()), c(366, 4))
  expect_equal(dim(generateCompetitorData()), c(366, 4))
  expect_equal(dim(generatePriceData()), c(366, 4))
  expect_equal(generateOnlineData() %>% sapply(dim), offonsize)
  expect_equal(generateOfflineData() %>% sapply(dim), offonsize)
})
