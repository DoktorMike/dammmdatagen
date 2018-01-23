# library(stringr)
context("Dimensions check on data")

test_that("we get the correct dimensions", {
  expect_output(str(generateMacroData()), "366 obs. of  4 variables")
  expect_output(str(generateEventData()), "366 obs. of  3 variables")
  expect_output(str(generateWeatherData()), "366 obs. of  4 variables")
  expect_output(str(generateCompetitorData()), "366 obs. of  4 variables")
  expect_output(str(generateMacroData()), "366 obs. of  4 variables")
  expect_output(str(generatePriceData()), "366 obs. of  4 variables")
  expect_output(str(generateOnlineData()), "List of 3")
  expect_output(str(generateOfflineData()), "List of 3")
})
