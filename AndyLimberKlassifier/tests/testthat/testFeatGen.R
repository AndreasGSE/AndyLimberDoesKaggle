context("Output checking of feature gen")

# Apply the functions. Do to both as one contains an empty popularity column
trainN <- alFeatureGen(train)
testN <- alFeatureGen(test)

# Making sure the function is returning sensible things
test_that("Testing structure of the output", {
  expect_equal(nrow(trainN), nrow(train))
  expect_equal(nrow(testN), nrow(test))
  
  expect_true(length(names(trainN)) != length(names(train)))
  expect_true(length(names(testN)) != length(names(test)))
})

# Checking for production of NAs and NaNs. Useful to check on both to expand the sample.
test_that("NA / NaN checking", {
  expect_equal(sum(is.na(trainN)), 0)
  expect_equal(sum(is.na(testN)), 0)
  
  expect_equal(sum(is.nan(as.matrix(trainN))), 0)
  expect_equal(sum(is.nan(as.matrix(testN))), 0)
})

# Checking the added features
test_that("Get the months / years right", {
  expect_equal(mean(as.numeric(trainN$Month) %in% 1:12), 1)
  expect_equal(mean(trainN$Year %in% c(2013,2014)), 1)
  
  expect_equal(mean(as.numeric(testN$Month) %in% 1:12), 1)
  expect_equal(mean(testN$Year %in% c(2013,2014)), 1)
})