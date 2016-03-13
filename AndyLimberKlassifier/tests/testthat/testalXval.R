context("Checking the X validation function")

# Making sure it only accepts valid methods
test_that("Making sure only valid methods", {
  expect_error(alXvalidate(train, test, method = "alWRONG"), 
               "Error: `%in%`(x = \"alWRONG\", table = c(\"rf\", \"xgb\", \"alkk\")) is not TRUE",
               fixed = TRUE)
})

testXval.RF <- Xvalidate(n = 2, m = 20, train[1:100,], NT = 1, method = "rf")
testXval.XG <- Xvalidate(n = 2, m = 20, train[1:100,], NR = 5, method = "xgb")

# Checking outputs
test_that("Looking at output", {
  expect_output(str(testXval.RF), "num")
  expect_output(str(testXval.XG), "num")
  
  expect_equal(length(testXval.RF), 2)
  expect_equal(length(testXval.XG), 2)
  })