context("Output checking of alRF")

# Xtesting
test_that("Testing to see how Xtest works", {
  expect_error(alRF(train, test, NT = 1, CSV = F, Xtest = T), 
               "Error : test$popularity has an empty dimension", fixed = TRUE)
  
  expect_output(alRF(trainN[1:100,], trainN[101:200,], NR = 5, CSV = F, Xtest = T), "0.")
})

testTree <- alRF(train, test, NT = 1, CSV = F, Xtest = T)
# Expect output as a dataframe
test_that("Structure of output", {
  expect_output(str(testTree), "data.frame")
  
  expect_equal(nrow(testTree), nrow(test))
  
  expect_equal(ncol(testTree), 7)
})