context("Output checking of alXGB")

# Xtesting
test_that("Testing to see how Xtest works", {
  expect_error(alXGB(train, test, NR = 5, CSV = F, Xtest = T), 
               "Error : test$popularity has an empty dimension", fixed = TRUE)
  
  expect_output(alXGB(train[1:100,], train[101:200,], NR = 5, CSV = F, Xtest = T), "0.")
})

testTree <- alXGB(train, test, NR = 5, CSV = F, Xtest = T)

# Expect output as a dataframe
test_that("Structure of output", {
  expect_output(str(testTree), "data.frame")
  
  expect_equal(nrow(testTree), nrow(test))
  
  expect_equal(ncol(testTree), 7)
})