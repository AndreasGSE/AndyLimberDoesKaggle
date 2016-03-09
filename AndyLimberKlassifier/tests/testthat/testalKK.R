context("Output checking of classifier fuction")

# Running the random forest for a low number of trees for speed
trainN <- alFeatureGen(train)
testN <- alFeatureGen(test)
testForest <- alKK(trainN, testN, NT = 10, CSV = F)

# Expect output of the form "model"
test_that("Structure of output", {
  expect_output(str(testForest), "data.frame")
})

# Xtesting
test_that("Testing to see how Xtest works", {
  expect_error(alKK(train, test, NT = 1, CSV = F, Xtest = T), 
               "Error : test$popularity has an empty dimension", fixed = TRUE)
  
  expect_output(alKK(trainN[1:100,], trainN[101:200,], NT = 1, CSV = F, Xtest = T), "0.")
})