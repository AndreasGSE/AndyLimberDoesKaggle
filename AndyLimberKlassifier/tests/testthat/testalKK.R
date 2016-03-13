context("Output checking of classifier fuction")

alXGB.control <- list(NR = 5, eta = 1, gamma = 1, MCW = 1, SS = 0.5, 
                      colsbt = 1, vars = NULL)
alRF.control <- list(NT = 1, MT = 12, NS = 25, vars = NULL)

# Will run on feature gen data as that is appropriate for submission
trainN <- alFeatureGen(train)
testN <- alFeatureGen(test)
testSub <- alKK(trainN, testN, alXGB.control = alXGB.control, alRF.control = alRF.control,
                CSV = F)

# Expect output as a dataframe
test_that("Structure of output", {
  expect_output(str(testSub), "data.frame")
  
  expect_equal(nrow(testSub), nrow(test))
})
