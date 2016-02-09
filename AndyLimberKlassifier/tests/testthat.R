library(testthat)
library(AndyLimberKlassifier)

train <- read.csv("test_train.csv", stringsAsFactors = T)
test <- read.csv("test_test.csv", stringsAsFactors = T)

test_check("AndyLimberKlassifier")
