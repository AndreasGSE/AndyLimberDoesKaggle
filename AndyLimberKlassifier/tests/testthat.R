library(testthat)
library(AndyLimberKlassifier)

train <- read.csv("test_train.CSV", stringsAsFactors = T)
test <- read.csv("test_test.CSV", stringsAsFactors = T)

test_check("AndyLimberKlassifier")
