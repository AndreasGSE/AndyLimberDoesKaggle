# load data
setwd("C:\\Users\\Dre\\Desktop\\Data")
trainData <- read.csv("news_popularity_training.csv", stringsAsFactors = T)

# package
library(xgboost)

# cross validation function including process to get xgboost working
# Would be cool if we could get the xvalidation function we already have, working
# for both methods with just an argument
xv <- function(n = 5, m = 9000, data, nr = 20, eta = 0.3, ss = 1, mcw = 1, seed = 123){
  set.seed(seed)
  
  score <- rep(0,n)
  for(i in 1:n){
    print(i)
    
    # getting a sample - xval
    trialVec <- sample(c(1:30000), m)
    trialTrain <- data[-trialVec,-c(1,2)] # xgboost does NOT take a formula
    trialTest <- data[trialVec,-c(1,2)] # so need to remove unwanted columns
    
    # Need to prepare data for use in xgboost
    trainPop <- as.numeric(trialTrain$popularity) - 1 # "target" variable MUST start from 0
    testPop <- as.numeric(trialTest$popularity) - 1
    
    trialTrain$popularity = NULL # as we do not feed a formula, must blank the target column
    trialTest$popularity = NULL
    
    # xgboost ONLY takes numerical values and matrix data types
    trialTrain <- as.matrix(apply(trialTrain, 2, as.numeric))
    trialTest <- as.matrix(apply(trialTest, 2, as.numeric))
    
    # To optimise need to look at the parameters of xgboost - look at xgb.train
    testBoost <- xgboost(data = trialTrain, label = trainPop,
                         nrounds = nr, eta = eta, subsample = ss, min_child_weight = mcw,
                         objective = "multi:softprob", num_class = 5)
    
    xgbpred <- predict(testBoost, trialTest) # this gives us a list of probabilities
    probs <- t(matrix(xgbpred, nrow=5, ncol=length(xgbpred)/5)) # transform to a matrix 
    
    predLabs <- apply(probs, 1, which.max) # get the label - lucky that things are in the right order
    
    acc <- mean(ifelse(predLabs == (testPop + 1), 1, 0)) # ch-ch-check it
    
    score[i] <- acc
  }
  
  print(mean(score))
  return(score)
}


library(AndyLimberKlassifier) # XGBOOST IMRPOVES WITH NEW VARS
newTrain <- alFeatureGen(trainData)
xv(n = 10, data = newTrain, eta = 0.01, nr = 700, ss = 0.5, mcw = 2)


## Results ## NOTE that when testing - often need to increase nrounds so that it converges
# default params > 0.5175
# 0.01 500, ss = 0.5 > 0.5232556