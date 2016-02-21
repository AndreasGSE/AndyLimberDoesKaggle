#' Andy Limber's X validation
#'
#' Perform a cross validation on some set of data by dividing it into random samples, then 
#' using the alKK function on the training and test set created.
#'
#' @param n The number of validations done.
#' @param m The size of the validation sample.
#' @param data The data on which Xvalidation will be done
#' @param NT Integer. The number of trees used by the forest (ntree).
#' @param MT Integer. The mtry parameter for randomForest.
#' @param NS Integer. The nodesize parameter.
#' @param vars Vector of column indices for the features to be used in the random forest.
#' Defaults to NA and all features are used. 
#' @param seed The seed to be used.
#' @return Vector of out of sample accuracy
#' @export
alXvalidate <- function(n = 5, m = 9000, data, NT = 100, MT = 12, NS = 25, vars = NA, seed = 123){
  set.seed(seed)
  
  score <- rep(0,n)
  for(i in 1:n){
    print(i)
    
    trialVec <- sample(c(1:30000), m)
    trialTrain <- data[-trialVec,]
    trialTest <- data[trialVec,]
    
    score[i] <- alKK(trialTrain, trialTest, NT = NT, MT = MT, NS = NS, Xtest = T, vars = vars)
  }
  
  print(mean(score))
  return(score)
}