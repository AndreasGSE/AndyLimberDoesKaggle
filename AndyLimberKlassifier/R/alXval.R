#' Andy Limber's X validation
#'
#' Perform a cross validation on some set of data by dividing it into random samples, then 
#' using the alKK function on the training and test set created.
#'
#' @param k The number of validations done.
#' @param m The size of the validation sample.
#' @param data The data on which Xvalidation will be done
#' @param NT Integer. The number of trees used by the forest (ntree).
#' @param MT Integer. The mtry parameter for randomForest.
#' @param NS Integer. The nodesize parameter.
#' @param NR Integer. The maximum number of rounds performed.
#' @param eta Double. Controls the learning rate, 0 < eta < 1, determines the level 
#' of contribution of each tree.
#' @param gamma Integer. Minimum loss reduction required to make a futher cut on a 
#' leaf node, a larger value for this parameter translates to a more 
#' conservative algorithm. 
#' @param MCW Integer. Minimum child weight: minimum sum of instance weight needed 
#' to form a child node.
#' @param SS Double. Subsample: ratio of the training set used to train.
#' @param colsbt Double. Column sample by tree: subsample ratio of columns for 
#' constructing each tree.
#' @param thresh The probability threshold for favouring a method. thresh = 0 would use
#' only alXGB. thresh = 1 would use only alRF. 
#' @param vars Vector of column indices for the features to be used in the random forest.
#' Defaults to NULL and all features are used. 
#' @param seed The seed to be used.
#' @param method Select between "rf" for alRF, "xgb" for alXGB, or "alKK" for al KK,
#' for the desired cross validation.
#' @return Vector of out of sample accuracy
#' @import assertthat
#' @export
alXvalidate <- function(k = 5, m = 9000, data, NT = 100, MT = 12, NS = 25, 
                        NR = 700, eta = 0.01, gamma = 1, MCW = 2, SS = 0.5, 
                        colsbt = 1, thresh = 0.4, vars = NULL, seed = 123, method){
  set.seed(seed)
  assert_that(method %in% c("rf", "xgb", "alkk"))
  
  score <- rep(0,k)
  for(i in 1:k){
    print(i)
    
    trialVec <- sample(c(1:nrow(data)), m)
    trialTrain <- data[-trialVec,]
    trialTest <- data[trialVec,]
    
    if(method == "rf"){
      score[i] <- alRF(trialTrain, trialTest, NT = NT, MT = MT, NS = NS, Xtest = T, vars = vars)
    } else if(method == "xgb"){
      score[i] <- alXGB(trialTrain, trialTest, NR = NR, eta = eta, gamma = gamma, 
                        MCW = MCW, SS = SS, Xtest = T, vars = vars)
    } else if(method == "alkk"){
      score[i] <- alKK(trialTrain, trialTest, thresh = thresh, Xtest = T, vars = vars)
    }
  }
  
  print(mean(score))
  return(score)
}
