# ---------------------------------------#
#        Kaggle XGBoost Function         #
#----------------------------------------#
#' Andy Limber's Kaggle xgboost function
#' 
#' Runs xgboost algorithm on training and test set
#' 
#' @param train A dataframe containing the training data. Cannot include any text variables,
#' URL is already taken care of in the function. Requires it to be the second column.
#' @param test A dataframe containing the test data. See train for conditions on
#' variables
#' @param nr Integer. The maximum number of rounds performed.
#' @param eta Double. Controls the learning rate, 0 < eta < 1, determines the level 
#' of contribution of each tree.
#' @param gamma Integer. Minimum loss reduction required to make a futher cut on a 
#' leaf node, a larger value for this parameter translates to a more 
#' conservative algorithm. 
#' @param mcw Integer. Minimum child weight: minimum sum of instance weight needed 
#' to form a child node.
#' @param ss Double. Subsample: ratio of the training set used to train.
#' @param colsbt Double. Column sample by tree: subsample ratio of columns for 
#' constructing each tree.
#' @param npt Integer. Number of parallel trees: an experimental parameter, 
#' number of trees to grow per interation.
#' @param vars Vector of variables to be EXCLUDED from the training. Note
#' that this is differently defined to the random forest function.
#' @param Xtest A logical that indicates whether you want to perform some
#' cross validation. Default is FALSE. If TRUE, will print an accuracy value.
#' Will over-ride other returned values.
#' @param CSV A logical that indicates whether a CSV of predictions should be
#' saved. Default is TRUE.
#' @return Returns the predicted labels, probabilities and IDs, as well 
#' as a CSV of predictions if CSV is set to TRUE.
#' If Xtest is set to true, will return an accuracy.
#' @export
#' @import xgboost
#' @import assertthat

alXGB <-function(train, test, nr= 700, eta = 0.01, gamma = 1, mcw = 2, ss = 0.5, 
                 colsbt = 1, npt = 1, vars = NULL, Xtest = FALSE, CSV = TRUE, seed = 123){
  set.seed(seed) 
  
  # testing inputs
  not_empty(test); not_empty(train);
  
  # Will produce an error if we include text columns, so require right order
  assert_that(sum(names(train) == c("id", "url")) == 1) 
  assert_that(sum(names(train) == c("id", "url")) == 1)
  
  if(Xtest){
    assert_that(noNA(test$popularity)) # making sure we have values
    assert_that(not_empty(test$popularity))
  } 
  
  # Data prep for xgboost
  trainPop <- as.numeric(train$popularity) - 1 # must start at 0
  testPop <- as.numeric(test$popularity) - 1
  
  train$popularity = NULL # no formula, must blank the target column
  test$popularity = NULL
  
  xg.train <- as.matrix(apply(train[,-c(1,2, vars)], 2, as.numeric)) # only matrices
  xg.test <- as.matrix(apply(test[,-c(1,2, vars)], 2, as.numeric))
  
  # To optimise need to look at the parameters of xgboost - see xgb.train
  print("Getting xgboosted trees")
  xg.boost <- xgboost(data = train, label = trainPop,
                       nrounds = nr, eta = eta, subsample = ss, min_child_weight = mcw,
                       number_parallel_tree = npt, colsample_bytree = colsbt, 
                       objective = "multi:softprob", num_class = 5)
  
  # Predicting labels
  print("Getting labels")
  xgbpred <- predict(xg.boost, test) # this gives us a list of probabilities
  probs <- t(matrix(xgbpred, nrow=5, ncol=length(xgbpred)/5)) # transform to a matrix 
  
  Prediction <- apply(probs, 1, which.max) # get the label
  
  popularityClass <- data.frame(id = test$id, popularity = Prediction)
  
  popularityClass <- cbind(popularityClass, probs)
  
  
  # Comparing values for training and test set
  if(Xtest){
    acc <- mean(ifelse(predLabs == (testPop + 1), 1, 0))
    print(acc)
    return(acc)
  }
  
  # Printing CSV
  if(CSV){
    write.csv(popularityClass,"kagglesub.csv", row.names = F, quote = F)
  }
  
  return(popularityClass)
}