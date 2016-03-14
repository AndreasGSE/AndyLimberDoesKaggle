# --------------------------------------- #
#       Kaggle prediction function        #
# --------------------------------------- #
#' Andy Limber's Kaggle Klassifier
#'
#' Assign popularity values to news articles for submission to Kaggle. Can be used with
#' any set of features as unreferenced by name, but best submission includes feature generation
#' done by the alFeatuerGen function (must be called on train and test data separately).
#' The random forest and xgboost functions are both called internally.
#'
#' @param train A dataframe containing the training data for the
#' popularity classifications.
#' @param test A dataframe containing the data to be used to predict labels.
#' If a column of NAs is not supplied for the "popularity", one will be added.
#' @param vars Vector of column indices for the features to be used in the random forest.
#' Defaults to NULL and all features are used.
#' @param thresh The probability threshold for favouring a method. thresh = 0 would use
#' only alXGB. thresh = 1 would use only alRF.
#' @param alXGB.control Optional list containing parameters for alXGB. All must be named params
#' must be named and provided according to alXGB documentation. Train, test, seed, and
#' TRUE / FALSE parameters do not need to be provided.
#' @param alRF.control Optional list containing parameters for alRF. See alXGB.control. 
#' Vars argument does not need to be the same for both functions.
#' @param seed The seed to be used.
#' @param Xtest A logical that indicates whether you want to perform some
#' cross validation. Default is FALSE. If TRUE, will print an accuracy value.
#' Must supply values for popularity. Will over-ride other returned values.
#' @param CSV A logical that indicates whether a CSV of predictions should be
#' saved. Default is TRUE.
#' @return Returns the predicted labels, probabilities and IDs, as well 
#' as a CSV of predictions if CSV is set to TRUE.
#' If Xtest is set to true, will return an accuracy.
#' @export
#' @import assertthat

alKK <- function(train, test, vars = NULL, thresh = 0.4, 
                 alXGB.control = NULL, alRF.control = NULL,
                 seed = 123, Xtest = FALSE, CSV = TRUE){
  # Input test
  assert_that(thresh >= 0 & thresh <= 1) # as a probability thing
  
  # Getting a fit for xgb
  if(is.null(alXGB.control)){
    resXGB <- alXGB(train, test, vars = vars, seed = seed, CSV = F)
  } else {
    resXGB <- alXGB(train, test, vars = alXGB.control$vars, seed = seed, CSV = F,
                    NR = alXGB.control$NR, eta = alXGB.control$eta,
                    gamma = alXGB.control$gamma, MCW = alXGB.control$MCW,
                    SS = alXGB.control$SS, colsbt = alXGB.control$colsbt)
  }
  
  # Getting a fit for RF
  if(is.null(alRF.control)){
    resRF <- alRF(train, test,  vars = vars, seed = seed, CSV = F)
  } else {
    resRF <- alRF(train, test,  vars = alRF.control$vars, seed = seed, CSV = F,
                  NT = alRF.control$NT, MT = alRF.control$MT, 
                  NS = alRF.control$NS)
  }
  
  # Extracting the max probabilities and the predictions for both
  maxP.xgb <- apply(resXGB[,c(3:7)], 1, max)
  maxP.rf <- apply(resRF[,c(3:7)], 1, max)
  
  predL.xgb <- resXGB$popularity
  predL.rf <- resXGB$popularity
  
  # Here we choose how to "mix" the two models
  predLabs <- ifelse(maxP.xgb > thresh, predL.xgb, predL.rf)
  
  popularityClass <- data.frame(id = test$id, popularity = predLabs)
  
  # Comparing values for training and test set
  if(Xtest){
    acc <- mean(ifelse(predLabs == test$popularity, 1, 0))
    print(acc)
    return(acc)
  }
  
  # Printing CSV
  if(CSV){
    write.csv(popularityClass,"kagglesub.csv", row.names = F, quote = F)
  }
  
  return(popularityClass)
  
  
}