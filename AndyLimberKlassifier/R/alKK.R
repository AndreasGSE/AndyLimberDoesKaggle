# --------------------------------------- #
#       Kaggle prediction function        #
# --------------------------------------- #
#' Andy Limber's Kaggle Klassifier
#'
#' Assign popularity values to news articles for submission to Kaggle. Can be used with
#' any set of features as unreferenced by name, but best submission includes feature generation
#' done by the alFeatuerGen function (must be called on train and test data separately).
#'
#' @param train A dataframe containing the training data for the
#' popularity classifications.
#' @param test A dataframe containing the data to be used to predict labels.
#' If a column of NAs is not supplied for the "popularity", one will be added.
#' @param NT Integer. The number of trees used by the forest (ntree).
#' @param MT Integer. The mtry parameter for randomForest.
#' @param NS Integer. The nodesize parameter.
#' @param vars Vector of column indices for the features to be used in the random forest.
#' Defaults to NA and all features are used.
#' @param imp Logical. Whether or not the importance should be calculated (default is false).
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
#' @import randomForest

alKK <- function(train, test, NT = 1000, MT = 12, NS = 25, vars = NA, imp = FALSE,
                 seed = 123, Xtest = FALSE, CSV = TRUE){
  set.seed(seed)

  # testing inputs
  not_empty(test); not_empty(train);

  if(Xtest){
    assert_that(noNA(test$popularity)) # making sure we have values
    assert_that(not_empty(test$popularity))
  } 

  # Will not work if things are in a different order / under different names
  assert_that(ncol(train) == ncol(test) | (ncol(train) - 1) == ncol(test))
  assert_that(are_equal(names(train), names(test)) | 
                are_equal(names(train), c(names(test), "popularity")))
  
  # Making sure popularity is the last column
  assert_that(names(train)[ncol(train)] == "popularity")

  # Getting the variables for formula, requires "popularity" to come last
  if(is.na(vars)){
    vars <- 3:(ncol(train)-1) # taking all variables
  }
  
  variables <- names(train)[vars]
  features <- paste(variables,collapse = "+")
  form <- as.formula(paste0("as.factor(popularity)~",
                            features))

  # Getting the random tree with specified features
  print("Generating Random Tree. This may take a while...")
  randomFor <- randomForest(form, data = train, importance = imp, ntree = NT,
                             OOB = T, nodesize = NS, mtry = MT)

  # Predicting labels
  print("Getting labels")
  Prediction <- predict(randomFor, test, type = "class")
  probs <- predict(randomFor, test, type = "prob")

  popularityClass <- data.frame(id = test$id, popularity = Prediction)
  
  popularityClass <- cbind(popularityClass, probs)

  # Comparing values for training and test set
  if(Xtest){
    acc <- mean(ifelse(popularityClass$popularity == test$popularity, 1, 0))
    print(acc)
    return(acc)
  }

  # Printing CSV
  if(CSV){
    write.csv(popularityClass,"kagglesub.csv", row.names = F, quote = F)
  }

  return(popularityClass)
}