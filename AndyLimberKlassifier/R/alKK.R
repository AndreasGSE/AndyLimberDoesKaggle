# --------------------------------------- #
#       Kaggle prediction function        #
# --------------------------------------- #
#' Andy Limber's Kaggle Klassifier
#'
#' Assign popularity values to news articles for submission to Kaggle
#'
#' @param train A dataframe containing the training data for the
#' popularity classifications.
#' @param test A dataframe containing the data to be used to predict labels.
#' If a column of NAs is not supplied for the "popularity", one will be added.
#' @param NT Integer. The number of trees used by the forest (ntree).
#' @param MT Integer. The mtry parameter for randomForest.
#' @param NS Integer. The nodesize parameter.
#' @param imp Logical. Whether or not the importance should be calculated (default is false).
#' @param seed The seed to be used.
#' @param Xtest A logical that indicates whether you want to perform some
#' cross validation. Default is FALSE. If TRUE, will print an accuracy value.
#' Must supply values for popularity.
#' @param CSV A logical that indicates whether a CSV of predictions should be
#' saved. Default is TRUE.
#' @return The random forest model as well as a CSV of predictions
#' if CSV is set to TRUE.
#' @export
#' @import assertthat
#' @import randomForest

alKK <- function(train, test, NT = 1000, MT = 12, NS = 25, imp = FALSE,
                 seed = 123, Xtest = FALSE, CSV = TRUE){
  set.seed(seed)

  # testing inputs
  library(assertthat)
  not_empty(test); not_empty(train);

  if(Xtest) see_if(noNA(test$popularity)) # making sure we have values

  if(!Xtest) test$popularity <- NA # filling in the column for later

  # Will not work if things are in a different order / under different names
  assert_that(ncol(train) == ncol(test))
  are_equal(names(train), names(test))

  # Combining the data frames to manipulate variables
  nTrain <- nrow(train)
  comb <- rbind(train,test)

  # Pulling out year and month info
  getDates <- function(comb){

    # Looks for the year and month inside the URL
    year <- as.vector(sapply(as.character(comb$url), function(x){
      word <- strsplit(x,"/")[[1]][4]
      return(word)
    }))

    months <- as.vector(sapply(as.character(comb$url), function(x){
      word <- strsplit(x,"/")[[1]][5]
      return(word)
    }))

    comb$Year <- as.factor(year)
    comb$Month <- as.factor(months)

    # Re-arranging the columns to make pretty
    a <- ncol(comb)
    comb <- comb[, c(1:(a-3),a,(a-1),(a-2))]

    return(comb)
  }

  comb <- getDates(comb)

  # Pulling out details of titles of articles
  getTitles <- function(comb){

    title <- as.vector(sapply(as.character(comb$url), function(x){
      word <- strsplit(x,"/")[[1]][7]
      return(word)
    }))

    comb$Title <- nchar(title)

    # Re-arranging the columns otherwise trees aren't happy
    a <- ncol(comb)
    comb <- comb[, c(1:(a-2),a,(a-1))]

    return(comb)
  }

  comb <- getTitles(comb)


  # Splitting comb again. This should always come after any feature analysis
  train <- comb[1:nTrain,]
  test <- comb[-(1:nTrain),]

  if (!require("randomForest")) install.packages("randomForest"); library(randomForest)

  # Getting the variables for formula, requires "popularity" to come last
  vars <- 3:(ncol(train)-1)
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

  popularityClass <- data.frame(id = test$id, popularity = Prediction)

  # Comparing values for training and test set
  if(Xtest){
    print(mean(ifelse(popularityClass$popularity == test$popularity, 1, 0)))
  }

  # Printing CSV
  if(CSV){
    write.csv(popularityClass,"kagglesub.csv", row.names = F, quote = F)
  }

  return(randomFor)
}