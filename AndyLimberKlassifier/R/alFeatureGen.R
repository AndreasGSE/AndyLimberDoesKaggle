# ----------------------------------------------- #
#       Kaggle feature generation function        #
# ----------------------------------------------- #
#' Andy Limber's Kaggle feature generation
#'
#' Generates features from the original training and test data to be used in the classifier.
#'
#' @param train A dataframe containing the training data for the
#' popularity classifications.
#' @param test A dataframe containing the data to be used to predict labels.
#' If a column of NAs is not supplied for the "popularity", one will be added.
#' @param Xtest A logical that indicates whether you want to perform some
#' cross validation at a later stage. Default is FALSE and a popularity column is added.
#' Should be set to TRUE if popularity column is to be unmodified, but non-NA values
#' must be provided.
#' @return A list containing the new "test" and "train" dataframes with added features.
#' @export
#' @import assertthat

alFeatureGen <- function(train, test, Xtest = FALSE){
  
  # testing inputs
  library(assertthat)
  not_empty(test); not_empty(train);
  
  if(Xtest) see_if(noNA(test$popularity)) # making sure we have values
  
  if(!Xtest) test$popularity <- NA # filling in the column for later
  
  # Will not work if things are in a different order / under different names
  assert_that(ncol(train) == ncol(test))
  are_equal(names(train), names(test))
  
  # Making sure popularity is the last column
  assert_that(names(train)[ncol(train)] == "popularity")
  
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
  
  dataList <- list(train = train, test = test)
  
  return(dataList)
}