# ----------------------------------------------- #
#       Kaggle feature generation function        #
# ----------------------------------------------- #
#' Andy Limber's Kaggle feature generation
#'
#' Generates features from the original training and test data to be used in the classifier.
#' If to be used with alKK - must be called on both train and test data
#'
#' @param data A dataframe containing the data for the
#' popularity classifications that will be modified. Can be train or test data.
#' no modifications are needed.
#' @return The modified data with added columns. Currently adds a title length field and
#' year and month field.
#' @export
#' @import assertthat

alFeatureGen <- function(data){
  
  # testing inputs
  #library(assertthat)
  not_empty(data)
  not_empty(data$url)
  
  # Pulling out year and month info
  getDates <- function(data){
    
    # Looks for the year and month inside the URL
    year <- as.vector(sapply(as.character(data$url), function(x){
      word <- strsplit(x,"/")[[1]][4]
      return(word)
    }))
    
    months <- as.vector(sapply(as.character(data$url), function(x){
      word <- strsplit(x,"/")[[1]][5]
      return(word)
    }))
    
    data$Year <- as.factor(year)
    data$Month <- as.factor(months)
    
    return(data)
  }
  
  data <- getDates(data)
  
  # Pulling out details of titles of articles
  getTitles <- function(data){
    
    title <- as.vector(sapply(as.character(data$url), function(x){
      word <- strsplit(x,"/")[[1]][7]
      return(word)
    }))
    
    data$Title <- nchar(title)
    
    return(data)
  }
  
  data <- getTitles(data)
  
  # Re-arranging columns to put popularity last
  data <- data[,c(which(names(data) != "popularity"), which(names(data) == "popularity"))]
  
  # Return original data with the added columns
  return(data)
}