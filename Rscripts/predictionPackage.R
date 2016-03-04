# The script is the prediction process so far. CSV files are formatted
# for submission.
# Further ideas are to improve the actual random forest generation using different
# functions as well as parameters, because I think this will help the most.
# Otherwise there is a thing of variable selection, but inclusion of all of them
# seems to be the best. Creation of new variables is another shout.

setwd("C:\\Users\\Dre\\Dropbox\\Barna\\Master\\AdvancedComputing\\kagglecomp\\AndyLimberDoesKaggle\\AndyLimberKlassifier")
setwd("C:\\Users\\Dre\\Dropbox\\Barna\\Master\\AdvancedComputing\\kagglecomp\\AndyLimberDoesKaggle\\AndyLimberKlassifier\\tests")
setwd("C:\\Users\\Dre\\Desktop\\Data")

library(rpart)
if (!require("rattle")) install.packages("rattle"); library(rattle)
if (!require("rpart.plot")) install.packages("rpart.plot"); library(rpart.plot)
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer)

trainData <- read.csv("news_popularity_training.csv", stringsAsFactors = T)
testData <- read.csv("news_popularity_test.csv", stringsAsFactors = T)

### Variable creation section ###

# This function pulls the year and months from the URL included in the data
# Will probably add all other "designed" features here
getDates <- function(train,test){
  test$popularity <- NA
  n <- nrow(train)
  comb <- rbind(train,test) # Combining the two to make easier
  
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
  
  # Re-arranging the columns otherwise trees aren't happy
  a <- ncol(comb)
  comb <- comb[, c(1:(a-3),a,(a-1),(a-2))]
  
  # Returns a list containing the changed train and test sets
  reslist <- list(train = comb[1:n,],
                  test = comb[(n+1):nrow(comb),])
  
  return(reslist)
}

reslist <- getDates(trainData, testData)

train <- reslist$train # this is required to properly put things back in place
test <- reslist$test

# Function to just return the column indices of the variables you want
# Made this just to clean up some code. The idea is that you have a vector
# With all variables, then use this to get the indices of the ones you want to
# remove.
# e.g.
# vec <- c(1:(length(names(train))-1)) ->>> all the variables
# variables <- c(names(train)[1:2], "Year") ->>> exclude these variables
# newVec <- vec[-getVar(variables,train)] ->>> variables want to include
getVar <- function(varNames, frame){
  col.names <- names(frame)
  
  indices <- sapply(varNames, function(word,y){
    a <- as.numeric(grep(word, y)[1])
  }, y = col.names)
  
  return(indices)
}

# Getting a testing set - important I think so that things are kept seperate
testSet <- function(data){
  m <- ceiling(nrow(data)/3)
  subset <- sample(c(1:(3*m)),2*m)
  test <- data[-subset,]
  train <- data[subset,]
  l <- list(train = train, test = test)
  return(l)
}

# A separate function for evaluation, just for flexibility
evaluation <- function(pred, test){
  score <- mean(ifelse(pred$popularity == test$popularity),1,0)
  return(score)
}

# Getting the titles of articles
getTitles <- function(train,test){
  n <- nrow(train)
  comb <- rbind(train,test) # Combining the two to make easier
  
  title <- as.vector(sapply(as.character(comb$url), function(x){
    word <- strsplit(x,"/")[[1]][7]
    return(word)
  }))
  
  comb$Title <- title
  
  # Re-arranging the columns otherwise trees aren't happy
  a <- ncol(comb)
  comb <- comb[, c(1:(a-2),a,(a-1))]
  
  # Returns a list containing the changed train and test sets
  reslist <- list(train = comb[1:n,],
                  test = comb[(n+1):nrow(comb),])
  
  return(reslist)
}

reslist <- getTitles(train, test)

train <- reslist$train
test <- reslist$test

train$Title <- as.numeric(nchar(train$Title))
test$Title <- as.numeric(nchar(test$Title))

# Adding a new "channel" for other
getProp <- function(train,test){
  n <- nrow(train)
  comb <- rbind(train,test)
  
  tot_med <- comb$num_imgs+comb$num_videos
  
  comb$n_t_c_m <- comb$n_tokens_content/tot_med
  comb$n_t_c_i <- comb$n_tokens_content/comb$num_imgs
  comb$n_t_c_v <- comb$n_tokens_content/comb$num_videos
  
  comb$n_u_c_m <- comb$n_unique_tokens/tot_med
  comb$n_u_c_i <- comb$n_unique_tokens/comb$num_imgs
  comb$n_u_c_v <- comb$n_unique_tokens/comb$num_videos
  
  comb$self_reference_range_shares <- comb$self_reference_max_shares-comb$self_reference_min_shares
  
  comb$kw_max_range <- comb$kw_max_max - comb$kw_max_min
  comb$kw_avg_range <- comb$kw_avg_max - comb$kw_avg_min
  comb$kw_min_range <- comb$kw_min_max - comb$kw_min_min
  
  comb$global_diff_rate <- comb$global_rate_positive_words - comb$global_rate_negative_words
  comb$avg_pol_diff <- comb$avg_positive_polarity - comb$avg_negative_polarity
  
  comb <- comb[,c(which(names(comb) != "popularity"), which(names(comb) == "popularity"))]
  
  reslist <- list(train = comb[1:n,],
                  test = comb[(n+1):nrow(comb),])
  return(reslist)
}

reslist <- getProp(train, test)

train <- reslist$train
test <- reslist$test

### RANDOM TREES ###
if (!require("unbalanced")) install.packages("unbalanced"); library(unbalanced)

# A function that does a similar thing to above, but now using random trees.
# Two options of method, changed by setting "rand" to T or F - F is a bit buggy
# dummytest will allow you to do a cross validation type thing
# Returns the MODEL instead, for the "importance" plot
# Set NT for the number of trees - 100 provides a pretty good result
# PDF = TRUE CURRENTLY DOES NOT WORK
# NOTE that dummytest = T will not provide a "submittable" CSV file
predLabsRF <- function(train, test, vars = NA, NT = 100, NS = 25, MT = 99999, seed = 123, 
                       pdf = FALSE, csv = TRUE, rand = TRUE, dummytest = FALSE,
                       DATA = FALSE){
  if (!require("randomForest")) install.packages("randomForest"); library(randomForest)
  if (!require("party")) install.packages("party"); library(party)
  
  set.seed(seed)

  if(is.na(vars)){
    n <- length(names(train)) - 1
    form <- as.formula(paste0("as.factor(popularity)~",
                              paste(names(train)[4:n],collapse = "+")))
  } else {
    variables <- names(train)[vars]
    features <- paste(variables,collapse = "+")
    form <- as.formula(paste0("as.factor(popularity)~",
                              features))
  }

  print("Generating random tree. This may take a while.")
  
  # Creating the random forests. The second method only works for certain seeds.
  if(rand){
    randomTree <- randomForest(form, data = train, importance = TRUE, ntree = NT,
                               OOB = T, nodesize = NS, mtry = MT)
    Prediction <- predict(randomTree, test, type = "class")
    
  } else {
    randomTree <- cforest(form, data = train, controls = cforest_unbiased(ntree = NT))
    Prediction <- predict(randomTree, test, OOB=TRUE, type = "response")
  }
  
  # Saving the diagram to pdf
  if(pdf){
    pdf("treediagram.pdf")
    fancyRpartPlot(tree)
    dev.off()
  }
  
  # Getting predicted labels
  print("Getting labels")
  popularityClass <- data.frame(id = test$id, popularity = Prediction)
  
  # Doing the cross validating to see how accurate out of sample.
  if(dummytest){
    popularityClass$real.popularity <- test$popularity
    print(mean(ifelse(popularityClass$popularity == test$popularity, 1, 0)))
  }
  
  if(csv){
    write.csv(popularityClass,"kagglesub.csv", row.names = F, quote = F)
  }
  
  if(!DATA) return(randomTree)
  if(DATA) return(popularityClass)
  
}

# Getting a test set to work with
trialData <- testSet(train)

trialTrain <- trialData$train
trialTest <- trialData$test

randomFor <- predLabsRF(trialTrain,trialTest, 
                        csv = F, rand = T, dummytest = T,
                        vars = newVec, NT = 500, NS = 25)

# FEATURE TESTING AREA
toElim <- names(train)[(ncol(train)-12):(ncol(train) - 1)]
toElim <- toElim[-c(7,11)]

vec <- c(1:(length(names(train)))) # all the variables

variables <- c(names(train)[1:2],"popularity", toElim, "weekday_is_monday", "weekday_is_wednesday")
variables <- c(names(train)[1:2],"popularity", toElim)

newVec <- vec[-getVar(variables,train)]

randomFor <- predLabsRF(trialTrain,trialTest, 
                        csv = F, rand = T, dummytest = T,
                        vars = newVec, NT = 500, NS = 25, MT = 12)


head(sort(randomFor$importance[,6]))
head(sort(randomFor$importance[,7]))
sum(randomFor$importance[,6]) + sum(randomFor$importance[,7])
varImpPlot(randomFor)
# Example run with "importance plot"
vec <- c(1:(length(names(train)))) # all the variables

variables <- c(names(train)[1:2],"popularity", toElim)
newVec <- vec[-getVar(variables,train)]

randomFor <- predLabsRF(train,test, 
                        csv = T, rand = T, dummytest = F,
                        vars = newVec, NT = 1000,
                        DATA = F, NS = 25, MT = 12)

# LOOKING AT IMPORTANCE
trialData <- testSet(train)
trialTrain <- trialData$train
trialTest <- trialData$test

impvarG <- order(randomFor$importance[,6], decreasing = T)
n <- length(impvarG)
notImp <- names(train)[(impvarG + 2)]
notImp <- notImp[(n-10):n]

variables <- c(names(train)[1:2],"popularity", "data_channel_is_other")
newVec <- vec[-getVar(variables,train)]

randomFor <- predLabsRF(trialTrain,trialTest, 
                        csv = F, rand = T, dummytest = T,
                        vars = newVec, NT = 500, NS = 25, MT = 12)
# so far 11 is the best

# The plot of which variables are important
varImpPlot(randomFor)



# Balancing data - doesn't really work well
if (!require("DMwR")) install.packages("DMwR"); library(DMwR)

variables <- names(train)[newVec]
features <- paste(variables,collapse = "+")
form <- as.formula(paste0("popularity~",
                          features))
trialTrain[,"popularity"] <- as.factor(trialTrain[,"popularity"])
newData <- SMOTE(form, trialTrain, perc.over = 1000, k = 10)

randomFor <- predLabsRF(rbind(trialTrain,newData),trialTest, 
                        csv = F, rand = T, dummytest = T,
                        vars = newVec, NT = 100, seed = 100)

for(i in 1:7){
  newData <- SMOTE(form, trialTrain, perc.over = 4000, k = 10)
  
  randomFor <- predLabsRF(rbind(trialTrain,newData),trialTest, 
                          csv = F, rand = T, dummytest = T,
                          vars = newVec, NT = 100, seed = 100)
}

# rate of media per words?

# Need to look a bit at removing features in exchange for more trees to see if improves

