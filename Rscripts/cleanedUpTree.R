chooseVars <- function(x, data){
  featRem <- which(names(data) %in% c("id", "url", "popularity", x))
  featVec <- c(1:length(names(data)))[-featRem]
  return(featVec)
}

library(AndyLimberKlassifier)
# Getting data
setwd("C:\\Users\\Dre\\Desktop\\Data")
trainData <- read.csv("news_popularity_training.csv", stringsAsFactors = T)
testData <- read.csv("news_popularity_test.csv", stringsAsFactors = T)

trainData$popularity <- as.factor(trainData$popularity)

# Getting a benchmark Xval for all features and untouched data
plainData_acc <- alXvalidate(data = trainData, NT = 300)

## 51.30


# Now with the added features
train1 <- alFeatureGen(trainData)
gen1_acc <- alXvalidate(data = train1, NT = 300) # 51.91

# Pumping up the trees a bit
gen2_acc <- alXvalidate(data = train1, NT = 1000)


# Looking at some other added features
getProp <- function(data){
  data$self_reference_range_shares <- data$self_reference_max_shares-data$self_reference_min_shares
  
  data$kw_max_range <- data$kw_max_max - data$kw_max_min
  data$kw_avg_range <- data$kw_avg_max - data$kw_avg_min
  data$kw_min_range <- data$kw_min_max - data$kw_min_min
  
  data$global_diff_rate <- data$global_rate_positive_words - data$global_rate_negative_words
  data$avg_pol_diff <- data$avg_positive_polarity - data$avg_negative_polarity
  
  data <- data[,c(which(names(data) != "popularity"), which(names(data) == "popularity"))]
  
  return(data)
}
train2 <- getProp(train1)

newFeat <- names(train2)[65:(length(train2)-1)]
featVec <- chooseVars(newFeat[-5], train2)

# First try by just "adding" the feature
newFeat <- names(train2)[65:(length(train2)-1)]
featVec <- chooseVars(newFeat[-5], train2)

gen2_acc <- alXvalidate(data = train2, NT = 300, vars = featVec) # 0.5201

featVec <- chooseVars(newFeat[-1], train2)
gen3_acc <- alXvalidate(data = train2, NT = 300, vars = featVec) # 0.5200

featVec <- chooseVars(newFeat[-4], train2) # 0.5200
gen4_acc <- alXvalidate(data = train2, NT = 300, vars = featVec)



featVec <- chooseVars(newFeat[-c(1,5)], train2) 

gen4_acc <- alXvalidate(data = train2, NT = 300, vars = featVec) # 0.5190


# Now try by removing a low importance one
featVec <- chooseVars(c(newFeat[-5], "weekday_is_monday"), train2)
gen5_acc <- alXvalidate(data = train2, NT = 300, vars = featVec) # 0.5187

featVec <- chooseVars(c(newFeat[-5], "weekday_is_wednesday"), train2) 
gen6_acc <- alXvalidate(data = train2, NT = 300, vars = featVec) # 0.5176

featVec <- chooseVars(c(newFeat[-c(1, 5)], "weekday_is_wednesday", "weekday_is_monday"), train2)
gen7_acc <- alXvalidate(data = train2, NT = 300, vars = featVec) # 0.5174

featVec <- chooseVars(c(newFeat[-c(1, 5)], "weekday_is_wednesday"), train2)
gen8_acc <- alXvalidate(data = train2, NT = 300, vars = featVec) # 0.516


# Trying to do some media related stuff
train2$avg_pol_diff <- ifelse(train2$num_videos == 0, 0, 1) # 0.5215 + 0.5211
featVec <- chooseVars(newFeat[-6], train2)
gen4_acc <- alXvalidate(data = train2, NT = 300, vars = featVec)

featVec <- chooseVars(c(newFeat[-c(5, 6)]), train2)
gen4_acc <- alXvalidate(data = train2, NT = 500, vars = featVec)


# LDA stuff 41 - 45
train1$LDAmax <- apply(train1[,41:45], 1, which.max)
train1 <- train1[,c(which(names(train1) != "popularity"), which(names(train1) == "popularity"))]


gen4_acc <- alXvalidate(data = train1, NT = 500) # 0.5200

featVec <- chooseVars("is_Video", train1)
gen4_acc <- alXvalidate(data = train1, NT = 500, vars = featVec)

############ sub
train <- alFeatureGen(trainData)
test <- alFeatureGen(testData)

sub <- alKK(train, test, NT = 300, imp = TRUE) 


