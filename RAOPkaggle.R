#Created by Camilo Dorado
#Created on August 19th/2014
#Last modification on September 3rd/2014

set.seed(871005)
#Required libraries
library(ggplot2)
library(jsonlite)
library(plyr)
library(rpart)
#Some possibly useful constants
trainFraction <- 0.5
calibrateFraction <- 0.2
testFraction <- 0.3
#Set the working directory
setwd("d:/Users/Camilo/Desktop/DataScience/IntroDataSci")
#Download and unzip the data
dataURL <- "http://cs.stanford.edu/~althoff/raop-dataset/pizza_request_dataset.tar.gz"
#Create a temporary directory and a placeholder file
tempDir <- tempdir()
tempFile <- tempfile(tmpdir = tempDir, fileext = ".gz")
#Download data into placeholder file
download.file(dataURL, tempFile)
#Extract the files to the current working directory
untar(tempFile, exdir = ".")
jsonFile <- "pizza_request_dataset/pizza_request_dataset.json"
#Load the data
originalData <- fromJSON(jsonFile)
#Take away some useless variables
data <- originalData[, c(6, 9, 10, seq(13, 21, by = 2), seq(24, 30, by = 2))]
names(data) <- c("id", "text", "title", "age", "comments", "RAOPcomments", 
                 "posts", "RAOPposts", "success", "upMinusDown", "upPlusDown", 
                 "flair")
#Get some uselful variables
data <- transform(data, hasGiven = !(is.na(flair) | flair != "PIF"))
data <- transform(data, upvotes = (upPlusDown + upMinusDown) / 2)
data <- transform(data, downvotes = (upPlusDown - upMinusDown) / 2)
data <- transform(data, success = as.numeric(success))
data <- transform(data, hasGiven = as.numeric(hasGiven))
#Create a dataframe w/ records <term, narrative>
narratives <- matrix(nrow = 0, ncol = 2)
narrativeFiles <- dir("./pizza_request_dataset/narratives")
for(file in narrativeFiles){
  terms <- read.table(paste("./pizza_request_dataset/narratives/", file, sep = ""))
  names(terms) <- "term"
  terms <- unique(terms)
  narrativeName <- substr(file, 1, nchar(file) - 4)
  narrative <- data.frame(term = terms, 
                          narrative = rep(narrativeName, nrow(terms)))
  narratives <- rbind(narratives, narrative)
  rm(terms, narrative)}
#Calculate the number of words and the score of each record in narratives
for(row in 1:nrow(data)){
  #All the words w/ "" and repetitions
  allText <- paste(data[row, c("title", "text")])
  allTerms <- strsplit(tolower(gsub("[0-9]|[[:punct:]]|[\n]", " ", 
                                    allText)), " ")[[1]]
  #Report the number of words
  nWords <- length(allTerms[allTerms != ""])
  data[row, "words"] <- nWords
  #Consider just the unique terms
  terms <- unique(allTerms)
  #Calculate the score of each record in narratives
  narrativeTerms <- join(data.frame(term = terms), narratives, by = "term", 
                         type = "inner")
  for(narrativeName in levels(narratives$narrative)){
    data[row, narrativeName] <- table(narrativeTerms$narrative)[narrativeName]}}
#Create thereIs variables
data$thereIscomments <- data$comments > 0
data$thereIsRAOPcomments <- data$RAOPcomments > 0
data$thereIsposts <- data$posts > 0
data$thereIsRAOPposts <- data$RAOPposts > 0
data$thereIsAge <- data$age > 0
data$thereIsDesire <- data$desire > 0
data$thereIsFamily <- data$family > 0
data$thereIsJob <- data$job > 0
data$thereIsMoney <- data$money > 0
data$thereIsStudent <- data$student > 0
#Determine the train and non-train subsets
data$train <- F
trainIndex <- sample(nrow(data), trainFraction * nrow(data))
data$train[trainIndex] <- T
#Keep just the useful (numeric) variables
newdata <- data[, c(1, 32, 4:8, 13:31, 9)]
#Partition the data into train and test subsets
training <- newdata[newdata$train,]
testing <- newdata[!newdata$train,]
#Generate a generalized linear model w/o interactions
form0 <- formula(success ~ age + comments + RAOPcomments + posts + RAOPposts +
                hasGiven + Upvotes + Downvotes + words + desire + family +
                job + money + student + thereIscomments + thereIsRAOPcomments +
                thereIsposts + thereIsRAOPposts + thereIsAge + thereIsDesire +
                thereIsFamily + thereIsJob + thereIsMoney + thereIsStudent)
model0 <- glm(form0, data = training)
#Generate a generalized linear model w interactions
form1 <- formula(success ~ age * comments * RAOPcomments * posts * RAOPposts *
                   hasGiven * Upvotes * Downvotes * words * desire * family *
                   job * money * student)
model1 <- glm(form1, data = training)
#Testing the prediction algorithm in the testing dataset
testing$prediction <- 0
for(row in 1:nrow(testing)){
  if(testing$hasGiven[row] == 1){
    testing$prediction[row] <- 1}
  else{
    if(testing$RAOPcomments[row] > 11){
      testing$prediction[row] <- 1}}}
#Assessing the success of the prediction in the testing dataset
print(table(testing$success == testing$prediction))
print(table(testing$success, testing$prediction))
