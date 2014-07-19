################################################################################
### 
## 
## Created on: 
## Author: Kazuki Yoshida
################################################################################


### Prepare environment
################################################################################

### Load packages use by random forest
library(mvpart)
library(plyr)

### Load my own implementation of random forest
source("./rf.R")


### Prepare data
################################################################################
data(iris)
set.seed(20140719)
trainIndices <- sample(nrow(iris), size = 100)
irisTrain    <- iris[trainIndices,]
irisTest     <- iris[-1*trainIndices,]


### Test using iris dataset
################################################################################

## test code for a single tree
myTree <- CreateTree(iris[1:3],iris$Species)
## predict(myTree, type = "class", newdata = iris)
## predict(myTree, type = "prob", newdata = iris)

## test code for a forest
myTrees <- CreateForest(xs = irisTrain[,1:4], y = irisTrain$Species, 10)

## Check predictions against gold standard
xtabs( ~ predict(myTrees, newdata = irisTest) + irisTest$Species)


### Repeat with different 2/3 of the dataset
################################################################################
out <- sapply(1:100, function(i) {
    ## Create datasets
    trainIndices <- sample(nrow(iris), size = 100)
    irisTrain    <- iris[trainIndices,]
    irisTest     <- iris[-1*trainIndices,]

    ## Create a forest
    myTrees <- CreateForest(xs = irisTrain[,1:4], y = irisTrain$Species, 10)

    ## Correct rate
    mean(predict(myTrees, newdata = irisTest) == irisTest$Species)
})
plot(density(out))



### Test using a different dataset
################################################################################

library(randomForest)
data(imports85)
head(imports85)
summary(imports85)


vars <- c("symboling", "fuelType", "aspiration", 
          "bodyStyle", "driveWheels", "engineLocation", "wheelBase", 
          "length", "width", "height", "curbWeight", "engineType", "numOfCylinders", 
          "engineSize", "fuelSystem", "compressionRatio", 
          "cityMpg", "highwayMpg")

## 2-pane
layout(matrix(1:2, ncol = 2))


### 10-tree forest
out <- sapply(1:10, function(i) {
    ## Create datasets
    trainIndices <- sample(nrow(imports85), size = 135)
    imports85Train    <- imports85[trainIndices,]
    imports85Test     <- imports85[-1*trainIndices,]

    ## Create a forest
    myTrees <- CreateForest(xs = imports85Train[vars], y = imports85Train$make, 10)

    ## Correct rate
    mean(predict(myTrees, newdata = imports85Test) == imports85Test$make)
})
plot(density(out), main = "10-tree forest", xlim = c(0,1))


### 100-tree forest
out2 <- sapply(1:10, function(i) {
    ## Create datasets
    trainIndices <- sample(nrow(imports85), size = 135)
    imports85Train    <- imports85[trainIndices,]
    imports85Test     <- imports85[-1*trainIndices,]

    ## Create a forest
    myTrees <- CreateForest(xs = imports85Train[vars], y = imports85Train$make, 100)

    ## Correct rate
    mean(predict(myTrees, newdata = imports85Test) == imports85Test$make)
})
plot(density(out2), main = "100-tree forest", xlim = c(0,1))
