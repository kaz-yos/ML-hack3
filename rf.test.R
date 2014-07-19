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
trainIndices <- sample(nrow(iris), size = 100)
irisTrain <- iris[trainIndices,]
irisTest  <- iris[-1*trainIndices,]


### Test
################################################################################

## test code for a single tree
myTree <- CreateTree(iris[1:3],iris$Species)
predict(myTree, type = "class", newdata = iris)
predict(myTree, type = "prob", newdata = iris)

## test code for a forest
myTrees <- CreateForest(xs = irisTrain[,1:4], y = irisTrain$Species, 10)


## Check predictions against gold standard
xtabs( ~ predict(myTrees, newdata = irisTest) + irisTest$Species)
