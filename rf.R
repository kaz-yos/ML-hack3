################################################################################
### Function for random forest
##
## Created on: 2014-07-19
## Author: Kazuki Yoshida
################################################################################

library(mvpart)
library(plyr)

data(iris)
trainIndices <- sample(nrow(iris), size = 100)
irisTrain <- iris[trainIndices,]
irisTest  <- iris[-1*trainIndices,]



### A function to create one tree
CreateTree <- function(xs,y) {

    ## Combine as a data frame
    ## This breaks down if y is present in xs
    dat <- cbind(y = y, xs)

    ## Variable names separated by "+"
    formulaRhs <- sub(" \\+ $", "", paste0(names(xs), sep = " + ", collapse = ""))

    ## Create formula
    form1 <- paste0("y ~ ", formulaRhs, collapse = "")
    print(form1)

    ## Create a tree
    rpart(formula = form1, data = dat)
}

## test code
myTree <- CreateTree(iris[1:3],iris$Species)
predict(myTree, type = "class", newdata = iris)
predict(myTree, type = "prob", newdata = iris)


### A function to create M trees
CreateForest <- function(xs, y, numberOfTrees) {

    ## 1:M vector
    vecAlongVars <- seq_len(ncol(xs))

    ## Create (numberOfTrees) trees
    treesList <- lapply(seq_len(numberOfTrees), function(i) {

        ## Bootstrap from row numbers
        nrowXs <- nrow(xs)
        rowIndices <- sample(x = seq_len(nrowXs), size = nrowXs, replace = TRUE)
        
        ## Create column numbers for variables to use in this iteration
        ## Randomly pick a number between 1 and ncol(xs)
        colIndices <- sort(sample(vecAlongVars, sample(vecAlongVars, 1)))

        ## Create a single tree object
        singleTree <- CreateTree(xs[rowIndices,colIndices,drop = FALSE], y[rowIndices])

        ## 
        singleTree
    })

    ## Give S3 class
    class(treesList) <- "MyRandomForest"
    treesList
}

## test code
myTrees <- CreateForest(xs = irisTrain[,1:4], y = irisTrain$Species, 10)


### A function to predict 
predict.MyRandomForest <- function(object, newdata) {

    listOfProbMat <- lapply(object, function(obj) {

        predict(obj, newdata, type = "prob")
    })

    ## list -> array
    arrayOfProbs <- laply(listOfProbMat, .fun = function(x) {x})

    ## mean of all trees
    meanMat <- apply(arrayOfProbs, MARGIN = c(2,3), mean)

    ## Pick names as predicted class
    colnames(meanMat)[apply(meanMat, 1, which.max)]
}


xtabs( ~ predict(myTrees, newdata = irisTest) + irisTest$Species)
