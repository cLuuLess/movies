library(rpart)
dataIn <- read.csv("~/Documents/Research/Preliminary Data - Sheet1.csv", header=TRUE)
dataIn <- dataIn[,-1] # Get rid of first col with movie names

# Toy example, will use 3 trees
numTrees <- 3
treeIndices <- seq(1, numTrees)
treelist <- vector(mode="list", length=numTrees)
predlist <- vector(mode="list", length=numTrees)
# For bag-CART, sample with replacement and create trees
for (tree in treeIndices) {
  # Number of examples = 12, subsampling 6 examples -- hardcoded for now
  subsample = dataIn[sample.int(12, size = 6, replace = TRUE),]
  # train the tree
  treelist[[tree]] <- rpart(Gross~., data = subsample, method = "anova", control=rpart.control(minsplit=2, cp=0.001))
  # prediction for this particular tree
  predlist[[tree]] <- predict(treelist[[tree]], newdata = dataIn, type = "vector")
}

# do an element-wise addition of all the predictions
sums = rep(0, 12)
for (tree in treeIndices) {
  sums = sums + predlist[[tree]]
}
# Final predcted movie gross for bag-CART is the average prediction from
# all the trees in the ensemble.
finalPrediction = sums / 3


# Random toy code for viewing trees
printcp(treelist[[1]]) # display the results
summary(treelist[[1]]) # detailed summary of splits

# plot tree 
plot(treelist[[1]], uniform=TRUE, main="Preliminary Regression Tree")
text(treelist[[1]], use.n=TRUE, all=TRUE, cex=.8)