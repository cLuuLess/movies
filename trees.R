library(rpart)
dataIn <- read.table("~/Research/Data/Action Movies Results.csv", header=TRUE, sep=",")
dataIn <- dataIn[,4:26] # Get rid of first col with movie names

# Toy example, will use 3 trees
numTrees <- 20
numExamples <- nrow(dataIn)
treeIndices <- seq(1, numTrees)
treelist <- vector(mode="list", length=numTrees)
predlist <- vector(mode="list", length=numTrees)
# For bag-CART, sample with replacement and create trees
for (tree in treeIndices) {
  # Number of examples = 12, subsampling 6 examples -- hardcoded for now
  subsample = dataIn[sample.int(numExamples, size = 35, replace = TRUE),]
  # train the tree
  treelist[[tree]] <- rpart(ROI~., data = subsample, method = "anova", control=rpart.control(minsplit=2, cp=0.001))
  # prediction for this particular tree
  predlist[[tree]] <- predict(treelist[[tree]], newdata = dataIn, type = "vector")
}

# do an element-wise addition of all the predictions
sums = rep(0, numExamples)
for (tree in treeIndices) {
  sums = sums + predlist[[tree]]
}
# Final predcted movie gross for bag-CART is the average prediction from
# all the trees in the ensemble.
finalPrediction = sums / numTrees


# Random toy code for viewing trees
printcp(treelist[[1]]) # display the results
summary(treelist[[1]]) # detailed summary of splits

# plot trees, save all of them
for (tree in treeIndices) {
  png(paste('~/Research/Output/tree',tree,'.png',sep=''))
  plot(treelist[[tree]], uniform=TRUE, main="Preliminary Regression Tree")
  text(treelist[[tree]], use.n=TRUE, all=TRUE, cex=.8)
  dev.off()
}

MSE = sum((finalPrediction-dataIn[,1])^2)/40
E_data = mean(dataIn[,1])
# how well it will fit to future data -- closer to 1 is better
R2 = 1 - sum((finalPrediction-dataIn[,1])^2)/sum((finalPrediction-E_data)^2)

MSE
R2
