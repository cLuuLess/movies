library(rpart)
dataIn <- read.table("~/Research/Data/Action Movies Results.csv", header=TRUE, sep=",")
dataIn <- dataIn[,4:26] # Get rid of first col with movie names

set.seed(692)
# nTrees <- c(5, 10, 20, 30, 40, 50, 100)
# minSplit <- c(2, 3, 4, 5)
# cpVals <- c(0.1, 0.01, 0.001, 0.0001)
nTrees <- c(20)
minSplit <- c(2)
cpVals <- c(0.001)

maxR2 = 0
t_final = 0
m_final = 0
c_final = 0

for (numTrees in nTrees) {
  for (m in minSplit) {
    for (c in cpVals) {

numExamples <- nrow(dataIn)
treeIndices <- seq(1, numTrees)
treelist <- vector(mode="list", length=numTrees)
predlist <- vector(mode="list", length=numTrees)
# For bag-CART, sample with replacement and create trees
for (tree in treeIndices) {
  # Number of examples = 12, subsampling 6 examples -- hardcoded for now
  subsample = dataIn[sample.int(numExamples, size = 30, replace = TRUE),]
  # train the tree
  treelist[[tree]] <- rpart(ROI~., data = subsample, method = "anova", control=rpart.control(minsplit=m, cp=c))
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
#printcp(treelist[[1]]) # display the results
#summary(treelist[[1]]) # detailed summary of splits

# plot trees, save all of them
#for (tree in treeIndices) {
#  png(paste('~/Research/Output/tree',tree,'.png',sep=''))
#  plot(treelist[[tree]], uniform=TRUE, main="Preliminary Regression Tree")
#  text(treelist[[tree]], use.n=TRUE, all=TRUE, cex=.8)
#  dev.off()
#}

MSE = sum((finalPrediction-dataIn[,1])^2)/40
E_data = mean(dataIn[,1])
# how well it will fit to future data -- closer to 1 is better
R2 = 1 - sum((finalPrediction-dataIn[,1])^2)/sum((finalPrediction-E_data)^2)
# 
if (R2 > maxR2) {
  t_final = numTrees
  m_final = m
  c_final = c
  maxR2 = R2
}

MSE
R2

}
}
}

t_final
m_final
c_final