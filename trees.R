library(rpart)
dataIn <- read.table("~/Research/Data/Movie Results (Cleaned) - All.csv", header=TRUE, sep=",")
dataIn <- dataIn[,4:26] # Get rid of first col with movie names
#dataIn <- dataIn[,4:31] # with genres

#nTrees <- c(40, 50, 60, 70, 80, 90)
#minSplit <- c(2, 3, 4, 5)
#cpVals <- c(0.01, 0.001, 0.0001)
#R2scores <- rep(0, length(nTrees)*length(minSplit)*length(cpVals))
#R2index = 1
nTrees <- c(70)
minSplit <- c(3)
cpVals <- c(0.0001)

maxR2 = 0
t_final = 0
m_final = 0
c_final = 0

for (numTrees in nTrees) {
  for (m in minSplit) {
    for (c in cpVals) {
      set.seed(692)
      numExamples <- nrow(dataIn)
      treeIndices <- seq(1, numTrees)
      treelist <- vector(mode="list", length=numTrees)
      predlist <- vector(mode="list", length=numTrees)
      # For bag-CART, sample with replacement and create trees
      for (tree in treeIndices) {
        # Number of examples = 12, subsampling 6 examples -- hardcoded for now
        sampleInd = sample.int(numExamples, size = 0.8*numExamples, replace = TRUE)
        subsample = dataIn[sampleInd,]
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
      
      
      
      MSE = sum((finalPrediction-dataIn[,1])^2)/numExamples
      E_data = mean(dataIn[,1])
      # how well it will fit to future data -- closer to 1 is better
      R2 = 1 - sum((finalPrediction-dataIn[,1])^2)/sum((finalPrediction-E_data)^2)
      R2scores[R2index] = R2
      R2index = R2index + 1
      # 
      if (R2 > maxR2) {
        t_final = numTrees
        m_final = m
        c_final = c
        maxR2 = R2
        
        # plot trees, save all of them
        for (tree in treeIndices) {
          png(paste('~/Research/Output/All-grid/tree',tree,'.png',sep=''))
          plot(treelist[[tree]], uniform=TRUE, main="Preliminary Regression Tree")
          text(treelist[[tree]], use.n=TRUE, all=TRUE, cex=.8)
          dev.off()
        }
      }
    }
  }
}

t_final
m_final
c_final

#########
# relevance index (this code is broken, get relevance index from the next one)
finaltreelist = table(data.frame(c(names(dataIn[-1]),"<leaf>")))
for (t in seq(1,length(treelist))) {
  treelength = length(treelist[[t]]$frame$var)
  for (v in seq(1,treelength)) {
    feature = as.character(treelist[[t]]$frame$var[v])
    finaltreelist[feature] = finaltreelist[feature] + 1
  }
}
write.table(finaltreelist,file="~/Research/Data/relevanceindex.csv",sep=",")
###########
# depth - use this
numcols = 22
depths <- as.data.frame(setNames(replicate(numcols,numeric(0), simplify = F), names(dataIn[-1])))
for(i in 1:15) depths[i, ] <- rep(0,numcols)
depths["<leaf>"] <- 0
for (t in seq(1,length(treelist))) {
  treelength = length(treelist[[t]]$frame$var)
  varInd <- as.numeric(row.names(treelist[[t]]$frame))
  for (v in seq(1,treelength)) {
    row = floor(log2(varInd[v])) + 1
    col = as.character(treelist[[t]]$frame$var[v])
    depths[row, col] <- depths[row, col] + 1
  }
}
write.table(depths,file="~/Research/Data/depths.csv",sep=",")
