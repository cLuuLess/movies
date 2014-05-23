library(rpart)
# dataIn <- read.table("~/Research/Data/Movie Results (Cleaned) - New Movies.csv", header=TRUE, sep=",")
dataIn <- read.csv("~/Research/Data/Movie Results (Cleaned) - Final Training Set - 44 - 1st dim 2.csv")
dataIn = dataIn[dataIn[,1]!="Juno",] # Remove Juno
#dataIn = dataIn[dataIn[,3]=="Comedy",]
dataIn <- dataIn[,8:dim(dataIn)[2]] # Get rid of first col with movie names
#dataIn <- dataIn[,4:31] # with genres

# pc <- dataIn[,-1]
# pc_res <- princomp(pc)
# topvecs <- as.matrix(pc_res$loadings[,1:22])
# res <- as.matrix(pc) %*% topvecs
# res <- data.frame(dataIn[,1],res)
# names(res)[1]="ROI"
# dataIn = res

# Get genres
genreData <- read.csv("~/Research/Data/Movie Results (Cleaned) - Final Train Genres.csv")
genreData = genreData[genreData[,1]!="Juno",] # Remove Juno
genreData <- genreData[,-1] # Take out movie names
dataIn <- data.frame(dataIn,genreData)
# 
nTrees <- c(30, 40, 50, 60, 70, 80, 100)
minSplit <- c(4, 5, 6, 7, 8)
cpVals <- c(0.1, 0.01, 0.001, 0.0001)
# nTrees <- c(50)
# minSplit <- c(8)
# cpVals <- c(0.0001)
R2scores <- rep(0, length(nTrees)*length(minSplit)*length(cpVals))
R2index = 1

maxR2 = -100
minMSE = 100
t_final = 0
m_final = 0
c_final = 0

for (numTrees in nTrees) {
  for (m in minSplit) {
    for (c in cpVals) {
      set.seed(100)
      numExamples <- nrow(dataIn)
      treeIndices <- seq(1, numTrees)
      treelist <- vector(mode="list", length=numTrees)
      predlist <- vector(mode="list", length=numTrees)
      # For bag-CART, sample with replacement and create trees
      for (tree in treeIndices) {
        sampleInd = sample.int(numExamples, size = 0.8*numExamples, replace = TRUE)
        subsample = dataIn[sampleInd,]
        
        #allInd = seq(1,numExamples)
        #testgroup = allInd[-sampleInd]
        #testInd = sample(testgroup, size = 0.2*numExamples, replace = FALSE)
        
        treelist[[tree]] <- rpart(ROI~., data = subsample, method = "anova", control=rpart.control(minsplit=2, cp=c, maxdepth=m))
        # prediction for this particular tree
        #predlist[[tree]] <- predict(treelist[[tree]], newdata = dataIn[testInd,], type = "vector")
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
      
      E_data = mean(dataIn[,1])
      MSE = sum((finalPrediction-E_data)^2)/numExamples
      MAE = sum(abs(finalPrediction-E_data))/numExamples
      # how well it will fit to future data -- closer to 1 is better
      R2 = 1 - sum((finalPrediction-dataIn[,1])^2)/sum((dataIn[,1]-E_data)^2)
      R2scores[R2index] = R2
      R2index = R2index + 1
      
      if (MSE < minMSE) {
      #if (R2 > maxR2) {
        t_final = numTrees
        m_final = m
        c_final = c
        maxR2 = R2
        minMSE = MSE
        
        # plot trees, save all of them
        for (tree in treeIndices) {
          png(paste('~/Research/Output/Latent Features/tree',tree,'.png',sep=''))
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
# Holdout set
holdoutSet <- read.csv("~/Research/Data/Movie Results (Cleaned) - Holdout Set - 44 - 2.csv")
#dataIn = dataIn[dataIn[,3]=="Comedy",] # choose specific genre
holdoutSet <- holdoutSet[,9:dim(holdoutSet)[2]-1] # Get rid of first col with movie names
# 
# hpc <- holdoutSet[,-1]
# res <- as.matrix(hpc) %*% topvecs
# res <- data.frame(holdoutSet[,1],res)
# holdoutSet = res

hgenreData <- read.csv("~/Research/Data/Movie Results (Cleaned) - Holdout Set Genres.csv")
hgenreData <- hgenreData[,-1] # Take out movie names
holdoutSet <- data.frame(holdoutSet,hgenreData)

hpredlist <- vector(mode="list", length=numTrees)
# For bag-CART, sample with replacement and create trees
for (tree in treeIndices) {
  # prediction for this particular tree
  hpredlist[[tree]] <- predict(treelist[[tree]], newdata = holdoutSet, type = "vector")
}

# do an element-wise addition of all the predictions
numHoldout = nrow(holdoutSet)
hsums = rep(0, numHoldout)
for (tree in treeIndices) {
  hsums = hsums + hpredlist[[tree]]
}
# Final predcted movie gross for bag-CART is the average prediction from
# all the trees in the ensemble.
hfinalPrediction = hsums / numTrees

# Random toy code for viewing trees
#printcp(treelist[[1]]) # display the results
#summary(treelist[[1]]) # detailed summary of splits

hE_data = mean(holdoutSet[,1])
hMSE = sum((hfinalPrediction-hE_data)^2)/numHoldout
hMAE = sum(abs(hfinalPrediction-hE_data))/numHoldout
# how well it will fit to future data -- closer to 1 is better
hR2 = 1 - sum((hfinalPrediction-holdoutSet[,1])^2)/sum((holdoutSet[,1]-hE_data)^2)

#########
#relevance index (this code is broken, get relevance index from the next one)
finaltreelist = table(data.frame(c(names(dataIn[-1]),"<leaf>")))
for (t in seq(1,length(treelist))) {
  treelength = length(treelist[[t]]$frame$var)
  for (v in seq(1,treelength)) {
    feature = as.character(treelist[[t]]$frame$var[v])
    finaltreelist[feature] = finaltreelist[feature] + 1
  }
}
 write.table(finaltreelist,file="~/Research/Data/relevanceindex.csv",sep=",")
# ###########
# # depth - use this
numcols = dim(dataIn)[2]
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
