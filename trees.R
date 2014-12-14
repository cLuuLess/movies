library(rpart)
dataIn <- read.csv("~/Research/Data/Movie Results (Cleaned) - Final Entire Set - 0.005_saved.csv")
front <- dataIn[,1:8]
gross = dataIn[,5]
dataIn <- data.frame(dataIn,genreData, gross)
dataIn = dataIn[dataIn[,1]!="Parsed_Juno_g2.txt",] # Remove Juno
dataIn <- dataIn[,8:dim(dataIn)[2]] # Get rid of first col with movie names
#combined <- dataIn[,-seq(2,45)]
combined <- dataIn[,-seq(2,46)] # 44 features
#combined <- dataIn[,-seq(2,46)] # 44 features

####
output_feats <- read.csv("~/Research/Data/output_features_saved_44_nothresh.csv")
#output_feats2 <- read.csv("~/Research/Data/output_features_saved_44_nothresh_dim3.csv")
output_feats <- output_feats[,seq(-1,-44)]
#output_feats2 <- output_feats2[,seq(-1,-44)]
#dataIn <- data.frame(front,output_feats,output_feats2)
year <- front[,"Year"]
dataIn <- output_feats
dataIn <- data.frame(front,dataIn)
dataIn = dataIn[dataIn[,1]!="Parsed_Juno_g2.txt",]

dataIn = dataIn[dataIn[,"Year"]>=1995,]
dataIn <- dataIn[,8:dim(dataIn)[2]]
combined = dataIn
all = combined

####

dataIn <- combined[1:100,]
holdoutSet <- combined[101:110,]
# dataIn <- read.table("~/Research/Data/Movie Results (Cleaned) - New Movies.csv", header=TRUE, sep=",")
dataIn <- read.csv("~/Research/Data/Movie Results (Cleaned) - Final Training Set - 44 - 1st dim 2.csv")
#dataIn = dataIn[dataIn[,3]=="Comedy",]
#dataIn <- dataIn[,4:31] # with genres

# pc <- dataIn[,-1]
# pc_res <- princomp(pc)
# topvecs <- as.matrix(pc_res$loadings[,1:22])
# res <- as.matrix(pc) %*% topvecs
# res <- data.frame(dataIn[,1],res)
# names(res)[1]="ROI"
# dataIn = res

# Get genres
genreData <- read.csv("~/Research/Data/Movie Results (Cleaned) - Final Entire Genres.csv")
#genreData = genreData[genreData[,1]!="Juno",] # Remove Juno
genreData <- genreData[,-1] # Take out movie names
dataIn <- data.frame(dataIn,genreData)
# 
# nTrees <- c(30, 40, 50, 60, 70, 80, 100)
# minSplit <- c(4, 5, 6, 7, 8)
# cpVals <- c(0.1, 0.01, 0.001, 0.0001)
sampleInd = sample(1:120, size = trSize, replace = FALSE)
dataIn <- combined[sampleInd,]
holdoutSet <- combined[121:149,]
nTrees <- c(100)
minSplit <- c(5)
cpVals <- c(0.1)
R2scores <- rep(0, length(nTrees)*length(minSplit)*length(cpVals))
R2index = 1

maxR2 = -100
maxAcc = 0
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
        sampleInd2 = sample.int(numExamples, size = 0.5*numExamples, replace = TRUE)
        subsample = dataIn[sampleInd2,]
        
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
      MSE = sum((finalPrediction-dataIn[,1])^2)/numExamples
      MAE = sum(abs(finalPrediction-dataIn[,1]))/numExamples
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
hMSE = sum((hfinalPrediction-holdoutSet[,1])^2)/numHoldout
hMAE = sum(abs(hfinalPrediction-holdoutSet[,1]))/numHoldout
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

MSEs <- c()
MAEs <- c()
R2s <- c()
accs <- c()
maxR2 = -100
maxAcc = 0
minMSE = 100
bestT = 0
bestM = 0
bestC = 0
#trSizes = c(50,60,70,80,90,100,110,120)
#trSizes = c(0.2,0.3,0.4,0.5,0.6,0.7)
#nTrees <- c(40, 50, 60, 70, 80, 90, 100)
# minSplit <- c(4, 5, 6, 7)
# cpVals <- c(0.1, 0.01, 0.001)
nTrees <- c(50)
minSplit <- c(7)
cpVals <- c(0.001)
years <- c(1980, 1985, 1990, 1995, 2000, 2005)
#years <- c(1995)
trSize = 0.8*dim(combined)[1]
#num
#all = combined

for (m in minSplit) {
  for (c in cpVals) {
    MSEs <- c()
    MAEs <- c()
    R2s <- c()
    accs <- c()
    for (numTrees in nTrees) {
      for (y in years) {
        set.seed(150)
#         new = all[all[,"Year"]>=y,]
#         new = new[new[,"Year"]<y+10,]
#         combined <- new[,8:dim(new)[2]]
#         trSize = 0.8*dim(combined)[1]
        #sampleInd = sample(1:120, size = trSize, replace = FALSE)
        #dataIn <- combined[sampleInd,]
        dataIn <- combined[1:floor(trSize),]
        holdoutSet <- combined[floor(trSize+1):dim(combined)[1],]
        # Brieman bagged trees
        bag <- ipredbagg(dataIn[,1],dataIn[,-1], nbagg = numTrees, ns=0.5*trSize, control=rpart.control(minsplit=2, cp=c, maxdepth=m), coob=TRUE)
        hfinalPrediction = predict(bag, holdoutSet)
        # Adaboost
        #model <- gbm(ROI~., data=dataIn, distribution="gaussian", n.trees=200, bag.fraction=0.5)
        #hfinalPrediction = predict(model, holdoutSet, n.trees=100)
        hE_data = mean(holdoutSet[,1])
        hMSE = sum((hfinalPrediction-holdoutSet[,1])^2)/nrow(holdoutSet)
        hMAE = sum(abs(hfinalPrediction-holdoutSet[,1]))/nrow(holdoutSet)
        # how well it will fit to future data -- closer to 1 is better
        hR2 = 1 - sum((hfinalPrediction-holdoutSet[,1])^2)/sum((holdoutSet[,1]-hE_data)^2)
        MSEs <- c(MSEs, hMSE)
        MAEs <- c(MAEs, hMAE)
        R2s <- c(R2s, hR2)
        
        h1=hfinalPrediction>median(dataIn[,1])
        h2=holdoutSet[,1]>median(dataIn[,1])
        h=table(h1,h2)
        if (length(h) > 2) {
          acc=(h[1,1]+h[2,2])/sum(h)
        } else {
          acc=(h[,"TRUE"])/sum(h)
        }
        accs <- c(accs, acc)
        
        if (hMSE < minMSE) {
          maxAcc = acc
          maxR2 = hR2
          minMSE = hMSE
          bestT = numTrees
          bestM = m
          bestC = c
          maxR2s = R2s
          minMSEs = MSEs
          minMAEs = MAEs
        }
      }
    }
  }
}

plot(years,MSEs,type="l",xlab="Starting Year",ylab="MSE")
plot(trSizes,minMAEs,type="l",xlab="Number of Training Examples",ylab="MAE")
plot(trSizes,maxR2s,type="l",xlab="Number of Training Examples",ylab="R2s")

h1=hfinalPrediction>median(dataIn[,1])
h2=holdoutSet[,1]>median(dataIn[,1])
h=table(h1,h2)

######
# Portfolio Comparison
newfront = front[front[,1]!="Parsed_Juno_g2.txt",]
newfront = newfront[newfront[,"Year"]>=1995,]
newcombined = data.frame(newfront,combined[,-1])
newcombined = newcombined[newcombined[,"Year"]>=1995,]
hold = newcombined[(nrow(newcombined)-nrow(holdoutSet)+1):nrow(newcombined),]

portfolioSize = 18

rROIs = c()
for (y in 5:portfolioSize) {
  s=c()
  for (x in 1:1000) {
    samp=sample(1:nrow(hold), size=y, replace=FALSE)
    setSamples = hold[samp,"orig.ROI"]
    prod=sum(as.numeric(hold[samp,"Production.Budget"]))
    gros=sum(as.numeric(hold[samp,"Gross"]))
    cur=(0.55*gros-prod)/prod
    #s=c(s,sum(setSamples))
    s=c(s,cur)
  }
  rROIs=c(rROIs,mean(s))
}

predROIs=c()
sortedIdx = sort(hfinalPrediction,decreasing=TRUE,index.return=TRUE)
for (y in 5:portfolioSize) {
  portfolioROI = hold[sortedIdx$ix[1:y],"orig.ROI"]
  prod=sum(as.numeric(hold[sortedIdx$ix[1:y],"Production.Budget"]))
  gros=sum(as.numeric(hold[sortedIdx$ix[1:y],"Gross"]))
  cur=(0.55*gros-prod)/prod
  #predROIs=c(predROIs,sum(portfolioROI))
  predROIs=c(predROIs,cur)
}

rRating=hold[hold[,"Rating"]=="R",]
notRRating=hold[hold[,"Rating"]!="R",]
ratingROIs=c()
for (y in 5:portfolioSize) {
  s=c()
  for (x in 1:1000) {
    Rsamp=sample(1:nrow(rRating), size=0.6*y, replace=FALSE)
    notRsamp=sample(1:nrow(notRRating), size=0.4*y, replace=FALSE)
    prodR=sum(as.numeric(hold[Rsamp,"Production.Budget"]))
    grosR=sum(as.numeric(hold[Rsamp,"Gross"]))
    prodnotR=sum(as.numeric(hold[notRsamp,"Production.Budget"]))
    grosnotR=sum(as.numeric(hold[notRsamp,"Gross"]))
    
    prod=sum(prodR,prodnotR)
    gros=sum(grosR,grosnotR)
    cur=(0.55*gros-prod)/prod
    
    setSamples = c(rRating[Rsamp,"orig.ROI"],notRRating[notRsamp,"orig.ROI"])
    s=c(s,cur)
  }
  ratingROIs=c(ratingROIs,mean(s))
}

res=rbind(predROIs,rROIs,ratingROIs)
plot(0,0,xlim = c(5,portfolioSize),ylim = c(-0.4,0.4),type = "n",xlab="Number of movies in portfolio", ylab="Portfolio ROI")
title("Portfolio Comparison")
cl <- rainbow(3)
for (i in 1:3){
  lines(5:portfolioSize, res[i,],col = cl[i],type = 'b')
}
legend(12.5,0.4, c("Bag-CART selection","Random selection", "MPAA-based selection"), lty=c(1,1), col=cl, cex=0.8)
