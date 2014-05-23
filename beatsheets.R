library(fda)
filepath <- "~/Research/Holdout_set/"
movies <- list.dirs(path = filepath, full.names = FALSE, recursive = FALSE)
#scenes <- c(224, 137, 187, 100, 86, 116, 137, 197, 165, 163, 152, 126, 150, 195)
numMovies <- seq(1,length(movies))
#numDims <- seq(1,3)
numDims <- 1

#movies <- c("Parsed_Final-Destination-2_g2.txt")
#scenes <- c(224)
#numMovies <- 1

resvar_table = c()
resvar_ratio = c()

for (m in numMovies) {
  for (dim in numDims) {
    
    xtabloc <- paste(filepath, movies[m], "/xtab.txt", sep="");
    allwordsloc <- paste(filepath, movies[m], "/allwords.txt", sep="");
    caloc <- ("~/Research/ca.r");
    fdaloc <- ("~/Research/fdafeatures.R");
    clickloc <- ("~/Research/clink_hcluswtd.r");
    tt <- movies[m] #("Titanic");
    
    xtab <- read.table(xtabloc);
    allwords <- read.table(allwordsloc);
    source(caloc);
    source(fdaloc);
    xtabx <- ca(t(xtab));
    source(clickloc);
    
    wordcount = apply(xtab,2,sum);
    totalwords = sum(wordcount); # total words in the entire script
    cutoffpercent = 0.00099; #0.00099
    wordcutoff = min(which(wordcount <= totalwords*cutoffpercent))
    wordsel <- c(1:wordcutoff); 
    wordsel2 <- which(xtabx$rcntr[1:wordcutoff,1] < mean(xtabx$rcntr[1:wordcutoff,1]) + 3*sd(xtabx$rcntr[1:wordcutoff,1]))
    ytab <- xtab[,wordsel2];
    
    #cutoffselection <- c(1:1000);
    #cutoffselection <- allwords[,2]>2;
    #ytab <- xtab[,cutoffselection];
    #ytab <- xtab
    apply(ytab,1,sum);
    source(caloc);
    ytaby <- ca(t(ytab));
    numscenes <- dim(xtab)[1];
    
    # Beat sheet graph
    x1 <- ytaby$cproj[1:numscenes,dim]  
    #     png(paste('~/Research/RPlots/Beat Sheets/Dimension ',dim,'/',movies[m],'.png',sep=''))
    #     plot(x1,type="l")
    #     #x1f <- filter(x1, rep(1, 3), sides = 2)
    #     x1f <- filter(x1, c(0.5, 1, 0.5), sides = 2)
    x1f <- filter(x1, c(0.25, 0.5, 0.75, 1, 0.75, 0.5, 0.25), sides = 2)
    #     plot(x1f,type="l")
    #     title(tt)
    #     #bs2name <- c("Theme Stated","Catalyst","Break into 2","B Story","Midpoint","All Is Lost","Break into 3")
    #bs2sect <- c("T","C","R2","B","M","L","R3")
    bs2sect <- c("T","C","R2","M","L","R3")
    bs2page <- c(5,12,25,30,55,75,85)
    #     text(bs2page*numscenes/110,x1f[bs2page*numscenes/110],bs2sect,col="red")
    #     grid()
    #     dev.off()
    
    # Generate features
  }
  # fda features
  file6feats <- "~/Research/Data/6feats.csv"
  file22feats <- "~/Research/Data/22feats.csv"
  #fdafeatures(x1, x1f, file7feats, file22feats, resval_table, resval_ratio, numscenes)
  #DO THIS PART:
  # --------------
  # setting the time index (scene numbers more precisely)
  #library('fda')
  #full x1
  fdatime = (1:length(x1))
  numsplines = 44
  #smoothed version of x1
  #fdatime = (1:length(x1f))
  
  #creating the basis functions
  basis.range = range(fdatime);
  bsbasis = create.bspline.basis(basis.range,numsplines);
  #plot(bsbasis[1:5])
  Xmat = eval.basis(fdatime,bsbasis);
  
  #SKIP THIS PART - WE ARE GOING TO USE ALL 22
  #selecting a subset of basis functions to fit the beat-sheet predicted locations
  # bs2basis <- c(5,12,25,30,55,75,85)/110*22
  # Xsel = Xmat[,bs2basis]
  # a = lm(x1f~Xsel)
  
  # the program creates a new index ii to replace the old bs2page
  # it uses the wordcount from xtabx
  scenewords = apply(xtab,1,sum)
  cumwords = cumsum(scenewords)
  totalwords = sum(scenewords)
  k = totalwords/110;
  pagenum = cumwords/k
  
  ii = NULL # scene number 
  for (i in seq(bs2page)){
    ii[i]=max(which(pagenum<bs2page[i]))
    if(is.infinite(ii[i])) {ii[i] = 1}
    ii[i] = ii[i]+1 # fix the off by one error
  }
  
  bs2sect <- c("T","C","R2","B","M","L","R3")
  bs2page <- c(5,12,25,30,55,75,85)
#   png(paste('~/Research/RPlots/Beat Sheets/Adjusted/',movies[m],'_dim',dim,'.png',sep=''))
   plot(x1f,type='l')
#   #new page order
   text(ii,x1f[ii],bs2sect)
#   #compare to the old the old method
   text(bs2page*numscenes/110,x1f[bs2page*numscenes/110],bs2sect,col="red")
#   title(tt)
#   dev.off()
  
  # SELECT THE BEAT SHEET FEATURES
  #now to use it with the fda package, the old bs2basis should be replaced with the new ii
   bs2basis <- round(ii/numscenes*numsplines)
   # TODO might be off by one since first number is intercept
   Xsel = Xmat[,bs2basis]
   a = lm(x1f~Xsel)
  
  #CONTINUE FROM HERE
  #least square fit of x1 (or x1f) to the basis functions
  a2 = lm(x1~Xmat) 
#   a = lm(x1f~Xmat)
  
  #IT WOULD HELP PLOTTING THESE GRAPHS FOR ALL THE FILMS FOR LATER REVIEW 
  png(paste('~/Research/RPlots/Basis/',movies[m],'.png',sep=''))
#   plot(x1f,type='l',col='red') #division by 4 due to the filter coefficients that sums to 4
  plot(x1,type='l',col='red')
  title(tt)
  lines(a2$fitted)
#   lines(a$fitted)
  dev.off()
  
  #THESE ARE THE FEATURES TO BE PLUGGED INTO DECISION TREE
  #the features should be the coefficients. do this for x1, x2, and possibly combined x1 & x2
  #a$coefficients
  #a2$coefficients
  #cat(a$coefficients,file=file6feats,sep=',',append=TRUE)
  #cat("\n",file=file6feats,sep='',append=TRUE)
  cat(a2$coefficients,file=file22feats,sep='\t',append=TRUE)
  cat("\n",file=file22feats,sep='',append=TRUE)
  
  #KEEP A TABLE WITH THESE 2 NUMBERS FOR LATER REVIEW AS WELL
  #in addition to using the coefficients in prediction tree pring out resvar and the ratio
  #error
  resvar = var(a2$residuals)
  resvar_table = c(resvar_table, resvar)
  #ratio of residual variance and the x1 variance
  resratio = resvar/var(x1)
  resvar_ratio = c(resvar_ratio, resratio)

  # want the dist between all scenes
  #   png(paste('~/Research/RPlots/Beat Sheets/scene_dist/',movies[m],'_dim2.png',sep=''))
  #   s1 <- ytaby$cproj[1:numscenes-1,1:2]
  #   s2 <- ytaby$cproj[2:numscenes,1:2]
  #   euclDist <- sqrt(rowSums((s1-s2)^2))
  #   plot(euclDist, type="l")
  #   title(paste(movies[m],"2 Dimensions",sep='-'))
  #   bs2sect <- c("T","C","R2","B","M","L","R3")
  #   bs2page <- c(5,12,25,30,55,75,85)
  #   text(bs2page*numscenes/110,euclDist[bs2page*numscenes/110],bs2sect,col="red")
  #   grid()
  #   dev.off()
  #   
  #   png(paste('~/Research/RPlots/Beat Sheets/scene_dist/',movies[m],'_dim3.png',sep=''))
  #   s1 <- ytaby$cproj[1:numscenes-1,1:3]
  #   s2 <- ytaby$cproj[2:numscenes,1:3]
  #   euclDist <- sqrt(rowSums((s1-s2)^2))
  #   plot(euclDist, type="l")
  #   title(paste(movies[m],"3 Dimensions",sep='-'))
  #   text(bs2page*numscenes/110,euclDist[bs2page*numscenes/110],bs2sect,col="red")
  #   grid()
  #   dev.off()
  
}
cat(resvar_table,file="~/Research/Data/resvar.tsv",sep='\n')
cat(resvar_ratio,file="~/Research/Data/resrat.tsv",sep='\n')
