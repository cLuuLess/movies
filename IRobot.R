xtabloc <- ("~/Research/Parsed/xtab.txt");
caloc <- ("~/Research/ca.r");
clickloc <- ("~/Research/clink_hcluswtd.r");
pdfloc <- ("~/Research/RPlots/IRobot.pdf");
tt <- ("I, Robot");

xtab <- read.table(xtabloc);
source(caloc);
xtabx <- ca(t(xtab));
source(clickloc);

numScenes <- 163

# xtabxh <- hierclust(xtabx$cproj);
# xtabxhr <- reorder(as.dendrogram(xtabxh, hang=-1), 1:nrow(xtabx$cproj), agglo.FUN=min);

#selection <- c(2, 8, 12, 22, 18, 35, 72, 78, 206, 209, 230, 596, 919);
#selnames <- c("neo", "morpheus", "trinity", "smith", "tank", "cypher", "oracle", "matrix", "anderson", "dozer", "cops", "machines", "guard");
selection <- c(2, 8, 12, 22, 18, 35, 72, 78, 206, 209, 230);
selnames <- c("spooner", "robot", "sonny", "susan", "calvin", "lanning", "hogenmiller", "victor", "alfred");

# Analysis based on selected names only
ytab <- xtab[,selnames];

apply(ytab,1,sum);
source(caloc);
ytaby <- ca(t(ytab));

# pdf(pdfloc, width = 60, height = 30);
# plot(xtabxhr);
# title(tt, sub = "Sorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");
# 
# plot(xtabxh);
# title(tt, sub = "Unsorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");

plot(ytaby$cproj[1:numScenes,1],ytaby$cproj[1:numScenes,2],type="n",xlab="Factor 1",ylab="Factor 2");
title(tt, sub = "Scatter Plot", cex.main = 3, col.main = "blue", cex.sub = 2, col.sub = "sky blue");
#text(ytaby$rproj[,1],ytaby$rproj[,2],selnames,col="red");
text(ytaby$rproj[1:length(selection),1],ytaby$rproj[1:length(selection),2],selnames,col="red");

text(ytaby$cproj[1:numScenes,1],ytaby$cproj[1:numScenes,2],rep(".",numScenes));
#text(ytaby$cproj[1:217,1],ytaby$cproj[1:217,2],1:217);
#dev.off();

library(rgl)
#plot3d(type = "l",ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],xlab="Factor1",ylab="Factor2",zlab="Factor3");
plot3d(ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],xlab="Factor1",ylab="Factor2",zlab="Factor3");
#text3d(ytaby$rproj[,1],ytaby$rproj[,2],ytaby$rproj[,3],selnames,col="red");
text3d(ytaby$rproj[1:length(selection),1],ytaby$rproj[1:length(selection),2],ytaby$rproj[1:length(selection),3],selnames,col="red");
#text3d(ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],1:217);


# Full Analysis
# using first 1069 words..

cutoffselection <- c(1:1069); #, 660, 891);
ytab <- xtab[,cutoffselection];

apply(ytab,1,sum);
source(caloc);
ytaby <- ca(t(ytab));


# pdf(pdfloc, width = 60, height = 30);
# plot(xtabxhr);
# title(tt, sub = "Sorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");
# 
# plot(xtabxh);
# title(tt, sub = "Unsorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");

#Dimensions 1,2 
plot(ytaby$cproj[1:numScenes,1],ytaby$cproj[1:numScenes,2],type="n",xlab="Factor 1",ylab="Factor 2");
title(tt, sub = "Scatter Plot", cex.main = 3, col.main = "blue", cex.sub = 2, col.sub = "sky blue");
#text(ytaby$rproj[,1],ytaby$rproj[,2],selnames,col="red");
text(ytaby$rproj[selection,1],ytaby$rproj[selection,2],selnames,col="red");
text(ytaby$cproj[1:numScenes,1],ytaby$cproj[1:numScenes,2],rep(".",numScenes));

#Dimensions 1,3 
plot(ytaby$cproj[1:numScenes,1],ytaby$cproj[1:numScenes,3],type="n",xlab="Factor 1",ylab="Factor 3");
title(tt, sub = "Scatter Plot", cex.main = 3, col.main = "blue", cex.sub = 2, col.sub = "sky blue");
#text(ytaby$rproj[,1],ytaby$rproj[,2],selnames,col="red");
text(ytaby$rproj[selection,1],ytaby$rproj[selection,3],selnames,col="red");
text(ytaby$cproj[1:numScenes,1],ytaby$cproj[1:numScenes,3],rep(".",numScenes));

#Dimensions 2,3 
plot(ytaby$cproj[1:numScenes,2],ytaby$cproj[1:numScenes,3],type="n",xlab="Factor 2",ylab="Factor 3");
title(tt, sub = "Scatter Plot", cex.main = 3, col.main = "blue", cex.sub = 2, col.sub = "sky blue");
#text(ytaby$rproj[,1],ytaby$rproj[,2],selnames,col="red");
text(ytaby$rproj[selection,2],ytaby$rproj[selection,3],selnames,col="red");
text(ytaby$cproj[1:numScenes,2],ytaby$cproj[1:numScenes,3],rep(".",numScenes));
#dev.off();

library(rgl)
#plot3d(type = "l",ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],xlab="Factor1",ylab="Factor2",zlab="Factor3");
plot3d(ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],xlab="Factor1",ylab="Factor2",zlab="Factor3");
#text3d(ytaby$rproj[,1],ytaby$rproj[,2],ytaby$rproj[,3],selnames,col="red");
text3d(ytaby$rproj[selection,1],ytaby$rproj[selection,2],ytaby$rproj[selection,3],selnames,col="red");
#text3d(ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],1:numScenes);

plot3d(ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],xlab="Factor1",ylab="Factor2",zlab="Factor3");
#text3d(ytaby$rproj[,1],ytaby$rproj[,2],ytaby$rproj[,3],selnames,col="red");
text3d(ytaby$rproj[selection,1],ytaby$rproj[selection,2],ytaby$rproj[selection,3],selnames,col="red");
text3d(ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],1:numScenes);

plot3d(ytaby$cproj[,2],ytaby$cproj[,3],ytaby$cproj[,4],xlab="Factor2",ylab="Factor3",zlab="Factor4");
#text3d(ytaby$rproj[,1],ytaby$rproj[,2],ytaby$rproj[,3],selnames,col="red");
text3d(ytaby$rproj[selection,2],ytaby$rproj[selection,3],ytaby$rproj[selection,4],selnames,col="red");
#text3d(ytaby$cproj[,2],ytaby$cproj[,3],ytaby$cproj[,4],1:numScenes);

x1 <- ytaby$cproj[1:numScenes,1]
plot(x1,type="l")
#x1f <- filter(x1, rep(1, 3), sides = 2)
x1f <- filter(x1, c(0.5, 1, 0.5), sides = 2)
x1f <- filter(x1, c(0.25, 0.5, 0.75, 1, 0.75, 0.5, 0.25), sides = 2)
plot(x1f,type="l")
title("Story Line dimension 1")
#bs2name <- c("Theme Stated","Catalyst","Break into 2","B Story","Midpoint","All Is Lost","Break into 3")
bs2sect <- c("T","C","R2","B","M","L","R3")
bs2page <- c(5,12,25,30,55,75,85)
text(bs2page*211/110,x1f[bs2page*211/110],bs2sect,col="red")
grid()
#pretty()
#axTicks(1)
#text(1:217,x1f,1:217);
peaks1 = c(6,13,16,20,27,44,48,52,58,62,74,79,85,88,94,98,106,116,123,126,134,138,144,149,150,154,158,160,166,173,178,182,186,192,200,211)
text(peaks1,x1f[peaks1],peaks1);

x2 <- ytaby$cproj[1:numScenes,2]
#plot(x2,type="l")
x2f <- filter(x2, c(0.25, 0.5, 0.75, 1, 0.75, 0.5, 0.25), sides = 2)
plot(x2f,type="l")
title("Story Line dimension 2")
#bs2name <- c("Theme Stated","Catalyst","Break into 2","B Story","Midpoint","All Is Lost","Break into 3")
bs2sect <- c("T","C","R2","B","M","L","R3")
bs2page <- c(5,12,25,30,55,75,85)
text(bs2page*211/110,x2f[bs2page*211/110],bs2sect,col="red")
grid()
#text(1:217,x2f,1:217);
peaks2 = c(12,16,25,28,36,40,48,52,58,63,79,86,93,98,109,114,123,125,128,134,141,144,150,157,161,167,170,175,179,191,195,200,numScenes)
text(peaks2,x2f[peaks2],peaks2);

# x3 <- ytaby$cproj[1:217,3] SHLOMO'S with old one
x3 <- ytaby$cproj[1:numScenes,3]
plot(x3,type="l")
x3f <- filter(x3, c(0.25, 0.5, 0.75, 1, 0.75, 0.5, 0.25), sides = 2)
plot(x3f,type="l")
title("Story Line dimension 3")
#bs2name <- c("Theme Stated","Catalyst","Break into 2","B Story","Midpoint","All Is Lost","Break into 3")
bs2sect <- c("T","C","R2","B","M","L","R3")
bs2page <- c(5,12,25,30,55,75,85)
text(bs2page*211/110,x3f[bs2page*211/110],bs2sect)
grid()
#text(1:217,x3f,1:217);
peaks3 = c(16,28,50,62,69,94,98,105,113,121,133,141,148,150,154,162,177,199,207)
text(peaks3,x3f[peaks3],peaks3);

#xmat <- ytaby$cproj[1:217,1:2]
xmat <- cbind(x1f,x2f,x3f)
xrownorm <- sqrt(apply(xmat^2,1,sum))
xmatn <- xmat/(xrownorm[row(xmat)])
#dxvec = diff(xvec)+0.0000001
#image(xmatn)
Smat <- xmatn%*%t(xmatn)
### SHLOMO'S OLD
#image(1:217,1:217,Smat,xlab = "Scene number", ylab = "Scene number")
#text(bs2page/110*217,bs2page/110*217,bs2sect)
image(1:numScenes,1:numScenes,Smat,xlab = "Scene number", ylab = "Scene number")
text(bs2page/110*numScenes,bs2page/110*numScenes,bs2sect)
title("The Matrix (new) Self-Similarity using dimensions 1:3")
#text(bs2page/110,bs2page/110+0.02,bs2sect)
#text(bs2page/110,bs2page/110,"+")
#image(log(abs(1-Smat)+0.01))
#image(log(1-abs(Smat)+0.01))

xmat <- cbind(x1f,x2f)
xrownorm <- sqrt(apply(xmat^2,1,sum))
xmatn <- xmat/(xrownorm[row(xmat)])
#dxvec = diff(xvec)+0.0000001
#image(xmatn)
Smat <- xmatn%*%t(xmatn)
## SHLOMO'S OLD
#image(1:217,1:217,Smat,xlab = "Scene number", ylab = "Scene number")
#text(bs2page/110*217,bs2page/110*217,bs2sect)
image(1:numScenes,1:numScenes,Smat,xlab = "Scene number", ylab = "Scene number")
text(bs2page/110*numScenes,bs2page/110*numScenes,bs2sect)
title("The Matrix (new) Self-Similarity using dimensions 1:2")


# http://stats.stackexchange.com/questions/8605/column-wise-matrix-normalization-in-r
# This is what sweep and scale are for.
# 
# sweep(m, 2, colSums(m), FUN="/")
# scale(m, center=FALSE, scale=colSums(m))
# 
# Alternatively, you could use recycling, but you have to transpose it twice.
# 
# t(t(m)/colSums(m))
# 
# Or you could construct the full matrix you want to divide by, like you did in your question. Here's another way you might do that.
# 
# m/colSums(m)[col(m)]
