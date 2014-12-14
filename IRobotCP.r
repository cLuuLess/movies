xtabloc <- ("~/Research/Scripts/Parsed_I,-Robot_g2.txt/xtab.txt");
allwordsloc <- ("~/Research/Scripts/Parsed_I,-Robot_g2.txt/allwords.txt");
caloc <- ("~/Research/ca.r");
clickloc <- ("~/Research/clink_hcluswtd.r");
#pdfloc <- ("~/Documents/Research/Work/Story/FiveMovieTest/RPlots/RushHour2.pdf");
tt <- ("I Robot");

xtab <- read.table(xtabloc);
allwords <- read.table(allwordsloc);
source(caloc);
xtabx <- ca(t(xtab));
source(clickloc);
#xtabxh <- hierclust(xtabx$cproj);
#xtabxhr <- reorder(as.dendrogram(xtabxh, hang=-1), 1:nrow(xtabx$cproj), agglo.FUN=min);

selection <- c(2, 6, 4, 16, 45, 34, 423, 72, 165); 
selnames <- c("spooner", "calvin", "robot", "sonny", "victor", "hogenmiller", "susan", "robertson", "truck");
#selnames <- c("spooner", "calvin", "robot", "sonny", "robertson", "susan", "lance", "bergin", "lanning", "chin", "robot");
cutoffselection <- allwords[,2]>2
ytab <- xtab[,cutoffselection];
apply(ytab,1,sum);
source(caloc);
ytaby <- ca(t(ytab));
numscenes <- 163;

#pdf(pdfloc, width = 60, height = 30);
#plot(xtabxhr);
#title(tt, sub = "Sorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");

#plot(xtabxh);
#title(tt, sub = "Unsorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");

#plot(xtabx$cproj[,1], xtabx$cproj[,2], type="n", xlab="Factor 1", ylab= "Factor 2");
#text(xtabx$cproj[,1], xtabx$cproj[,2],1:115);
#source("~/Desktop/FiveMovieTest/plaxes.r");
#plaxes(xtabx$cproj[,1], xtabx$cproj[,2]);

#film has numscenes scenes
plot(ytaby$cproj[1:numscenes,1],ytaby$cproj[1:numscenes,2],xlab="Factor 1",ylab="Factor 2");
title(tt, sub = "Scatter Plot", cex.main = 3, col.main = "blue", cex.sub = 2, col.sub = "sky blue");
text(ytaby$rproj[selection,1],ytaby$rproj[selection,2],selnames,col="red");

plot(ytaby$cproj[1:numscenes,1],ytaby$cproj[1:numscenes,2],xlab="Factor 1",ylab="Factor 2");
title(tt, sub = "Scatter Plot", cex.main = 3, col.main = "blue", cex.sub = 2, col.sub = "sky blue");
text(ytaby$cproj[1:numscenes,1],ytaby$cproj[1:numscenes,2],1:numscenes);
text(ytaby$rproj[selection,1],ytaby$rproj[selection,2],selnames,col="red");

#dev.off();

library(rgl)

plot3d(ytaby$cproj[1:numscenes,1],ytaby$cproj[1:numscenes,2],ytaby$cproj[1:numscenes,3],xlab="Factor1",ylab="Factor2",zlab="Factor3");
text3d(ytaby$rproj[selection,1],ytaby$rproj[selection,2],ytaby$rproj[selection,3],selnames,col="red");
text3d(ytaby$cproj[1:numscenes,1],ytaby$cproj[1:numscenes,2],ytaby$cproj[,3],1:numscenes);

plot3d(type = "l",ytaby$cproj[1:numscenes,1],ytaby$cproj[1:numscenes,2],ytaby$cproj[1:numscenes,3],xlab="Factor1",ylab="Factor2",zlab="Factor3");
text3d(ytaby$cproj[1:numscenes,1],ytaby$cproj[1:numscenes,2],ytaby$cproj[1:numscenes,3],1:numscenes);
text3d(ytaby$rproj[selection,1],ytaby$rproj[selection,2],ytaby$rproj[selection,3],selnames,col="red");


#dimensions 2,3,4
plot(ytaby$cproj[1:numscenes,3],ytaby$cproj[1:numscenes,4],xlab="Factor 3",ylab="Factor 4");
title(tt, sub = "Scatter Plot D3-4", cex.main = 3, col.main = "blue", cex.sub = 2, col.sub = "sky blue");
text(ytaby$cproj[1:numscenes,3],ytaby$cproj[1:numscenes,4],1:numscenes);
text(ytaby$rproj[selection,1],ytaby$rproj[selection,2],selnames,col="red");

plot3d(type = "l",ytaby$cproj[1:numscenes,2],ytaby$cproj[1:numscenes,3],ytaby$cproj[1:numscenes,4],xlab="Factor2",ylab="Factor3",zlab="Factor4");
text3d(ytaby$rproj[selection,2],ytaby$rproj[selection,3],ytaby$rproj[selection,4],selnames,col="red");
text3d(ytaby$cproj[,2],ytaby$cproj[,3],ytaby$cproj[,4],1:numscenes);

#beat sheet
#x1 <- ytaby$cproj[1:numscenes,1]
x1 <- ytaby$cproj[1:numscenes,1]
plot(x1,type="l")
#x1f <- filter(x1, rep(1, 3), sides = 2)
x1f <- filter(x1, c(0.5, 1, 0.5), sides = 2)
x1f <- filter(x1, c(0.25, 0.5, 0.75, 1, 0.75, 0.5, 0.25), sides = 2)
plot(x1f,type="l")
title("Story Line dimension 1")
#bs2name <- c("Theme Stated","Catalyst","Break into 2","B Story","Midpoint","All Is Lost","Break into 3")
bs2sect <- c("T","C","R2","B","M","L","R3")
bs2page <- c(5,12,25,30,55,75,85)
text(bs2page*numscenes/110,x1f[bs2page*numscenes/110],bs2sect,col="red")
grid()
