xtabloc <- ("~/Desktop/FiveMovieTest/Movies/RushHour2/xtab.txt");
caloc <- ("~/Desktop/FiveMovieTest/ca.r");
clickloc <- ("~/Desktop/FiveMovieTest/clink_hcluswtd.r");
#pdfloc <- ("~/Desktop/FiveMovieTest/RPlots/RushHour2.pdf");
tt <- ("Rush Hour 2");

xtab <- read.table(xtabloc);
source(caloc);
xtabx <- ca(t(xtab));
source(clickloc);
#xtabxh <- hierclust(xtabx$cproj);
#xtabxhr <- reorder(as.dendrogram(xtabxh, hang=-1), 1:nrow(xtabx$cproj), agglo.FUN=min);

selection <- c(2, 3, 18, 20, 28, 52, 134); # 251, 252);
selnames <- c("carter", "lee", "tan", "isabella", "hu-li", "reign", "sterling");# "naked", "tourist");
cutoffselection <- c(1:150); #, 660, 891);
ytab <- xtab[,cutoffselection];
apply(ytab,1,sum);
source(caloc);
ytaby <- ca(t(ytab));

#pdf(pdfloc, width = 60, height = 30);
#plot(xtabxhr);
#title(tt, sub = "Sorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");

#plot(xtabxh);
#title(tt, sub = "Unsorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");

#plot(xtabx$cproj[,1], xtabx$cproj[,2], type="n", xlab="Factor 1", ylab= "Factor 2");
#text(xtabx$cproj[,1], xtabx$cproj[,2],1:115);
#source("~/Desktop/FiveMovieTest/plaxes.r");
#plaxes(xtabx$cproj[,1], xtabx$cproj[,2]);

plot(ytaby$cproj[,1],ytaby$cproj[,2],type="l",xlab="Factor 1",ylab="Factor 2");
text(ytaby$cproj[,1],ytaby$cproj[,2],0:114);
title(tt, sub = "Scatter Plot", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");
text(ytaby$rproj[selection,1],ytaby$rproj[selection,2],selnames,col="red");
#dev.off();

library(rgl)
plot3d(type = "l",ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],xlab="Factor1",ylab="Factor2",zlab="Factor3");
text3d(ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],0:114);
text3d(ytaby$rproj[selection,1],ytaby$rproj[selection,2],ytaby$rproj[selection,3],selnames,col="red");

plot3d(ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],xlab="Factor1",ylab="Factor2",zlab="Factor3");
text3d(ytaby$rproj[selection,1],ytaby$rproj[selection,2],ytaby$rproj[selection,3],selnames,col="red");
text3d(ytaby$cproj[,1],ytaby$cproj[,2],ytaby$cproj[,3],0:114);

plot3d(ytaby$cproj[,2],ytaby$cproj[,3],ytaby$cproj[,4],xlab="Factor2",ylab="Factor3",zlab="Factor4");
text3d(ytaby$rproj[selection,2],ytaby$rproj[selection,3],ytaby$rproj[selection,4],selnames,col="red");
text3d(ytaby$cproj[,2],ytaby$cproj[,3],ytaby$cproj[,4],0:114);