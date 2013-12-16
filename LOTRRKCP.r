xtabloc <- ("~/Documents/Research/Movies/LOTRRK/xtab.txt");
caloc <- ("~/Documents/Research/ca.r");
clickloc <- ("~/Documents/Research/clink_hcluswtd.r");
pdfloc <- ("~/Documents/Research/RPlots/LOTRRK3.pdf");
tt <- ("Lord of the Rings Return of the King");

xtab <- read.table(xtabloc);
source(caloc);
xtabx <- ca(t(xtab));
source(clickloc);
xtabxh <- hierclust(xtabx$cproj);
xtabxhr <- reorder(as.dendrogram(xtabxh, hang=-1), 1:nrow(xtabx$cproj), agglo.FUN=min);

selection <- c(6, 12, 10, 101, 171, 489, 19, 17, 40, 77, 509, 86, 28);
selnames <- c("frodo", "gandalf", "sam", "ring", "bilbo", "boromir", "aragorn", "pippin", "merry", "gimli", "fellowship", "legolas", "gollum");
ytab <- xtab[,selection];
apply(ytab,1,sum);
source(caloc);
ytaby <- ca(t(ytab));


pdf(pdfloc, width = 80, height = 40);
plot(xtabxhr, cex = 2);
title(tt, sub = "Sorted Hierarchical Cluster", cex.main = 10, col.main = "blue", cex.sub = 7, col.sub = "sky blue");

plot(xtabxh, cex = 2);
title(tt, sub = "Unsorted Hierarchical Cluster", cex.main = 10, col.main = "blue", cex.sub = 7, col.sub = "sky blue");

plot(ytaby$cproj[,1],ytaby$cproj[,2],type="l",xlab="Factor 1",ylab="Factor 2", cex = 2);
text(ytaby$cproj[,1],ytaby$cproj[,2],1:203, cex = 2);
title(tt, sub = "Scatter Plot", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");
text(ytaby$rproj[,1],ytaby$rproj[,2],selnames,col="red", cex=2);
dev.off();




