xtabloc <- ("Movies/LOTRF/xtab.txt");
caloc <- ("ca.r");
clickloc <- ("clink_hcluswtd.r");
pdfloc <- ("RPlots/LOTRF_scatter.pdf");
tt <- ("Lord of the Rings Felowship of the Ring");

xtab <- read.table(xtabloc);
source(caloc);
xtabx <- ca(t(xtab));
source(clickloc);
xtabxh <- hierclust(xtabx$cproj);
xtabxhr <- reorder(as.dendrogram(xtabxh, hang=-1), 1:nrow(xtabx$cproj), agglo.FUN=min);

selection <- c(3, 6, 15, 16, 22, 23, 24, 28, 32, 64, 76, 80, 231);
selnames <- c("frodo", "gandalf", "sam", "ring", "bilbo", "boromir", "aragorn", "pippin", "merry", "gimli", "fellowship", "legolas", "gollum");
ytab <- xtab[,selection];
apply(ytab,1,sum);
source(caloc);
ytaby <- ca(t(ytab));


pdf(pdfloc, width = 80, height = 40);
plot(xtabxhr);
title(tt, sub = "Sorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");

plot(xtabxh);
title(tt, sub = "Unsorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");

plot(ytaby$cproj[,1],ytaby$cproj[,2],type="n",xlab="Factor 1",ylab="Factor 2");
# For lines, use the following
# plot(ytaby$cproj[,1],ytaby$cproj[,2],type="l",xlab="Factor 1",ylab="Factor 2");
text(ytaby$cproj[,1],ytaby$cproj[,2],1:147);
title(tt, sub = "Scatter Plot", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");
text(ytaby$rproj[,1],ytaby$rproj[,2],selnames,col="red");
dev.off();



