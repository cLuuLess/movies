xtabloc <- ("~/Documents/Research/Movies/TheMatrixReloaded/xtab.txt");
caloc <- ("~/Documents/Research/ca.r");
clickloc <- ("~/Documents/Research/clink_hcluswtd.r");
pdfloc <- ("~/Documents/Research/RPlots/MatrixRe.pdf");
tt <- ("The Matrix Reloaded");

xtab <- read.table(xtabloc);
source(caloc);
xtabx <- ca(t(xtab));
source(clickloc);
xtabxh <- hierclust(xtabx$cproj);
xtabxhr <- reorder(as.dendrogram(xtabxh, hang=-1), 1:nrow(xtabx$cproj), agglo.FUN=min);

selection <- c(7, 9, 17, 23, 36, 38, 112, 466, 345, 510);
selnames <- c("neo", "morpheus", "smith", "trinity", "niobe", "matrix", "tank", "oracle", "machines", "anderson");
ytab <- xtab[,selection];
apply(ytab,1,sum);
source(caloc);
ytaby <- ca(t(ytab));


pdf(pdfloc, width = 60, height = 30);
plot(xtabxhr);
title(tt, sub = "Sorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");

plot(xtabxh);
title(tt, sub = "Unsorted Hierarchical Cluster", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");

plot(ytaby$cproj[,1],ytaby$cproj[,2],type="n",xlab="Factor 1",ylab="Factor 2");
text(ytaby$cproj[,1],ytaby$cproj[,2],1:85);
title(tt, sub = "Scatter Plot", cex.main = 8, col.main = "blue", cex.sub = 5, col.sub = "sky blue");
text(ytaby$rproj[,1],ytaby$rproj[,2],selnames,col="red");
dev.off();



