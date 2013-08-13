xtabloc <- ("~/Desktop/FiveMovieTest/Parsed/xtab.txt");
caloc <- ("~/Desktop/FiveMovieTest/ca.r");
clickloc <- ("~/Desktop/FiveMovieTest/clink_hcluswtd.r");
pdfloc <- ("~/Desktop/FiveMovieTest/RPlots/LOTRF.pdf");
tt <- ("Lord of  the Rings Felowship");

xtab <- read.table(xtabloc);
source(caloc);
xtabx <- ca(t(xtab));
source(clickloc);
xtabxh <- hierclust(xtabx$cproj);
xtabxhr <- reorder(as.dendrogram(xtabxh, hang=-1), 1:nrow(xtabx$cproj), agglo.FUN=min);


pdf(pdfloc, width = 60, height = 30);
plot(xtabxhr);#, horiz = TRUE);
title(tt);
dev.off();

l <- list(names(xtab));
for (kk in seq(along=l)) {
  name <- names(l)[kk];
  value <- l[[kk]];
  #str(xtabxhr, stem = l[[kk]]);
  #cat(value)
};
rownames(xtab) <- name;


l <- list(names(xtab),0);
for (kk in seq(along=l)) {
  rownames <- names(l)[kk];
  colnames <- l[[kk]];
};

xtabxhrw <-dendrapply(xtabxhr, rownames, colnames);


colLab <<- function(n){
    if(is.leaf(n)){
       a <- attributes(n)
       i <<- i+1
       attr(n, "label") <- c(a$label, list(lab.col = l[i]))
    }
    n
  } 
xtabxhrw <-dendrapply(xtabxhr, colLab);


#plot(xtabx$cproj[,1], xtabx$cproj[,2], type="n", xlab="Factor 1", ylab= "Factor 2");
#text(xtabx$cproj[,1], xtabx$cproj[,2],1:77);
#source("~/Desktop/TestingScenes/for-jo/plaxes.r");
#plaxes(xtabx$cproj[,1], xtabx$cproj[,2]);
#plot(xtabx$cproj[,1], xtabx$cproj[,2], type="n", xlab="Factor 1",ylab= "Factor 2") ;
#text(xtabx$cproj[,1], xtabx$cproj[,2], rep(".",77));
#text(xtabx$rproj[2,1], xtabx$rproj[2,2],"Rick") ;  
#text(xtabx$rproj[10,1], xtabx$rproj[10,2],"Ilsa") ; 
#plaxes(xtabx$cproj[,1], xtabx$cproj[,2]);

l <- list(names(xtab));
i <- seq(1,4198);
for (kk in seq(along=list)) {
  name <- names(l)[kk];
  i <- list(kk);
};

#xtab[1,1:100];
#selection <- seq(1,4197);
#selnames <- names(xtab);
selection <- c(3,13, 25, 34, 59);
selnames <- c("carson", "miranda", "lee", "ambassador", "shot");
ytab <- xtab[,selection];
#nrow(ytab); ncol(ytab);
apply(ytab,1,sum);
source(caloc);
ytaby <- ca(t(ytab));

pdf(pdfloc, width = 30, height = 30);
plot(ytaby$cproj[,1],ytaby$cproj[,2],type="n",xlab="Factor 1",ylab="Factor 2");
text(ytaby$cproj[,1],ytaby$cproj[,2],1:77);
title(tt);
#plaxes(ytaby$cproj[,1],ytaby$cproj[,2]);
text(ytaby$rproj[,1],ytaby$rproj[,2],selnames,col="red");
dev.off();



