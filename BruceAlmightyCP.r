xtabloc <- ("~/Research/Scripts/Parsed_Bruce-Almighty_g2.txt/xtab.txt");
caloc <- ("~/Research/ca.r");
clickloc <- ("~/Research/clink_hcluswtd.r");
tt <- ("Bruce Almighty");

xtab <- read.table(xtabloc);
source(caloc);
xtabx <- ca(t(xtab));
source(clickloc);

cutoffselection <- c(1:1000);
ytab <- xtab[,cutoffselection];
apply(ytab,1,sum);
source(caloc);
ytaby <- ca(t(ytab));
numscenes <- 137;

x1 <- ytaby$cproj[1:numscenes,1]
plot(x1,type="l")
#x1f <- filter(x1, rep(1, 3), sides = 2)
x1f <- filter(x1, c(0.5, 1, 0.5), sides = 2)
x1f <- filter(x1, c(0.25, 0.5, 0.75, 1, 0.75, 0.5, 0.25), sides = 2)
plot(x1f,type="l")
title(tt)
#bs2name <- c("Theme Stated","Catalyst","Break into 2","B Story","Midpoint","All Is Lost","Break into 3")
bs2sect <- c("T","C","R2","B","M","L","R3")
bs2page <- c(5,12,25,30,55,75,85)
text(bs2page*numscenes/110,x1f[bs2page*numscenes/110],bs2sect,col="red")
grid()