movies <- c("Parsed_awalktoremember_g2.txt", "Parsed_Bruce-Almighty_g2.txt", "Parsed_Collateral-Damage_g2.txt",
            "Parsed_Final-Destination-2_g2.txt", "Parsed_Gladiator_g2.txt", "Parsed_Grudge,-The_g2.txt", "Parsed_Harold-and-Kumar-Go-to-White-Castle_g2.txt",
            "Parsed_hotel_rwanda_g2.txt", "Parsed_insomnia_g2.txt", "Parsed_I,-Robot_g2.txt", "Parsed_Land-of-the-Dead_g2.txt",
            "Parsed_Legally-Blonde_g2.txt", "Parsed_Manchurian-Candidate,-The_g2.txt", "Parsed_Titanic_g2.txt")
scenes <- c(224, 137, 188, 100, 86, 116, 138, 197, 165, 163, 152, 127, 150, 195)
numMovies <- seq(1,14)
numDims <- seq(1,3)

#movies <- c("Parsed_awalktoremember_g2.txt")
#scenes <- c(224)
#numMovies <- 1

for (dim in numDims) {
  for (m in numMovies) {
    
    xtabloc <- paste("~/Research/Scripts/", movies[m], "/xtab.txt", sep="");
    allwordsloc <- paste("~/Research/Scripts/", movies[m], "/allwords.txt", sep="");
    caloc <- ("~/Research/ca.r");
    clickloc <- ("~/Research/clink_hcluswtd.r");
    tt <- movies[m] #("Titanic");
    
    xtab <- read.table(xtabloc);
    allwords <- read.table(allwordsloc);
    source(caloc);
    xtabx <- ca(t(xtab));
    source(clickloc);
    
    wordcount = apply(xtab,2,sum);
    totalwords = sum(wordcount);
    cutoffpercent = 0.0009;
    wordcutoff = min(which(wordcount <= totalwords*cutoffpercent))
    wordsel <- c(1:wordcutoff); 
    wordsel2 <- which(xtabx$rcntr[1:wordcutoff,1:3] < mean(xtabx$rcntr[1:wordcutoff,1:3]) + 3*sd(xtabx$rcntr[1:wordcutoff,1:3]))
    ytab <- xtab[,wordsel2];
    
    #cutoffselection <- c(1:1000);
    #cutoffselection <- allwords[,2]>2;
    #ytab <- xtab[,cutoffselection];
    #ytab <- xtab
    apply(ytab,1,sum);
    source(caloc);
    ytaby <- ca(t(ytab));
    numscenes <- scenes[m];
    
    x1 <- ytaby$cproj[1:numscenes,dim]
    png(paste('~/Research/RPlots/Beat Sheets/Dimension ',dim,'/',movies[m],'.png',sep=''))
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
    dev.off()
  }
}
