fdafeatures <- function(x1, x1f, resvar_table, resvar_ratio, file7feats, file22feats, numscenes) {
#DO THIS PART:
# --------------
# setting the time index (scene numbers more precisely)
library('fda')
#full x1
fdatime = (1:length(x1))
#smoothed version of x1
#fdatime = (1:length(x1f))

#creating the basis functions
basis.range = range(fdatime);
bsbasis = create.bspline.basis(basis.range,22);
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

ii = NULL
for (i in seq(bs2page)){ii[i]=max(which(pagenum<bs2page[i]))}

bs2sect <- c("T","C","R2","B","M","L","R3")
bs2page <- c(5,12,25,30,55,75,85)
plot(x1f,type='l')
#new page order
text(ii,x1f[ii],bs2sect)
#compare to the old the old method
text(bs2page*numscenes/110,x1f[bs2page*numscenes/110],bs2sect,col="red")

#now to use it with the fda package, the old bs2basis should be replaced with the new ii
bs2basis <- ceiling(ii/110*22)
Xsel = Xmat[,bs2basis]
a = lm(x1f~Xsel)

#CONTINUE FROM HERE
#least square fit of x1 (or x1f) to the basis functions
a2 = lm(x1~Xmat) 
#a = lm(x1f~Xmat)

#IT WOULD HELP PLOTTING THESE GRAPHS FOR ALL THE FILMS FOR LATER REVIEW 
#plot(x1f/4) #division by 4 due to the filter coefficients that sums to 4
plot(x1,type='l',col='red')
lines(a$fitted)

#THESE ARE THE FEATURES TO BE PLUGGED INTO DECISION TREE
#the features should be the coefficients. do this for x1, x2, and possibly combined x1 & x2
a$coefficients
a2$coefficients
cat(a$coefficients,file=file7feats,sep='\n',append=TRUE)
cat(a$coefficients,file=file22feats,sep='\n',append=TRUE)

#KEEP A TABLE WITH THESE 2 NUMBERS FOR LATER REVIEW AS WELL
#in addition to using the coefficients in prediction tree pring out resvar and the ratio
#error
resvar = var(a$residuals)
resvar_table = c(resvar_table, resvar)
#ratio of residual variance and the x1 variance
resratio = resvar/var(x1)
resvar_ratio = c(resvar_ratio, resvar)
}