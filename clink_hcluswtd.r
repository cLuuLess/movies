# Update Aug 29, 2008, to return clusmat, sequence nos. of cluster 
# memberships at each level.  

#  CLINK - FM 2007/3/23   WORKS FINE. 

getnns <- function(diss, flag) {
# Inputs.  diss: full distance matrix.
#          flag: "live" rows indicated by 1 are to be processed.
# Returns. List of: nn, nndiss.
#          nn:   list of nearest neighbor of each row.
#          nndiss: nearest neigbbor distance of each row.
# FM, 2003/11/16


   nn <- rep(0, nrow(diss))
   nndiss <- rep(0.0, nrow(diss))
   MAXVAL <- 1.0e12
   if (nrow(diss) != ncol(diss)) stop("Invalid input first parameter.")
   if (nrow(diss) != length(flag)) stop("Invalid inputs 1st/2nd parameters.")
 # if (nrow(diss) != length(nn)) stop("Invalid inputs 1st/3rd parameters.")
 # if (nrow(diss) != length(nndiss)) stop("Invalid inputs 1st/4th parameters.")

   for (i1 in 1:nrow(diss)) {
       if (flag[i1]) {

          minobs <- -1
          mindis <- MAXVAL

#          for (i2 in 1:ncol(diss)) {
#              NEW IS: flag[i2] == 1 CONDINTION
#              if ( (flag[i2] == 1) && (diss[i1,i2] < mindis) && (i1 != i2) ) {
#                 mindis <- diss[i1,i2]
#                 minobs <- i2
#              }
#          }
#          nn[i1] <- minobs
#          nndiss[i1] <- mindis

#         Just get closest neighbor from below info
          mindislo <- 1e5
          minobslo <- 0
          if (i1 > 1) {
             for (i2 in 1:(i1-1)) {
                 if (flag[i2]) {
                     mindislo <- diss[i1,i2]
                    minobslo <- i2
                 }
             }
          }

#         Just get closest neighbor from above info
          mindishi <- 1e5
          minobshi <- 0
          if (i1 < ncol(diss)) {
             for (i2 in ncol(diss):(i1+1)) {
                 if (flag[i2]) {
                     mindishi <- diss[i1,i2]
                    minobshi <- i2
                 }
             }
          }

          if (mindislo < mindishi) {
             nn[i1] <- minobslo
             nndiss[i1] <- mindislo
          }
          if (mindislo >= mindishi) {
             nn[i1] <- minobshi
             nndiss[i1] <- mindishi
          }


       }
   }
   list(nn = nn, nndiss = nndiss)
}

hierclust <- function(a) {

   MAXVAL <- 1.0e12

   n <- nrow(a)                               
   diss <- as.matrix(dist(a, diag=T, upper=T)) # calculate all dissims
   flag <- rep(T, n)                          # active/dead indicator
   a <- rep(0, n-1)                           # left subnode on clustering
   b <- rep(0, n-1)                           # right subnode on clustering
   ia <- rep(0, n-1)                          # R-compatible version of a
   ib <- rep(0, n-1)                          # R-compatible version of b
   lev <- rep(0, n-1)                         # level or criterion values
   card <- rep(1, n)                          # cardinalities
   order <- rep(0, n)                         # R-compatible order for plotting

   nnsnnsdiss <- getnns(diss, flag)           # call to function getnns
   clusmat <- matrix(0, n, n)                 # cluster memberships
   for (i in 1:n) clusmat[i,n] <- i           # init. trivial partition

   for (ncl in (n-1):1) {                      # main loop 
       # check for agglomerable pair
       minobs <- -1;  
       mindis <- MAXVAL;
       for (i in 1:n) {
           if (flag[i]) {
              if (nnsnnsdiss$nndiss[i] < mindis) {
                  mindis <- nnsnnsdiss$nndiss[i]
                  minobs <- i
              }
           }
       }
       # find agglomerands clus1 and clus2, with former < latter
       if (minobs < nnsnnsdiss$nn[minobs]) {
          clus1 <- minobs
          clus2 <- nnsnnsdiss$nn[minobs]
       }
       if (minobs > nnsnnsdiss$nn[minobs]) {
          clus2 <- minobs
          clus1 <- nnsnnsdiss$nn[minobs]
       }
       # So, agglomeration of pair clus1 < clus2 defines cluster ncl

       #------------------------------------ Block for subnode labels 
       a[ncl] <- clus1                       # aine, or left child node
       b[ncl] <- clus2                       # benjamin, or right child node
       # Now build up ia, ib as version of a, b which is R-compliant
       if (card[clus1] == 1) ia[ncl] <- (-clus1)     # singleton
       if (card[clus2] == 1) ib[ncl] <- (-clus2)     # singleton
       if (card[clus1] > 1) {                # left child is non-singleton
          lastind <- 0
          for (i2 in (n-1):(ncl+1)) {        # Must have n-1 >= ncl+1 here
              if (a[i2] == clus1) lastind <- i2    # Only concerns a[i2]
          }
          ia[ncl] <- n - lastind             # label of non-singleton
       }
       if (card[clus2] > 1) {                # right child is non-singleton
          lastind <- 0
          for (i2 in (n-1):(ncl+1)) {        # Must have n-1 >= ncl+1 here
              if (a[i2] == clus2) lastind <- i2    # Can only concern a[i2]
          }
          ib[ncl] <- n - lastind             # label of non-singleton
       }
       if (ia[ncl] > 0 || ib[ncl] > 0) {     # Check that left < right
          left <- min(ia[ncl],ib[ncl])
          right <- max(ia[ncl],ib[ncl])
          ia[ncl] <- left                    # Just get left < right
          ib[ncl] <- right
       }
       #--------------------------------------------------------------------

       lev[ncl] <- mindis
       for (i in 1:n) {
           clusmat[i,ncl] <- clusmat[i,ncl+1]
           if (clusmat[i,ncl] == clus2) clusmat[i,ncl] <- clus1
       }
       # Next we need to update diss array
       for (i in 1:n) {
           if ( (i != clus1) && (i != clus2) && (flag[i]) ) {
              diss[clus1,i] <- 
                  max( diss[clus1,i], diss[clus2,i] )
#                  Identical to:
#                  0.5*diss[clus1,i] + 0.5*diss[clus2,i] +
#                  0.5*abs(diss[clus1,i] - diss[clus2,i])
              diss[i, clus1] <- diss[clus1, i]
           }
       }
       card[clus1] <- card[clus1] + card[clus2]    # Update card of new cluster
       # Cluster label clus2 is knocked out; following not nec. but no harm
       flag[clus2] <- F
       nnsnnsdiss$nndiss[clus2] <- MAXVAL
       for (i in 1:n) {
           diss[clus2,i] <- MAXVAL
           diss[i,clus2] <- diss[clus2,i]
       }
       # Finally update nnsnnsdiss$nn and nnsnnsdiss$nndiss
       # i.e. nearest neighbors and the nearest neigh. dissimilarity
       nnsnnsdiss <- getnns(diss, flag)
   }

   temp <- cbind(a,b)
   merge2 <- temp[nrow(temp):1, ]
   temp <- cbind(ia,ib)
   merge <- temp[nrow(temp):1,]
   dimnames(merge) <- NULL
   # merge is R-compliant; later suppress merge2


   # Following helps but very little really
   for (k in 1:nrow(temp)) {
       if ( abs(merge[k,1]) > abs(merge[k,2]) ) {
          # switch
          verytemp <- merge[k,1]
          merge[k,1] <- merge[k,2]
          merge[k,2] <- verytemp
       }
   }
    
   #-------------------------------- Build R-compatible order from ia, ib
   orderlist <- c(merge[n-1,1], merge[n-1,2])
#cat("start", orderlist, "\n")
   norderlist <- 2
   for (i in 1:(n-2)) {           # For precisely n-2 further node expansions
       for (i2 in 1:norderlist) {       # Scan orderlist
           if (orderlist[i2] > 0) {     # Non-singleton to be expanded
              tobeexp <- orderlist[i2]
              if (i2 == 1) {
                 orderlist <- c(merge[tobeexp,1],merge[tobeexp,2],
                                orderlist[2:norderlist])
#cat(i2, orderlist, "\n")
              }
              if (i2 == norderlist) {
                 orderlist <- c(orderlist[1:(norderlist-1)],
                                merge[tobeexp,1],merge[tobeexp,2])
#cat(i2, orderlist, "\n")
              }
              if (i2 > 1 && i2 < norderlist) {
                 orderlist <- c(orderlist[1:(i2-1)], 
                                merge[tobeexp,1],merge[tobeexp,2],
                                orderlist[(i2+1):norderlist])
#cat(i2, orderlist, "\n")
              }
              norderlist <- length(orderlist)
#cat("end", orderlist, "\n")
           }
        }
   }
   orderlist <- (-orderlist)
   class(orderlist) <- "integer"
   
   xcall <- "hierclust(a)"
   class(xcall) <- "call"
   #clusmat=clusmat
   #labels=as.character(1:n)

   clusmat2 <- clusmat
   # clusmat has:
   # col. n    sequence nos. in input sequence of terminals 
   # col. n-1  first agglomeration indicated 
   # ...
   # col. 1   all agglomerations carried out; all n obs. have clusterid = 1
   for (j in 2:(n-1)) {
       onecol <- clusmat[,j]
       uniqelts <- unique(onecol)
       for (i in 1:n) {
           for (k in 1:length(uniqelts)) {  # Do more efficiently later
               if (clusmat[i,j] == uniqelts[k]) clusmat2[i,j] <- k
           }
       }
    }
   clusmat <- clusmat2
 

   retlist <- list(merge=merge,height=as.single(lev[(n-1):1]),order=orderlist,
         labels=dimnames(a)[[1]],method="c-complete",call=xcall,
         dist.method="euclidean-sqd")
   # VERIFIED AUG 2008: including clusmat does not hinder plotting of 
   # dendrogram with named components: merge, height, order
   retlist <- list(merge=merge,height=lev[(n-1):1],order=orderlist,clusmat=clusmat)
   class(retlist) <- "hclust"
   retlist
}
