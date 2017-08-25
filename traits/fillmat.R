#functions to do matrix interpolation
#Sept. 16, 2016, CC

fillnas <- function(mat) {
outmat <- mat #make a copy to hold the output
NAS <- which(is.na(mat)) #Identify indices of NA cells

# A function to generate replace values. I just invent function names inside        
replacevalue <- function(index){
  neigh <- findneighbors(index)
  mean(neigh, na.rm = T) # In case all surrounding values are NA, this will return NaN 
}

# implement findneighbours by inventing three new functions it would be nice to have
findneighbors <- function(index){
  indices <- allneighbors(index)
  indices <- removeoutsidematrix(indices)
  getvalues(indices)
}

# To get row and column sums from the index, we need these special matrices
xind <- row(mat) 
yind <- col(mat)

# implement the functions from inside findneighbors
allneighbors <- function(index){
  x <- xind[index]
  y <- yind[index]
  xs <- x + rep(-1:1,3)
  ys <- y + rep(-1:1, each = 3)
  cbind(xs, ys)
}

removeoutsidematrix <- function(indices){ 
  inside <- apply(indices, 1, isinside) #for each x and y of neighbours, check if they are inside the matrix
  indices[inside,] # and return only those
}

dims <- dim(mat) #find the limits
isinside <- function(x){ # the function that checks we are inside the matrix
  x[1] > 0 & x[1] <= dims[1] & x[2] > 0 & x[2] <= dims[2]
}

getvalues <- function(indices) #get a value from the indices
  apply(indices, 1, getval)

getval <- function(index)
  mat[index[1],index[2]]


# then we can do the operation with
for(i in NAS)
  outmat[i] <- replacevalue(i)
return(outmat) }

refmat<-data.frame(factor(c(1:9)))
colnames(refmat) <- 'bingrp'

getmats <- function(mat) {newm <- merge(mat, refmat, all.y=T)
newm<- arrange(newm, as.numeric(as.character(newm$bingrp)))
newerm<- matrix(newm[,3], nrow=3, ncol=3)
nextmat <- fillnas(newerm)
nicemat <- data.frame(refmat, c(nextmat))
colnames(nicemat) <- c("bingrp", "trait")
return(nicemat)
}