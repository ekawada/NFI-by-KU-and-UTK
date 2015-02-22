# Script to read all the raw dendro.

direc <- './Data/dendrochronology/tree ring datasets/'
filelist <- list.files(direc, pattern='*.rwl')
cfilelist <- list.files(direc, pattern='*.crn')

library(dplR)

rawfiles <- list()

for (i in 1:length(filelist)) {
  rawfiles[[i]] <- try(read.rwl(paste(direc,filelist[i],sep=''), format='auto'), TRUE)
}

load('~/rawtreefiles.r')

chronfiles <- list()

splitchronology <- function(filename) {
  x <- readLines(filename)
  tag <- substr(x, nchar(x) - 2, nchar(x))
  raw <- paste(x[tag == 'RAW'], collapse = '\n')
  std <- paste(x[tag == 'STD'], collapse = '\n')
  res <- paste(x[tag == 'RES'], collapse = '\n')
  ars <- paste(x[tag == 'ARS'], collapse = '\n')
  return(list(raw=raw, std=std, res=res, ars=ars))
}


for (i in 1:length(cfilelist)) {
  chronfiles[[i]] <- try(read.crn(paste(direc,cfilelist[i],sep='')), TRUE)
}

for (i in 1:length(cfilelist)) {
  if (class(chronfiles[[i]])[1] == 'try-error') {
    x <- splitchronology(paste(direc, cfilelist[i],sep=''))
    raw <- x$raw
    writeLines(raw, paste('~/R/dendro/',i,'.crn',sep=''))
    chronfiles[[i]] <- try(read.crn(paste('~/R/dendro/',i,'.crn',sep='')),TRUE)
  }
}

save(chronfiles, file=paste(direc,'chronologies.r',sep=''))

line1 <- list()
for (i in 1:length(cfilelist)) {
  line1[[i]] <- readLines(paste(direc,cfilelist[i],sep=''), n=1)
  }

chronout <- list()
for (i in 1:length(chronfiles)) {
  if (class(chronfiles[[i]])[1] != 'try-error') 
    chronout[[i]] <- data.frame(fileid=cfilelist[i], 
                                year=as.numeric(row.names(chronfiles[[i]])),
                                width=chronfiles[[i]][,1])
  else chronout[[i]] <- data.frame(fileid=NULL,year=NULL,width=NULL)
}

chronout <- do.call('rbind', chronout)
save(chronout, file='./Data/dendrochronology/masterchron.r')