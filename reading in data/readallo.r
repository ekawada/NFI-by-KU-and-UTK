# Read Allometry Files downloaded from GlobAllomeTree.org

DIR <- './Lit/Allometry/globallometree_files/'
flist <- paste(DIR, list.files(DIR), sep = '')
eqlist <- list()

for (i in flist[-12]) {
  eqlist[[length(eqlist)+1]] <- read.table(i, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
}

eqlist <- do.call('rbind', eqlist)

# Extract the equations and evaluate them, only using DBH-based allometries.
eqlist2 <- subset(eqlist, X=='DBH' | X=='DBH^2')
eqlist2 <- subset(eqlist2, Output!='Volume')

getallo <- function(n, L=eqlist2) {
  R <- L[n,]
  rangex <- as.numeric(c(R$Min_X, R$Max_X))
  if(any(is.na(rangex))) rangex <- c(0, 100)
  xs <- with(R, seq(rangex[1], rangex[2], by = 0.01))
  if (R$X == 'DBH^2') xs <- sqrt(xs)
  ys <- eval(parse(text = R$Substitute_equation), list(DBH=xs))
  return(list(n=n, x=xs, y=ys))
}

plotallo <- function(data, L=eqlist2) {
  R <- L[data$n,]
  plot(data$x, data$y, type = 'l', main = with(R, paste(Genus, Species, Country, '\n', Output, '=', Substitute_equation)))
}