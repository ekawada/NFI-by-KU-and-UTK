# Read the very large data file we got from TRY.

trydata <- read.delim('./Data/trait data/457.txt')

meastable <- table(trydata$DataName)
write.table(meastable)

lifespandata <- trydata[union(grep('lifespan', tolower(trydata$DataName)),
                              grep('longevity', tolower(trydata$DataName))), ]
table1 <- table(lifespandata$DataName)
table1[table1>0]
plantlifespan <- lifespandata[grep('plant', tolower(lifespandata$DataName)),]
