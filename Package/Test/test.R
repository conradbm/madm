# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

getwd()
setwd("/Users/bmc/Desktop/madm/Package")

source("Functions/FileIO.R")
source("Functions/Algorithms.R")
source("Functions/Sensitivity.R")

###
### Example data if needed.
###
#DM <- data.frame(cost=as.numeric(runif(5,100, 200)),                                   #cost attribute, 100-200
#             productivity=as.numeric(abs(rnorm(5))),                               #benefit attribute, abs(normalDist)
#             location=as.numeric(floor(runif(5, 0, 5))),                           #benefit attribute, 0-5
#             row.names = sprintf("alternative_%d",seq(1:5))
#       )
#w <- data.frame(t(matrix(c(0.45, 0.35, 0.2))))
#names(w) <- names(DM)
#DM <- rbind(w,DM)
#row.names(DM)[1] = "weights"
#DM
###
###

#dm <- read.data.matrix("Data/topsis_validate.csv", header=TRUE)
#topsisRes <-TOPSIS(dm)
#topsisRes$Results

#topsisRes

dm <- read.data.matrix("Data/maut_validate.csv", header=TRUE)
#mautRes <- MAUT(dm)
#mautRes$Results
#mautRes

FinalDB <- sensitivity(data=dm, verbose=FALSE)
fdb <- sensitivity(data=dm, algs=c("MAUT", "ELECTRE"), algsParams=c(list(),list()), verbose=TRUE)
fdb <- sensitivity(data=dm, attr=c("x1","x2"),algs=c("TOPSIS","MAUT", "ELECTRE"), algsParams=c(list(normalize="vector"),list(scales=c("linear","logarithmic","exponential")),list(preferences=c(list(p1=2,q1=4), list(p2=50, q2=100)))), verbose=TRUE)
write.csv(FinalDB,"test.csv")


