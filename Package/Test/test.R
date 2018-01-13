# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/


getwd()
setwd("/Users/bmc/Desktop/madm/Package")

source("Functions/FileIO.R")
source("Functions/Algorithms.R")

#dm <- read.data.matrix("Data/topsis_validate.csv", header=TRUE)
#topsisRes <-TOPSIS(dm)
#topsisRes$Results

#topsisRes

dm <- read.data.matrix("Data/maut_validate.csv", header=TRUE)
#mautRes <- MAUT(dm)
#mautRes$Results
#mautRes



FinalDB <- sensitivity(dm, verbose=FALSE)

write.csv(FinalDB,"test.csv")


