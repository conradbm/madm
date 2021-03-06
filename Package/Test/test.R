# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/


setwd("C:/Users/1517766115.CIV/Desktop/Topics")
source("Functions/FileIO.R")
source("Functions/Algorithms.R")
source("Functions/Sensitivity.R")
dm <- read.data.matrix("Data/maut_validate.csv", header=TRUE)

names(dm)
FinalDB <- sensitivity(data=dm,
                       step=.1,
                       attr="Sq.Foot",
                       algs=c("TOPSIS", "MAUT"),
                       algParams=list(MAUT=list(scales=list("linear", "linear", "exponential"))),
                       verbose=FALSE) #optional

FinalDB
#sensitivity <- function(data=c(), 
#                        algs=c(),
#                        algParams=c(),
#                        step=0.01,
#                        attr=c(), 
#                        splitPercentages="uniform", 
#                        verbose=FALSE){

FinalDB
summary(FinalDB)
FinalDB[nrow(FinalDB),]
#trimmedDB <- subset(FinalDB, (FinalDB[,c(1:ncol(dm))] > step & FinalDB[,c(1:ncol(dm))] < (1-step)))
#trimmedDB <- trimmedDB[complete.cases(trimmedDB),]

#errors in not touching step, but touching (1-step).
#errors in some rows containing negative numbers.

#nrow(trimmedDB)
summary(FinalDB)
nrow(FinalDB)
#summary(trimmedDB)
write.csv(FinalDB, "final.csv")
#write.csv(trimmedDB, "trimmed.csv")
#FinalDB[89:92,c(1,2,3)] < 0.01
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

#dm <- read.data.matrix("Data/maut_validate.csv", header=TRUE)
#mautRes <- MAUT(dm)
#mautRes$Results
#mautRes
#dm <- read.data.matrix("Data/maut_validate.csv", header=TRUE)
#FinalDB <- sensitivity(data=dm, verbose=TRUE)
#FinalDB
#fdb <- sensitivity(data=dm, algs=c("MAUT", "ELECTRE"), algsParams=c(list(),list()), verbose=TRUE)
#fdb <- sensitivity(data=dm, attr=c("x1","x2"),algs=c("TOPSIS","MAUT"), algsParams=c(list(),list(scales=c("linear","logarithmic","exponential"))),verbose=TRUE)
#write.csv(FinalDB,"test.csv")



