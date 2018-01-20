# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

# TODO ...
# UNWRINKLE all parameters and what a user COULD put into it.
# Allow sensitivty to report edge cases where change occurs.
# Allow sensitivty to INCLUDE border cases
# Allow sensitivty to include window sizes (rather than just [step,1-step]
# Allow sensitivty to have split percentages

getwd()
setwd("/Users/bmc/Desktop/madm/Package/")
source("Functions/FileIO.R")
source("Functions/Algorithms.R")
source("Functions/Sensitivity.R")
#dm <- read.data.matrix("Data/maut_validate_benefits.csv", header=TRUE)
#dm <- read.data.matrix("Data/topsis_validate_benefits.csv", header=TRUE)
dm <- read.data.matrix("Data/maut_validate.csv", header=TRUE)
names(dm)
dm
FinalDB <- sensitivity(data=dm,
                       step=0.1,
                       window=0.1,
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
write.csv(FinalDB, "final2.csv")
# Trimmed
#trimmedDB <- subset(FinalDB, (FinalDB[,c(1:ncol(dm))] > step & FinalDB[,c(1:ncol(dm))] < (1-step)))
#trimmedDB <- trimmedDB[complete.cases(trimmedDB),]




###############################
### Example data for docs   ###
###############################
#DM <- data.frame(cost=as.numeric(runif(5,100, 200)),                                   #cost attribute, 100-200
#             productivity=as.numeric(abs(rnorm(5))),                               #benefit attribute,         abs(normalDist)
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
######################
### TOPSIS EXAMPLE ###
######################
#dm <- read.data.matrix("Data/topsis_validate.csv", header=TRUE)
#topsisRes <-TOPSIS(dm)
#topsisRes$Results

####################
### MAUT EXAMPLE ###
####################
#dm <- read.data.matrix("Data/maut_validate.csv", header=TRUE)
#mautRes <- MAUT(dm)
#mautRes$Results

###########################
### SENSITIVITY EXAMPLE ###
###########################
#dm <- read.data.matrix("Data/maut_validate.csv", header=TRUE)
#FinalDB <- sensitivity(data=dm) #exaustive
#FinalDB
#fdb <- sensitivity(data=dm,
#                   algs=c("MAUT"),
#                   algsParams=c(list(scales=c("linear","linear","exponential"))),
#                   verbose=TRUE) #MAUT exuastive only
#fdb <- sensitivity(data=dm,
#                   attr=c("Sq.Foot",)
#                   algs=c("TOPSIS"),
#                   algsParams=c(), #<- not needed, but kept for clarity
#                   verbose=TRUE) #MAUT exuastive only



