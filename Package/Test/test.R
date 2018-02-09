
install.packages('devtools')
library(devtools)
devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("/Users/bmc/Desktop/madm")
create("madmr")
setwd("/Users/bmc/Desktop/madm/madmr")
# make changes
document()

getwd()
setwd("/Users/bmc/Desktop/madm/Package")
source("Functions/FileIO.R")
source("Functions/Algorithms.R")
source("Functions/Sensitivity.R")
source("Globals/DB_Globals.R")
#dm <- read.data.matrix("Data/maut_validate_benefits.csv", header=TRUE)
#dm <- read.data.matrix("Data/topsis_validate_benefits.csv", header=TRUE)
dm <- read.data.matrix("Data/topsis_validate.csv", header=TRUE)
dm
str(dm)
topRes <- TOPSIS(dm)
topRes$Results
mautRes <- MAUT(dm, scales=c("linear", "linear", "linear", "exponential", "exponential", "exponential", "linear"))
mautRes$Results
topRes$Results
FinalDB <- sensitivity(data=dm,
                       step=0.01,
                       algs="TOPSIS",
                       algParams=list(MAUT=list(scales=list("linear",
                                                            "linear",
                                                            "linear",
                                                            "exponential",
                                                            "exponential",
                                                            "exponential",
                                                            "linear"))),
                       plotLabels = TRUE,
                       window=c(0.1,0.5)
                       #algParams=list(MAUT=list(scales=list("linear", "linear", "exponential")))
                       ) #optional
FinalDB$Plot
#sensitivity <- function(data=c(), 
#                        algs=c(),
#                        algParams=c(),
#                        step=0.01,
#                        attr=c(), 
#                        splitPercentages="uniform", 
#                        verbose=FALSE){


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



