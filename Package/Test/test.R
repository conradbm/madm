# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/


setwd("C:/Users/1517766115.CIV/Desktop/Topics")
source("functions/FileIO.R")
source("functions/Algorithms.R")

dm <- read.data.matrix("data/topsis_validate.csv", header=TRUE)
topsisRes <-TOPSIS(dm)
topsisRes

dm <- read.data.matrix("data/maut_validate.csv", header=TRUE)
mautRes <- MAUT(dm, scales=c("linear","linear","linear"))
mautRes


library(ggplot2)
ggplot(data=topsisRes[[1]], aes(x=Alternative,y=Rank,color=as.factor(Rank))) + geom_point()
ggplot(data=mautRes[[1]], aes(x=Alternative,y=Rank,color=as.factor(Rank))) + geom_point()
