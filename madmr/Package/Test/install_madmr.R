# Script to install madmR on any machine.
.libPaths("U:/Documents/R/Packages")
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage('devtools')
library(devtools)
setwd( "M:/AD/Professional Development/Conrad/madmR")
devtools::load_all()
devtools::install()
library(madmR)

