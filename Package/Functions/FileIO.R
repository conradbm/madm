# FileIO.R

#' A function to transform a standard csv data matrix into the correct row column format.
#'
#' This function allows you to import a csv into the correct data.matrix format for the madm package
#'
#' @author Blake Conrad
#' @param path, header=TRUE 
#' @keywords data.frame, read.csv
#' @return data.matrix format of a data.frame
#' @examples
#' read.data.matrix("C:/Desktop")
#' 
#' Convert a standard import of:
#'
#'         name      x1  x2  x3
#' 1       weight    0.2 0.2 0.6
#' 2 alternative1    0.5 0.5 0.5
#' 3 alternative2    0.5 0.5 0.5
#' 4 alternative3    0.5 0.5 0.5
#'
#' into (with rownames instead of a rownames column):
#'
#'               x1  x2  x3
#' weight       0.2 0.2 0.6
#' alternative1 0.5 0.5 0.5
#' alternative2 0.5 0.5 0.5
#' alternative3 0.5 0.5 0.5
#'
#' 
#' Standard Decision Matrix Format (N+1)xD:
#'
#'               attribute1 attribute2 ... attributei ... attributeD
#' weight     
#' alternative1
#' alternative2
#' .
#' .
#' .
#' alternativeN
#'
#' 
#' 
#' 
read.data.matrix <- function(path, header=TRUE){
  
  if(path == ""){
    cat("No path provided. Please try again with another destination.\n")
  }
  
  dm <- read.csv(path, header=header)
  #allow a benefit row named: benefit, to contain + + - ect.. to represent benefit or cost
  row.names(dm) <- dm[,1]
  dm <- dm[,2:ncol(dm)]
  dm
}
