# FileIO.R

#' A function to transform a standard csv data matrix into the correct row column format.
#'
#' This function allows you to import a csv into the correct data.matrix format for the madm package
#'
#' @author Blake Conrad
#' @param path, an absolute path to the raw decision matrix in the 'read.data.matrix' format.
#' @param header, Boolean, default to true, will flag the read.csv function with whether header columns are included or not.
#' @keywords data.frame, read.csv.
#' @return data.matrix format of a data.frame
#' @examples
#' read.data.matrix("C:/Desktop")
#' 
#' Convert a standard import of:
#'
#'         name      x1  x2  x3Cost
#' 1       weight    0.2 0.2 0.6
#' 2 alternative1    0.5 0.5 0.5
#' 3 alternative2    0.5 0.5 0.5
#' 4 alternative3    0.5 0.5 0.5
#' 
#' OR
#' 
#'         name      x1  x2  x3
#'         benefit   +   +    -
#' 1       weight    0.2 0.2 0.6
#' 2 alternative1    0.5 0.5 0.5
#' 3 alternative2    0.5 0.5 0.5
#' 4 alternative3    0.5 0.5 0.5
#'
#'
#' into (with rownames instead of a rownames column):
#'
#'               x1  x2  x3Cost
#' weight       0.2 0.2 0.6
#' alternative1 0.5 0.5 0.5
#' alternative2 0.5 0.5 0.5
#' alternative3 0.5 0.5 0.5
#'
#' OR
#' 
#'                    x1  x2  x3
#'        benefit     +   +    -
#' 1       weight    0.2 0.2 0.6
#' 2 alternative1    0.5 0.5 0.5
#' 3 alternative2    0.5 0.5 0.5
#' 4 alternative3    0.5 0.5 0.5
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


flagDominated <- function(DM){
  
  #boolean vector for each alternative -- if true it is dominated, else false undominated
  dominated <- c(rep(TRUE,(nrow(DM)-1)))
  for (i in 2:(nrow(DM)-1)){
    for (j in 1:(nrow(DM)-1)){
      if(j != i){
        # Execute
        
        a<-0
        b<-0
        # for benefit attributes
        if(any(DM[i,] > DM[j,])){
          a <- FALSE  
        }
        
        # for cost attributes
        if(any(DM[i,] < DM[j,])){
          b <- FALSE  
        }
        
        if(a == FALSE && b == FALSE){
          dominated[i] <- FALSE
        }
      }
    }
  }
  
  return(DM[c(TRUE, !dominated),])
  # for each alternative
  #for each alternative except me
  # for each attribute
  # if i > someone return FALSE and break
  # if(any(DM["alternative_i", "attr_k"] > DM["alternative_j", "attr_k"])) altDomVect[i] <- FALSE
}
