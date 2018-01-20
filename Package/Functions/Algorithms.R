# Algorithms.R


#' A function to perform the TOPSIS algorithm on a data.frame in read.data.matrix format
#'
#' This function allows you to import a csv into the correct data.matrix format for the madm package
#' @author Blake Conrad
#' @
#' @param data.frame in read.data.matrix format
#' @keywords data.frame, ranking
#' @return list
#' @examples
#' TOPSIS(read.data.matrix("C:/Desktop"))
#' TOPSIS(DM)
#' 
#' @details More theory on the following: https://github.com/conradbm/madm/blob/master/Examples/SAW_and_Topsis.xls
#' 
#'
#'
#' 

TOPSIS <- function(data=DM, algParams=c(), verbose=FALSE){
  
  DM <- data
  #AltIdxStart <- (which(row.names(DM)=="weight")+1)
  
  #print(algParams)
  #Sys.sleep(100000)
  
  #if(verbose) cat("Starting the TOPSIS algorithm.\n")
  
  
  # Sanitize parameters from sensitivity function
  if(length(algParams) != 0){
    if("TOPSIS" %in% names(algParams)){
      # ... Detemrine if specific parameter exists ...
      # ... Set specific parameter here ...
    }
  }
  
  weightVectorNormalize <- function(DM){
    VNDM <- DM
    for (i in 1:ncol(DM)){
      
      denom <- sqrt(sum(DM[,i]^2))
      
      for(j in 2:length(DM[,i])){
        
        VNDM[j,i] <- DM[1,i]*((DM[j,i])/denom)
      }
    }
    return(VNDM)
  }
  
  VNDM <- weightVectorNormalize(DM)
  #VNDM
  
  PNI <- function(DM){
    s <- data.frame(row.names=FALSE)
    for (name in names(DM)){
      p<-0
      n<-0
      if(grepl("cost", name) || grepl("Cost", name) || grepl("COST", name)){
        # min is better
        p <- min(DM[2:nrow(DM),name])         #remember to ignore the weight row
        n <- max(DM[2:nrow(DM),name])         #remember to ignore the weight row
        #cat(name, "neg:",n,"\tpos:",p,"\n")
      }
      else{
        # max is better
        p <- max(DM[2:nrow(DM),name])         #remember to ignore the weight row
        n <- min(DM[2:nrow(DM),name])         #remember to ignore the weight row
        #cat(name, "neg:",n,"\tpos:",p,"\n")
      }
      
      s <- rbind(s,t(data.frame(c(name, p, n))))
    }
    
    # Structure labeling
    names(s) <- c("Name", "PositiveIdeal", "NegativeIdeal")
    row.names(s) <- names(DM)
    
    # Structure & Convert data
    d <- data.frame(t(s[,2:ncol(s)]))
    d2 <- data.frame(apply(d, 2, as.double))
    
    # Re-structure labeling
    names(d2) <- names(d)
    row.names(d2) <- row.names(d)
    
    return(d2)
  }
  ideals <- PNI(VNDM)
  #ideals
  
  distanceFromIdeals <- function(ideals, DM){
    SPlus <- c()
    SMinus <- c()
    for(i in 2:nrow(DM)){
      SPlus[i] <- sqrt(sum((DM[i,] - ideals[1,])^2))
      SMinus[i] <- sqrt(sum((DM[i,] - ideals[2,])^2))
    }
    return (data.frame(SPlus, SMinus))
  }
  
  #if(verbose) cat("Finished the TOPSIS algorithm.\n")
  
  S <- distanceFromIdeals(ideals, VNDM)
  row.names(S) <- row.names(VNDM)
  #S
  
  CStar <- data.frame(S[,2] / (S[,1]+S[,2]))
  names(CStar) <- "C"
  row.names(CStar) <- row.names(VNDM)
  
  # Next order them by descending, then return
  retDf <- CStar
  retDf$Rank <- rank(-retDf$C)
  retDf$Alternative <- row.names(retDf)
  row.names(retDf) <- NULL
  retDf$Rank <- as.factor(retDf$Rank)
  retDf <- retDf[2:nrow(retDf),c("Alternative", "C","Rank")]
  
  # Return as much as we collected
  retList <- list(retDf, CStar, S, ideals, VNDM, DM)
  names(retList) <- c("Results","CStar","S","ideals","VNDM","DM")
  
  return(retList)
}



#' A function to perform the MAUT algorithm on a data.frame in read.data.matrix format
#'
#' This function allows you to import a csv into the correct data.matrix format for the madm package
#' @author Blake Conrad
#' @
#' @param read.data.matrix(data.frame), c()
#' @param scales, default to c(), if scales supplied and supplied correctly, each attribute will get the appropriate scaling measure (linear, exponential, or logarithmic). Defaults to all linear scales if none supplied.
#' @param algParams, default to c(), when the sensitivity function calls this function the user can pass specific algorithm parameters. These have a sanitiy check and then append in the correct parameter if correctly supplied. 
#' @keywords data.frame, ranking
#' @return list
#' @examples
#' MAUT(read.data.matrix("C:/Desktop"), scales=c()) #all linear scales
#' MAUT(read.data.matrix("C:/Desktop"), scales=c("linear", "exponential", "logarithmic"))
#' 
#' @details More theory on the following: 1. https://github.com/conradbm/madm/blob/master/Examples/MAUT.xls
#' 2. https://github.com/conradbm/madm/blob/master/Examples/SAW_and_Topsis.xls
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

MAUT <- function(data=DM, algParams=c(), scales=c(), verbose=FALSE){
  
  DM <- data
  
  #print(algParams)
  #print(typeof(algParams))
  #print("scales" %in% algParams$MAUT)
  #Sys.sleep(1)
  
  #if(verbose) cat("Starting the MAUT algorithm.\n")
  
  # Sanitize parameters from the sensitivity function
  if(length(algParams) != 0){
    if("MAUT" %in% names(algParams)){
      # ... Detemrine if specific parameter exists ...
      if("scales" %in% names(algParams$MAUT)){
        # ... Set specific parameter here ...
        if(verbose) cat("Setting scales to: ",algParams$MAUT$scales,".\n")
        scales <- algParams$MAUT$scales
      }
      else{
        if(verbose) cat("Invalid scales structure.\n")
      }
    }
    else{
      if(verbose) cat("Invalid method structure.\n")
    }
  }
  else{
    if(verbose) cat("No algParams provided.\n")
  }
  
  
  # Scales not supplied sufficiently
  if(length(scales)==0 || length(scales) < ncol(DM)){
      if(verbose) cat("No scales were provided or provided incompletely. Setting default scaling to linear.\n")
      scales = rep("linear", ncol(DM))
  }


  for(i in 1:ncol(DM)){
    # Normalize each column to the following:
      # benefit attributes: ((val-min)/(max-min))
      # cost attributes: ((min-val)/(max- min))+1
    
    
    tmin <- min(DM[2:nrow(DM),i])
    tmax <- max(DM[2:nrow(DM),i])
    
    tnames <- names(DM)
    for(j in 2:nrow(DM)){
      
      # Cost attributes
      if(grepl("cost", tnames[i]) || grepl("Cost", tnames[i]) || grepl("COST", tnames[i])){
        DM[j,i] <-  ((tmin-DM[j,i])/(tmax-tmin))+1
      }
      # Benefit attributes
      else{
        
        DM[j,i] <-  ((DM[j,i] - tmin)/(tmax-tmin))
      }
      

      
      # Handle scaling for diverse attributes appropriately
      if(scales[i] == "linear"){
        # Handle linear case -- indifferent to risk
        #
        # Already scaled, no action needed.
        #
        #
        #cat("linear\t")
      }
      else if(scales[i] == "exponential"){
        # Handle exponential case
        # exp(val^2)+1/1.7889 -- risky attribute is more benefitial
        
        DM[j,i] <- ((exp(DM[j,i]^2)-1)/1.718282)
        #cat("exp\t")
      }
      else if (scales[i] == "logarithmic"){
        # Handle logarithmic case
        # log(val) -- risk averse is more benefitial
        DM[j,i] <- log(DM[j,i])
        #cat("log\t")
      }
      else{
        cat("Incorrect function. Defaulting to linear.\n")
      }
    }
    
  }
  
  #if(verbose) cat("Finished the MAUT algorithm.\n")
  
  normalizedDM <- DM
  # SAW -- Sum each column based on its unique 'Utility' to provide a final utility score.
  # sum each column after scaling each by their columns weights -- sumproduct
     # result = Dx2 data.frame to show an alternative, its score, and its rank.

  tscores <- c()
  tnames <- c()
  for(j in 2:nrow(DM)){
    tscores <- c(tscores, sum(DM[1,]*DM[j,]))
    tnames <- c(tnames, row.names(DM)[j])
    #cat(sum(DM[1,]*DM[j,]),"\n")
    #cat(row.names(DM)[j],"\n")
  }
  trank <- rank(-tscores)
  
  retDf <- data.frame(Alternative=tnames,
                      Score=tscores,
                      Rank=trank)
  
  retList <- list(retDf, DM, scales)
  names(retList) <- c("Results","DM","scales")
  
  return(retList)
}

###
### Validated? No
###
### For more theory visit: 
### 1. https://github.com/conradbm/madm/blob/master/Examples/ELECTRE.xlsx
###
ELECTRE <- function(DM){
  
}

###
### Validated? No
###
### For more theory visit: 
### 1. https://github.com/conradbm/madm/blob/master/Examples/PROMETHEE.xlsx
###
PROMETHEE <- function(DM){
  
}

