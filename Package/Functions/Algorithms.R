# Algorithms.R

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

TOPSIS <- function(DM){
  
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

MAUT <- function(DM, scales=c()){
  
  # Scales not supplied sufficiently
  if(length(scales)==0 || length(scales) < ncol(DM)){
      cat("No scales were provided or provided incompletely. Setting default scaling to linear.\n")
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
      }
      else if(scales[i] == "exponential"){
        # Handle exponential case
        # exp(val^2)+1/1.7889 -- risky attribute is more benefitial
        
        DM[j,i] <- ((exp(DM[j,i]^2)-1)/1.718282)
      }
      else if (scales[i] == "logarithmic"){
        # Handle logarithmic case
        # log(val) -- risk averse is more benefitial
        DM[j,i] <- log(DM[j,i])
      }
      else{
        cat("Incorrect cost/benefit function specified.\nPlease specify the scales parameter to this function per the documentation.\n")
      }
    }
    
  }
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


#' A function to perform the sensitivity on a data.frame in read.data.matrix format.
#' The spirit behind this function is to interpretively take in the given parameters
#' of a specified read.data.matrix style data.frame and populate a database.
#' From this database, the user can pivot, plot, and reshape for appropriate analysis
#' in deciding what different weighting schemes might have looked like for the decision maker.
#' This function is meant to scale between various methods as well as attributes.
#' Custom split separation is available for each sensitivity step.
#' Custom step sizes are also available to allow the user to do multiple sensitivity
#' runs and determine what fidelity the sensitivty is needed to make an impact on the 
#' overarching decision. 
#'
#'
#' @author Blake Conrad
#' @param data, read.data.matrix(data.frame), data.frame in data matrix format.
#' @param step, automatically set to 0.01 is the steps you want an attribute to climb up or down.
#' @param attr, automatically set to c() will trigger a full sensitivity accross all attributes.
#'  If a list of valid attribuets are provided, the sensitivity will only trigger these attributes
#'  in the sensitivity and reflect them only in the returned database.
#' @param splitPercentages, automatically set to uniform. This means as you incrementally increase and
#' decrease an attributes weight value, the percetnage given to each other attribute is equal. An example would
#' be with 3 attributes, we toggle attribute2 down from 0.3 to 0.2. So we lost 0.1 from attribute2, so we will take
#' 0.1/(3-1) to give us 0.1/2==0.05, therefore attribuet1 and attribute2 will increase by 0.05 when attribute2 goes down 0.1.
#' The generalization to this is step/(1-N) in the algorithm. 
#' @param verbose, default to FALSE. As with most linux command line applications, this will give you a highly detailed picture of what type of iterations are taking place under the hood.
#' @example FinalDB <- sensitivity(dm, verbose=FALSE)
sensitivity <- function(data=DM, step=0.01, attr=c(), splitPercentages="uniform", verbose=FALSE){
  
  FinalDB = data.frame()
  cat("step size: ", step, "\n")
  cat("percentageSplit: ", splitPercentages, "\n")
  
  if(is.vector(attr) || is.list(attr)){
    if(length(attr) == 0){
      cat("attr left empty will default to examine all attributes.\n")
    }
    else{
      cat("attributes focus: ", attr, "\n")
    }
  }
  
  if(splitPercentages == "uniform"){
    cat("default uniform splitting for weights of all non-specified attributes will be applied. The total number of attributes supplied is ", length(names(DM)), " meaning ", length(names(DM))-1, " attributes can acquire the split percentages, therefore " ,1/(length(names(DM))-1),"% will be split amongst each on every step through the sensitivity analysis.","\n")
    
    cat("Default splits assumed.\n")
    FinalDB <- default_sensitivity(DM, DB, step)
  }
  else{
    cat("Custom splits were decided at: ", splitPercentages, "\n")
    FinalDB <- custom_sensitivity(DM, DB, step, attr, splitPercentages)
  }
  return(FinalDB)
}






# Helper for sensitivity -- bug, only goes to 0.02, not including the 0.01 case
decreaseAttribute <- function(DMCopyj, attr_j, step, split_step){
  
  # Update just the target attributes weight -> 0
  DMCopyj["weight",attr_j] <- DMCopyj["weight",attr_j] - step
  
  # Update every other attribute's weight in the DM
  DMCopyj["weight",][which(names(DMCopyj) != attr_j)] <- DMCopyj["weight",][which(names(DMCopyj) != attr_j)]+split_step
  return(DMCopyj)
}

# Helper for sensitivity -- bug, only goes to 0.98, not including the 0.99 case
increaseAttribute <- function(DMCopyj, attr_j, step, split_step){
  
  # Update just the target attributes weight -> 0
  DMCopyj["weight",attr_j] <- DMCopyj["weight",attr_j] + step
  
  # Update every other attribute's weight in the DM
  DMCopyj["weight",][which(names(DMCopyj) != attr_j)] <- DMCopyj["weight",][which(names(DMCopyj) != attr_j)]-split_step
  return(DMCopyj)
}

# Helper for sensitivity
updateDB <- function(DMCopy, DB, alg, iterid){
  # Constants to store
  weights <- as.list(t(DMCopy["weight",]))
  names(weights) <- row.names(t(DMCopy["weight",]))
  
  # Run the algorithm
  res <- TOPSIS(DMCopy)
  
  # Store algorithm results
  alts <- res$Results[,"Alternative"]#get alternatives
  ranks <- res$Results[,"Rank"]#get ranks
  
  container <- data.frame(weights, alts, ranks)
  container$alg <- alg
  container$attr_i <- attr_i
  container$iterid <- iterid
  
  # Store in the database
  DB <- rbind(DB, container)
  return(DB)
}

# Helper for sensitivity
default_sensitivity <- function(DM, DB, step=0.01){ 
  cat("Default sensitivity beginning ... \n")
  
  # Constants
  algs   <- c("TOPSIS", "MAUT")
  iterid <-1
  algid  <-1
  N      <- length(names(DM))
  splits <- step/(N-1)
  DMCopy <- DM       # For updating the midpoint upward, we need to reset.
  DB     <- data.frame() # Database to store our results
  
  for(alg in algs){
    cat("Running method: ", alg, ".\n")
    
    for(attr_i in names(DMCopy)){
      cat("Focus attribute: ", attr_i,"\n")
      cat("*** Analyzing from ", DMCopy["weight", attr_i], " to ",step,".*** \n")
      cat("Current weight...\t")
      
      while(DMCopy["weight",attr_i] >= step){
        cat(DMCopy["weight",attr_i],"\t")
        
        if(alg == "TOPSIS"){
          # Store the results of TOPSIS
          DB <- updateDB(DMCopy, DB, alg, iterid)
          # Update the DM
          DMCopy <- decreaseAttribute(DMCopy, attr_i, step, split_step)
          # Iterate for the next index
          iterid<-iterid+1
          
        } #endif
        else if(alg == "MAUT"){
          # Store the results of MAUT  
          DB <- updateDB(DMCopy, DB, alg, iterid)
          # Update the DM
          DMCopy <- decreaseAttribute(DMCopy, attr_i, step, split_step)
          # Iterate for the next index
          iterid<-iterid+1
          
        } #endelseif
      } #endwhile
      cat("\n")
      DMCopy <- DM
      
      cat("*** Analyzing from ", DMCopy["weight", attr_i], " to ",(1-step),".*** \n")
      cat("Current weight...\t")
      
      while(DMCopy["weight",attr_i] <= (1-step)){
        cat(DMCopy["weight",attr_i],"\t")
        
        if(alg == "TOPSIS"){
          # Store the results of TOPSIS
          DB <- updateDB(DMCopy, DB, alg, iterid)
          # Update the DM
          DMCopy <- increaseAttribute(DMCopy, attr_i, step, split_step)
          # Iterate for the next index
          iterid<-iterid+1
          
        } #endif
        else if(alg == "MAUT"){
          # Store the results of MAUT  
          DB <- updateDB(DMCopy, DB, alg, iterid)
          # Update the DM
          DMCopy <- increaseAttribute(DMCopy, attr_i, step, split_step)
          # Iterate for the next index
          iterid<-iterid+1
          
        } #endelseif
      } #endwhile
      cat("\n")
      DMCopy <- DM
    }#endforattri
    
  }#endforalg
  cat("Default sensitivity finished.\n")
  return(DB)
}#enddefault_sensitivity

# Helper for sensitivity
custom_sensitivity <- function(DM, DB, step=0.01, attr, splitPercentages){
  
  cat("Custom sensitivity beginning... ")
  
}