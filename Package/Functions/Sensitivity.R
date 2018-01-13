#Sensitivity.R
source("Functions/Algorithms.R")

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
sensitivity <- function(data=c(), step=0.01, attr=c(), splitPercentages="uniform", verbose=FALSE){
  
  DM <- data
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