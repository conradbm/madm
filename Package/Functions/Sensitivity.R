#Sensitivity.R

#' 
#' 
#' Import libraries
#' 
#' 
source("Functions/Algorithms.R")


#' 
#' 
#' 
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
#' 
#' 
#' Consider adding a StepRange=c(0.1:0.5, by=0.1) to run this for multiple steps
#' 
sensitivity <- function(data=c(), 
                        algs=c(),
                        algParams=c(),
                        step=0.01,
                        attr=c(), 
                        splitPercentages="uniform", 
                        verbose=FALSE){
  
  # Disable scientific notation
  #options(scipen=999)
  # Enable scientific notation
  #options(scipen=9)
  
  DM <- data
  FinalDB = data.frame()

  
  if(verbose) cat("step size: ", step, "\n")
  if(verbose) cat("percentageSplit: ", splitPercentages, "\n")
  
  if(is.vector(attr) || is.list(attr)){
    if(length(attr) == 0){
      if(verbose) cat("attr left empty will default to examine all attributes.\n")
    }
    else{
      if(verbose) cat("attributes focus: ", attr, "\n")
    }
  }
  
  if(splitPercentages == "uniform"){
    if(verbose) cat("default uniform splitting for weights of all non-specified attributes will be applied. The total number of attributes supplied is ", length(names(DM)), " meaning ", length(names(DM))-1, " attributes can acquire the split percentages, therefore " ,1/(length(names(DM))-1),"% will be split amongst each on every step through the sensitivity analysis.","\n")
    
    cat("Default splits assumed.\n")
    FinalDB <- default_sensitivity(DM, DB, algs, algParams, step, verbose)
    
    # Data scrubbing -- remove all invalid rows
    trimmedDB <- subset(FinalDB, (FinalDB[,c(1:ncol(DM))] > step & FinalDB[,c(1:ncol(DM))] < (1-step)))
    FinalDB <- trimmedDB[complete.cases(trimmedDB),]
    
  }
  else{
    cat("Custom splits were decided at: ", splitPercentages, "\n")
    cat("Non-uniform split percentages is not yet supported in this package. Try a later version for this functionality.\n")
    FinalDB <- custom_sensitivity(DM, DB, algs, algParams, step, attr, splitPercentages, verbose)
  }
  return(FinalDB)
}






#' Helper for sensitivity
#'
#'
#'
#'
#'
#'
#'
#'
default_sensitivity <- function(DM, 
                                DB,
                                algs,
                                algParams,
                                step,
                                verbose){ 
  
  
  cat("Default sensitivity beginning. \n")
  
  # If no algorithm specified, do all!
  if(length(algs)==0) algs <- c("TOPSIS", "MAUT")
  
  # Constants for DB & calculations
  iterid <-1
  algid  <-1
  N      <- length(names(DM))
  split_step <- step/(N-1)
  DB     <- data.frame() # Database to store our results
  
  # Keep a fresh copy of the DM from points: attrVal->step
  DMCopy <- DM
  
  for(alg in algs){
    
    if(verbose) cat("Running method: ", alg, ".\n")
    
    for(attr_i in names(DMCopy)){
      
      cat(". ")
      
      # For debugging
      if(verbose) cat("Focus attribute: ", attr_i,"\n")
      if(verbose) cat("*** Analyzing from ", DMCopy["weight", attr_i], " to ",step,".*** \n")
      if(verbose) cat("Current weight...\t")
      
      # Original run on standard weights, i.e., there will be 1 duplicate for each attribute
      DB <- updateDB(DMCopy, DB, alg, algParams, attr_i, iterid, verbose)
      
      while(DMCopy["weight",attr_i] > 0){
        
        if(verbose) cat(DMCopy["weight",attr_i],"\t")
        
        # Update the DM
        DMCopy <- decreaseAttribute(DMCopy, attr_i, step, split_step)
        
        # Store the results of alg
        DB <- updateDB(DMCopy, DB, alg, algParams, attr_i, iterid, verbose)
       
        
        # update iter
        iterid<-iterid+1
      } #endwhile
      
      # For debugging
      if(verbose) cat("\n")
      
      # Keep a fresh copy of the DM from points: attrVal->(1-step)
      DMCopy <- DM
      
      # If next step is valid, while will engage
      DMCopy <- increaseAttribute(DMCopy, attr_i, step, split_step)
      
      # For debugging
      if(verbose) cat("*** Analyzing from ", DMCopy["weight", attr_i], " to ",(1-step),".*** \n")
      if(verbose) cat("Current weight...\t")
      
      while(DMCopy["weight",attr_i] < 1){
        
        if(verbose) cat(DMCopy["weight",attr_i],"\t")
        
        # Store the results of alg
        DB <- updateDB(DMCopy, DB, alg, algParams, attr_i, iterid, verbose)
        
        # If next step is valid, while will engage
        DMCopy <- increaseAttribute(DMCopy, attr_i, step, split_step)
        
        # update iter
        iterid<-iterid+1
        
      } #endwhile

      
      # For debugging
      if(verbose) cat("\n")
      
      # Reset to original weights for the next attribute.
      DMCopy <- DM
      
    }#endforattri
    
  }#endforalg
  
  cat("\n")
  cat("Default sensitivity finished.\n")
  
  return(DB)
}#enddefault_sensitivity


#'
#'
#'
#' Helper function for the default_sensitivity(...) and custom_sensitivity(...) function
#' 
#' 
#' @author Blake Conrad
#' @param DMCopyj, read.data.matrix(data.frame), data.frame in data matrix format.
#' @param attr, charater, a single specified attribute within DMCopyj
#' @param step, double, is the steps you want an attribute that is being decreased.
#' @param split_step, double, give a number of attributes in a decision matrix the formula for this is: step/(1-N), this is meant to be 
#' incrementally added to an attributes weight during the sensitivity database building phase.
#' @return DMCopyj, read.data.matrix(data.frame), This is an updated version of the input DMCopyj which has adjusted weights now from the decrease in step and addition from split_step.
#' 
#' 
#' 
decreaseAttribute <- function(DMCopyj,
                              attr_j,
                              step, 
                              split_step){
  
  # Update just the target attributes weight -> 0
  DMCopyj["weight",attr_j] <- DMCopyj["weight",attr_j] - step
  
  # Update every other attribute's weight in the DM
  DMCopyj["weight",][which(names(DMCopyj) != attr_j)] <- DMCopyj["weight",][which(names(DMCopyj) != attr_j)]+split_step
  return(DMCopyj)
}

#'
#'
#'
#' Helper function for the default_sensitivity(...) and custom_sensitivity(...) function
#' 
#' @author Blake Conrad
#' @param DMCopyj, read.data.matrix(data.frame) -- data.frame in data matrix format.
#' @param attr, charater -- a single specified attribute within DMCopyj
#' @param step, double -- is the steps you want an attribute that is being increased
#' @param split_step, double -- give a number of attributes in a decision matrix the formula for this is: step/(1-N), this is meant to be 
#' decrementally added to an attributes weight during the sensitivity database building phase.
#' @return DMCopyj, read.data.matrix(data.frame) -- data.frame in data matrix format. This is an updated version of the input DMCopyj which has adjusted weights now from the decrease in step and addition from split_step.
#' 
#' 
#' 
increaseAttribute <- function(DMCopyj,
                              attr_j,
                              step,
                              split_step){
  
  # Update just the target attributes weight -> 0
  DMCopyj["weight",attr_j] <- DMCopyj["weight",attr_j] + step
  
  # Update every other attribute's weight in the DM
  DMCopyj["weight",][which(names(DMCopyj) != attr_j)] <- DMCopyj["weight",][which(names(DMCopyj) != attr_j)]-split_step
  return(DMCopyj)
}

#' Helper for sensitivity'
#' 
#' @author Blake Conrad
#' @param DMCopy, a read.data.matrix style data frame, expected to be brought in from default_sensitivity(...)
#' @param DB, data.frame ,the 'database' or most updated dataframe object that is storing each iterations results. This function will append to it and return it.
#' @param alg, character, specified as 'TOPSIS' or "MAUT' in version 1.0.0
#' @param algParams, list, each named item in the list is a specific parameter to the alg param specified.
#' @param attr_i, character, a specific column name in the DMCopy which is being sensitively analyzed.
#' @param iterid, integer, meant to keep track of indices for the DB.
#' @param verbose, boolean, useful if TRUE for debugging.
#' 
#' 
updateDB <- function(DMCopy, 
                     DB,
                     alg,
                     algParams,
                     attr_i, 
                     iterid,
                     verbose){
  
  
  # Constants to store
  weights <- as.list(t(DMCopy["weight",]))
  names(weights) <- row.names(t(DMCopy["weight",]))
  res <- data.frame()
  
  
  # Run the appropriate algorithm
  if(alg == "TOPSIS") 
    res <- TOPSIS(data=DMCopy, algParams=algParams, verbose=verbose)
  else if(alg == "MAUT")
    res <- MAUT(data=DMCopy, algParams=algParams, verbose=verbose)
  else{
    cat("Invalid algs supplied.\n")
  }
  
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
custom_sensitivity <- function(DM,
                               DB,
                               step, 
                               attr, 
                               splitPercentages,
                               verbose){
  
  cat("Custom sensitivity beginning.\n")
  cat("Custom sensitivity not yet established ... \n")
  cat("Custom sensitivity finished.\n")
  
  return(data.frame())
}