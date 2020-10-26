#' This is a function for calculate SEEM in parallel
#'
#'@param x A vector of chemical descriptors
#'  
#'@return S
#'
#'@details 
#'
#'@examples 

#'
#'@author John Wambaugh 
#'
#'@references Ring, Caroline L., et al. 
#' "Consensus modeling of median chemical intake for the US population based on
#' predictions of exposure pathways." 
#' Environmental Science & Technology 53.2 (2018): 719-732.
#'
fast_seem <- function(x)
{
  delta.vec <- unlist(chem.preds[dsstox_substance_id==x,deltas,with=F])
  names(delta.vec) <- PATHWAY.NAMES
  # replicate predictions based on number of times they occur in each pathway:
  pred.vec <- unlist(chem.preds[dsstox_substance_id==x, 
    model.preds,with=F])[wmdl]
  chem.delta.draws <- matrix(NA,nrow=num.draws,ncol=NUM.PATHWAYS)
  colnames(chem.delta.draws) <- PATHWAY.NAMES
  for (i in PATHWAY.NAMES)
  {
    chem.delta.draws[,i] <- rbinom(num.draws,1,delta.vec[i])
  }
  means <- apply(chem.delta.draws*sub.samp[,2:(1+NUM.PATHWAYS)],1,sum)
  weightedpreds <-as.matrix(
    chem.delta.draws[,wpath] *
    sub.samp[,(2+NUM.PATHWAYS):(1+NUM.PATHWAYS+NUM.NONZERO.WEIGHTS)]) %*%
    pred.vec
  return(sub.samp[,1]+means+weightedpreds)
}