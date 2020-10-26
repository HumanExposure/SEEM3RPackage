#'Calculate the Geometric Mean of PRoduction Volume Bins
#'
#'@param x A string describing the production volume
#'  
#'@return A numeric value that is the geometric mean of the bin
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
CDR.to.geometric.mean <- function(x)
{
  if (x == "Withheld")
 # Assume somewhere between 500,000 lbs and 1 billion lbs):
    return(exp((log(25000)+log(5*10^11))/2)/2.204/365.25)
  else if (x == "< 25,000 lb")
    return(25000/2/2.204/365.25)
  else if (x == ">200,000,000,000 lb")
    return(5*10^11/2.204/365.25)
  else if (regexpr(" - ",x)!=-1)
  {
    temp <- strsplit(x," ")[[1]]
    lower.bound <- as.numeric(gsub(",","",temp[1]))
    upper.bound <- as.numeric(gsub(",","",temp[3]))
    return(exp((log(lower.bound)+log(upper.bound))/2)/2.204/365.25)
  } else print(CDR.HPV[this.row,"CHEM_AGG_P"])
}